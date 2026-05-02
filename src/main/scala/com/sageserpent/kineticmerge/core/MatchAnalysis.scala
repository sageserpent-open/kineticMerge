package com.sageserpent.kineticmerge.core

import alleycats.std.set.given
import cats.collections.{Diet, Range as CatsInclusiveRange}
import cats.data.State
import cats.implicits.catsKernelOrderingForOrder
import cats.instances.seq.*
import cats.syntax.all.*
import cats.{Eq, FlatMap}
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import com.google.common.hash.{Funnel, HashFunction, PrimitiveSink}
import com.sageserpent.kineticmerge
import com.sageserpent.kineticmerge.core.MatchAnalysis.{
  GenericMatch,
  MatchedSections
}
import com.sageserpent.kineticmerge.{
  NoProgressRecording,
  ProgressRecording,
  ProgressRecordingSession,
  core
}
import com.typesafe.scalalogging.StrictLogging
import de.sciss.fingertree.RangedSeq

import java.lang.Byte as JavaByte
import scala.annotation.tailrec
import scala.collection.immutable.{
  MultiDict,
  SortedMap,
  SortedMultiSet,
  SortedSet
}
import scala.collection.parallel.CollectionConverters.*
import scala.collection.{immutable, mutable}
import scala.util.{Success, Using}

trait MatchAnalysis[Path, Element]:
  def withAllSmallFryMatches(): MatchAnalysis[Path, Element]

  def parallelMatchesOnly: MatchAnalysis[Path, Element]

  def reconcileMatches: MatchAnalysis[Path, Element]

  def withoutRedundantPairwiseMatches: MatchAnalysis[Path, Element]

  def purgedOfMatchesWithOverlappingSections(
      enabled: Boolean
  ): MatchAnalysis[Path, Element]

  def tinyMatchesOnly(): MatchAnalysis[Path, Element]

  def baseSections: Set[Section[Element]]

  def leftSections: Set[Section[Element]]

  def rightSections: Set[Section[Element]]

  def sectionsAndTheirMatches: MatchedSections[Element]

  def matches: Set[GenericMatch[Element]] =
    sectionsAndTheirMatches.values.toSet
end MatchAnalysis

object MatchAnalysis extends StrictLogging:
  type GenericMatch[Element]    = Match[Section[Element]]
  type MatchedSections[Element] =
    MultiDict[Section[Element], GenericMatch[Element]]

  /** Analyse matches between the sources of {@code base}, {@code left} and
    * {@code right}.
    *
    * @param baseSources
    *   The common base sources from which the left and right sources are
    *   derived.
    * @param leftSources
    *   'Our' sources, from the Git standpoint...
    * @param rightSources
    *   'Their' sources, from the Git standpoint...
    * @param configuration
    *   [[Configuration]] parameter object.
    * @tparam Path
    * @return
    *   A [[MatchAnalysis]] that contains a breakdown into [[GenericMatch]]
    *   instances.
    */
  def of[Path, Element: Eq: Funnel](
      baseSources: Sources[Path, Element],
      leftSources: Sources[Path, Element],
      rightSources: Sources[Path, Element]
  )(configuration: AbstractConfiguration)(using
      hashFunction: HashFunction
  ): MatchAnalysis[Path, Element] =
    // Yes, this entire method could be moved into `AbstractConfiguration`, but
    // the level of indentation of quite a lot of code would be increased.
    // Anyway, it's only configuration, after all - performing the analysis
    // belongs to the companion object for `MatchAnalysis`.
    import configuration.*

    val newline = "\n"

    val baseSizesByPath = baseSources.filesByPath.map { case (path, file) =>
      path -> file.size
    }

    logger.debug(
      s"Analysis considering base paths:\n\n${baseSizesByPath.mkString(newline)}\n"
    )

    val leftSizesByPath = leftSources.filesByPath.map { case (path, file) =>
      path -> file.size
    }

    logger.debug(
      s"Analysis considering left paths:\n\n${leftSizesByPath.mkString(newline)}\n"
    )

    val rightSizesByPath = rightSources.filesByPath.map { case (path, file) =>
      path -> file.size
    }

    logger.debug(
      s"Analysis considering right paths:\n\n${rightSizesByPath.mkString(newline)}\n"
    )

    val fileSizes = SortedMultiSet.from(
      baseSizesByPath.values ++ leftSizesByPath.values ++ rightSizesByPath.values
    )

    val totalContentSize = fileSizes.sum

    val minimumFileSizeAcrossAllFilesOverAllSides =
      fileSizes.headOption.getOrElse(0)

    def thresholdSizeForMatching(fileSize: Int) =
      (thresholdSizeFractionForMatching * fileSize).floor.toInt

    // This is the minimum window size that would be allowed in *some* file
    // across the sources.
    val minimumWindowSizeAcrossAllFilesOverAllSides =
      minimumMatchSize max thresholdSizeForMatching(
        minimumFileSizeAcrossAllFilesOverAllSides
      )

    // The penultimate largest file size from the three sides is the largest
    // potential match size - because a match has to span at least two sides.
    val maximumPossibleMatchSize =
      Seq(
        baseSizesByPath.values.maxOption,
        leftSizesByPath.values.maxOption,
        rightSizesByPath.values.maxOption
      ).flatten.sorted(Ordering[Int].reverse).take(2).lastOption.getOrElse(0)

    val maximumFileSizeAcrossAllFilesOverAllSides =
      fileSizes.lastOption.getOrElse(0)

    // This is the minimum window size that would be allowed in *all* files
    // across the sources.
    val minimumSureFireWindowSizeAcrossAllFilesOverAllSides =
      maximumPossibleMatchSize min (minimumMatchSize max thresholdSizeForMatching(
        maximumFileSizeAcrossAllFilesOverAllSides
      ))

    logger.debug(
      s"Minimum match window size across all files over all sides: $minimumWindowSizeAcrossAllFilesOverAllSides."
    )
    logger.debug(
      s"Minimum sure-fire match window size across all files over all sides: $minimumSureFireWindowSizeAcrossAllFilesOverAllSides."
    )
    logger.debug(
      s"Maximum match window size across all files over all sides: $maximumPossibleMatchSize."
    )
    logger.debug(
      s"File sizes across all files over all sides: $fileSizes."
    )

    type PairwiseMatch = Match.BaseAndLeft[Section[Element]] |
      Match.BaseAndRight[Section[Element]] |
      Match.LeftAndRight[Section[Element]]

    type ParedDownMatch[MatchType <: GenericMatch[Element]] = MatchType match
      case GenericMatch[Element] => GenericMatch[Element]
      case PairwiseMatch         => PairwiseMatch

    type SectionsSeen = RangedSeq[Section[Element], Int]

    type FingerprintedInclusions = Diet[Int]

    type ParallelMatchesGroupId = Int

    type ParallelMatchesGroupIdsByMatch =
      Map[GenericMatch[Element], ParallelMatchesGroupId]

    val tiebreakContentSamplingLimit = 5

    object MatchesAndTheirSections:
      type ParallelMatchesGroupIdTracking[X] =
        State[ParallelMatchesGroupIdsByMatch, X]

      lazy val empty: MatchesAndTheirSections = MatchesAndTheirSections(
        baseSectionsByPath = Map.empty,
        leftSectionsByPath = Map.empty,
        rightSectionsByPath = Map.empty,
        sectionsAndTheirMatches = MultiDict.empty,
        baseFingerprintedInclusionsByPath =
          fingerprintedInclusionsByPath(baseSources),
        leftFingerprintedInclusionsByPath =
          fingerprintedInclusionsByPath(leftSources),
        rightFingerprintedInclusionsByPath =
          fingerprintedInclusionsByPath(rightSources),
        parallelMatchesGroupIdsByMatch = Map.empty
      )
      private val rollingHashFactoryCache: Cache[Int, RollingHash.Factory] =
        Caffeine.newBuilder().build()

      def withAllMatchesOfAtLeastTheSureFireWindowSize()
          : MatchesAndTheirSections =
        if 0 < maximumPossibleMatchSize then
          Using(
            progressRecording.newSession(
              label = "Minimum match size considered:",
              maximumProgress = maximumPossibleMatchSize
            )(initialProgress = maximumPossibleMatchSize)
          ) { progressRecordingSession =>
            given MatchSearchingContext =
              new MatchSearchingContext:
                override val minimumMatchSizeConsidered: Int =
                  minimumSureFireWindowSizeAcrossAllFilesOverAllSides
                override def thresholdSize(fileSize: Int): Int =
                  thresholdSizeForMatching(fileSize)

            withAllMatches(
              matchesAndTheirSections = empty,
              looseExclusiveUpperBoundOnMaximumMatchSize =
                1 + maximumPossibleMatchSize
            )(using progressRecordingSession)
          }.get
        else empty
      end withAllMatchesOfAtLeastTheSureFireWindowSize

      given Ordering[BiteEdge] = Ordering.fromLessThan {
        case (
              BiteEdge.Start(startOffset),
              BiteEdge.End(onePastEndOffset)
            ) =>
          // Bites should have positive width, so that way a start comes
          // strictly before the end of its bite.
          startOffset < onePastEndOffset
        case (
              BiteEdge.End(onePastEndOffset),
              BiteEdge.Start(startOffset)
            ) =>
          // If two bites abut, then the end position of the preceding bite
          // will be equal to the start position of the following bite.
          onePastEndOffset <= startOffset
        case (
              BiteEdge.Start(firstStartOffset),
              BiteEdge.Start(secondStartOffset)
            ) =>
          // Bites should have positive width, so this is a strict equality.
          firstStartOffset < secondStartOffset
        case (
              BiteEdge.End(firstOnePastEndOffset),
              BiteEdge.End(secondOnePastEndOffset)
            ) =>
          // Bites should have positive width, so this is a strict equality.
          firstOnePastEndOffset < secondOnePastEndOffset
      }

      private def fingerprintedInclusionsByPath(
          sources: Sources[Path, Element]
      ): Map[Path, FingerprintedInclusions] = sources.filesByPath.collect {
        case (path, file) if 0 < file.size =>
          // To start with, any non-empty file is completely covered by one
          // fingerprinted inclusion.
          path -> Diet.fromRange(
            CatsInclusiveRange(start = 0, end = file.size - 1)
          )
      }

      private def knockOutFromFingerprintedInclusions(
          sources: Sources[Path, Element],
          fingerprintedInclusionsByPath: Map[Path, FingerprintedInclusions]
      )(
          knockedOut: Section[Element]
      ): Map[Path, FingerprintedInclusions] =
        val path = sources.pathFor(knockedOut)

        fingerprintedInclusionsByPath.updatedWith(path)(
          _.map(
            _.removeRange(
              CatsInclusiveRange(
                start = knockedOut.startOffset,
                end = knockedOut.onePastEndOffset - 1
              )
            )
          )
        )
      end knockOutFromFingerprintedInclusions

      private def reinstateInFingerprintedInclusions(
          sources: Sources[Path, Element],
          fingerprintedInclusionsByPath: Map[Path, FingerprintedInclusions]
      )(
          reinstated: Section[Element]
      ): Map[Path, FingerprintedInclusions] =
        val path = sources.pathFor(reinstated)

        fingerprintedInclusionsByPath.updatedWith(path)(
          _.map(
            _.addRange(
              CatsInclusiveRange(
                start = reinstated.startOffset,
                end = reinstated.onePastEndOffset - 1
              )
            )
          )
        )
      end reinstateInFingerprintedInclusions

      @tailrec
      private final def withAllMatches(
          matchesAndTheirSections: MatchesAndTheirSections,
          looseExclusiveUpperBoundOnMaximumMatchSize: Int
      )(using
          progressRecordingSession: ProgressRecordingSession,
          matchSearchingContext: MatchSearchingContext
      ): MatchesAndTheirSections =
        import matchSearchingContext.*

        // Essentially a binary chop algorithm, but using
        // `fallbackImprovedState` to track the best solution.
        @tailrec
        def keepTryingToImproveThis(
            bestMatchSize: Int,
            looseExclusiveUpperBoundOnMaximumMatchSize: Int,
            guessAtOptimalMatchSize: Option[Int],
            fallbackImprovedState: MatchesAndTheirSections,
            pathInclusions: PathInclusions
        ): MatchesAndTheirSections =
          require(
            bestMatchSize < looseExclusiveUpperBoundOnMaximumMatchSize
          )

          guessAtOptimalMatchSize.foreach(guess =>
            require(looseExclusiveUpperBoundOnMaximumMatchSize > guess)
          )

          if 1 + bestMatchSize < looseExclusiveUpperBoundOnMaximumMatchSize
          then
            // There is at least one candidate window size greater than
            // `bestMatchSize`...
            val candidateWindowSize = guessAtOptimalMatchSize
              .getOrElse {
                // Speculative optimisation - if the largest file was
                // modified on just one side (or coincidentally added as
                // exact duplicates on both sides), then we may as well go
                // straight to it to avoid the cost of working back up to
                // that matching size with lots of overlapping matches.
                val potentialFullMatchSize = for
                  largestPertinentFileSize <- fileSizes
                    .rangeFrom(bestMatchSize)
                    .rangeTo(looseExclusiveUpperBoundOnMaximumMatchSize)
                    .lastOption
                  largestFileMightBeUnchangedOnOneSideOrCoincidentallyAddedAsInDuplicate =
                    1 < fileSizes.get(largestPertinentFileSize)
                  if largestFileMightBeUnchangedOnOneSideOrCoincidentallyAddedAsInDuplicate
                yield largestPertinentFileSize

                val bisectedSize =
                  (bestMatchSize + looseExclusiveUpperBoundOnMaximumMatchSize) / 2
                potentialFullMatchSize.fold(ifEmpty = bisectedSize)(
                  _ max bisectedSize
                )
              }

            val MatchingResult(
              stateAfterTryingCandidate,
              numberOfMatchesForTheGivenWindowSize,
              estimatedWindowSizeForOptimalMatch,
              pathInclusionsAfterTryingCandidate
            ) = matchesAndTheirSections.matchesForWindowSize(
              candidateWindowSize,
              pathInclusions,
              thresholdSize
            )

            estimatedWindowSizeForOptimalMatch match
              case None =>
                // Failed to improve the match size, try again with the
                // contracted upper bound.
                keepTryingToImproveThis(
                  bestMatchSize = bestMatchSize,
                  looseExclusiveUpperBoundOnMaximumMatchSize =
                    candidateWindowSize,
                  guessAtOptimalMatchSize = None,
                  fallbackImprovedState = fallbackImprovedState,
                  pathInclusions = pathInclusions
                )
              case Some(estimate)
                  if estimate == candidateWindowSize || 1 == numberOfMatchesForTheGivenWindowSize =>
                // Found the optimal solution; try searching for the next
                // lowest optimal size. NOTE: this won't pick up multiple
                // distinct optimal matches, see below.
                logger.debug(
                  s"Search has found an optimal match at window size: $candidateWindowSize, number of matches is: $numberOfMatchesForTheGivenWindowSize, restarting search to look for smaller matches."
                )
                progressRecordingSession.upTo(candidateWindowSize)
                withAllMatches(
                  stateAfterTryingCandidate,
                  looseExclusiveUpperBoundOnMaximumMatchSize =
                    candidateWindowSize
                )
              case Some(estimate) =>
                // We have an improvement, move the lower bound up and note
                // the improved state.

                if looseExclusiveUpperBoundOnMaximumMatchSize > estimate then
                  logger.debug(
                    s"Search has found an improved match at window size: $candidateWindowSize, number of matches is: $numberOfMatchesForTheGivenWindowSize, looking for a more optimal match with estimated window size of: $estimate."
                  )
                  keepTryingToImproveThis(
                    bestMatchSize = candidateWindowSize,
                    looseExclusiveUpperBoundOnMaximumMatchSize,
                    guessAtOptimalMatchSize = Some(estimate),
                    fallbackImprovedState = stateAfterTryingCandidate,
                    pathInclusions = pathInclusionsAfterTryingCandidate
                  )
                else
                  logger.debug(
                    s"Search has found an improved match at window size: $candidateWindowSize, number of matches is: $numberOfMatchesForTheGivenWindowSize, looking for a more optimal match."
                  )
                  keepTryingToImproveThis(
                    bestMatchSize = candidateWindowSize,
                    looseExclusiveUpperBoundOnMaximumMatchSize,
                    guessAtOptimalMatchSize = None,
                    fallbackImprovedState = stateAfterTryingCandidate,
                    pathInclusions = pathInclusionsAfterTryingCandidate
                  )
                end if
            end match
          else if minimumMatchSizeConsidered == looseExclusiveUpperBoundOnMaximumMatchSize
          then
            // There is nowhere left to search.
            logger.debug(
              s"Search for matches whose size is no less than: $minimumMatchSizeConsidered has terminated; results are:\n${pprintCustomised(fallbackImprovedState)}"
            )
            progressRecordingSession.upTo(minimumMatchSizeConsidered)
            fallbackImprovedState
          else
            // The optimal matches are in the fallback improved state; try
            // searching for the next lowest optimal size. This is necessary
            // as we may have *multiple* distinct optimal matches at a given
            // window size.
            logger.debug(
              s"Search has found optimal matches at window size: $bestMatchSize, restarting search to look for smaller matches."
            )
            progressRecordingSession.upTo(bestMatchSize)
            withAllMatches(
              fallbackImprovedState,
              looseExclusiveUpperBoundOnMaximumMatchSize = bestMatchSize
            )
          end if
        end keepTryingToImproveThis

        keepTryingToImproveThis(
          bestMatchSize = minimumMatchSizeConsidered - 1,
          looseExclusiveUpperBoundOnMaximumMatchSize,
          guessAtOptimalMatchSize = None,
          fallbackImprovedState = matchesAndTheirSections,
          pathInclusions = PathInclusions.all
        )
      end withAllMatches

      // NOTE: this is partially applied in the class so that it means: "is
      // there anything on the given side that subsumes `section`".
      private def subsumes(
          side: Sources[Path, Element],
          sectionsByPath: Map[Path, SectionsSeen]
      )(section: Section[Element]): Boolean =
        val trivialSubsumptionSize = section.size

        sectionsByPath
          .get(side.pathFor(section))
          .fold(ifEmpty = false)(
            _.filterIncludes(section.closedOpenInterval)
              .exists(trivialSubsumptionSize < _.size)
          )
      end subsumes

      private def subsumingPairwiseMatchesIncludingTriviallySubsuming(
          sectionsAndTheirMatches: MatchedSections[Element]
      )(
          side: Sources[Path, Element],
          sectionsByPath: Map[Path, SectionsSeen]
      )(section: Section[Element]): Set[PairwiseMatch] =
        subsumingMatches(sectionsAndTheirMatches)(
          side,
          sectionsByPath
        )(section, includeTrivialSubsumption = true)
          .collect {
            case baseAndLeft: Match.BaseAndLeft[Section[Element]] =>
              baseAndLeft: PairwiseMatch
            case baseAndRight: Match.BaseAndRight[Section[Element]] =>
              baseAndRight: PairwiseMatch
            case leftAndRight: Match.LeftAndRight[Section[Element]] =>
              leftAndRight: PairwiseMatch
          }
      end subsumingPairwiseMatchesIncludingTriviallySubsuming

      private def subsumingMatches(
          sectionsAndTheirMatches: MatchedSections[Element]
      )(
          side: Sources[Path, Element],
          sectionsByPath: Map[Path, SectionsSeen]
      )(
          section: Section[Element],
          includeTrivialSubsumption: Boolean
      ): Set[GenericMatch[Element]] =
        val trivialSubsumptionSize = section.size

        sectionsByPath
          .get(side.pathFor(section))
          .fold(ifEmpty = Set.empty)(
            _.filterIncludes(section.closedOpenInterval)
              // NOTE: convert to a set at this point as we expect sections to
              // be duplicated when involved in ambiguous matches.
              .toSet
              .filter(
                includeTrivialSubsumption || trivialSubsumptionSize < _.size
              )
              .flatMap(sectionsAndTheirMatches.get)
          )
      end subsumingMatches

      // NOTE: this is partially applied in the class so that it means: "is
      // there anything on the given side that overlaps or is subsumed by
      // `section`".
      private def overlapsOrIsSubsumedBy(
          side: Sources[Path, Element],
          sectionsByPath: Map[Path, SectionsSeen]
      )(section: Section[Element]): Boolean =
        sectionsByPath
          .get(side.pathFor(section))
          .fold(ifEmpty = false)(
            _.filterOverlaps(section.closedOpenInterval)
              // Subsuming sections are considered to be overlapping by the
              // implementation of `SectionSeen.filterOverlaps`, so use an
              // existence check with a nuanced predicate.
              .exists(candidateSection =>
                candidateSection.startOffset > section.startOffset || candidateSection.onePastEndOffset < section.onePastEndOffset
              )
          )
      end overlapsOrIsSubsumedBy

      private def including(
          side: Sources[Path, Element],
          sectionsByPath: Map[Path, SectionsSeen]
      )(
          section: Section[Element]
      ): Map[Path, SectionsSeen] =
        sectionsByPath.updatedWith(
          side.pathFor(section)
        ) {
          case Some(sections) =>
            // Allow the same section to be added more than once, on behalf of
            // ambiguous matches.
            Some(sections + section)
          case None =>
            Some(
              // NOTE: don't use `Ordering[Int]` as while that is valid, it will
              // pull in a Cats `Order[Int]` which round-trips back to an
              // `Ordering`. That makes profiling difficult.
              RangedSeq(section)(_.closedOpenInterval, Ordering.Int)
            )
        }
      end including

      private def excluding(
          side: Sources[Path, Element],
          sectionsByPath: Map[Path, SectionsSeen]
      )(
          section: Section[Element]
      ): Map[Path, SectionsSeen] =
        sectionsByPath.updatedWith(
          side.pathFor(section)
        ) { case Some(sections) =>
          // Allow the same section to be removed more than once, on behalf of
          // ambiguous matches.
          val withoutSection = sections - section
          Option.unless(withoutSection.isEmpty)(withoutSection)
        }
      end excluding

      // When the window size used to calculate matches is lower than the
      // optimal match size, overlapping matches will be made that cover the
      // elements of the optimal match. Estimate the size of the optimal match
      // by coalescing the overlaps.
      private def estimateOptimalMatchSize(
          matches: collection.Set[GenericMatch[Element]]
      ): Option[Int] =
        val (baseSections, leftSections, rightSections) = matches.toSeq.map {
          case Match.AllSides(baseSection, leftSection, rightSection) =>
            (Some(baseSection), Some(leftSection), Some(rightSection))
          case Match.BaseAndLeft(baseSection, leftSection) =>
            (Some(baseSection), Some(leftSection), None)
          case Match.BaseAndRight(baseSection, rightSection) =>
            (Some(baseSection), None, Some(rightSection))
          case Match.LeftAndRight(leftSection, rightSection) =>
            (None, Some(leftSection), Some(rightSection))

        }.unzip3

        def sectionsGroupedByPathAndSorted(
            sources: Sources[Path, Element],
            sections: Seq[Section[Element]]
        ) =
          sections
            .groupBy(sources.pathFor)
            .map((_, sharingTheSamePath) =>
              sharingTheSamePath.sortBy(_.startOffset)
            )

        val sectionGroupsForAllPathsAcrossAllSides =
          sectionsGroupedByPathAndSorted(
            baseSources,
            baseSections.flatten
          ) ++ sectionsGroupedByPathAndSorted(
            leftSources,
            leftSections.flatten
          ) ++ sectionsGroupedByPathAndSorted(
            rightSources,
            rightSections.flatten
          )

        sectionGroupsForAllPathsAcrossAllSides
          .flatMap(maximumSizeOfCoalescedSections)
          .maxOption
      end estimateOptimalMatchSize

      // Coalesces runs of overlapping sections together and reports the size of
      // the largest coalescence.
      private def maximumSizeOfCoalescedSections(
          sectionsInOrderOfStartOffset: Seq[Section[Element]]
      ): Option[Int] =
        case class PutativeCoalescence(
            startOffset: Int,
            onePastEndOffset: Int
        )

        @tailrec
        def maximumSizeOfCoalescedSections(
            sections: Seq[Section[Element]],
            putativeCoalescence: Option[PutativeCoalescence],
            partialResult: Option[Int]
        ): Option[Int] =
          if sections.isEmpty then
            (partialResult ++ putativeCoalescence.map {
              case PutativeCoalescence(startOffset, onePastEndIndex) =>
                onePastEndIndex - startOffset
            }).maxOption
          else
            val head = sections.head

            putativeCoalescence match
              case Some(
                    PutativeCoalescence(startOffset, onePastEndOffset)
                  ) =>
                assume(head.startOffset >= startOffset)

                if head.startOffset > onePastEndOffset then
                  // The head section neither overlaps nor abuts the
                  // putative coalescence, so tally the size of the now
                  // finalised coalescence and start a new one that
                  // encompasses just the head section.
                  val size = onePastEndOffset - startOffset

                  maximumSizeOfCoalescedSections(
                    sections.tail,
                    putativeCoalescence = Some(
                      PutativeCoalescence(
                        head.startOffset,
                        head.onePastEndOffset
                      )
                    ),
                    partialResult =
                      partialResult.map(_ max size).orElse(Some(size))
                  )
                else
                  // The head section extends the putative coalescence.
                  maximumSizeOfCoalescedSections(
                    sections.tail,
                    putativeCoalescence = Some(
                      PutativeCoalescence(
                        startOffset,
                        head.onePastEndOffset max onePastEndOffset
                      )
                    ),
                    partialResult = partialResult
                  )
                end if

              case None =>
                // Start a new putative coalescence that encompasses just
                // the head section.
                maximumSizeOfCoalescedSections(
                  sections.tail,
                  putativeCoalescence = Some(
                    PutativeCoalescence(
                      head.startOffset,
                      head.onePastEndOffset
                    )
                  ),
                  partialResult = partialResult
                )
            end match
          end if
        end maximumSizeOfCoalescedSections

        maximumSizeOfCoalescedSections(
          sectionsInOrderOfStartOffset,
          putativeCoalescence = None,
          partialResult = None
        )
      end maximumSizeOfCoalescedSections

      private def fragmentsOf(
          pairwiseMatchesToBeEaten: MultiDict[
            PairwiseMatch,
            (Match.AllSides[Section[Element]], BiteEdge, BiteEdge)
          ]
      ): ParallelMatchesGroupIdTracking[Set[PairwiseMatch]] =
        pairwiseMatchesToBeEaten.sets.toSeq
          .flatTraverse { case (pairwiseMatch, bites) =>
            for
              parallelMatchesGroupIdsByMatch <- State
                .get[ParallelMatchesGroupIdsByMatch]

              groupIdsBySortedBiteEdge = SortedMap.from(bites.flatMap {
                case (allSides, biteStart, biteEnd) =>
                  Seq(biteStart, biteEnd)
                    .map(_ -> parallelMatchesGroupIdsByMatch(allSides))
              })

              sortedBiteEdges = groupIdsBySortedBiteEdge.keySet

              fragmentsFromPairwiseMatch <-
                sortedBiteEdges.eatIntoPairwiseMatch(
                  pairwiseMatch,
                  groupIdsBySortedBiteEdge
                )
            yield
              logger.debug(
                s"Eating into pairwise match:\n${pprintCustomised(pairwiseMatch)} on behalf of all-sides matches:\n${pprintCustomised(bites)}, resulting in fragments:\n${pprintCustomised(fragmentsFromPairwiseMatch)}"
              )

              fragmentsFromPairwiseMatch
          }
          .map(_.toSet)

      // There are preconditions buried in the implementations that require the
      // bite edges to be sorted in terms of their offsets and not exceed the
      // section's boundaries.
      extension (biteEdges: SortedSet[BiteEdge])
        private def eatIntoPairwiseMatch(
            pairwiseMatch: PairwiseMatch,
            groupIdsBySortedBiteEdge: Map[BiteEdge, ParallelMatchesGroupId]
        ): ParallelMatchesGroupIdTracking[
          Vector[PairwiseMatch]
        ] =
          // NOTE: here we work with zero-relative offsets from the start of the
          // meal, thus we can work directly with the offsets from the bite
          // edges.

          val (firstSide, firstSection, secondSide, secondSection, factory) =
            (pairwiseMatch match
              case Match.BaseAndLeft(baseSection, leftSection) =>
                (
                  baseSources,
                  baseSection,
                  leftSources,
                  leftSection,
                  Match.BaseAndLeft.apply[Section[Element]]
                )
              case Match.BaseAndRight(baseSection, rightSection) =>
                (
                  baseSources,
                  baseSection,
                  rightSources,
                  rightSection,
                  Match.BaseAndRight.apply[Section[Element]]
                )
              case Match.LeftAndRight(leftSection, rightSection) =>
                (
                  leftSources,
                  leftSection,
                  rightSources,
                  rightSection,
                  Match.LeftAndRight.apply[Section[Element]]
                )
            ): (
                Sources[Path, Element],
                Section[Element],
                Sources[Path, Element],
                Section[Element],
                (Section[Element], Section[Element]) => PairwiseMatch
            )

          val firstPath  = firstSide.pathFor(firstSection)
          val secondPath = secondSide.pathFor(secondSection)

          val sectionSize = firstSection.size
          assume(sectionSize == secondSection.size)

          case class RecursionState(
              deferredGroupIdFromPrecedingBite: Option[ParallelMatchesGroupId],
              mealStartOffsetRelativeToMeal: Int,
              biteDepth: Int,
              remainingBiteEdges: Seq[BiteEdge],
              fragments: Vector[PairwiseMatch]
          ):
            final def biteEdgeStep: ParallelMatchesGroupIdTracking[
              Either[RecursionState, Vector[PairwiseMatch]]
            ] =
              remainingBiteEdges match
                case Seq() =>
                  if sectionSize > mealStartOffsetRelativeToMeal then
                    val size = sectionSize - mealStartOffsetRelativeToMeal

                    for
                      assignedGroupId <- deferredGroupIdFromPrecedingBite
                        .fold(ifEmpty = freshGroupId)(State.pure)

                      fragment = factory(
                        firstSide.section(firstPath)(
                          firstSection.startOffset + mealStartOffsetRelativeToMeal,
                          size
                        ),
                        secondSide.section(secondPath)(
                          secondSection.startOffset + mealStartOffsetRelativeToMeal,
                          size
                        )
                      )

                      _ <- assignGroupId(fragment, assignedGroupId)
                    yield Right(fragments.appended(fragment))
                    end for
                  else State.pure(Right(fragments))

                case Seq(
                      biteEdge @ BiteEdge.Start(startOffsetRelativeToMeal),
                      tail*
                    ) =>
                  require(
                    startOffsetRelativeToMeal >= mealStartOffsetRelativeToMeal
                  )
                  require(sectionSize > startOffsetRelativeToMeal)

                  for updatedFragments <-
                      if 0 == biteDepth && startOffsetRelativeToMeal > mealStartOffsetRelativeToMeal
                      then
                        val size =
                          startOffsetRelativeToMeal - mealStartOffsetRelativeToMeal

                        val groupIdFromSucceedingBite =
                          groupIdsBySortedBiteEdge(biteEdge)

                        val assignedGroupId =
                          deferredGroupIdFromPrecedingBite.fold(ifEmpty =
                            State.pure(groupIdFromSucceedingBite)
                          )(groupIdFromPrecedingBite =>
                            if groupIdFromPrecedingBite != groupIdFromSucceedingBite
                            then
                              // If the preceding and succeeding bite belong to
                              // different groups, we regard the fragment as
                              // 'staying put' and assign it a fresh group id.
                              // We don't reuse the group id of the original
                              // pairwise match being bitten into because that
                              // might result in multiple fragments sharing the
                              // same group id but with intervening bites
                              // belonging to other groups.
                              freshGroupId
                            else State.pure(groupIdFromSucceedingBite)
                          )

                        val fragment = factory(
                          firstSide.section(firstPath)(
                            firstSection.startOffset + mealStartOffsetRelativeToMeal,
                            size
                          ),
                          secondSide.section(secondPath)(
                            secondSection.startOffset + mealStartOffsetRelativeToMeal,
                            size
                          )
                        )

                        assignedGroupId
                          .flatMap(assignGroupId(fragment, _))
                          .as(fragments.appended(fragment))
                      else State.pure(fragments)
                  yield Left(
                    this
                      .copy(
                        deferredGroupIdFromPrecedingBite = None,
                        mealStartOffsetRelativeToMeal =
                          startOffsetRelativeToMeal,
                        biteDepth = 1 + biteDepth,
                        remainingBiteEdges = tail,
                        fragments = updatedFragments
                      )
                  )

                case Seq(
                      biteEdge @ BiteEdge.End(onePastEndOffsetRelativeToMeal),
                      tail*
                    ) =>
                  assume(0 < biteDepth)

                  require(
                    mealStartOffsetRelativeToMeal <= onePastEndOffsetRelativeToMeal
                  )
                  require(onePastEndOffsetRelativeToMeal <= sectionSize)

                  State.pure(
                    Left(
                      this
                        .copy(
                          // NOTE: nested or overlapping bites to the right
                          // overwrite any prior contribution of a group id to
                          // the *succeeding* context.
                          deferredGroupIdFromPrecedingBite =
                            Some(groupIdsBySortedBiteEdge(biteEdge)),
                          mealStartOffsetRelativeToMeal =
                            onePastEndOffsetRelativeToMeal,
                          biteDepth = biteDepth - 1,
                          remainingBiteEdges = tail,
                          fragments = fragments
                        )
                    )
                  )
              end match
            end biteEdgeStep
          end RecursionState

          FlatMap[ParallelMatchesGroupIdTracking].tailRecM(
            RecursionState(
              deferredGroupIdFromPrecedingBite = None,
              mealStartOffsetRelativeToMeal = 0,
              biteDepth = 0,
              remainingBiteEdges = biteEdges.toSeq,
              fragments = Vector.empty
            )
          )(_.biteEdgeStep)
        end eatIntoPairwiseMatch

      end extension

      private def assignGroupId(
          fragment: PairwiseMatch,
          groupId: ParallelMatchesGroupId
      ): ParallelMatchesGroupIdTracking[Unit] =
        State.modify[ParallelMatchesGroupIdsByMatch](_ + (fragment -> groupId))

      private def freshGroupId
          : ParallelMatchesGroupIdTracking[ParallelMatchesGroupId] =
        // TODO: trawling linearly through the group ids to find the maximum
        // isn't a great idea. Perhaps there should be a maximum group id too?
        State.inspect[ParallelMatchesGroupIdsByMatch, ParallelMatchesGroupId](
          _.values.maxOption.fold(ifEmpty = 0)(1 + _)
        )

      private def propagateGroupIds(
          original: GenericMatch[Element],
          replacement: GenericMatch[Element]
      ): ParallelMatchesGroupIdTracking[Unit] =
        State.modify { groupIdsByMatch =>
          val groupIds = groupIdsByMatch.get(original)

          groupIds.fold(ifEmpty = groupIdsByMatch)(groupId =>
            groupIdsByMatch + (replacement -> groupId)
          )
        }

      trait PathInclusions:
        def isIncludedOnBase(basePath: Path): Boolean

        def isIncludedOnLeft(leftPath: Path): Boolean

        def isIncludedOnRight(rightPath: Path): Boolean
      end PathInclusions

      trait MatchSearchingContext:
        val minimumMatchSizeConsidered: Int
        def thresholdSize(fileSize: Int): Int
      end MatchSearchingContext

      enum BiteEdge:
        case Start(startOffsetRelativeToMeal: Int)
        case End(onePastEndOffsetRelativeToMeal: Int)
      end BiteEdge

      case class MatchingResult(
          matchesAndTheirSections: MatchesAndTheirSections,
          numberOfMatchesForTheGivenWindowSize: Int,
          estimatedWindowSizeForOptimalMatch: Option[Int],
          pathInclusions: PathInclusions
      )

      object PathInclusions:
        val all: PathInclusions = new PathInclusions:
          override def isIncludedOnBase(
              basePath: Path
          ): Boolean = true

          override def isIncludedOnLeft(
              leftPath: Path
          ): Boolean = true

          override def isIncludedOnRight(
              rightPath: Path
          ): Boolean = true
      end PathInclusions

    end MatchesAndTheirSections

    case class MatchesAndTheirSections(
        baseSectionsByPath: Map[Path, SectionsSeen],
        leftSectionsByPath: Map[Path, SectionsSeen],
        rightSectionsByPath: Map[Path, SectionsSeen],
        sectionsAndTheirMatches: MatchedSections[Element],
        baseFingerprintedInclusionsByPath: Map[Path, FingerprintedInclusions],
        leftFingerprintedInclusionsByPath: Map[Path, FingerprintedInclusions],
        rightFingerprintedInclusionsByPath: Map[Path, FingerprintedInclusions],
        parallelMatchesGroupIdsByMatch: ParallelMatchesGroupIdsByMatch
    ) extends MatchAnalysis[Path, Element]:
      import MatchesAndTheirSections.*

      private val baseSubsumes: Section[Element] => Boolean =
        subsumes(baseSources, baseSectionsByPath)
      private val leftSubsumes: Section[Element] => Boolean =
        subsumes(leftSources, leftSectionsByPath)
      private val rightSubsumes: Section[Element] => Boolean =
        subsumes(rightSources, rightSectionsByPath)
      private val baseOverlapsOrIsSubsumedBy: Section[Element] => Boolean =
        overlapsOrIsSubsumedBy(baseSources, baseSectionsByPath)
      private val leftOverlapsOrIsSubsumedBy: Section[Element] => Boolean =
        overlapsOrIsSubsumedBy(leftSources, leftSectionsByPath)
      private val rightOverlapsOrIsSubsumedBy: Section[Element] => Boolean =
        overlapsOrIsSubsumedBy(rightSources, rightSectionsByPath)
      private val baseIncluding: Section[Element] => Map[Path, SectionsSeen] =
        including(baseSources, baseSectionsByPath)
      private val leftIncluding: Section[Element] => Map[Path, SectionsSeen] =
        including(leftSources, leftSectionsByPath)
      private val rightIncluding: Section[Element] => Map[Path, SectionsSeen] =
        including(rightSources, rightSectionsByPath)
      private val baseExcluding: Section[Element] => Map[Path, SectionsSeen] =
        excluding(baseSources, baseSectionsByPath)
      private val leftExcluding: Section[Element] => Map[Path, SectionsSeen] =
        excluding(leftSources, leftSectionsByPath)
      private val rightExcluding: Section[Element] => Map[Path, SectionsSeen] =
        excluding(rightSources, rightSectionsByPath)
      private val knockOutFromBaseFingerprintedInclusions
          : Section[Element] => Map[Path, FingerprintedInclusions] =
        knockOutFromFingerprintedInclusions(
          baseSources,
          baseFingerprintedInclusionsByPath
        )
      private val knockOutFromLeftFingerprintedInclusions
          : Section[Element] => Map[Path, FingerprintedInclusions] =
        knockOutFromFingerprintedInclusions(
          leftSources,
          leftFingerprintedInclusionsByPath
        )
      private val knockOutFromRightFingerprintedInclusions
          : Section[Element] => Map[Path, FingerprintedInclusions] =
        knockOutFromFingerprintedInclusions(
          rightSources,
          rightFingerprintedInclusionsByPath
        )
      private val reinstateInBaseFingerprintedInclusions
          : Section[Element] => Map[Path, FingerprintedInclusions] =
        reinstateInFingerprintedInclusions(
          baseSources,
          baseFingerprintedInclusionsByPath
        )
      private val reinstateInLeftFingerprintedInclusions
          : Section[Element] => Map[Path, FingerprintedInclusions] =
        reinstateInFingerprintedInclusions(
          leftSources,
          leftFingerprintedInclusionsByPath
        )
      private val reinstateInRightFingerprintedInclusions
          : Section[Element] => Map[Path, FingerprintedInclusions] =
        reinstateInFingerprintedInclusions(
          rightSources,
          rightFingerprintedInclusionsByPath
        )

      def baseSections: Set[Section[Element]] =
        baseSectionsByPath.values.flatMap(_.iterator).toSet

      def leftSections: Set[Section[Element]] =
        leftSectionsByPath.values.flatMap(_.iterator).toSet

      def rightSections: Set[Section[Element]] =
        rightSectionsByPath.values.flatMap(_.iterator).toSet

      def withAllSmallFryMatches(): MatchesAndTheirSections =
        if minimumSureFireWindowSizeAcrossAllFilesOverAllSides > minimumWindowSizeAcrossAllFilesOverAllSides
        then
          val maximumSmallFryWindowSize =
            minimumSureFireWindowSizeAcrossAllFilesOverAllSides - 1

          Using(
            progressRecording.newSession(
              label = "Minimum match size considered:",
              maximumProgress = maximumSmallFryWindowSize
            )(initialProgress = maximumSmallFryWindowSize)
          ) { progressRecordingSession =>
            withAllSmallFryMatches(maximumSmallFryWindowSize)(using
              progressRecordingSession
            )
          }.get
        else this
      end withAllSmallFryMatches

      def tinyMatchesOnly(): MatchesAndTheirSections =
        val withContentCoveredByNonTinyMatchesKnockedOut =
          val nonTinyMatches = sectionsAndTheirMatches.values.filter {
            case Match.AllSides(baseSection, _, _) =>
              minimumWindowSizeAcrossAllFilesOverAllSides <= baseSection.size
            case Match.BaseAndLeft(baseSection, _) =>
              minimumWindowSizeAcrossAllFilesOverAllSides <= baseSection.size
            case Match.BaseAndRight(baseSection, _) =>
              minimumWindowSizeAcrossAllFilesOverAllSides <= baseSection.size
            case Match.LeftAndRight(leftSection, _) =>
              minimumWindowSizeAcrossAllFilesOverAllSides <= leftSection.size
          }

          // NASTY HACK: we're knocking out both the all-sides *and* the
          // pairwise matches here, but keeping the matches in the rest of the
          // state. The idea isn't to remove them, rather to prevent further
          // matching from rediscovering them.
          nonTinyMatches.foldLeft(MatchesAndTheirSections.empty) {
            case (
                  partialResult,
                  Match.AllSides(baseSection, leftSection, rightSection)
                ) =>
              partialResult.copy(
                baseFingerprintedInclusionsByPath =
                  partialResult.knockOutFromBaseFingerprintedInclusions(
                    baseSection
                  ),
                leftFingerprintedInclusionsByPath =
                  partialResult.knockOutFromLeftFingerprintedInclusions(
                    leftSection
                  ),
                rightFingerprintedInclusionsByPath =
                  partialResult.knockOutFromRightFingerprintedInclusions(
                    rightSection
                  )
              )
            case (
                  partialResult,
                  Match.BaseAndLeft(baseSection, leftSection)
                ) =>
              partialResult.copy(
                baseFingerprintedInclusionsByPath =
                  partialResult.knockOutFromBaseFingerprintedInclusions(
                    baseSection
                  ),
                leftFingerprintedInclusionsByPath =
                  partialResult.knockOutFromLeftFingerprintedInclusions(
                    leftSection
                  )
              )
            case (
                  partialResult,
                  Match.BaseAndRight(baseSection, rightSection)
                ) =>
              partialResult.copy(
                baseFingerprintedInclusionsByPath =
                  partialResult.knockOutFromBaseFingerprintedInclusions(
                    baseSection
                  ),
                rightFingerprintedInclusionsByPath =
                  partialResult.knockOutFromRightFingerprintedInclusions(
                    rightSection
                  )
              )
            case (
                  partialResult,
                  Match.LeftAndRight(leftSection, rightSection)
                ) =>
              partialResult.copy(
                leftFingerprintedInclusionsByPath =
                  partialResult.knockOutFromLeftFingerprintedInclusions(
                    leftSection
                  ),
                rightFingerprintedInclusionsByPath =
                  partialResult.knockOutFromRightFingerprintedInclusions(
                    rightSection
                  )
              )
          }
        end withContentCoveredByNonTinyMatchesKnockedOut

        Using(
          progressRecording.newSession(
            label = "Minimum match size considered:",
            maximumProgress = minimumWindowSizeAcrossAllFilesOverAllSides
          )(initialProgress = minimumWindowSizeAcrossAllFilesOverAllSides)
        ) { progressRecordingSession =>
          given MatchSearchingContext =
            new MatchSearchingContext:
              override val minimumMatchSizeConsidered: Int   = 1
              override def thresholdSize(fileSize: Int): Int = 1

          withAllMatches(
            matchesAndTheirSections =
              withContentCoveredByNonTinyMatchesKnockedOut,
            looseExclusiveUpperBoundOnMaximumMatchSize =
              minimumWindowSizeAcrossAllFilesOverAllSides
          )(using progressRecordingSession)
        }.get
      end tinyMatchesOnly

      def purgedOfMatchesWithOverlappingSections(
          enabled: Boolean
      ): MatchesAndTheirSections =
        def overlapsWithSomethingElse(aMatch: GenericMatch[Element]): Boolean =
          // NOTE: the invariant already guarantees that nothing will be
          // subsumed by the match's sections, so this is only testing for
          // overlaps.
          aMatch match
            case Match.AllSides(baseSection, leftSection, rightSection) =>
              baseOverlapsOrIsSubsumedBy(baseSection) ||
              leftOverlapsOrIsSubsumedBy(
                leftSection
              ) || rightOverlapsOrIsSubsumedBy(rightSection)
            case Match.BaseAndLeft(baseSection, leftSection) =>
              baseOverlapsOrIsSubsumedBy(baseSection) ||
              leftOverlapsOrIsSubsumedBy(
                leftSection
              )
            case Match.BaseAndRight(baseSection, rightSection) =>
              baseOverlapsOrIsSubsumedBy(
                baseSection
              ) || rightOverlapsOrIsSubsumedBy(rightSection)
            case Match.LeftAndRight(leftSection, rightSection) =>
              leftOverlapsOrIsSubsumedBy(
                leftSection
              ) || rightOverlapsOrIsSubsumedBy(rightSection)

        val overlappingMatches =
          // NOTE: have to convert to a set to remove duplicates.
          sectionsAndTheirMatches.values.toSet.filter(overlapsWithSomethingElse)

        if overlappingMatches.nonEmpty then
          if enabled then
            logger.debug(
              s"Removing overlapping matches:\n${pprintCustomised(overlappingMatches)}"
            )

            withoutTheseMatches(overlappingMatches)
          else
            throw new AdmissibleFailure(
              s"""Overlapping matches found: ${pprintCustomised(
                  overlappingMatches
                )}.
                   |Consider setting the command line parameter `--minimum-match-size` to something larger than ${overlappingMatches.map {
                  case Match.AllSides(baseSection, _, _)  => baseSection.size
                  case Match.BaseAndLeft(baseSection, _)  => baseSection.size
                  case Match.BaseAndRight(baseSection, _) => baseSection.size
                  case Match.LeftAndRight(leftSection, _) => leftSection.size
                }.min}.
                   |""".stripMargin
            )
        else this
        end if
      end purgedOfMatchesWithOverlappingSections

      def reconcileMatches: MatchesAndTheirSections =
        val matches = sectionsAndTheirMatches.values.toSet

        val Success(result) =
          Using(
            progressRecording.newSession(
              label = "Number of matches to reconcile:",
              maximumProgress = matches.size
            )(initialProgress = matches.size)
          ) { progressRecordingSession =>
            def reconcileUsing(
                allSidesMatches: Set[Match.AllSides[Section[Element]]]
            ): ParallelMatchesGroupIdTracking[
              Either[Set[
                Match.AllSides[Section[Element]]
              ], MatchesAndTheirSections]
            ] =
              val pairwiseMatchesToBeEaten: MultiDict[
                PairwiseMatch,
                (Match.AllSides[Section[Element]], BiteEdge, BiteEdge)
              ] =
                MultiDict.from(
                  allSidesMatches.flatMap(allSides =>
                    pairwiseMatchesSubsumingOnBothSidesWithBiteEdges(allSides)
                      .map { case (pairwiseMatch, biteStart, biteEnd) =>
                        pairwiseMatch -> (allSides, biteStart, biteEnd)
                      }
                  )
                )
              end pairwiseMatchesToBeEaten

              this.checkInvariant()

              for
                // Reset the state for each iteration of `reconcileUsing`. Refer
                // to the assumption below as well...
                _ <- State.set(parallelMatchesGroupIdsByMatch)

                fragments <- fragmentsOf(pairwiseMatchesToBeEaten).map(
                  _.diff(matches.asInstanceOf[Set[PairwiseMatch]])
                )

                takingFragmentationIntoAccount =
                  fragments.foldLeft(
                    withoutTheseMatches(pairwiseMatchesToBeEaten.keySet)
                  )(_ withMatch _)

                _ = takingFragmentationIntoAccount.checkInvariant()

                // NOTE: prefer `traverse` + `flatten` to `flatTraverse` as it
                // manages flattening `Option` values into an enclosing `Set`
                // nicely. The same holds a bit later on too.
                paredDownMatches <- matches
                  .traverse(
                    takingFragmentationIntoAccount.pareDownOrSuppressCompletely
                  )
                  .map(
                    _.flatten diff pairwiseMatchesToBeEaten.keySet
                      .asInstanceOf[Set[GenericMatch[Element]]]
                  )

                paredDownAllSidesMatches = paredDownMatches.collect {
                  case allSides: Match.AllSides[Section[Element]] => allSides
                }

                _ =
                  // NOTE: `pareDownOrSuppressCompletely` does not create
                  // modified all-sides matches, it always pares down to either
                  // a pairwise match or nothing at all. Advantage is taken of
                  // this when the state is reset above for each recursion step
                  // - we don't have to enrol the group ids for any modified
                  // all-sides matches.
                  assume(paredDownAllSidesMatches.subsetOf(allSidesMatches))

                stepResult <-
                  if paredDownAllSidesMatches == allSidesMatches then
                    for
                      // See note above.
                      paredDownFragments <- fragments.traverse(
                        takingFragmentationIntoAccount.pareDownOrSuppressCompletely
                      )
                      rebuilt =
                        (paredDownMatches union paredDownFragments.flatten)
                          .foldLeft(MatchesAndTheirSections.empty)(
                            _ withMatch _
                          )
                      _          = rebuilt.checkInvariant()
                      reconciled = rebuilt.withoutRedundantPairwiseMatches
                      _          = reconciled.checkInvariant()
                      _          = progressRecordingSession.upTo(0)
                      updatedParallelMatchesGroupIdsByMatch <- State.get
                    yield
                      val matches                        = reconciled.matches
                      val parallelMatchesGroupIdsByMatch =
                        updatedParallelMatchesGroupIdsByMatch
                          .filter((key, _) => matches.contains(key))

                      val compactGroupIdsKeyedByGroupsIdsWithPossibleGaps: Map[
                        ParallelMatchesGroupId,
                        ParallelMatchesGroupId
                      ] =
                        parallelMatchesGroupIdsByMatch.values.toSeq.distinct.zipWithIndex.toMap

                      val parallelMatchesWithReorganisedGroupIdsByMatch =
                        parallelMatchesGroupIdsByMatch.map((aMatch, groupId) =>
                          aMatch -> compactGroupIdsKeyedByGroupsIdsWithPossibleGaps(
                            groupId
                          )
                        )

                      Right(
                        reconciled.copy(parallelMatchesGroupIdsByMatch =
                          parallelMatchesWithReorganisedGroupIdsByMatch
                        )
                      )
                  else
                    progressRecordingSession.upTo(paredDownMatches.size)
                    State.pure(Left(paredDownAllSidesMatches))
              yield stepResult
              end for
            end reconcileUsing

            FlatMap[ParallelMatchesGroupIdTracking]
              .tailRecM(matches.collect {
                case allSides: Match.AllSides[Section[Element]] => allSides
              })(reconcileUsing)
              .runA(Map.empty)
              .value
          }: @unchecked

        result.reconciliationPostcondition()

        val groupsOfParallelMatches =
          given Ordering[GenericMatch[Element]] with
            override def compare(
                x: GenericMatch[Element],
                y: GenericMatch[Element]
            ): Int =
              (startOffsetOnLeft(x), startOffsetOnLeft(y)) match
                case (Some(xLeftStartOffset), Some(yLeftStartOffset)) =>
                  Ordering[Int].compare(xLeftStartOffset, yLeftStartOffset)
                case _ =>
                  (startOffsetOnRight(x), startOffsetOnRight(y)) match
                    case (Some(xRightStartOffset), Some(yRightStartOffset)) =>
                      Ordering[Int].compare(
                        xRightStartOffset,
                        yRightStartOffset
                      )
                    case _ =>
                      ((
                        startOffsetOnBase(x),
                        startOffsetOnBase(y)
                      ): @unchecked) match
                        case (Some(xBaseStartOffset), Some(yBaseStartOffset)) =>
                          Ordering[Int].compare(
                            xBaseStartOffset,
                            yBaseStartOffset
                          )
          end given

          SortedMap.from(
            result.parallelMatchesGroupIdsByMatch.groupBy(_._2).map {
              (groupId, group) => groupId -> SortedSet.from(group.keys)
            }
          )
        end groupsOfParallelMatches
        logger.debug(
          s"Groups of parallel matches after reconciliation:\n${pprintCustomised(groupsOfParallelMatches)}"
        )

        result

      end reconcileMatches

      def parallelMatchesOnly: MatchesAndTheirSections =
        // PLAN:

        // 1. Build up sources composed of matched sections concatenated
        // together by path preserving their original order.

        val baseMatchedSections =
          sectionsAndTheirMatches.sets
            .map((key, values) => key -> values.head)
            .collect {
              case (section, Match.AllSides(baseSection, _, _))
                  if section == baseSection =>
                section
              case (section, Match.BaseAndLeft(baseSection, _))
                  if section == baseSection =>
                section
              case (section, Match.BaseAndRight(baseSection, _))
                  if section == baseSection =>
                section
            }

        val leftMatchedSections =
          sectionsAndTheirMatches.sets
            .map((key, values) => key -> values.head)
            .collect {
              case (section, Match.AllSides(_, leftSection, _))
                  if section == leftSection =>
                section
              case (section, Match.BaseAndLeft(_, leftSection))
                  if section == leftSection =>
                section
              case (section, Match.LeftAndRight(leftSection, _))
                  if section == leftSection =>
                section
            }

        val rightMatchedSections =
          sectionsAndTheirMatches.sets
            .map((key, values) => key -> values.head)
            .collect {
              case (section, Match.AllSides(_, _, rightSection))
                  if section == rightSection =>
                section
              case (section, Match.BaseAndRight(_, rightSection))
                  if section == rightSection =>
                section
              case (section, Match.LeftAndRight(_, rightSection))
                  if section == rightSection =>
                section
            }

        case class MetaMatchContentSources(
            override val contentsByPath: Map[Path, IndexedSeq[
              Section[Element]
            ]],
            override val label: String
        ) extends MappedContentSources[Path, Section[Element]]

        def sourcesForMetaMatching(label: String)(
            sources: Sources[Path, Element],
            matchedSections: Iterable[Section[Element]]
        ) = MetaMatchContentSources(
          contentsByPath = matchedSections
            .groupBy(sources.pathFor)
            .map((path, sections) =>
              path -> sections.toIndexedSeq
                .sortBy(section =>
                  // NOTE: use the negative of the section size as a tiebreaker
                  // if more than one section has the same start offset. This
                  // makes sure that a subsuming section comes *before* any
                  // sections that it might subsume, so it can't interpose
                  // itself within what would have been a run of parallel
                  // subsumed sections; that way we don't accidentally split
                  // what should be larger meta-matches. That can occur when the
                  // subsuming section is contributed by a larger pairwise match
                  // that subsumes several all-sides matches, and the first
                  // all-sides match aligns with the start of the pairwise
                  // match. The test
                  // `SectionedCodeExtensionTest.codeMotionAmbiguousWithAPreservation`
                  // has an example of this.
                  section.startOffset -> -section.size
                )
            ),
          label = label
        )

        val baseSourcesForMetaMatching =
          sourcesForMetaMatching("meta-base")(
            baseSources,
            baseMatchedSections
          )
        val leftSourcesForMetaMatching =
          sourcesForMetaMatching("meta-left")(
            leftSources,
            leftMatchedSections
          )
        val rightSourcesForMetaMatching =
          sourcesForMetaMatching("meta-right")(
            rightSources,
            rightMatchedSections
          )

        // 2. Apply `MatchAnalysis.of` to these sections, using the
        // potential match key of the section to underpin equality and
        // hashing.

        object metaMatchConfiguration extends AbstractConfiguration:
          override val minimumMatchSize: Int                    = 1
          override val thresholdSizeFractionForMatching: Double = 0
          override val minimumAmbiguousMatchSize: Int           = 1
          override val ambiguousMatchesThreshold: Int           = Int.MaxValue
          override val progressRecording: ProgressRecording     =
            configuration.progressRecording
        end metaMatchConfiguration

        given Eq[Section[Element]] = Eq.by(_.content: Seq[Element])

        given Funnel[Section[Element]] with
          override def funnel(
              from: Section[Element],
              into: PrimitiveSink
          ): Unit =
            from.content.foreach(summon[Funnel[Element]].funnel(_, into))

        end given

        val metaMatchAnalysis = of(
          baseSourcesForMetaMatching,
          leftSourcesForMetaMatching,
          rightSourcesForMetaMatching
        )(metaMatchConfiguration)

        // 3. The resulting meta-matches provide parallel sequences of sections
        // that are unzipped to yield corresponding all-sides and pairwise
        // matches.

        val metaMatches = metaMatchAnalysis.matches

        // NOTE: because meta-matching starts with matched *sections* and
        // ignores gaps, we have to guard against sections that would have
        // formed the sides of a suppressed outer match making a second attempt
        // at building a match.
        val groupsOfBackTranslatedParallelMatches = metaMatches
          .map {
            case Match.AllSides(
                  baseMetaSection,
                  leftMetaSection,
                  rightMetaSection
                ) =>
              (baseMetaSection.content lazyZip leftMetaSection.content lazyZip rightMetaSection.content)
                .collect {
                  case (baseSection, leftSection, rightSection)
                      if !isSubsumedNonTriviallyByAnAllSidesMatch(
                        baseSection,
                        leftSection,
                        rightSection
                      ) =>
                    Match.AllSides(baseSection, leftSection, rightSection)
                }
            case Match.BaseAndLeft(baseMetaSection, leftMetaSection) =>
              (baseMetaSection.content lazyZip leftMetaSection.content)
                .collect {
                  case (baseSection, leftSection)
                      if !isSubsumedNonTriviallyByAMatchOnTheBaseAndLeft(
                        baseSection,
                        leftSection
                      ) =>
                    Match.BaseAndLeft(baseSection, leftSection)
                }
            case Match.BaseAndRight(baseMetaSection, rightMetaSection) =>
              (baseMetaSection.content lazyZip rightMetaSection.content)
                .collect {
                  case (baseSection, rightSection)
                      if !isSubsumedNonTriviallyByAMatchOnTheBaseAndRight(
                        baseSection,
                        rightSection
                      ) =>
                    Match.BaseAndRight(baseSection, rightSection)
                }
            case Match.LeftAndRight(leftMetaSection, rightMetaSection) =>
              (leftMetaSection.content lazyZip rightMetaSection.content)
                .collect {
                  case (leftSection, rightSection)
                      if !isSubsumedNonTriviallyByAMatchOnTheLeftAndRight(
                        leftSection,
                        rightSection
                      ) =>
                    Match.LeftAndRight(leftSection, rightSection)
                }
          }
          .filter(_.nonEmpty)
          .toSeq

        // 4. Build putative groups from the back-translated matches. These
        // won't be perfectly accurate, but are refined later by
        // `reconcileMatches`.

        // NOTE: need to build a new instance of `MatchesAndTheirSections` for
        // the back-translated matches, because the thinning out of ambiguous
        // matches performed by the meta-matching and back translation above
        // means there are new opportunities for matches to be generated - while
        // the vast majority of these are redundant pairwise matches, there are
        // some that are vital to capture things such as moves with migrated
        // edits / deletions - the test
        // `SectionedCodeExtensionTest.codeMotionAmbiguousWithAPreservation` has
        // an example of this.
        val backTranslatedMatchesAndTheirSections =
          MatchesAndTheirSections.empty
            .withMatches(
              groupsOfBackTranslatedParallelMatches.foldLeft(Set.empty)(_ ++ _),
              haveTrimmedMatches = false
            )
            .matchesAndTheirSections
            .withoutRedundantPairwiseMatches

        val backTranslatedMatches =
          backTranslatedMatchesAndTheirSections.matches

        val parallelMatchesGroupIdsByMatch =
          Map.from(
            groupsOfBackTranslatedParallelMatches.zipWithIndex.flatMap(
              (parallelMatches, groupId) =>
                parallelMatches
                  .filter(backTranslatedMatches.contains)
                  .map(_ -> groupId)
            )
          )

        backTranslatedMatchesAndTheirSections
          .copy(parallelMatchesGroupIdsByMatch = parallelMatchesGroupIdsByMatch)
      end parallelMatchesOnly

      // Cleans up the state when a putative all-sides match that would have
      // been ambiguous on one side with another all-sides match was partially
      // suppressed by a larger pairwise match. This situation results in a
      // pairwise match that shares its sections on both sides with the other
      // all-sides match; remove any such redundant pairwise matches.
      def withoutRedundantPairwiseMatches: MatchesAndTheirSections =
        val redundantMatches =
          sectionsAndTheirMatches.values.toSet.filter(isRedundantPairwiseMatch)

        if redundantMatches.nonEmpty then
          logger.debug(
            s"Removing redundant pairwise matches:\n${pprintCustomised(redundantMatches)} as their sections also belong to all-sides matches."
          )
        end if

        withoutTheseMatches(redundantMatches)
      end withoutRedundantPairwiseMatches

      private def withoutTheseMatches(
          matches: Iterable[GenericMatch[Element]]
      ): MatchesAndTheirSections =
        matches.foldLeft(this) {
          case (
                matchesAndTheirSections,
                allSides @ Match.AllSides(
                  baseSection,
                  leftSection,
                  rightSection
                )
              ) =>
            matchesAndTheirSections.copy(
              baseSectionsByPath =
                matchesAndTheirSections.baseExcluding(baseSection),
              leftSectionsByPath =
                matchesAndTheirSections.leftExcluding(leftSection),
              rightSectionsByPath =
                matchesAndTheirSections.rightExcluding(rightSection),
              sectionsAndTheirMatches =
                matchesAndTheirSections.sectionsAndTheirMatches
                  .remove(baseSection, allSides)
                  .remove(leftSection, allSides)
                  .remove(rightSection, allSides),
              baseFingerprintedInclusionsByPath =
                matchesAndTheirSections.reinstateInBaseFingerprintedInclusions(
                  baseSection
                ),
              leftFingerprintedInclusionsByPath =
                matchesAndTheirSections.reinstateInLeftFingerprintedInclusions(
                  leftSection
                ),
              rightFingerprintedInclusionsByPath =
                matchesAndTheirSections.reinstateInRightFingerprintedInclusions(
                  rightSection
                ),
              parallelMatchesGroupIdsByMatch =
                parallelMatchesGroupIdsByMatch.removed(allSides)
            )

          case (
                matchesAndTheirSections,
                baseAndLeft @ Match.BaseAndLeft(baseSection, leftSection)
              ) =>
            matchesAndTheirSections.copy(
              baseSectionsByPath =
                matchesAndTheirSections.baseExcluding(baseSection),
              leftSectionsByPath =
                matchesAndTheirSections.leftExcluding(leftSection),
              sectionsAndTheirMatches =
                matchesAndTheirSections.sectionsAndTheirMatches
                  .remove(baseSection, baseAndLeft)
                  .remove(leftSection, baseAndLeft),
              parallelMatchesGroupIdsByMatch =
                parallelMatchesGroupIdsByMatch.removed(baseAndLeft)
            )

          case (
                matchesAndTheirSections,
                baseAndRight @ Match.BaseAndRight(baseSection, rightSection)
              ) =>
            matchesAndTheirSections.copy(
              baseSectionsByPath =
                matchesAndTheirSections.baseExcluding(baseSection),
              rightSectionsByPath =
                matchesAndTheirSections.rightExcluding(rightSection),
              sectionsAndTheirMatches =
                matchesAndTheirSections.sectionsAndTheirMatches
                  .remove(baseSection, baseAndRight)
                  .remove(rightSection, baseAndRight),
              parallelMatchesGroupIdsByMatch =
                parallelMatchesGroupIdsByMatch.removed(baseAndRight)
            )

          case (
                matchesAndTheirSections,
                leftAndRight @ Match.LeftAndRight(leftSection, rightSection)
              ) =>
            matchesAndTheirSections.copy(
              leftSectionsByPath =
                matchesAndTheirSections.leftExcluding(leftSection),
              rightSectionsByPath =
                matchesAndTheirSections.rightExcluding(rightSection),
              sectionsAndTheirMatches =
                matchesAndTheirSections.sectionsAndTheirMatches
                  .remove(leftSection, leftAndRight)
                  .remove(rightSection, leftAndRight),
              parallelMatchesGroupIdsByMatch =
                parallelMatchesGroupIdsByMatch.removed(leftAndRight)
            )
        }
      end withoutTheseMatches

      private def isRedundantPairwiseMatch(aMatch: GenericMatch[Element]) =
        aMatch match
          case Match.BaseAndLeft(baseSection, leftSection) =>
            sectionsAndTheirMatches
              .get(baseSection)
              .intersect(sectionsAndTheirMatches.get(leftSection))
              .exists(_.isAnAllSidesMatch)
          case Match.BaseAndRight(baseSection, rightSection) =>
            sectionsAndTheirMatches
              .get(baseSection)
              .intersect(sectionsAndTheirMatches.get(rightSection))
              .exists(_.isAnAllSidesMatch)
          case Match.LeftAndRight(leftSection, rightSection) =>
            sectionsAndTheirMatches
              .get(leftSection)
              .intersect(sectionsAndTheirMatches.get(rightSection))
              .exists(_.isAnAllSidesMatch)
          case _: Match.AllSides[Section[Element]] => false

      private def isSubsumedNonTriviallyByAnAllSidesMatch(
          baseSection: Section[Element],
          leftSection: Section[Element],
          rightSection: Section[Element]
      ) =
        val subsumingOnBase =
          subsumingMatches(
            sectionsAndTheirMatches
          )(
            baseSources,
            baseSectionsByPath
          )(
            baseSection,
            includeTrivialSubsumption = false
          )

        val subsumingOnLeft =
          subsumingMatches(
            sectionsAndTheirMatches
          )(
            leftSources,
            leftSectionsByPath
          )(
            leftSection,
            includeTrivialSubsumption = false
          )

        val subsumingOnRight =
          subsumingMatches(
            sectionsAndTheirMatches
          )(
            rightSources,
            rightSectionsByPath
          )(
            rightSection,
            includeTrivialSubsumption = false
          )

        (subsumingOnBase intersect subsumingOnLeft intersect subsumingOnRight).nonEmpty
      end isSubsumedNonTriviallyByAnAllSidesMatch

      private def isSubsumedNonTriviallyByAMatchOnTheLeftAndRight(
          leftSection: Section[Element],
          rightSection: Section[Element]
      ) =
        val subsumingOnLeft =
          subsumingMatches(
            sectionsAndTheirMatches
          )(
            leftSources,
            leftSectionsByPath
          )(
            leftSection,
            includeTrivialSubsumption = false
          )

        val subsumingOnRight =
          subsumingMatches(
            sectionsAndTheirMatches
          )(
            rightSources,
            rightSectionsByPath
          )(
            rightSection,
            includeTrivialSubsumption = false
          )

        (subsumingOnLeft intersect subsumingOnRight).nonEmpty
      end isSubsumedNonTriviallyByAMatchOnTheLeftAndRight

      private def isSubsumedNonTriviallyByAMatchOnTheBaseAndRight(
          baseSection: Section[Element],
          rightSection: Section[Element]
      ) =
        val subsumingOnBase =
          subsumingMatches(
            sectionsAndTheirMatches
          )(
            baseSources,
            baseSectionsByPath
          )(
            baseSection,
            includeTrivialSubsumption = false
          )

        val subsumingOnRight =
          subsumingMatches(
            sectionsAndTheirMatches
          )(
            rightSources,
            rightSectionsByPath
          )(
            rightSection,
            includeTrivialSubsumption = false
          )

        (subsumingOnBase intersect subsumingOnRight).nonEmpty
      end isSubsumedNonTriviallyByAMatchOnTheBaseAndRight

      private def isSubsumedNonTriviallyByAMatchOnTheBaseAndLeft(
          baseSection: Section[Element],
          leftSection: Section[Element]
      ) =
        val subsumingOnBase =
          subsumingMatches(
            sectionsAndTheirMatches
          )(
            baseSources,
            baseSectionsByPath
          )(
            baseSection,
            includeTrivialSubsumption = false
          )

        val subsumingOnLeft =
          subsumingMatches(
            sectionsAndTheirMatches
          )(
            leftSources,
            leftSectionsByPath
          )(
            leftSection,
            includeTrivialSubsumption = false
          )

        (subsumingOnBase intersect subsumingOnLeft).nonEmpty
      end isSubsumedNonTriviallyByAMatchOnTheBaseAndLeft

      private def withMatches(
          matches: Set[GenericMatch[Element]],
          haveTrimmedMatches: Boolean
      ): MatchingResult =
        val updatedMatchesAndTheirSections =
          matches.foldLeft(this)(_ withMatch _)

        val pathInclusions =
          if !haveTrimmedMatches then
            case class PathInclusionsImplementation(
                basePaths: Set[Path],
                leftPaths: Set[Path],
                rightPaths: Set[Path]
            ) extends PathInclusions:
              override def isIncludedOnBase(basePath: Path): Boolean =
                basePaths.contains(basePath)

              override def isIncludedOnLeft(leftPath: Path): Boolean =
                leftPaths.contains(leftPath)

              override def isIncludedOnRight(rightPath: Path): Boolean =
                rightPaths.contains(rightPath)

              def addPathOnBaseFor(
                  baseSection: Section[Element]
              ): PathInclusionsImplementation =
                copy(basePaths = basePaths + baseSources.pathFor(baseSection))
              def addPathOnLeftFor(
                  leftSection: Section[Element]
              ): PathInclusionsImplementation =
                copy(leftPaths = leftPaths + leftSources.pathFor(leftSection))
              def addPathOnRightFor(
                  rightSection: Section[Element]
              ): PathInclusionsImplementation =
                copy(rightPaths =
                  rightPaths + rightSources.pathFor(rightSection)
                )
            end PathInclusionsImplementation

            matches.foldLeft(
              PathInclusionsImplementation(Set.empty, Set.empty, Set.empty)
            )((partialPathInclusions, aMatch) =>
              aMatch match
                case Match.AllSides(baseSection, leftSection, rightSection) =>
                  partialPathInclusions
                    .addPathOnBaseFor(baseSection)
                    .addPathOnLeftFor(leftSection)
                    .addPathOnRightFor(rightSection)
                case Match.BaseAndLeft(baseSection, leftSection) =>
                  partialPathInclusions
                    .addPathOnBaseFor(baseSection)
                    .addPathOnLeftFor(leftSection)
                case Match.BaseAndRight(baseSection, rightSection) =>
                  partialPathInclusions
                    .addPathOnBaseFor(baseSection)
                    .addPathOnRightFor(rightSection)
                case Match.LeftAndRight(leftSection, rightSection) =>
                  partialPathInclusions
                    .addPathOnLeftFor(leftSection)
                    .addPathOnRightFor(rightSection)
            )
          else PathInclusions.all

        MatchingResult(
          matchesAndTheirSections = updatedMatchesAndTheirSections,
          numberOfMatchesForTheGivenWindowSize = matches.size,
          estimatedWindowSizeForOptimalMatch =
            estimateOptimalMatchSize(matches),
          pathInclusions = pathInclusions
        )
      end withMatches

      private def withMatch(
          aMatch: GenericMatch[Element]
      ): MatchesAndTheirSections =
        aMatch match
          case Match.AllSides(baseSection, leftSection, rightSection) =>
            copy(
              baseSectionsByPath = baseIncluding(baseSection),
              leftSectionsByPath = leftIncluding(leftSection),
              rightSectionsByPath = rightIncluding(rightSection),
              sectionsAndTheirMatches =
                sectionsAndTheirMatches + (baseSection -> aMatch) + (leftSection -> aMatch) + (rightSection -> aMatch),
              baseFingerprintedInclusionsByPath =
                knockOutFromBaseFingerprintedInclusions(baseSection),
              leftFingerprintedInclusionsByPath =
                knockOutFromLeftFingerprintedInclusions(leftSection),
              rightFingerprintedInclusionsByPath =
                knockOutFromRightFingerprintedInclusions(rightSection)
            )
          case baseAndLeft @ Match.BaseAndLeft(baseSection, leftSection) =>
            copy(
              baseSectionsByPath = baseIncluding(baseSection),
              leftSectionsByPath = leftIncluding(leftSection),
              sectionsAndTheirMatches =
                sectionsAndTheirMatches + (baseSection -> aMatch) + (leftSection -> aMatch)
            )
          case baseAndRight @ Match.BaseAndRight(baseSection, rightSection) =>
            copy(
              baseSectionsByPath = baseIncluding(baseSection),
              rightSectionsByPath = rightIncluding(rightSection),
              sectionsAndTheirMatches =
                sectionsAndTheirMatches + (baseSection -> aMatch) + (rightSection -> aMatch)
            )
          case leftAndRight @ Match.LeftAndRight(leftSection, rightSection) =>
            copy(
              leftSectionsByPath = leftIncluding(leftSection),
              rightSectionsByPath = rightIncluding(rightSection),
              sectionsAndTheirMatches =
                sectionsAndTheirMatches + (leftSection -> aMatch) + (rightSection -> aMatch)
            )
        end match
      end withMatch

      private def checkInvariant(): Unit =
        // We expect to tally either two lots of a given pairwise match or three
        // lots of a given all-sides match; we therefore scale the
        // raw multiplicities of the section to take this into account.
        val multiplicityScaleSubdivisibleByTwoOrThree = 6

        val multiplicityScaleDividedIntoAThirdForEachSideOfAnAllSidesMatch =
          multiplicityScaleSubdivisibleByTwoOrThree / 3

        val multiplicityScaleDividedIntoAHalfForEachSideOfAPairwiseMatch =
          multiplicityScaleSubdivisibleByTwoOrThree / 2

        val baseSectionMultiplicities = baseSectionsByPath.values
          .flatMap(_.iterator)
          .groupBy(identity)
          .map((section, group) =>
            section -> multiplicityScaleSubdivisibleByTwoOrThree * group.size
          )

        val leftSectionMultiplicities = leftSectionsByPath.values
          .flatMap(_.iterator)
          .groupBy(identity)
          .map((section, group) =>
            section -> multiplicityScaleSubdivisibleByTwoOrThree * group.size
          )

        val rightSectionMultiplicities = rightSectionsByPath.values
          .flatMap(_.iterator)
          .groupBy(identity)
          .map((section, group) =>
            section -> multiplicityScaleSubdivisibleByTwoOrThree * group.size
          )

        val tallyAllSides: Option[Int] => Option[Int] = {
          case Some(multiplicity) =>
            // Once we've accounted for the multiplicity, remove the entry.
            Option.unless(
              multiplicityScaleDividedIntoAThirdForEachSideOfAnAllSidesMatch == multiplicity
            )(
              multiplicity - multiplicityScaleDividedIntoAThirdForEachSideOfAnAllSidesMatch
            )
          // If we have an occurrence and there is no entry, start a negative
          // count.
          case None =>
            Some(
              -multiplicityScaleDividedIntoAThirdForEachSideOfAnAllSidesMatch
            )
        }

        val tallyPairwise: Option[Int] => Option[Int] = {
          case Some(multiplicity) =>
            // Once we've accounted for the multiplicity, remove the entry.
            Option.unless(
              multiplicityScaleDividedIntoAHalfForEachSideOfAPairwiseMatch == multiplicity
            )(
              multiplicity - multiplicityScaleDividedIntoAHalfForEachSideOfAPairwiseMatch
            )
          // If we have an occurrence and there is no entry, start a negative
          // count.
          case None =>
            Some(-multiplicityScaleDividedIntoAHalfForEachSideOfAPairwiseMatch)
        }

        val (
          unbalancedBaseSectionMultiplicities,
          unbalancedLeftSectionMultiplicities,
          unbalancedRightSectionMultiplicities
        ) = sectionsAndTheirMatches.values.foldLeft(
          (
            baseSectionMultiplicities,
            leftSectionMultiplicities,
            rightSectionMultiplicities
          )
        ) {
          case (
                (
                  baseSectionMultiplicities,
                  leftSectionMultiplicities,
                  rightSectionMultiplicities
                ),
                aMatch
              ) =>
            aMatch match
              case Match.AllSides(
                    baseSection,
                    leftSection,
                    rightSection
                  ) =>
                (
                  baseSectionMultiplicities.updatedWith(baseSection)(
                    tallyAllSides
                  ),
                  leftSectionMultiplicities.updatedWith(leftSection)(
                    tallyAllSides
                  ),
                  rightSectionMultiplicities.updatedWith(rightSection)(
                    tallyAllSides
                  )
                )
              case Match.BaseAndLeft(
                    baseSection,
                    leftSection
                  ) =>
                (
                  baseSectionMultiplicities.updatedWith(baseSection)(
                    tallyPairwise
                  ),
                  leftSectionMultiplicities.updatedWith(leftSection)(
                    tallyPairwise
                  ),
                  rightSectionMultiplicities
                )
              case Match.BaseAndRight(
                    baseSection,
                    rightSection
                  ) =>
                (
                  baseSectionMultiplicities.updatedWith(baseSection)(
                    tallyPairwise
                  ),
                  leftSectionMultiplicities,
                  rightSectionMultiplicities.updatedWith(rightSection)(
                    tallyPairwise
                  )
                )
              case Match.LeftAndRight(
                    leftSection,
                    rightSection
                  ) =>
                (
                  baseSectionMultiplicities,
                  leftSectionMultiplicities.updatedWith(leftSection)(
                    tallyPairwise
                  ),
                  rightSectionMultiplicities.updatedWith(rightSection)(
                    tallyPairwise
                  )
                )
        }

        assert(
          unbalancedBaseSectionMultiplicities.isEmpty,
          s"Found unbalanced base section multiplicities, these are:\n${pprintCustomised(unbalancedBaseSectionMultiplicities)}"
        )
        assert(
          unbalancedLeftSectionMultiplicities.isEmpty,
          s"Found unbalanced left section multiplicities, these are:\n${pprintCustomised(unbalancedLeftSectionMultiplicities)}"
        )
        assert(
          unbalancedRightSectionMultiplicities.isEmpty,
          s"Found unbalanced right section multiplicities, these are:\n${pprintCustomised(unbalancedRightSectionMultiplicities)}"
        )
      end checkInvariant

      private def reconciliationPostcondition(): Unit =
        baseSectionsByPath.values.flatMap(_.iterator).foreach { baseSection =>
          assert(!baseSubsumes(baseSection))
        }
        leftSectionsByPath.values.flatMap(_.iterator).foreach { leftSection =>
          assert(!leftSubsumes(leftSection))
        }
        rightSectionsByPath.values.flatMap(_.iterator).foreach { rightSection =>
          assert(!rightSubsumes(rightSection))
        }

        // No match should be redundant - i.e. no match should involve sections
        // that all belong to another match. This goes without saying for
        // all-sides matches, as any redundancy would imply equivalent
        // all-sides matches being associated with the same sections - this
        // isn't allowed by a `MultiDict` instance. The same applies for
        // pairwise matches of the same kind; pairwise matches of different
        // kinds can't make each other redundant.
        // What we have to watch out for are pairwise matches having *both*
        // sections also belonging to an all-sides match. Note that it *is*
        // legitimate to have a pairwise match sharing just one section with an
        // all-sides match; they are just ambiguous matches.

        val matchesBySectionPairs =
          sectionsAndTheirMatches.values.toSet.foldLeft(
            MultiDict.empty[(Section[Element], Section[Element]), Match[
              Section[Element]
            ]]
          )((matchesBySectionPairs, aMatch) =>
            aMatch match
              case Match.AllSides(baseSection, leftSection, rightSection) =>
                matchesBySectionPairs + ((
                  baseSection,
                  leftSection
                ) -> aMatch) + ((baseSection, rightSection) -> aMatch) + ((
                  leftSection,
                  rightSection
                ) -> aMatch)
              case Match.BaseAndLeft(baseSection, leftSection) =>
                matchesBySectionPairs + ((
                  baseSection,
                  leftSection
                ) -> aMatch)
              case Match.BaseAndRight(baseSection, rightSection) =>
                matchesBySectionPairs + ((
                  baseSection,
                  rightSection
                ) -> aMatch)
              case Match.LeftAndRight(leftSection, rightSection) =>
                matchesBySectionPairs + ((
                  leftSection,
                  rightSection
                ) -> aMatch)
          )

        matchesBySectionPairs.keySet.foreach { sectionPair =>
          val matches = matchesBySectionPairs.get(sectionPair)

          val (allSides, pairwiseMatches) =
            matches.partition(_.isAnAllSidesMatch)

          if allSides.nonEmpty then
            assert(
              pairwiseMatches.isEmpty,
              s"Found redundancy between pairwise matches: ${pprintCustomised(pairwiseMatches)} and an all-sides match in: ${pprintCustomised(allSides)}."
            )
          end if
        }

        if parallelMatchesGroupIdsByMatch.nonEmpty then
          assert(
            parallelMatchesGroupIdsByMatch.keySet == matches,
            s"If groups of parallel matches have been discovered, they should cover the overall population of matches exactly."
          )

          parallelMatchesGroupIdsByMatch.toSeq
            .groupBy(_._2)
            .values
            .foreach { group =>
              val matchesInGroup = group.map(_._1)

              case class SidePerspective(path: Path, startOffset: Int)

              case class MatchSynopsis(
                  base: Option[SidePerspective],
                  left: Option[SidePerspective],
                  right: Option[SidePerspective]
              )

              object MatchSynopsis:
                def apply(aMatch: GenericMatch[Element]): MatchSynopsis =
                  MatchSynopsis(
                    base = (pathOnBase(aMatch), startOffsetOnBase(aMatch))
                      .mapN(SidePerspective.apply),
                    left = (pathOnLeft(aMatch), startOffsetOnLeft(aMatch))
                      .mapN(SidePerspective.apply),
                    right = (pathOnRight(aMatch), startOffsetOnRight(aMatch))
                      .mapN(SidePerspective.apply)
                  )
              end MatchSynopsis

              val basePaths = matchesInGroup.flatMap(pathOnBase).toSet
              assert(
                basePaths.size <= 1,
                s"""Base paths for a group of parallel matches should be the same,
                   |but vary: $basePaths.
                   |Matches are: ${pprintCustomised(
                    matchesInGroup.map(MatchSynopsis.apply)
                  )}""".stripMargin
              )

              val leftPaths = matchesInGroup.flatMap(pathOnLeft).toSet
              assert(
                leftPaths.size <= 1,
                s"""Left paths for a group of parallel matches should be the same,
                   |but vary: $leftPaths.
                   |Matches are: ${pprintCustomised(
                    matchesInGroup.map(MatchSynopsis.apply)
                  )}""".stripMargin
              )

              val rightPaths = matchesInGroup.flatMap(pathOnRight).toSet
              assert(
                rightPaths.size <= 1,
                s"""Right paths for a group of parallel matches should be the same,
                   |but vary: $rightPaths.
                   |Matches are: ${pprintCustomised(
                    matchesInGroup.map(MatchSynopsis.apply)
                  )}""".stripMargin
              )

              def checkOrderIsConsistentAcrossSides(
                  firstSide: String,
                  secondSide: String,
                  firstSideStartOffset: GenericMatch[Element] => Option[Int],
                  secondSideStartOffset: GenericMatch[Element] => Option[Int]
              ): Unit =
                extension (matches: Seq[GenericMatch[Element]])
                  private def sortWhereRelevantBy(
                      startOffsetOf: GenericMatch[Element] => Option[Int]
                  ): Seq[GenericMatch[Element]] =
                    matches
                      .flatMap(aMatch =>
                        startOffsetOf(aMatch)
                          .map(offset => aMatch -> offset)
                      )
                      .sortBy(_._2)
                      .map(_._1)

                end extension

                // NOTE: in the next two bindings, we have to sort first
                // according to the other side's perspective. This is to avoid
                // situations where several matches happen to have the same
                // start offset on one side but different offsets on the other.
                // As sorting is stable, that could lead to the side with
                // colliding offsets having the 'wrong' initial order that would
                // be preserved by the sort. Tip of the hat to Jules for
                // spotting the pitfall in the first place.

                val matchesOrderedFromFirstSidePerspective =
                  matchesInGroup
                    .sortWhereRelevantBy(secondSideStartOffset)
                    .sortWhereRelevantBy(firstSideStartOffset)

                val matchesOrderedFromSecondSidePerspective =
                  matchesInGroup
                    .sortWhereRelevantBy(firstSideStartOffset)
                    .sortWhereRelevantBy(secondSideStartOffset)

                val commonMatches =
                  matchesOrderedFromFirstSidePerspective.toSet intersect matchesOrderedFromSecondSidePerspective.toSet

                val commonMatchesOrderedFromFirstSidePerspective =
                  matchesOrderedFromFirstSidePerspective.filter(
                    commonMatches.contains
                  )
                val commonMatchesOrderedFromSecondSidePerspective =
                  matchesOrderedFromSecondSidePerspective.filter(
                    commonMatches.contains
                  )

                assert(
                  commonMatchesOrderedFromFirstSidePerspective == commonMatchesOrderedFromSecondSidePerspective,
                  s"""
                     |Expected a consistent ordering of matches relevant to the sides $firstSide and $secondSide,
                     |on the $firstSide they are:
                     |${pprintCustomised(
                      commonMatchesOrderedFromFirstSidePerspective.map(
                        MatchSynopsis.apply
                      )
                    )}
                     |and on the $secondSide they are:
                     |${pprintCustomised(
                      commonMatchesOrderedFromSecondSidePerspective.map(
                        MatchSynopsis.apply
                      )
                    )}
                     |""".stripMargin
                )
              end checkOrderIsConsistentAcrossSides

              checkOrderIsConsistentAcrossSides(
                "base",
                "left",
                startOffsetOnBase,
                startOffsetOnLeft
              )
              checkOrderIsConsistentAcrossSides(
                "base",
                "right",
                startOffsetOnBase,
                startOffsetOnRight
              )
              checkOrderIsConsistentAcrossSides(
                "left",
                "right",
                startOffsetOnLeft,
                startOffsetOnRight
              )
            }
        end if
      end reconciliationPostcondition

      private def pathOnBase(aMatch: GenericMatch[Element]): Option[Path] =
        aMatch match
          case Match.AllSides(baseSection, _, _) =>
            Some(baseSources.pathFor(baseSection))
          case Match.BaseAndLeft(baseSection, _) =>
            Some(baseSources.pathFor(baseSection))
          case Match.BaseAndRight(baseSection, _) =>
            Some(baseSources.pathFor(baseSection))
          case _ => None

      private def pathOnLeft(aMatch: GenericMatch[Element]): Option[Path] =
        aMatch match
          case Match.AllSides(_, leftSection, _) =>
            Some(leftSources.pathFor(leftSection))
          case Match.BaseAndLeft(_, leftSection) =>
            Some(leftSources.pathFor(leftSection))
          case Match.LeftAndRight(leftSection, _) =>
            Some(leftSources.pathFor(leftSection))
          case _ => None

      private def pathOnRight(aMatch: GenericMatch[Element]): Option[Path] =
        aMatch match
          case Match.AllSides(_, _, rightSection) =>
            Some(rightSources.pathFor(rightSection))
          case Match.BaseAndRight(_, rightSection) =>
            Some(rightSources.pathFor(rightSection))
          case Match.LeftAndRight(_, rightSection) =>
            Some(rightSources.pathFor(rightSection))
          case _ => None

      private def startOffsetOnBase(
          aMatch: GenericMatch[Element]
      ): Option[Int] =
        aMatch match
          case Match.AllSides(baseSection, _, _) =>
            Some(baseSection.startOffset)
          case Match.BaseAndLeft(baseSection, _) =>
            Some(baseSection.startOffset)
          case Match.BaseAndRight(baseSection, _) =>
            Some(baseSection.startOffset)
          case _ => None

      private def startOffsetOnLeft(
          aMatch: GenericMatch[Element]
      ): Option[Int] =
        aMatch match
          case Match.AllSides(_, leftSection, _) =>
            Some(leftSection.startOffset)
          case Match.BaseAndLeft(_, leftSection) =>
            Some(leftSection.startOffset)
          case Match.LeftAndRight(leftSection, _) =>
            Some(leftSection.startOffset)
          case _ => None

      private def startOffsetOnRight(
          aMatch: GenericMatch[Element]
      ): Option[Int] =
        aMatch match
          case Match.AllSides(_, _, rightSection) =>
            Some(rightSection.startOffset)
          case Match.BaseAndRight(_, rightSection) =>
            Some(rightSection.startOffset)
          case Match.LeftAndRight(_, rightSection) =>
            Some(rightSection.startOffset)
          case _ => None

      private def pareDownOrSuppressCompletely[MatchType <: GenericMatch[
        Element
      ]](
          aMatch: MatchType
      ): ParallelMatchesGroupIdTracking[Option[ParedDownMatch[MatchType]]] =
        // NOTE: one thing to watch out is when fragments resulting from
        // larger pairwise matches being eaten into collide with equivalent
        // pairwise matches found by fingerprint matching. This can take the
        // form of the fragments coming first due to larger all-sides matches,
        // or the pairwise matches from fingerprint matching can be followed
        // by fragmentation if the all-sides eating into the larger pairwise
        // matches also come from the same fingerprinting that yielded the
        // pairwise matching. Intercepting this here addresses both cases. {
        val result: Option[ParedDownMatch[MatchType]] = aMatch match
          case Match.AllSides(baseSection, leftSection, rightSection) =>
            val trivialSubsumptionSize = baseSection.size

            val subsumingOnBase =
              subsumingMatches(
                sectionsAndTheirMatches
              )(
                baseSources,
                baseSectionsByPath
              )(
                baseSection,
                includeTrivialSubsumption = false
              )

            val subsumingOnLeft =
              subsumingMatches(
                sectionsAndTheirMatches
              )(
                leftSources,
                leftSectionsByPath
              )(
                leftSection,
                includeTrivialSubsumption = false
              )

            val subsumingOnRight =
              subsumingMatches(
                sectionsAndTheirMatches
              )(
                rightSources,
                rightSectionsByPath
              )(
                rightSection,
                includeTrivialSubsumption = false
              )

            val allSidesSubsumingOnLeft =
              subsumingOnLeft.filter(_.isAnAllSidesMatch)
            val allSidesSubsumingOnRight =
              subsumingOnRight.filter(_.isAnAllSidesMatch)
            val allSidesSubsumingOnBase =
              subsumingOnBase.filter(_.isAnAllSidesMatch)

            val subsumedByAnAllSidesMatchOnMoreThanOneSide =
              (allSidesSubsumingOnLeft intersect allSidesSubsumingOnRight).nonEmpty
                || (allSidesSubsumingOnBase intersect allSidesSubsumingOnLeft).nonEmpty
                || (allSidesSubsumingOnBase intersect allSidesSubsumingOnRight).nonEmpty

            if !subsumedByAnAllSidesMatchOnMoreThanOneSide then
              val subsumedBySomeMatchOnJustTheBase =
                (subsumingOnBase diff (subsumingOnLeft union subsumingOnRight)).nonEmpty
              val subsumedBySomeMatchOnJustTheLeft =
                (subsumingOnLeft diff (subsumingOnBase union subsumingOnRight)).nonEmpty
              val subsumedBySomeMatchOnJustTheRight =
                (subsumingOnRight diff (subsumingOnBase union subsumingOnLeft)).nonEmpty

              // NOTE: an all-sides match could be subsumed by *some* match on
              // just one side for two or three sides; they would be
              // *different* matches, each doing a one-sided subsumption.
              (
                subsumedBySomeMatchOnJustTheBase,
                subsumedBySomeMatchOnJustTheLeft,
                subsumedBySomeMatchOnJustTheRight
              ) match
                case (false, false, false) =>
                  Option.unless(
                    baseSubsumes(baseSection) || leftSubsumes(
                      leftSection
                    ) || rightSubsumes(rightSection)
                  )(aMatch)
                case (true, false, false) =>
                  Option.unless(
                    leftSubsumes(leftSection) || rightSubsumes(
                      rightSection
                    )
                  )(Match.LeftAndRight(leftSection, rightSection))
                case (false, true, false) =>
                  Option.unless(
                    baseSubsumes(baseSection) || rightSubsumes(
                      rightSection
                    )
                  )(Match.BaseAndRight(baseSection, rightSection))
                case (false, false, true) =>
                  Option.unless(
                    baseSubsumes(baseSection) || leftSubsumes(
                      leftSection
                    )
                  )(Match.BaseAndLeft(baseSection, leftSection))
                case _ => None
              end match
            else None
            end if

          case Match.BaseAndLeft(baseSection, leftSection) =>
            Option.unless(
              baseSubsumes(baseSection) || leftSubsumes(
                leftSection
              )
            )(aMatch)

          case Match.BaseAndRight(baseSection, rightSection) =>
            Option.unless(
              baseSubsumes(baseSection) || rightSubsumes(
                rightSection
              )
            )(aMatch)

          case Match.LeftAndRight(leftSection, rightSection) =>
            Option.unless(
              leftSubsumes(leftSection) || rightSubsumes(
                rightSection
              )
            )(aMatch)

          case _ => None

        result.traverse(paredDownMatch =>
          propagateGroupIds(aMatch, paredDownMatch) as paredDownMatch
        )
      end pareDownOrSuppressCompletely

      private def pairwiseMatchesSubsumingOnBothSidesWithBiteEdges(
          allSides: Match.AllSides[Section[Element]]
      ): Set[(PairwiseMatch, BiteEdge, BiteEdge)] =
        val subsumingOnBase =
          subsumingPairwiseMatchesIncludingTriviallySubsuming(
            sectionsAndTheirMatches
          )(
            baseSources,
            baseSectionsByPath
          )(
            allSides.baseElement
          )
        val subsumingOnLeft =
          subsumingPairwiseMatchesIncludingTriviallySubsuming(
            sectionsAndTheirMatches
          )(
            leftSources,
            leftSectionsByPath
          )(
            allSides.leftElement
          )
        val subsumingOnRight =
          subsumingPairwiseMatchesIncludingTriviallySubsuming(
            sectionsAndTheirMatches
          )(
            rightSources,
            rightSectionsByPath
          )(
            allSides.rightElement
          )

        // NOTE: we have to be mindful that a pairwise match may have multiple
        // sites where the same content can be matched by a smaller all-sides
        // match that is ambiguous. That can lead to the biting all-sides match
        // being a 'cross-over', where the two sides are misaligned - each side
        // of the all-sides match refers to a different site in the pairwise
        // match.
        // NOTE: the above can also take place when only one of several
        // potential ambiguous matches isn't suppressed elsewhere - in which
        // case *no* fragmentation may occur. That leaves the misaligned
        // all-sides match hanging around with the larger pairwise match still
        // subsuming it, so it is left to `pareDownOrSuppressCompletely` to sort
        // that out later on.
        (subsumingOnBase intersect subsumingOnLeft).flatMap {
          case subsuming: Match.BaseAndLeft[Section[Element]] =>
            val offsetRelativeToSubsumingOnBaseSide =
              allSides.baseElement.startOffset - subsuming.baseElement.startOffset
            val offsetRelativeToSubsumingOnLeftSide =
              allSides.leftElement.startOffset - subsuming.leftElement.startOffset

            Option.when(
              offsetRelativeToSubsumingOnBaseSide == offsetRelativeToSubsumingOnLeftSide
            )(
              subsuming,
              BiteEdge.Start(startOffsetRelativeToMeal =
                offsetRelativeToSubsumingOnBaseSide
              ),
              BiteEdge.End(onePastEndOffsetRelativeToMeal =
                allSides.baseElement.onePastEndOffset - subsuming.baseElement.startOffset
              )
            )
        } union (subsumingOnBase intersect subsumingOnRight).flatMap {
          case subsuming: Match.BaseAndRight[Section[Element]] =>
            val offsetRelativeToSubsumingOnBaseSide =
              allSides.baseElement.startOffset - subsuming.baseElement.startOffset
            val offsetRelativeToSubsumingOnRightSide =
              allSides.rightElement.startOffset - subsuming.rightElement.startOffset

            Option.when(
              offsetRelativeToSubsumingOnBaseSide == offsetRelativeToSubsumingOnRightSide
            )(
              subsuming,
              BiteEdge.Start(startOffsetRelativeToMeal =
                offsetRelativeToSubsumingOnBaseSide
              ),
              BiteEdge.End(onePastEndOffsetRelativeToMeal =
                allSides.baseElement.onePastEndOffset - subsuming.baseElement.startOffset
              )
            )
        } union (subsumingOnLeft intersect subsumingOnRight).flatMap {
          case subsuming: Match.LeftAndRight[Section[Element]] =>
            val offsetRelativeToSubsumingOnLeftSide =
              allSides.leftElement.startOffset - subsuming.leftElement.startOffset
            val offsetRelativeToSubsumingOnRightSide =
              allSides.rightElement.startOffset - subsuming.rightElement.startOffset

            Option.when(
              offsetRelativeToSubsumingOnLeftSide == offsetRelativeToSubsumingOnRightSide
            )(
              subsuming,
              BiteEdge.Start(startOffsetRelativeToMeal =
                offsetRelativeToSubsumingOnLeftSide
              ),
              BiteEdge.End(onePastEndOffsetRelativeToMeal =
                allSides.leftElement.onePastEndOffset - subsuming.leftElement.startOffset
              )
            )
        }
      end pairwiseMatchesSubsumingOnBothSidesWithBiteEdges

      private def pairwiseMatchesSubsumingOnBothSides(
          allSides: Match.AllSides[Section[Element]]
      ): Set[PairwiseMatch] =
        val subsumingOnBase =
          subsumingPairwiseMatchesIncludingTriviallySubsuming(
            sectionsAndTheirMatches
          )(
            baseSources,
            baseSectionsByPath
          )(
            allSides.baseElement
          )
        val subsumingOnLeft =
          subsumingPairwiseMatchesIncludingTriviallySubsuming(
            sectionsAndTheirMatches
          )(
            leftSources,
            leftSectionsByPath
          )(
            allSides.leftElement
          )
        val subsumingOnRight =
          subsumingPairwiseMatchesIncludingTriviallySubsuming(
            sectionsAndTheirMatches
          )(
            rightSources,
            rightSectionsByPath
          )(
            allSides.rightElement
          )

        (subsumingOnBase intersect subsumingOnLeft)
          .union(subsumingOnBase intersect subsumingOnRight)
          .union(subsumingOnLeft intersect subsumingOnRight)
      end pairwiseMatchesSubsumingOnBothSides

      @tailrec
      private final def withAllSmallFryMatches(
          candidateWindowSize: Int
      )(using
          progressRecordingSession: ProgressRecordingSession
      ): MatchesAndTheirSections =
        require(
          minimumWindowSizeAcrossAllFilesOverAllSides until minimumSureFireWindowSizeAcrossAllFilesOverAllSides contains candidateWindowSize
        )

        // We will be exploring the small-fry window sizes below
        // `minimumSureFireWindowSizeAcrossAllFilesOverAllSides`; in this
        // situation, sizes below per-file thresholds lead to no matches, this
        // leads to gaps in validity as a size exceeds the largest match it
        // could participate in, but fails to meet the next highest per-file
        // threshold. Consequently, don't bother checking whether any matches
        // were found - instead, plod linearly down each window size.
        val MatchingResult(
          stateAfterTryingCandidate,
          numberOfMatchesForTheGivenWindowSize,
          _,
          _
        ) =
          this.matchesForWindowSize(
            candidateWindowSize,
            PathInclusions.all,
            thresholdSizeForMatching
          )

        if candidateWindowSize > minimumWindowSizeAcrossAllFilesOverAllSides
        then
          if 0 < numberOfMatchesForTheGivenWindowSize then
            logger.debug(
              s"Search has found a match at window size: $candidateWindowSize, number of matches is: $numberOfMatchesForTheGivenWindowSize, continuing to look for smaller matches."
            )
          end if
          progressRecordingSession.upTo(candidateWindowSize)

          stateAfterTryingCandidate.withAllSmallFryMatches(candidateWindowSize =
            candidateWindowSize - 1
          )
        else
          if 0 < numberOfMatchesForTheGivenWindowSize then
            logger.debug(
              s"Search has found a match at window size: $minimumWindowSizeAcrossAllFilesOverAllSides, number of matches is: $numberOfMatchesForTheGivenWindowSize, search for matches whose size is less than the sure-fire match window size of: $minimumSureFireWindowSizeAcrossAllFilesOverAllSides has terminated; results are:\n${pprintCustomised(stateAfterTryingCandidate)}"
            )
          else
            logger.debug(
              s"Search for matches whose size is less than the sure-fire match window size of: $minimumSureFireWindowSizeAcrossAllFilesOverAllSides has terminated at minimum window size: $minimumWindowSizeAcrossAllFilesOverAllSides; results are:\n${pprintCustomised(stateAfterTryingCandidate)}"
            )
          end if
          progressRecordingSession.upTo(
            minimumWindowSizeAcrossAllFilesOverAllSides
          )

          stateAfterTryingCandidate
        end if
      end withAllSmallFryMatches

      private def matchesForWindowSize(
          windowSize: Int,
          pathInclusions: PathInclusions,
          thresholdSize: Int => Int
      ): MatchingResult =
        require(0 < windowSize)

        val allowAmbiguousMatches =
          minimumAmbiguousMatchSize <= windowSize

        val maximumNumberOfMatchesSharingContent =
          if !allowAmbiguousMatches then 1
          else ambiguousMatchesThreshold

        assume(1 <= maximumNumberOfMatchesSharingContent)

        def fingerprintStartIndices(
            elements: IndexedSeq[Element]
        ): collection.Seq[(BigInt, Int)] =
          require(elements.size >= windowSize)

          val rollingHashFactory = rollingHashFactoryCache.get(
            windowSize,
            { (windowSize: Int) =>
              val fixedNumberOfBytesInElementHash =
                hashFunction.bits / JavaByte.SIZE

              val windowSizeInBytes =
                fixedNumberOfBytesInElementHash * windowSize

              new RollingHash.Factory(
                windowSize = windowSizeInBytes,
                numberOfFingerprintsToBeTaken =
                  fixedNumberOfBytesInElementHash * totalContentSize - windowSizeInBytes + 1
              )
            }
          )

          val rollingHash = rollingHashFactory()

          val accumulatingResults = mutable.Buffer.empty[(BigInt, Int)]

          def updateFingerprint(elementIndex: Int): Unit =
            val elementBytes =
              hashFunction
                .newHasher()
                .putObject(elements(elementIndex), summon[Funnel[Element]])
                .hash()
                .asBytes()

            elementBytes.foreach(rollingHash.pushByte)
          end updateFingerprint

          // NOTE: fingerprints are incrementally calculated walking *down* the
          // elements.
          val descendingIndices = elements.indices.reverse

          val (primingIndices, fingerprintingIndices) =
            descendingIndices.splitAt(windowSize - 1)

          // Prime to get ready for the first fingerprint...
          primingIndices.foreach(updateFingerprint)

          // ... henceforth, each pass records a new fingerprint, starting with
          // the first.
          fingerprintingIndices.foreach: fingerprintStartIndex =>
            updateFingerprint(fingerprintStartIndex)
            accumulatingResults.addOne(
              rollingHash.fingerprint -> fingerprintStartIndex
            )

          accumulatingResults
        end fingerprintStartIndices

        def sectionsByPotentialMatchKey(
            pathIsIncluded: Path => Boolean
        )(
            sources: Sources[Path, Element],
            fingerprintedInclusionsByPath: Map[Path, FingerprintedInclusions]
        ): MultiDict[PotentialMatchKey, Section[Element]] = MultiDict.from(
          sources.filesByPath
            .filter { case (path, file) =>
              pathIsIncluded(path) && {
                val fileSize          = file.size
                val minimumWindowSize = thresholdSize(fileSize)

                minimumWindowSize to fileSize contains windowSize
              }
            }
            // NOTE: the devil is in the details - `PotentialMatchKey`
            // instances that refer to the same content, but are associated
            // with different sections will collide if put into a map. We want
            // to keep such associations distinct so that they can go into the
            // `MultiDict`, so we change the type ascription to pick up the
            // overload of `flatMap` that will build a sequence, *not* a map.
            .toSeq
            .flatMap { case passThrough @ (path, _) =>
              val fingerprintedInclusions =
                fingerprintedInclusionsByPath(path)

              fingerprintedInclusions.toIterator
                .filter(inclusion =>
                  // The inclusion has to large enough to accommodate the
                  // window size.
                  windowSize + inclusion.start <= 1 + inclusion.end
                )
                .map(passThrough -> _)
            }
            .par
            .flatMap { case ((path, file), CatsInclusiveRange(start, end)) =>
              fingerprintStartIndices(
                file.content.slice(start, 1 + end)
              ).map((fingerprint, fingerprintStartIndex) =>
                val section = sources
                  .section(path)(
                    start + fingerprintStartIndex,
                    windowSize
                  )
                PotentialMatchKey(
                  fingerprint,
                  impliedContent = section
                ) -> section
              )
            }
        )
        end sectionsByPotentialMatchKey

        val baseSectionsByPotentialMatchKey =
          sectionsByPotentialMatchKey(pathInclusions.isIncludedOnBase)(
            baseSources,
            baseFingerprintedInclusionsByPath
          )
        val leftSectionsByPotentialMatchKey =
          sectionsByPotentialMatchKey(pathInclusions.isIncludedOnLeft)(
            leftSources,
            leftFingerprintedInclusionsByPath
          )
        val rightSectionsByPotentialMatchKey =
          sectionsByPotentialMatchKey(pathInclusions.isIncludedOnRight)(
            rightSources,
            rightFingerprintedInclusionsByPath
          )

        def matchKeysAcrossSides(
            basePotentialMatchKeys: collection.Set[PotentialMatchKey],
            leftPotentialMatchKeys: collection.Set[PotentialMatchKey],
            rightPotentialMatchKeys: collection.Set[PotentialMatchKey],
            haveTrimmedMatches: Boolean
        ): MatchingResult =
          val acrossBaseAndLeft =
            if basePotentialMatchKeys.size < leftPotentialMatchKeys.size then
              basePotentialMatchKeys intersect leftPotentialMatchKeys
            else leftPotentialMatchKeys intersect basePotentialMatchKeys

          val acrossBaseAndRight =
            if basePotentialMatchKeys.size < rightPotentialMatchKeys.size then
              basePotentialMatchKeys intersect rightPotentialMatchKeys
            else rightPotentialMatchKeys intersect basePotentialMatchKeys

          val acrossLeftAndRight =
            if leftPotentialMatchKeys.size < rightPotentialMatchKeys.size then
              leftPotentialMatchKeys intersect rightPotentialMatchKeys
            else rightPotentialMatchKeys intersect leftPotentialMatchKeys

          val Seq(smallest, intermediate, _) = Seq(
            acrossBaseAndLeft,
            acrossBaseAndRight,
            acrossLeftAndRight
          ).sortBy(_.size)

          val acrossAllSides = smallest intersect intermediate

          val acrossJustBaseAndLeft = acrossBaseAndLeft diff acrossAllSides

          val acrossJustBaseAndRight = acrossBaseAndRight diff acrossAllSides

          val acrossJustLeftAndRight = acrossLeftAndRight diff acrossAllSides

          case class MatchesFold(
              matches: Set[GenericMatch[Element]],
              haveTrimmedMatches: Boolean
          )

          object MatchesFold:
            def empty: MatchesFold =
              MatchesFold(matches = Set.empty, haveTrimmedMatches = false)
          end MatchesFold

          def allSidesMatchesFrom(
              fold: MatchesFold,
              matchKeyAcrossAllSides: PotentialMatchKey
          ): MatchesFold =
            val potentialMatchesForSynchronisedFingerprint =
              val baseSectionsThatDoNotOverlap = LazyList
                .from(
                  baseSectionsByPotentialMatchKey.get(matchKeyAcrossAllSides)
                )
                .filterNot(baseOverlapsOrIsSubsumedBy)

              val leftSectionsThatDoNotOverlap = LazyList
                .from(
                  leftSectionsByPotentialMatchKey.get(matchKeyAcrossAllSides)
                )
                .filterNot(leftOverlapsOrIsSubsumedBy)

              val rightSectionsThatDoNotOverlap = LazyList
                .from(
                  rightSectionsByPotentialMatchKey.get(matchKeyAcrossAllSides)
                )
                .filterNot(rightOverlapsOrIsSubsumedBy)

              for
                baseSection  <- baseSectionsThatDoNotOverlap
                leftSection  <- leftSectionsThatDoNotOverlap
                rightSection <- rightSectionsThatDoNotOverlap
                // TODO: this filtering isn't currently necessary, because of
                // the existing fingerprinted inclusions mechanism; that has the
                // effect of blocking any matches that would be subsumed by or
                // overlap with an all-sides match. So there can't be an
                // all-sides match subsuming this one. However, this may change
                // soon because of
                // https://github.com/sageserpent-open/kineticMerge/issues/147,
                // so leaving it in place for now...
                if !isSubsumedNonTriviallyByAnAllSidesMatch(
                  baseSection,
                  leftSection,
                  rightSection
                )
              yield (baseSection, leftSection, rightSection)
              end for
            end potentialMatchesForSynchronisedFingerprint

            val (permitted, superfluous) =
              potentialMatchesForSynchronisedFingerprint.splitAt(
                maximumNumberOfMatchesSharingContent
              )

            if superfluous.isEmpty then
              fold.copy(matches =
                fold.matches ++ permitted.map(Match.AllSides.apply)
              )
            else
              logger.warn(
                s"Discarding ambiguous all-sides matches of content: ${pprintCustomised(permitted.head._1.content)} as there are more than $maximumNumberOfMatchesSharingContent matches."
              )
              fold.copy(haveTrimmedMatches = true)
            end if
          end allSidesMatchesFrom

          def baseAndLeftMatchesFrom(
              fold: MatchesFold,
              matchKeyAcrossAllSides: PotentialMatchKey
          ): MatchesFold =
            val potentialMatchesForSynchronisedFingerprint =
              val baseSectionsThatDoNotOverlap = LazyList
                .from(
                  baseSectionsByPotentialMatchKey.get(matchKeyAcrossAllSides)
                )
                .filterNot(baseOverlapsOrIsSubsumedBy)

              val leftSectionsThatDoNotOverlap = LazyList
                .from(
                  leftSectionsByPotentialMatchKey.get(matchKeyAcrossAllSides)
                )
                .filterNot(leftOverlapsOrIsSubsumedBy)

              for
                baseSection <- baseSectionsThatDoNotOverlap
                leftSection <- leftSectionsThatDoNotOverlap
                if !isSubsumedNonTriviallyByAMatchOnTheBaseAndLeft(
                  baseSection,
                  leftSection
                )
              yield (baseSection, leftSection)
              end for
            end potentialMatchesForSynchronisedFingerprint

            val (permitted, superfluous) =
              potentialMatchesForSynchronisedFingerprint.splitAt(
                maximumNumberOfMatchesSharingContent
              )

            if superfluous.isEmpty then
              fold.copy(matches =
                fold.matches ++ permitted.map(Match.BaseAndLeft.apply)
              )
            else
              logger.warn(
                s"Discarding ambiguous base-left matches of content: ${pprintCustomised(permitted.head._1.content)} as there are more than $maximumNumberOfMatchesSharingContent matches."
              )
              fold.copy(haveTrimmedMatches = true)
            end if
          end baseAndLeftMatchesFrom

          def baseAndRightMatchesFrom(
              fold: MatchesFold,
              matchKeyAcrossAllSides: PotentialMatchKey
          ): MatchesFold =
            val potentialMatchesForSynchronisedFingerprint =
              val baseSectionsThatDoNotOverlap = LazyList
                .from(
                  baseSectionsByPotentialMatchKey.get(matchKeyAcrossAllSides)
                )
                .filterNot(baseOverlapsOrIsSubsumedBy)

              val rightSectionsThatDoNotOverlap = LazyList
                .from(
                  rightSectionsByPotentialMatchKey.get(matchKeyAcrossAllSides)
                )
                .filterNot(rightOverlapsOrIsSubsumedBy)

              for
                baseSection  <- baseSectionsThatDoNotOverlap
                rightSection <- rightSectionsThatDoNotOverlap
                if !isSubsumedNonTriviallyByAMatchOnTheBaseAndRight(
                  baseSection,
                  rightSection
                )
              yield (baseSection, rightSection)
              end for
            end potentialMatchesForSynchronisedFingerprint

            val (permitted, superfluous) =
              potentialMatchesForSynchronisedFingerprint.splitAt(
                maximumNumberOfMatchesSharingContent
              )

            if superfluous.isEmpty then
              fold.copy(matches =
                fold.matches ++ permitted.map(Match.BaseAndRight.apply)
              )
            else
              logger.warn(
                s"Discarding ambiguous base-right matches of content: ${pprintCustomised(permitted.head._1.content)} as there are more than $maximumNumberOfMatchesSharingContent matches."
              )
              fold.copy(haveTrimmedMatches = true)
            end if
          end baseAndRightMatchesFrom

          def leftAndRightMatchesFrom(
              fold: MatchesFold,
              matchKeyAcrossAllSides: PotentialMatchKey
          ): MatchesFold =
            val potentialMatchesForSynchronisedFingerprint =
              val leftSectionsThatDoNotOverlap = LazyList
                .from(
                  leftSectionsByPotentialMatchKey.get(matchKeyAcrossAllSides)
                )
                .filterNot(leftOverlapsOrIsSubsumedBy)

              val rightSectionsThatDoNotOverlap = LazyList
                .from(
                  rightSectionsByPotentialMatchKey.get(matchKeyAcrossAllSides)
                )
                .filterNot(rightOverlapsOrIsSubsumedBy)

              for
                leftSection  <- leftSectionsThatDoNotOverlap
                rightSection <- rightSectionsThatDoNotOverlap
                if !isSubsumedNonTriviallyByAMatchOnTheLeftAndRight(
                  leftSection,
                  rightSection
                )
              yield (leftSection, rightSection)
              end for
            end potentialMatchesForSynchronisedFingerprint

            val (permitted, superfluous) =
              potentialMatchesForSynchronisedFingerprint.splitAt(
                maximumNumberOfMatchesSharingContent
              )

            if superfluous.isEmpty then
              fold.copy(matches =
                fold.matches ++ permitted.map(Match.LeftAndRight.apply)
              )
            else
              logger.warn(
                s"Discarding ambiguous left-right matches of content: ${pprintCustomised(permitted.head._1.content)} as there are more than $maximumNumberOfMatchesSharingContent matches."
              )
              fold.copy(haveTrimmedMatches = true)
            end if
          end leftAndRightMatchesFrom

          val withAllMatches =
            acrossJustLeftAndRight.foldLeft(
              acrossJustBaseAndRight.foldLeft(
                acrossJustBaseAndLeft.foldLeft(
                  acrossAllSides.foldLeft(MatchesFold.empty)(
                    allSidesMatchesFrom
                  )
                )(baseAndLeftMatchesFrom)
              )(baseAndRightMatchesFrom)
            )(leftAndRightMatchesFrom)

          withMatches(
            withAllMatches.matches,
            withAllMatches.haveTrimmedMatches
          )
        end matchKeysAcrossSides

        matchKeysAcrossSides(
          baseSectionsByPotentialMatchKey.keySet,
          leftSectionsByPotentialMatchKey.keySet,
          rightSectionsByPotentialMatchKey.keySet,
          haveTrimmedMatches = false
        )
      end matchesForWindowSize
    end MatchesAndTheirSections

    object PotentialMatchKey:
      val impliedContentEquality: Eq[Section[Element]] =
        Eq.by[Section[Element], Seq[Element]](
          _.content.take(tiebreakContentSamplingLimit)
        )
    end PotentialMatchKey

    // NOTE: this is subtle - this type is used as an ordered key to find
    // matches across sides; fingerprints can and do collide, so we need the
    // content as a tiebreaker. However, we don't want to have to freight the
    // content around for keys that will never match across sides - there are a
    // lot of keys involved in finding matches at low window sizes, and large
    // window sizes imply large content sizes.
    //
    // The solution is to rely on lazy evaluation semantics for ordering of
    // pairs, and to evaluate the content of the section when it's really needed
    // to break a tie on fingerprints. However, this means that when there are
    // multiple matches whose keys collide, then only one key can represent the
    // matches in a `SortedMultiDict` - so we expect to see keys whose section
    // is unrelated to some of the matches it is associated with, but is a
    // legitimate key for them nonetheless.
    case class PotentialMatchKey(
        fingerprint: BigInt,
        impliedContent: Section[Element]
    ):
      // NOTE: instances of `PotentialMatchKey` are intended to be put into sets
      // using hashing, so we may as well get on with it and compute the
      // inevitable hash code.
      private val cachedHashCode: Int =
        val hasher = hashFunction.newHasher()

        hasher.putBytes(fingerprint.toByteArray)

        impliedContent.content
          .take(tiebreakContentSamplingLimit)
          .foreach(hasher.putObject(_, summon[Funnel[Element]]))

        hasher.hash().asInt()
      end cachedHashCode

      override def equals(another: Any): Boolean =
        another.asInstanceOf[Matchable] match
          case PotentialMatchKey(anotherFingerprint, anotherImpliedContent) =>
            fingerprint == anotherFingerprint && PotentialMatchKey.impliedContentEquality
              .eqv(
                impliedContent,
                anotherImpliedContent
              )
          case _ => false

      override def hashCode(): Int = cachedHashCode
    end PotentialMatchKey

    MatchesAndTheirSections.withAllMatchesOfAtLeastTheSureFireWindowSize()
  end of

  sealed trait AbstractConfiguration:
    val minimumMatchSize: Int
    val thresholdSizeFractionForMatching: Double
    val minimumAmbiguousMatchSize: Int
    val ambiguousMatchesThreshold: Int
    val progressRecording: ProgressRecording
  end AbstractConfiguration

  class AdmissibleFailure(message: String) extends RuntimeException(message)

  /** @param minimumMatchSize
    * @param thresholdSizeFractionForMatching
    *   A section's size must be at least this fraction of its containing file's
    *   size to qualify for matching.
    * @param minimumAmbiguousMatchSize
    * @param ambiguousMatchesThreshold
    *   The maximum number of ambiguous matches that may refer to the same
    *   content. If the threshold is exceeded, that content results in no
    *   matches at all.
    * @param progressRecording
    *   Used to record progress during matching.
    */
  case class Configuration(
      minimumMatchSize: Int,
      thresholdSizeFractionForMatching: Double,
      minimumAmbiguousMatchSize: Int,
      ambiguousMatchesThreshold: Int,
      progressRecording: ProgressRecording = NoProgressRecording
  ) extends AbstractConfiguration:
    // TODO: why would `minimumMatchSize` be zero?
    require(0 <= minimumMatchSize)
    require(0 <= thresholdSizeFractionForMatching)
    require(1 >= thresholdSizeFractionForMatching)
    require(0 <= minimumAmbiguousMatchSize)
    require(1 <= ambiguousMatchesThreshold)
  end Configuration
end MatchAnalysis
