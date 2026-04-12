package com.sageserpent.kineticmerge.core

import alleycats.std.set.given
import cats.collections.{Diet, DisjointSets, Range as CatsInclusiveRange}
import cats.data.State
import cats.implicits.catsKernelOrderingForOrder
import cats.instances.seq.*
import cats.syntax.all.{toFoldableOps, toTraverseOps}
import cats.{Eq, Order}
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
import scala.collection.immutable.{MultiDict, SortedMultiSet}
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*
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

    val tiebreakContentSamplingLimit = 5

    object MatchesAndTheirSections:
      type ParallelMatchesGroupIdTracking[X] =
        State[Map[GenericMatch[Element], ParallelMatchesGroupId], X]
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
        State.pure {
          pairwiseMatchesToBeEaten.sets
            .flatMap[PairwiseMatch] { case (pairwiseMatch, bites) =>
              val sortedBiteEdges = sortedBiteEdgesFrom(bites.flatMap {
                case (_, biteStart, biteEnd) => Seq(biteStart, biteEnd)
              })

              val fragmentsFromPairwiseMatch: Seq[PairwiseMatch] =
                pairwiseMatch match
                  case Match.BaseAndLeft(baseSection, leftSection) =>
                    (eatIntoSection(
                      baseSources,
                      sortedBiteEdges
                    )(
                      baseSection
                    ) zip eatIntoSection(
                      leftSources,
                      sortedBiteEdges
                    )(
                      leftSection
                    ))
                      .map(Match.BaseAndLeft.apply)

                  case Match.BaseAndRight(baseSection, rightSection) =>
                    (eatIntoSection(
                      baseSources,
                      sortedBiteEdges
                    )(
                      baseSection
                    ) zip eatIntoSection(
                      rightSources,
                      sortedBiteEdges
                    )(
                      rightSection
                    )).map(Match.BaseAndRight.apply)

                  case Match.LeftAndRight(leftSection, rightSection) =>
                    (eatIntoSection(
                      leftSources,
                      sortedBiteEdges
                    )(
                      leftSection
                    ) zip eatIntoSection(
                      rightSources,
                      sortedBiteEdges
                    )(
                      rightSection
                    )).map(Match.LeftAndRight.apply)

              logger.debug(
                s"Eating into pairwise match:\n${pprintCustomised(pairwiseMatch)} on behalf of all-sides matches:\n${pprintCustomised(bites)}, resulting in fragments:\n${pprintCustomised(fragmentsFromPairwiseMatch)}"
              )

              fragmentsFromPairwiseMatch
            }
            .toSet
        }

      def sortedBiteEdgesFrom(bites: collection.Set[BiteEdge]): Seq[BiteEdge] =
        bites.toSeq.sortWith {
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

      // Breaks up a section by eating into it with smaller bites, yielding the
      // fragment sections.
      private def eatIntoSection(
          side: Sources[Path, Element],
          biteEdges: Seq[BiteEdge]
      )(
          section: Section[Element]
      ): Seq[Section[Element]] =
        val mealOnePastEndOffset: Int = section.onePastEndOffset

        val path = side.pathFor(section)

        case class State(
            mealStartOffset: Int,
            biteDepth: Int
        ):
          @tailrec
          final def apply(
              biteEdges: Seq[BiteEdge],
              fragments: Vector[Section[Element]]
          ): Vector[Section[Element]] =
            biteEdges match
              case Seq() =>
                if mealOnePastEndOffset > mealStartOffset then
                  fragments.appended(
                    side.section(path)(
                      startOffset = mealStartOffset,
                      size = mealOnePastEndOffset - mealStartOffset
                    )
                  )
                else fragments
              case Seq(
                    BiteEdge.Start(startOffsetRelativeToMeal),
                    remainingBiteEdges*
                  ) =>
                val startOffset =
                  section.startOffset + startOffsetRelativeToMeal

                require(mealOnePastEndOffset > startOffset)
                val guardedStartOffset = startOffset max mealStartOffset

                this
                  .copy(
                    mealStartOffset = guardedStartOffset,
                    biteDepth = 1 + biteDepth
                  )
                  .apply(
                    remainingBiteEdges,
                    fragments =
                      if 0 == biteDepth && guardedStartOffset > mealStartOffset
                      then
                        fragments.appended(
                          side.section(path)(
                            startOffset = mealStartOffset,
                            size = guardedStartOffset - mealStartOffset
                          )
                        )
                      else fragments
                  )
              case Seq(
                    BiteEdge.End(onePastEndOffsetRelativeToMeal),
                    remainingBiteEdges*
                  ) =>
                require(0 < biteDepth)

                val onePastEndOffset =
                  section.startOffset + onePastEndOffsetRelativeToMeal

                require(mealStartOffset <= onePastEndOffset)
                val guardedOnePastEndOffset =
                  onePastEndOffset min mealOnePastEndOffset

                this
                  .copy(
                    mealStartOffset = guardedOnePastEndOffset,
                    biteDepth = biteDepth - 1
                  )
                  .apply(remainingBiteEdges, fragments)
        end State

        State(
          mealStartOffset = section.startOffset,
          biteDepth = 0
        )(biteEdges, fragments = Vector.empty)
      end eatIntoSection

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
        parallelMatchesGroupIdsByMatch: Map[GenericMatch[
          Element
        ], ParallelMatchesGroupId]
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

      def reconcileMatches: MatchesAndTheirSections =
        val matches = sectionsAndTheirMatches.values.toSet

        val Success(result) = Using(
          progressRecording.newSession(
            label = "Number of matches to reconcile:",
            maximumProgress = matches.size
          )(initialProgress = matches.size)
        ) { progressRecordingSession =>
          @tailrec
          def reconcileUsing(
              allSidesMatches: Set[Match.AllSides[Section[Element]]]
          ): MatchesAndTheirSections =
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

            val workflow = for
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

              stepResult <-
                if paredDownAllSidesMatches == allSidesMatches then
                  for
                    // See note above.
                    paredDownFragments <- fragments.traverse(
                      takingFragmentationIntoAccount.pareDownOrSuppressCompletely
                    )
                    rebuilt =
                      (paredDownMatches union paredDownFragments.flatten)
                        .foldLeft(MatchesAndTheirSections.empty)(_ withMatch _)
                    _      = rebuilt.checkInvariant()
                    result = rebuilt.withoutRedundantPairwiseMatches
                    _      = result.checkInvariant()
                  yield Right(result)
                else
                  State.pure(
                    Left(paredDownAllSidesMatches -> paredDownMatches.size)
                  )
            yield stepResult

            // NOTE: we want to re-evaluate the state change here and now
            // *within* the call to `reconcileUsing`, so don't cut over to
            // using `tailRecM` to drive the tail-recursion, as that would force
            // evaluation to occur around the initial call-site over all the
            // recursive steps.
            val outcome = workflow.runA(Map.empty).value

            outcome match
              case Left(paredDownAllSidesMatches, progress) =>
                progressRecordingSession.upTo(progress)
                reconcileUsing(paredDownAllSidesMatches)
              case Right(result) =>
                progressRecordingSession.upTo(0)
                result
            end match
          end reconcileUsing

          reconcileUsing(matches.collect {
            case allSides: Match.AllSides[Section[Element]] => allSides
          })
        }: @unchecked

        result.reconciliationPostcondition()

        result

      end reconcileMatches

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
        end if
      end reconciliationPostcondition

      private def pareDownOrSuppressCompletely[MatchType <: GenericMatch[
        Element
      ]](
          aMatch: MatchType
      ): ParallelMatchesGroupIdTracking[Option[ParedDownMatch[MatchType]]] =
        State.pure {
          // NOTE: one thing to watch out is when fragments resulting from
          // larger pairwise matches being eaten into collide with equivalent
          // pairwise matches found by fingerprint matching.
          // This can take the form of the fragments coming first due to larger
          // all-sides matches, or the pairwise matches from fingerprint
          // matching
          // can be followed by fragmentation if the all-sides eating into the
          // larger pairwise matches also come from the same fingerprinting that
          // yielded the pairwise matching. Intercepting this here addresses
          // both
          // cases.
          aMatch match
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
                // *different*
                // matches, each doing a one-sided subsumption.
                (
                  subsumedBySomeMatchOnJustTheBase,
                  subsumedBySomeMatchOnJustTheLeft,
                  subsumedBySomeMatchOnJustTheRight
                ) match
                  case (false, false, false) => Some(aMatch)
                  case (true, false, false)  =>
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
          end match
        }
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

        (subsumingOnBase intersect subsumingOnLeft).map {
          case subsuming: Match.BaseAndLeft[Section[Element]] =>
            (
              subsuming,
              BiteEdge.Start(startOffsetRelativeToMeal =
                allSides.baseElement.startOffset - subsuming.baseElement.startOffset
              ),
              BiteEdge.End(onePastEndOffsetRelativeToMeal =
                allSides.baseElement.onePastEndOffset - subsuming.baseElement.startOffset
              )
            )
        } union (subsumingOnBase intersect subsumingOnRight).map {
          case subsuming: Match.BaseAndRight[Section[Element]] =>
            (
              subsuming,
              BiteEdge.Start(startOffsetRelativeToMeal =
                allSides.baseElement.startOffset - subsuming.baseElement.startOffset
              ),
              BiteEdge.End(onePastEndOffsetRelativeToMeal =
                allSides.baseElement.onePastEndOffset - subsuming.baseElement.startOffset
              )
            )
        } union (subsumingOnLeft intersect subsumingOnRight).map {
          case subsuming: Match.LeftAndRight[Section[Element]] =>
            (
              subsuming,
              BiteEdge.Start(startOffsetRelativeToMeal =
                allSides.leftElement.startOffset - subsuming.leftElement.startOffset
              ),
              BiteEdge.End(onePastEndOffsetRelativeToMeal =
                allSides.leftElement.onePastEndOffset - subsuming.leftElement.startOffset
              )
            )
        }
      end pairwiseMatchesSubsumingOnBothSidesWithBiteEdges

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
              path -> sections.toIndexedSeq.sortBy(_.startOffset)
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

        // 4. Put the back-translated matches into their own disjoint sets, then
        // unify those matches that come from the same group of parallel
        // matches, then unify all-sides matches with pairwise matches that
        // subsume them, regardless of the original group.

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

        val disjointSetsOfMatches =
          // NASTY HACK: the Cats implementation of a disjoint sets data
          // structure insists on the set elements having `Order`. The problem
          // with this is that while it's easy to define an order on `Match`, it
          // gets fiddly down at the `Section` level, because we need to take
          // paths into account as well as the obvious start offset and size -
          // that means we have to distinguish between sections belonging to the
          // base, left or right sides and look up the path accordingly.
          // The workaround is to rely on *equality* of matches being
          // well-formed (it is) and then building a synthetic order based on
          // ids.
          val backTranslatedMatchIds = backTranslatedMatches.zipWithIndex.toMap

          given matchOrdering: Order[Match[Section[Element]]] =
            Order.by(backTranslatedMatchIds.apply)

          DisjointSets(
            backTranslatedMatches.toSeq*
          )
        end disjointSetsOfMatches

        val unusedValue = true

        val coalescenceWorkflow =
          for
            _ <- groupsOfBackTranslatedParallelMatches
              .map(_.filter(backTranslatedMatches.contains))
              .filter(
                _.nonEmpty
              ) // Guard the zipping of group members down below...
              .foldM(unusedValue)((_, group) =>
                // Unify the group members...
                group
                  .zip(group.tail)
                  .toSeq
                  .foldM(unusedValue) {
                    case (_, (precedingGroupMember, succeedingGroupMember)) =>
                      DisjointSets
                        .union(precedingGroupMember, succeedingGroupMember)
                  }
              )
            _ <- backTranslatedMatches
              .filter(_.isAnAllSidesMatch)
              .foldM(unusedValue) {
                case (_, allSidesMatch: Match.AllSides[Section[Element]]) =>
                  backTranslatedMatchesAndTheirSections
                    .pairwiseMatchesSubsumingOnBothSides(allSidesMatch)
                    .foldM(unusedValue)((_, pairwiseMatch) =>
                      DisjointSets.union(allSidesMatch, pairwiseMatch)
                    )
              }
            sets <- DisjointSets.toSets
          yield sets

        val parallelMatchesGroupIdsByMatch = coalescenceWorkflow
          .runA(disjointSetsOfMatches)
          .value
          .toList
          .map(_._2.toIterator)
          .zipWithIndex
          .flatMap((group, id) => group.map(_ -> id))
          .toMap

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
