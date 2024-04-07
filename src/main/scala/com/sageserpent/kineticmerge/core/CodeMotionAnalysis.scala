package com.sageserpent.kineticmerge.core

import cats.implicits.catsKernelOrderingForOrder
import cats.instances.seq.*
import cats.{Eq, Order}
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import com.google.common.hash.{Funnel, HashFunction}
import com.sageserpent.kineticmerge.core
import com.typesafe.scalalogging.StrictLogging
import de.sciss.fingertree.RangedSeq
import monocle.syntax.all.*

import java.lang.Byte as JavaByte
import scala.annotation.tailrec
import scala.collection.immutable.MultiDict
import scala.collection.{SortedMultiDict, mutable}
import scala.util.Try

trait CodeMotionAnalysis[Path, Element]:
  def base: Map[Path, File[Element]]
  def left: Map[Path, File[Element]]
  def right: Map[Path, File[Element]]

  def matchesFor(
      section: Section[Element]
  ): collection.Set[Match[Section[Element]]]
end CodeMotionAnalysis

object CodeMotionAnalysis extends StrictLogging:
  /** Analyse code motion from the sources of {@code base} to both {@code left}
    * and {@code right}, breaking them into [[File]] and thence [[Section]]
    * instances.
    *
    * Where a section moves from {@code base}, it enters into a match with one
    * or both corresponding sections in {@code left} and {@code right}; if both,
    * then one of those latter two sections is considered to be dominant and
    * therefore represents the match. If there is just one matching section,
    * that is taken to be the dominant one for the sake of picking up whitespace
    * changes.
    *
    * @note
    *   Although code motion is strictly speaking relative to the base sources,
    *   if the same section is added into both the left and right sources as a
    *   coincidental insertion (so not present in the base sources), this is
    *   treated as a match across the left and right sources anyway, so there
    *   will be a dominant section.
    * @param base
    *   The common base sources from which the left and right sources are
    *   derived.
    * @param left
    *   'Our' sources, from the Git standpoint...
    * @param right
    *   'Their' sources, from the Git standpoint...
    * @param thresholdSizeFractionForMatching
    *   A section's size must be at least this fraction of its containing file's
    *   size to qualify for matching.
    * @tparam Path
    * @return
    *   A [[CodeMotionAnalysis]] that contains a breakdown into [[File]]
    *   instances and thence into [[Section]] instances for each of the three
    *   sources.
    */
  def of[Path, Element](
      base: Sources[Path, Element],
      left: Sources[Path, Element],
      right: Sources[Path, Element]
  )(
      minimumMatchSize: Int,
      thresholdSizeFractionForMatching: Double,
      propagateExceptions: Boolean = true
  )(
      elementEquality: Eq[Element],
      elementOrder: Order[Element],
      elementFunnel: Funnel[Element],
      hashFunction: HashFunction
  ): Either[Throwable, CodeMotionAnalysis[Path, Element]] =
    require(0 <= thresholdSizeFractionForMatching)
    require(1 >= thresholdSizeFractionForMatching)

    given witness: Eq[Element] = elementEquality

    // TODO: suppose all the sources are empty? Could this happen?
    val fileSizes = base.filesByPath.values.map(_.size) ++
      left.filesByPath.values.map(_.size) ++
      right.filesByPath.values.map(_.size)

    val totalContentSize = fileSizes.sum

    val minimumFileSizeAcrossAllFilesOverAllSides = fileSizes.min

    // This is the minimum window size that would be allowed in *some* file
    // across the sources.
    val minimumWindowSizeAcrossAllFilesOverAllSides =
      minimumMatchSize max (minimumFileSizeAcrossAllFilesOverAllSides * thresholdSizeFractionForMatching).floor.toInt

    val maximumFileSizeAcrossAllFilesOverAllSides = fileSizes.max

    // This is the minimum window size that would be allowed in *all* files
    // across the sources.
    val minimumSureFireWindowSizeAcrossAllFilesOverAllSides =
      minimumMatchSize max (maximumFileSizeAcrossAllFilesOverAllSides * thresholdSizeFractionForMatching).floor.toInt

    logger.debug(
      s"Minimum match window size across all files over all sides: $minimumWindowSizeAcrossAllFilesOverAllSides"
    )
    logger.debug(
      s"Minimum sure-fire match window size across all files over all sides: $minimumSureFireWindowSizeAcrossAllFilesOverAllSides"
    )
    logger.debug(
      s"Maximum match window size across all files over all sides: $maximumFileSizeAcrossAllFilesOverAllSides"
    )

    type SectionsSeen = RangedSeq[Section[Element], Int]

    type MatchedSections = MultiDict[Section[Element], Match[Section[Element]]]

    object MatchesAndTheirSections:
      type PairwiseMatch = Match.BaseAndLeft[Section[Element]] |
        Match.BaseAndRight[Section[Element]] |
        Match.LeftAndRight[Section[Element]]

      lazy val empty = MatchesAndTheirSections(
        baseSectionsByPath = Map.empty,
        leftSectionsByPath = Map.empty,
        rightSectionsByPath = Map.empty,
        sectionsAndTheirMatches = MultiDict.empty
      )
      private val rollingHashFactoryCache: Cache[Int, RollingHash.Factory] =
        Caffeine.newBuilder().build()

      private def subsumesSection(
          side: Sources[Path, Element],
          sectionsByPath: Map[Path, SectionsSeen]
      )(section: Section[Element]): Boolean =
        sectionsByPath
          .get(side.pathFor(section))
          .fold(ifEmpty = false)(
            _.filterIncludes(section.closedOpenInterval).exists(_ != section)
          )

      private def subsumesSectionViaAtLeastOneAllSidesMatch(
          sectionsAndTheirMatches: MatchedSections
      )(
          side: Sources[Path, Element],
          sectionsByPath: Map[Path, SectionsSeen]
      )(section: Section[Element]): Boolean =
        sectionsByPath
          .get(side.pathFor(section))
          .fold(ifEmpty = false)(
            _.filterIncludes(section.closedOpenInterval)
              .filter(_ != section)
              .flatMap(sectionsAndTheirMatches.get)
              .exists {
                case _: Match.AllSides[Section[Element]] => true
                case _                                   => false
              }
          )

      private def subsumingPairwiseMatches(
          sectionsAndTheirMatches: MatchedSections
      )(
          side: Sources[Path, Element],
          sectionsByPath: Map[Path, SectionsSeen]
      )(section: Section[Element]): Set[PairwiseMatch] =
        sectionsByPath
          .get(side.pathFor(section))
          .fold(ifEmpty = Set.empty)(
            _.filterIncludes(section.closedOpenInterval)
              .filter(_ != section)
              .flatMap(sectionsAndTheirMatches.get)
              // NOTE: convert to a set at this point as we expect sections to
              // be duplicated when involved in ambiguous matches.
              .toSet
              .collect {
                case baseAndLeft: Match.BaseAndLeft[Section[Element]] =>
                  baseAndLeft
                case baseAndRight: Match.BaseAndRight[Section[Element]] =>
                  baseAndRight
                case leftAndRight: Match.LeftAndRight[Section[Element]] =>
                  leftAndRight
              }
          )

      private def overlapsSection(
          side: Sources[Path, Element],
          sectionsByPath: Map[Path, SectionsSeen]
      )(section: Section[Element]): Boolean =
        sectionsByPath
          .get(side.pathFor(section))
          .fold(ifEmpty = false)(
            _.filterOverlaps(section.closedOpenInterval)
              // Subsuming sections are considered to be overlapping by the
              // implementation of `SectionSeen`, so use an existence check with
              // a nuanced predicate.
              .exists(candidateSection =>
                candidateSection.startOffset > section.startOffset || candidateSection.onePastEndOffset < section.onePastEndOffset
              )
          )

      private def withSection(
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
      end withSection

      // Coalesces runs of overlapping sections together and reports the size of
      // the largest coalescence.
      private def maximumSizeOfCoalescedSections(
          sectionsSeen: SectionsSeen
      ): Option[Int] =
        val sectionsInOrderOfStartOffset = sectionsSeen.iterator.toSeq

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

      // Breaks up a section by eating into it with smaller bites, yielding the
      // leftover sections.
      private def eatIntoSection(
          side: Sources[Path, Element],
          bites: collection.Set[Section[Element]]
      )(
          section: Section[Element]
      ): Seq[Section[Element]] =
        // TODO: can this be made to eliminate overlaps too?

        enum BiteEdge:
          case Start(startOffset: Int)
          case End(onePastEndOffset: Int)
        end BiteEdge

        val biteEdges: Seq[BiteEdge] = bites.toSeq
          .flatMap { biteSection =>
            Seq(
              BiteEdge.Start(biteSection.startOffset),
              BiteEdge.End(biteSection.onePastEndOffset)
            )
          }
          .sortWith {
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

        val mealOnePastEndOffset: Int = section.onePastEndOffset

        val path = side.pathFor(section)

        case class State(
            mealStartOffset: Int,
            biteDepth: Int
        ):
          @tailrec
          final def apply(
              biteEdges: Seq[BiteEdge],
              leftovers: Vector[Section[Element]]
          ): Vector[Section[Element]] =
            biteEdges match
              case Seq() =>
                if mealOnePastEndOffset > mealStartOffset then
                  leftovers.appended(
                    side.section(path)(
                      startOffset = mealStartOffset,
                      size = mealOnePastEndOffset - mealStartOffset
                    )
                  )
                else leftovers
              case Seq(BiteEdge.Start(startOffset), remainingBiteEdges*) =>
                require(mealOnePastEndOffset > startOffset)
                val guardedStartOffset = startOffset max mealStartOffset

                this
                  .copy(
                    mealStartOffset = guardedStartOffset,
                    biteDepth = 1 + biteDepth
                  )
                  .apply(
                    remainingBiteEdges,
                    leftovers =
                      if 0 == biteDepth && guardedStartOffset > mealStartOffset
                      then
                        leftovers.appended(
                          side.section(path)(
                            startOffset = mealStartOffset,
                            size = guardedStartOffset - mealStartOffset
                          )
                        )
                      else leftovers
                  )
              case Seq(BiteEdge.End(onePastEndOffset), remainingBiteEdges*) =>
                require(0 < biteDepth)

                require(mealStartOffset < onePastEndOffset)
                val guardedOnePastEndOffset =
                  onePastEndOffset min mealOnePastEndOffset

                this
                  .copy(
                    mealStartOffset = guardedOnePastEndOffset,
                    biteDepth = biteDepth - 1
                  )
                  .apply(remainingBiteEdges, leftovers)
        end State

        State(
          mealStartOffset = section.startOffset,
          biteDepth = 0
        )(biteEdges, leftovers = Vector.empty)
      end eatIntoSection

      case class MatchingResult(
          matchesAndTheirSections: MatchesAndTheirSections,
          numberOfMatchesForTheGivenWindowSize: Int,
          estimatedWindowSizeForOptimalMatch: Option[Int]
      )

    end MatchesAndTheirSections

    case class MatchesAndTheirSections(
        baseSectionsByPath: Map[Path, SectionsSeen],
        leftSectionsByPath: Map[Path, SectionsSeen],
        rightSectionsByPath: Map[Path, SectionsSeen],
        sectionsAndTheirMatches: MatchedSections
    ):
      import MatchesAndTheirSections.*

      private val subsumesBaseSection: Section[Element] => Boolean =
        subsumesSection(base, baseSectionsByPath)
      private val subsumesLeftSection: Section[Element] => Boolean =
        subsumesSection(left, leftSectionsByPath)
      private val subsumesRightSection: Section[Element] => Boolean =
        subsumesSection(right, rightSectionsByPath)
      private val subsumesBaseSectionViaAtLeastOneAllSidesMatch
          : Section[Element] => Boolean =
        subsumesSectionViaAtLeastOneAllSidesMatch(sectionsAndTheirMatches)(
          base,
          baseSectionsByPath
        )
      private val subsumesLeftSectionViaAtLeastOneAllSidesMatch
          : Section[Element] => Boolean =
        subsumesSectionViaAtLeastOneAllSidesMatch(sectionsAndTheirMatches)(
          left,
          leftSectionsByPath
        )
      private val subsumesRightSectionViaAtLeastOneAllSidesMatch
          : Section[Element] => Boolean =
        subsumesSectionViaAtLeastOneAllSidesMatch(sectionsAndTheirMatches)(
          right,
          rightSectionsByPath
        )
      private val overlapsBaseSection: Section[Element] => Boolean =
        overlapsSection(base, baseSectionsByPath)
      private val overlapsLeftSection: Section[Element] => Boolean =
        overlapsSection(left, leftSectionsByPath)
      private val overlapsRightSection: Section[Element] => Boolean =
        overlapsSection(right, rightSectionsByPath)
      private val withBaseSection: Section[Element] => Map[Path, SectionsSeen] =
        withSection(base, baseSectionsByPath)
      private val withLeftSection: Section[Element] => Map[Path, SectionsSeen] =
        withSection(left, leftSectionsByPath)
      private val withRightSection
          : Section[Element] => Map[Path, SectionsSeen] =
        withSection(right, rightSectionsByPath)

      def baseSections: Set[Section[Element]] =
        baseSectionsByPath.values.flatMap(_.iterator).toSet

      def leftSections: Set[Section[Element]] =
        leftSectionsByPath.values.flatMap(_.iterator).toSet

      def rightSections: Set[Section[Element]] =
        rightSectionsByPath.values.flatMap(_.iterator).toSet

      @tailrec
      final def withAllMatchesOfAtLeastTheSureFireWindowSize(
          looseExclusiveUpperBoundOnMaximumMatchSize: Int =
            1 + maximumFileSizeAcrossAllFilesOverAllSides
      ): MatchesAndTheirSections =
        // Essentially a binary chop algorithm, but using
        // `fallbackImprovedState` to track the best solution.
        @tailrec
        def keepTryingToImproveThis(
            bestMatchSize: Int,
            looseExclusiveUpperBoundOnMaximumMatchSize: Int,
            guessAtOptimalMatchSize: Option[Int],
            fallbackImprovedState: MatchesAndTheirSections
        ): MatchesAndTheirSections =
          require(bestMatchSize < looseExclusiveUpperBoundOnMaximumMatchSize)

          if 1 + bestMatchSize < looseExclusiveUpperBoundOnMaximumMatchSize
          then
            // There is at least one candidate window size greater than
            // `bestMatchSize`...
            val candidateWindowSize = guessAtOptimalMatchSize
              .filter(_ < looseExclusiveUpperBoundOnMaximumMatchSize)
              .getOrElse(
                (bestMatchSize + looseExclusiveUpperBoundOnMaximumMatchSize) / 2
              )

            val MatchingResult(
              stateAfterTryingCandidate,
              numberOfMatchesForTheGivenWindowSize,
              estimatedWindowSizeForOptimalMatch
            ) = this.matchesForWindowSize(candidateWindowSize)

            estimatedWindowSizeForOptimalMatch match
              case None =>
                // Failed to improve the match size, try again with the
                // contracted upper bound.
                keepTryingToImproveThis(
                  bestMatchSize,
                  looseExclusiveUpperBoundOnMaximumMatchSize =
                    candidateWindowSize,
                  guessAtOptimalMatchSize = None,
                  fallbackImprovedState
                )
              case Some(estimate)
                  if estimate == candidateWindowSize || 1 == numberOfMatchesForTheGivenWindowSize =>
                // Found the optimal solution; try searching for the next lowest
                // optimal size. NOTE: this won't pick up multiple distinct
                // optimal matches, see below.
                logger.debug(
                  s"Search has found an optimal match at window size: $candidateWindowSize, number of matches is: $numberOfMatchesForTheGivenWindowSize, restarting search to look for smaller matches."
                )
                stateAfterTryingCandidate
                  .withAllMatchesOfAtLeastTheSureFireWindowSize(
                    looseExclusiveUpperBoundOnMaximumMatchSize =
                      candidateWindowSize
                  )
              case Some(estimate) =>
                // We have an improvement, move the lower bound up and note the
                // improved state.
                logger.debug(
                  s"Search has found an improved match at window size: $candidateWindowSize, number of matches is: $numberOfMatchesForTheGivenWindowSize, looking for a more optimal match with estimated window size of: $estimate."
                )
                keepTryingToImproveThis(
                  bestMatchSize = candidateWindowSize,
                  looseExclusiveUpperBoundOnMaximumMatchSize,
                  guessAtOptimalMatchSize = Some(estimate),
                  fallbackImprovedState = stateAfterTryingCandidate
                )
            end match
          else if minimumSureFireWindowSizeAcrossAllFilesOverAllSides == looseExclusiveUpperBoundOnMaximumMatchSize
          then
            // There is nowhere left to search.
            logger.debug(
              s"Search for matches whose size is no less than the sure-fire match window size of: $minimumSureFireWindowSizeAcrossAllFilesOverAllSides has terminated; results are:\n${pprintCustomised(fallbackImprovedState)}"
            )
            fallbackImprovedState
          else
            // The optimal matches are in the fallback improved state; try
            // searching for the next lowest optimal size. This is necessary as
            // we may have *multiple* distinct optimal matches at a given window
            // size.
            logger.debug(
              s"Search has found optimal matches at window size: $bestMatchSize, restarting search to look for smaller matches."
            )
            fallbackImprovedState.withAllMatchesOfAtLeastTheSureFireWindowSize(
              looseExclusiveUpperBoundOnMaximumMatchSize = bestMatchSize
            )
          end if
        end keepTryingToImproveThis

        keepTryingToImproveThis(
          bestMatchSize =
            minimumSureFireWindowSizeAcrossAllFilesOverAllSides - 1,
          looseExclusiveUpperBoundOnMaximumMatchSize,
          guessAtOptimalMatchSize = None,
          fallbackImprovedState = this
        )
      end withAllMatchesOfAtLeastTheSureFireWindowSize

      @tailrec
      final def withAllSmallFryMatches(
          candidateWindowSize: Int
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
          _
        ) =
          this.matchesForWindowSize(candidateWindowSize)

        if candidateWindowSize > minimumWindowSizeAcrossAllFilesOverAllSides
        then
          if 0 < numberOfMatchesForTheGivenWindowSize then
            logger.debug(
              s"Search has found a match at window size: $candidateWindowSize, number of matches is: $numberOfMatchesForTheGivenWindowSize, continuing to look for smaller matches."
            )
          end if
          stateAfterTryingCandidate.withAllSmallFryMatches(
            candidateWindowSize - 1
          )
        else
          if 0 < numberOfMatchesForTheGivenWindowSize then
            logger.debug(
              s"Search has found a match at window size: $candidateWindowSize, number of matches is: $numberOfMatchesForTheGivenWindowSize, search for matches whose size is less than the sure-fire match window size of: $minimumSureFireWindowSizeAcrossAllFilesOverAllSides has terminated at minimum window size: $minimumWindowSizeAcrossAllFilesOverAllSides; results are:\n${pprintCustomised(stateAfterTryingCandidate)}"
            )
          else
            logger.debug(
              s"Search for matches whose size is less than the sure-fire match window size of: $minimumSureFireWindowSizeAcrossAllFilesOverAllSides has terminated at minimum window size: $minimumWindowSizeAcrossAllFilesOverAllSides; results are:\n${pprintCustomised(stateAfterTryingCandidate)}"
            )
          end if
          stateAfterTryingCandidate
        end if
      end withAllSmallFryMatches

      // `MatchCalculationState` allows pairwise matches to subsume all-sides
      // matches on two sides: this condenses the state by having those
      // all-sides matches eat into their subsuming pairwise matches, replacing
      // them with smaller leftover sections.
      def withPairwiseMatchesEatenInto: MatchesAndTheirSections =
        val allSidesMatches = sectionsAndTheirMatches.values.collect {
          case allSides: Match.AllSides[Section[Element]] => allSides
        }.toSet

        def pairwiseMatchesSubsumingOnBothSides(
            allSides: Match.AllSides[Section[Element]]
        ): Set[PairwiseMatch] =
          val subsumingOnBase =
            subsumingPairwiseMatches(sectionsAndTheirMatches)(
              base,
              baseSectionsByPath
            )(
              allSides.baseElement
            )
          val subsumingOnLeft =
            subsumingPairwiseMatches(sectionsAndTheirMatches)(
              left,
              leftSectionsByPath
            )(
              allSides.leftElement
            )
          val subsumingOnRight =
            subsumingPairwiseMatches(sectionsAndTheirMatches)(
              right,
              rightSectionsByPath
            )(
              allSides.rightElement
            )

          (subsumingOnBase intersect subsumingOnLeft) union (subsumingOnBase intersect subsumingOnRight) union (subsumingOnLeft intersect subsumingOnRight)
        end pairwiseMatchesSubsumingOnBothSides

        val pairwiseMatchesToBeEaten
            : MultiDict[PairwiseMatch, Match.AllSides[Section[Element]]] =
          MultiDict.from(
            allSidesMatches.flatMap(allSides =>
              pairwiseMatchesSubsumingOnBothSides(allSides).map(_ -> allSides)
            )
          )

        val withoutThosePairwiseMatches: MatchesAndTheirSections =
          withoutTheseMatches(pairwiseMatchesToBeEaten.keySet)

        val pairwiseMatchesFromLeftovers =
          pairwiseMatchesToBeEaten.keySet.flatMap[PairwiseMatch] {
            pairwiseMatch =>
              val bites = pairwiseMatchesToBeEaten.get(pairwiseMatch)

              val leftovers: Seq[PairwiseMatch] = pairwiseMatch match
                case Match.BaseAndLeft(baseSection, leftSection) =>
                  (eatIntoSection(base, bites.map(_.baseElement))(
                    baseSection
                  ) zip eatIntoSection(left, bites.map(_.leftElement))(
                    leftSection
                  ))
                    .map(Match.BaseAndLeft.apply)

                case Match.BaseAndRight(baseSection, rightSection) =>
                  (eatIntoSection(base, bites.map(_.baseElement))(
                    baseSection
                  ) zip eatIntoSection(right, bites.map(_.rightElement))(
                    rightSection
                  )).map(Match.BaseAndRight.apply)

                case Match.LeftAndRight(leftSection, rightSection) =>
                  (eatIntoSection(left, bites.map(_.leftElement))(
                    leftSection
                  ) zip eatIntoSection(right, bites.map(_.rightElement))(
                    rightSection
                  )).map(Match.LeftAndRight.apply)

              logger.debug(
                s"Eating into pairwise match:\n${pprintCustomised(pairwiseMatch)} on behalf of all-sides matches:\n${pprintCustomised(bites)}, resulting in matches:\n${pprintCustomised(leftovers)}."
              )

              leftovers
          }

        withoutThosePairwiseMatches
          .withMatches(
            pairwiseMatchesFromLeftovers
              .asInstanceOf[collection.Set[Match[Section[Element]]]]
          )
          .matchesAndTheirSections
      end withPairwiseMatchesEatenInto

      // Eating into pairwise matches can create smaller pairwise matches that
      // are partially subsumed by other larger pairwise matches. Prefer keeping
      // the larger matches and remove the subsumed ones.
      def cleanedUp: MatchesAndTheirSections =
        val subsumedBaseSections = baseSectionsByPath.values
          .flatMap(_.iterator)
          .filter(subsumesBaseSection)
        val subsumedLeftSections = leftSectionsByPath.values
          .flatMap(_.iterator)
          .filter(subsumesLeftSection)
        val subsumedRightSections = rightSectionsByPath.values
          .flatMap(_.iterator)
          .filter(subsumesRightSection)

        val matchesToRemove =
          (subsumedBaseSections ++ subsumedLeftSections ++ subsumedRightSections)
            .flatMap(sectionsAndTheirMatches.get)
            .toSet

        if matchesToRemove.nonEmpty then
          logger.debug(
            s"Removing matches that have subsumed sections:\n${pprintCustomised(matchesToRemove)} as part of cleanup."
          )
        end if

        this.withoutTheseMatches(matchesToRemove)
      end cleanedUp

      private def withoutTheseMatches(
          matches: Iterable[Match[Section[Element]]]
      ): MatchesAndTheirSections =
        matches.foldLeft(this) {
          case (
                matches,
                allSides @ Match.AllSides(
                  baseSection,
                  leftSection,
                  rightSection
                )
              ) =>
            val basePath  = base.pathFor(baseSection)
            val leftPath  = left.pathFor(leftSection)
            val rightPath = right.pathFor(rightSection)
            matches
              .focus(_.baseSectionsByPath)
              .modify(_.updatedWith(basePath) { case Some(sectionsSeen) =>
                Some(sectionsSeen - baseSection)
              })
              .focus(_.leftSectionsByPath)
              .modify(_.updatedWith(leftPath) { case Some(sectionsSeen) =>
                Some(sectionsSeen - leftSection)
              })
              .focus(_.rightSectionsByPath)
              .modify(_.updatedWith(rightPath) { case Some(sectionsSeen) =>
                Some(sectionsSeen - rightSection)
              })
              .focus(_.sectionsAndTheirMatches)
              .modify(
                _.remove(baseSection, allSides)
                  .remove(leftSection, allSides)
                  .remove(rightSection, allSides)
              )

          case (
                matches,
                baseAndLeft @ Match.BaseAndLeft(baseSection, leftSection)
              ) =>
            val basePath = base.pathFor(baseSection)
            val leftPath = left.pathFor(leftSection)
            matches
              .focus(_.baseSectionsByPath)
              .modify(_.updatedWith(basePath) { case Some(sectionsSeen) =>
                Some(sectionsSeen - baseSection)
              })
              .focus(_.leftSectionsByPath)
              .modify(_.updatedWith(leftPath) { case Some(sectionsSeen) =>
                Some(sectionsSeen - leftSection)
              })
              .focus(_.sectionsAndTheirMatches)
              .modify(
                _.remove(baseSection, baseAndLeft)
                  .remove(leftSection, baseAndLeft)
              )

          case (
                matches,
                baseAndRight @ Match.BaseAndRight(baseSection, rightSection)
              ) =>
            val basePath  = base.pathFor(baseSection)
            val rightPath = right.pathFor(rightSection)
            matches
              .focus(_.baseSectionsByPath)
              .modify(_.updatedWith(basePath) { case Some(sectionsSeen) =>
                Some(sectionsSeen - baseSection)
              })
              .focus(_.rightSectionsByPath)
              .modify(_.updatedWith(rightPath) { case Some(sectionsSeen) =>
                Some(sectionsSeen - rightSection)
              })
              .focus(_.sectionsAndTheirMatches)
              .modify(
                _.remove(baseSection, baseAndRight)
                  .remove(rightSection, baseAndRight)
              )

          case (
                matches,
                leftAndRight @ Match.LeftAndRight(leftSection, rightSection)
              ) =>
            val leftPath  = left.pathFor(leftSection)
            val rightPath = right.pathFor(rightSection)
            matches
              .focus(_.leftSectionsByPath)
              .modify(_.updatedWith(leftPath) { case Some(sectionsSeen) =>
                Some(sectionsSeen - leftSection)
              })
              .focus(_.rightSectionsByPath)
              .modify(_.updatedWith(rightPath) { case Some(sectionsSeen) =>
                Some(sectionsSeen - rightSection)
              })
              .focus(_.sectionsAndTheirMatches)
              .modify(
                _.remove(leftSection, leftAndRight)
                  .remove(rightSection, leftAndRight)
              )
        }
      end withoutTheseMatches

      private def withMatches(
          matches: collection.Set[Match[Section[Element]]]
      ): MatchingResult =
        val (
          updatedMatchesAndTheirSections,
          matchesWithoutRedundantPairwiseMatches
        ) =
          matches
            .foldLeft(this)(_.withMatch(_))
            .withoutRedundantPairwiseMatchesIn(matches)

        MatchingResult(
          matchesAndTheirSections = updatedMatchesAndTheirSections,
          numberOfMatchesForTheGivenWindowSize =
            matchesWithoutRedundantPairwiseMatches.size,
          estimatedWindowSizeForOptimalMatch = updatedMatchesAndTheirSections
            .estimateOptimalMatchSize(matchesWithoutRedundantPairwiseMatches)
        )
      end withMatches

      // Cleans up the state when a putative all-sides match that would have
      // been ambiguous on one side with another all-sides match is partially
      // suppressed by a larger pairwise match. This situation results in a
      // pairwise match that shares its sections on both sides with the other
      // all-sides match; remove any such redundant pairwise matches.
      private def withoutRedundantPairwiseMatchesIn(
          matches: collection.Set[Match[Section[Element]]]
      ): (MatchesAndTheirSections, collection.Set[Match[Section[Element]]]) =
        val (redundantMatches, usefulMatches) =
          val isAnAllSidesMatch: Match[Section[Element]] => Boolean = {
            case _: Match.AllSides[Section[Element]] => true
            case _                                   => false
          }

          matches.partition {
            case Match.BaseAndLeft(baseSection, leftSection) =>
              sectionsAndTheirMatches
                .get(baseSection)
                .intersect(sectionsAndTheirMatches.get(leftSection))
                .exists(isAnAllSidesMatch)
            case Match.BaseAndRight(baseSection, rightSection) =>
              sectionsAndTheirMatches
                .get(baseSection)
                .intersect(sectionsAndTheirMatches.get(rightSection))
                .exists(isAnAllSidesMatch)
            case Match.LeftAndRight(leftSection, rightSection) =>
              sectionsAndTheirMatches
                .get(leftSection)
                .intersect(sectionsAndTheirMatches.get(rightSection))
                .exists(isAnAllSidesMatch)
            case _: Match.AllSides[Section[Element]] => false
          }
        end val

        if redundantMatches.nonEmpty then
          logger.debug(
            s"Removing redundant pairwise matches:\n${pprintCustomised(redundantMatches)} as their sections also belong to all-sides matches."
          )
        end if

        withoutTheseMatches(redundantMatches) -> usefulMatches
      end withoutRedundantPairwiseMatchesIn

      // When the window size used to calculate matches is lower than the
      // optimal match size, overlapping matches will be made that cover the
      // elements of the optimal math. Estimate the size of the optimal match by
      // coalescing the overlaps.
      private def estimateOptimalMatchSize(
          matches: collection.Set[Match[Section[Element]]]
      ): Option[Int] =
        // Deconstruct a throwaway instance of `MatchesAndTheirSections` made
        // from just `matches` as a quick-and-dirty way of organising the
        // matches' sections.
        val MatchesAndTheirSections(
          baseSectionsByPath,
          leftSectionsByPath,
          rightSectionsByPath,
          _
        ) = matches.foldLeft(empty)(_.withMatch(_))

        val sectionsSeenOnAllPathsAcrossAllSides =
          baseSectionsByPath.values ++ leftSectionsByPath.values ++ rightSectionsByPath.values

        sectionsSeenOnAllPathsAcrossAllSides
          .flatMap(maximumSizeOfCoalescedSections)
          .maxOption
      end estimateOptimalMatchSize

      private def withMatch(
          aMatch: Match[Section[Element]]
      ): MatchesAndTheirSections =
        aMatch match
          case Match.AllSides(baseSection, leftSection, rightSection) =>
            copy(
              baseSectionsByPath = withBaseSection(baseSection),
              leftSectionsByPath = withLeftSection(leftSection),
              rightSectionsByPath = withRightSection(rightSection),
              sectionsAndTheirMatches =
                sectionsAndTheirMatches + (baseSection -> aMatch) + (leftSection -> aMatch) + (rightSection -> aMatch)
            )
          case Match.BaseAndLeft(baseSection, leftSection) =>
            copy(
              baseSectionsByPath = withBaseSection(baseSection),
              leftSectionsByPath = withLeftSection(leftSection),
              sectionsAndTheirMatches =
                sectionsAndTheirMatches + (baseSection -> aMatch) + (leftSection -> aMatch)
            )
          case Match.BaseAndRight(baseSection, rightSection) =>
            copy(
              baseSectionsByPath = withBaseSection(baseSection),
              rightSectionsByPath = withRightSection(rightSection),
              sectionsAndTheirMatches =
                sectionsAndTheirMatches + (baseSection -> aMatch) + (rightSection -> aMatch)
            )
          case Match.LeftAndRight(leftSection, rightSection) =>
            copy(
              leftSectionsByPath = withLeftSection(leftSection),
              rightSectionsByPath = withRightSection(rightSection),
              sectionsAndTheirMatches =
                sectionsAndTheirMatches + (leftSection -> aMatch) + (rightSection -> aMatch)
            )
        end match
      end withMatch

      private def pairwiseMatchSubsumesJustOneSideOnly(
          allSides: Match.AllSides[Section[Element]]
      ): Boolean =
        val subsumingOnBase =
          subsumingPairwiseMatches(sectionsAndTheirMatches)(
            base,
            baseSectionsByPath
          )(
            allSides.baseElement
          )
        val subsumingOnLeft =
          subsumingPairwiseMatches(sectionsAndTheirMatches)(
            left,
            leftSectionsByPath
          )(
            allSides.leftElement
          )
        val subsumingOnRight =
          subsumingPairwiseMatches(sectionsAndTheirMatches)(
            right,
            rightSectionsByPath
          )(
            allSides.rightElement
          )

        val subsumedOnJustTheBase =
          (subsumingOnBase diff (subsumingOnLeft union subsumingOnRight)).nonEmpty
        val subsumedOnJustTheLeft =
          (subsumingOnLeft diff (subsumingOnBase union subsumingOnRight)).nonEmpty
        val subsumedOnJustTheRight =
          (subsumingOnRight diff (subsumingOnBase union subsumingOnLeft)).nonEmpty

        subsumedOnJustTheBase || subsumedOnJustTheLeft || subsumedOnJustTheRight
      end pairwiseMatchSubsumesJustOneSideOnly

      private def matchFrom(
          baseSection: Section[Element],
          leftSection: Section[Element],
          rightSection: Section[Element]
      ): Option[Match[Section[Element]]] =
        // Rules of the game:
        // 1. No overlaps on *any* side are permitted.
        // 2. An all-sides match may not be subsumed on *all three sides* by a
        // larger all-sides match.
        // 3. An all-sides match may not be subsumed on *two* of its sides by a
        // larger all-sides match.
        // 4. An all-sides match may be subsumed on *two* of its sides by a
        // larger pairwise match - this facilitates eating into the larger
        // pairwise section elsewhere.
        // 5. A putative all-sides match subsumed on *one* side by a larger
        // match (all-sides or pairwise) is partially suppressed, yielding a
        // pairwise match across the other two sides, provided that the pairwise
        // match is permitted by its own rules. If not, the all-sides match is
        // completely suppressed.

        val overlapped =
          overlapsBaseSection(baseSection) ||
            overlapsLeftSection(leftSection) ||
            overlapsRightSection(rightSection)

        Option.unless(overlapped)(()).flatMap { _ =>
          val baseIsSubsumedByAnAllSidesMatch =
            subsumesBaseSectionViaAtLeastOneAllSidesMatch(baseSection)
          val leftIsSubsumedByAnAllSidesMatch =
            subsumesLeftSectionViaAtLeastOneAllSidesMatch(leftSection)
          val rightIsSubsumedByAnAllSidesMatch =
            subsumesRightSectionViaAtLeastOneAllSidesMatch(rightSection)

          (
            baseIsSubsumedByAnAllSidesMatch,
            leftIsSubsumedByAnAllSidesMatch,
            rightIsSubsumedByAnAllSidesMatch
          ) match
            case (false, false, false) =>
              val tediousTypecheckingWorkaround
                  : Option[Match.AllSides[Section[Element]]] =
                Some(Match.AllSides(baseSection, leftSection, rightSection))

              tediousTypecheckingWorkaround
                .filterNot(pairwiseMatchSubsumesJustOneSideOnly)
            case (false, false, true) =>
              Option.unless(
                subsumesBaseSection(baseSection) || subsumesLeftSection(
                  leftSection
                )
              )(Match.BaseAndLeft(baseSection, leftSection))
            case (false, true, false) =>
              Option.unless(
                subsumesBaseSection(baseSection) || subsumesRightSection(
                  rightSection
                )
              )(Match.BaseAndRight(baseSection, rightSection))
            case (true, false, false) =>
              Option.unless(
                subsumesLeftSection(leftSection) || subsumesRightSection(
                  rightSection
                )
              )(Match.LeftAndRight(leftSection, rightSection))
            case _ => None
          end match
        }
      end matchFrom

      private def baseAndLeftMatchOf(
          baseSection: Section[Element],
          leftSection: Section[Element]
      ): Option[Match.BaseAndLeft[Section[Element]]] =
        // If anything overlaps on either side or fully or partially subsumes
        // either section in a putative pairwise match, then the match is
        // suppressed.
        val suppressed =
          overlapsBaseSection(baseSection) || overlapsLeftSection(
            leftSection
          ) || subsumesBaseSection(baseSection) || subsumesLeftSection(
            leftSection
          )

        Option.unless(suppressed)(
          Match.BaseAndLeft(
            baseSection,
            leftSection
          )
        )
      end baseAndLeftMatchOf

      private def baseAndRightMatchOf(
          baseSection: Section[Element],
          rightSection: Section[Element]
      ): Option[Match.BaseAndRight[Section[Element]]] =
        // If anything overlaps on either side or fully or partially subsumes
        // either section in a putative pairwise match, then the match is
        // suppressed.
        val suppressed =
          overlapsBaseSection(baseSection) || overlapsRightSection(
            rightSection
          ) || subsumesBaseSection(baseSection) || subsumesRightSection(
            rightSection
          )

        Option.unless(suppressed)(
          Match.BaseAndRight(
            baseSection,
            rightSection
          )
        )
      end baseAndRightMatchOf

      private def leftAndRightMatchOf(
          leftSection: Section[Element],
          rightSection: Section[Element]
      ): Option[Match.LeftAndRight[Section[Element]]] =
        // If anything overlaps on either side or fully or partially subsumes
        // either section in a putative pairwise match, then the match is
        // suppressed.
        val suppressed =
          overlapsLeftSection(leftSection) || overlapsRightSection(
            rightSection
          ) || subsumesLeftSection(leftSection) || subsumesRightSection(
            rightSection
          )
        Option.unless(suppressed)(
          Match.LeftAndRight(
            leftSection,
            rightSection
          )
        )
      end leftAndRightMatchOf

      private def matchesForWindowSize(
          windowSize: Int
      ): MatchingResult =
        require(0 < windowSize)

        def fingerprintStartIndices(
            elements: IndexedSeq[Element]
        ): SortedMultiDict[BigInt, Int] =
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

          val accumulatingResults = mutable.SortedMultiDict.empty[BigInt, Int]

          def updateFingerprint(elementIndex: Int): Unit =
            val elementBytes =
              hashFunction
                .newHasher()
                .putObject(elements(elementIndex), elementFunnel)
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

        def fingerprintSections(
            sources: Sources[Path, Element]
        ): SortedMultiDict[PotentialMatchKey, Section[Element]] =
          sources.filesByPath
            .filter { case (_, file) =>
              val fileSize = file.size
              val minimumWindowSize =
                (thresholdSizeFractionForMatching * fileSize).floor.toInt

              minimumWindowSize to fileSize contains windowSize
            }
            .map { case (path, file) =>
              fingerprintStartIndices(file.content).map(
                (fingerprint, fingerprintStartIndex) =>
                  val section = sources
                    .section(path)(fingerprintStartIndex, windowSize)
                  PotentialMatchKey(
                    fingerprint,
                    impliedContent = section
                  ) -> section
              )
            }
            // This isn't quite the same as flat-mapping / flattening, because
            // we want the type of the result to be a `SortedMultiDict`...
            .reduceOption(_ concat _)
            .getOrElse(SortedMultiDict.empty)
        end fingerprintSections

        val FingerprintSectionsAcrossSides(
          baseSectionsByFingerprint,
          leftSectionsByFingerprint,
          rightSectionsByFingerprint
        ) = FingerprintSectionsAcrossSides(
          baseSectionsByFingerprint = fingerprintSections(base),
          leftSectionsByFingerprint = fingerprintSections(left),
          rightSectionsByFingerprint = fingerprintSections(right)
        )

        @tailrec
        def matchingFingerprintsAcrossSides(
            baseFingerprints: Iterable[PotentialMatchKey],
            leftFingerprints: Iterable[PotentialMatchKey],
            rightFingerprints: Iterable[PotentialMatchKey],
            matches: Set[Match[Section[Element]]]
        ): MatchingResult =
          (
            baseFingerprints.headOption,
            leftFingerprints.headOption,
            rightFingerprints.headOption
          ) match
            case (Some(baseHead), Some(leftHead), Some(rightHead))
                if potentialMatchKeyOrder.eqv(
                  baseHead,
                  leftHead
                ) && potentialMatchKeyOrder.eqv(baseHead, rightHead) =>
              // Synchronised the fingerprints across all three sides...
              val matchesForSynchronisedFingerprint
                  : Set[Match[Section[Element]]] =
                val baseSections =
                  baseSectionsByFingerprint.get(baseHead)
                val leftSections =
                  leftSectionsByFingerprint.get(leftHead)
                val rightSections =
                  rightSectionsByFingerprint.get(rightHead)

                (for
                  baseSection  <- LazyList.from(baseSections)
                  leftSection  <- LazyList.from(leftSections)
                  rightSection <- LazyList.from(rightSections)

                  aMatch <- matchFrom(
                    baseSection,
                    leftSection,
                    rightSection
                  )
                yield aMatch).toSet
              end matchesForSynchronisedFingerprint

              matchingFingerprintsAcrossSides(
                baseFingerprints.tail,
                leftFingerprints.tail,
                rightFingerprints.tail,
                matches ++ matchesForSynchronisedFingerprint
              )

            case (Some(baseHead), Some(leftHead), Some(rightHead))
                if potentialMatchKeyOrder.eqv(
                  baseHead,
                  leftHead
                ) && potentialMatchKeyOrder.gt(
                  baseHead,
                  rightHead
                ) =>
              // Tentatively synchronised the fingerprints between the base and
              // left, need to advance the right to resolve ...
              matchingFingerprintsAcrossSides(
                baseFingerprints,
                leftFingerprints,
                rightFingerprints.tail,
                matches
              )

            case (Some(baseHead), Some(leftHead), _)
                if potentialMatchKeyOrder.eqv(baseHead, leftHead) =>
              // Synchronised the fingerprints between the base and left...
              val matchesForSynchronisedFingerprint
                  : Set[Match[Section[Element]]] =
                val baseSections =
                  baseSectionsByFingerprint.get(baseHead)
                val leftSections =
                  leftSectionsByFingerprint.get(leftHead)

                (for
                  baseSection <- LazyList.from(baseSections)
                  leftSection <- LazyList.from(leftSections)

                  baseAndLeftMatch <- baseAndLeftMatchOf(
                    baseSection,
                    leftSection
                  )
                yield baseAndLeftMatch).toSet
              end matchesForSynchronisedFingerprint

              matchingFingerprintsAcrossSides(
                baseFingerprints.tail,
                leftFingerprints.tail,
                rightFingerprints,
                matches ++ matchesForSynchronisedFingerprint
              )

            case (Some(baseHead), Some(leftHead), Some(rightHead))
                if potentialMatchKeyOrder.eqv(
                  baseHead,
                  rightHead
                ) && potentialMatchKeyOrder.gt(
                  baseHead,
                  leftHead
                ) =>
              // Tentatively synchronised the fingerprints between the base and
              // right, need to advance the left to resolve ...
              matchingFingerprintsAcrossSides(
                baseFingerprints,
                leftFingerprints.tail,
                rightFingerprints,
                matches
              )

            case (Some(baseHead), _, Some(rightHead))
                if potentialMatchKeyOrder.eqv(baseHead, rightHead) =>
              // Synchronised the fingerprints between the base and right...
              val matchesForSynchronisedFingerprint
                  : Set[Match[Section[Element]]] =
                val baseSections =
                  baseSectionsByFingerprint.get(baseHead)
                val rightSections =
                  rightSectionsByFingerprint.get(rightHead)

                (for
                  baseSection  <- LazyList.from(baseSections)
                  rightSection <- LazyList.from(rightSections)

                  baseAndRightMatch <- baseAndRightMatchOf(
                    baseSection,
                    rightSection
                  )
                yield baseAndRightMatch).toSet
              end matchesForSynchronisedFingerprint

              matchingFingerprintsAcrossSides(
                baseFingerprints.tail,
                leftFingerprints,
                rightFingerprints.tail,
                matches ++ matchesForSynchronisedFingerprint
              )

            case (Some(baseHead), Some(leftHead), Some(rightHead))
                if potentialMatchKeyOrder.eqv(
                  leftHead,
                  rightHead
                ) && potentialMatchKeyOrder.gt(
                  leftHead,
                  baseHead
                ) =>
              // Tentatively synchronised the fingerprints between the left and
              // right, need to advance the base to resolve ...
              matchingFingerprintsAcrossSides(
                baseFingerprints.tail,
                leftFingerprints,
                rightFingerprints,
                matches
              )

            case (_, Some(leftHead), Some(rightHead))
                if potentialMatchKeyOrder.eqv(leftHead, rightHead) =>
              // Synchronised the fingerprints between the left and right...
              val matchesForSynchronisedFingerprint
                  : Set[Match[Section[Element]]] =
                val leftSections =
                  leftSectionsByFingerprint.get(leftHead)
                val rightSections =
                  rightSectionsByFingerprint.get(rightHead)

                (for
                  leftSection  <- LazyList.from(leftSections)
                  rightSection <- LazyList.from(rightSections)

                  leftAndRightMatch <- leftAndRightMatchOf(
                    leftSection,
                    rightSection
                  )
                yield leftAndRightMatch).toSet
              end matchesForSynchronisedFingerprint

              matchingFingerprintsAcrossSides(
                baseFingerprints,
                leftFingerprints.tail,
                rightFingerprints.tail,
                matches ++ matchesForSynchronisedFingerprint
              )

            case (Some(baseHead), Some(leftHead), Some(rightHead)) =>
              // All the fingerprints disagree, so advance the side with the
              // minimum fingerprint to see if it can catch up and
              // synchronise...

              val minimumFingerprint =
                potentialMatchKeyOrder.min(
                  baseHead,
                  potentialMatchKeyOrder
                    .min(leftHead, rightHead)
                )

              // NOTE: just use `==` as we have already looked inside the
              // `PotentialMatchKey` instances - we just want to know which one
              // was the minimum.
              if leftHead == minimumFingerprint
              then
                matchingFingerprintsAcrossSides(
                  baseFingerprints,
                  leftFingerprints.tail,
                  rightFingerprints,
                  matches
                )
              // NOTE: just use `==` as we have already looked inside the
              // `PotentialMatchKey` instances - we just want to know which one
              // was the minimum.
              else if rightHead == minimumFingerprint
              then
                matchingFingerprintsAcrossSides(
                  baseFingerprints,
                  leftFingerprints,
                  rightFingerprints.tail,
                  matches
                )
              else
                matchingFingerprintsAcrossSides(
                  baseFingerprints.tail,
                  leftFingerprints,
                  rightFingerprints,
                  matches
                )
              end if

            case (Some(baseHead), Some(leftHead), None) =>
              // The base and left fingerprints disagree, so advance the side
              // with the minimum fingerprint to see if it can catch up and
              // synchronise...
              if potentialMatchKeyOrder.lt(baseHead, leftHead)
              then
                matchingFingerprintsAcrossSides(
                  baseFingerprints.tail,
                  leftFingerprints,
                  rightFingerprints,
                  matches
                )
              else
                matchingFingerprintsAcrossSides(
                  baseFingerprints,
                  leftFingerprints.tail,
                  rightFingerprints,
                  matches
                )

            case (Some(baseHead), None, Some(rightHead)) =>
              // The base and right fingerprints disagree, so advance the side
              // with the minimum fingerprint to see if it can catch up and
              // synchronise...
              if potentialMatchKeyOrder.lt(baseHead, rightHead)
              then
                matchingFingerprintsAcrossSides(
                  baseFingerprints.tail,
                  leftFingerprints,
                  rightFingerprints,
                  matches
                )
              else
                matchingFingerprintsAcrossSides(
                  baseFingerprints,
                  leftFingerprints,
                  rightFingerprints.tail,
                  matches
                )

            case (None, Some(leftHead), Some(rightHead)) =>
              // The left and right fingerprints disagree, so advance the side
              // with the minimum fingerprint to see if it can catch up and
              // synchronise...
              if potentialMatchKeyOrder.lt(leftHead, rightHead)
              then
                matchingFingerprintsAcrossSides(
                  baseFingerprints,
                  leftFingerprints.tail,
                  rightFingerprints,
                  matches
                )
              else
                matchingFingerprintsAcrossSides(
                  baseFingerprints,
                  leftFingerprints,
                  rightFingerprints.tail,
                  matches
                )

            case _ =>
              // There are no more opportunities to match a full triple or
              // just a pair, so this terminates the recursion.
              withMatches(matches)
          end match
        end matchingFingerprintsAcrossSides

        matchingFingerprintsAcrossSides(
          baseSectionsByFingerprint.keySet,
          leftSectionsByFingerprint.keySet,
          rightSectionsByFingerprint.keySet,
          matches = Set.empty
        )
      end matchesForWindowSize
    end MatchesAndTheirSections

    given potentialMatchKeyOrder: Order[PotentialMatchKey] =
      given Order[Element] = elementOrder

      Order.whenEqual(
        Order.by(_.fingerprint),
        // NOTE: need the pesky type ascription because `Order` is invariant on
        // its type parameter.
        Order.by(_.impliedContent.content: Seq[Element])
      )
    end potentialMatchKeyOrder

    // NOTE: this is subtle - this type is used as a ordered key to find matches
    // across sides; fingerprints can and do collide, so we need the content as
    // a tiebreaker. However, we don't want to have to freight the content
    // around for keys that will never match across sides - there are a lot of
    // keys involved in finding matches at low window sizes, and large window
    // sizes imply large content sizes.
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
    )

    case class FingerprintSectionsAcrossSides(
        baseSectionsByFingerprint: SortedMultiDict[PotentialMatchKey, Section[
          Element
        ]],
        leftSectionsByFingerprint: SortedMultiDict[PotentialMatchKey, Section[
          Element
        ]],
        rightSectionsByFingerprint: SortedMultiDict[PotentialMatchKey, Section[
          Element
        ]]
    )

    val matchesAndTheirSections =
      val withAllMatchesOfAtLeastTheSureFireWindowSize =
        MatchesAndTheirSections.empty
          .withAllMatchesOfAtLeastTheSureFireWindowSize()

      (if minimumSureFireWindowSizeAcrossAllFilesOverAllSides > minimumWindowSizeAcrossAllFilesOverAllSides
       then
         withAllMatchesOfAtLeastTheSureFireWindowSize
           .withAllSmallFryMatches(
             minimumSureFireWindowSizeAcrossAllFilesOverAllSides - 1
           )
       else
         withAllMatchesOfAtLeastTheSureFireWindowSize
      ).withPairwiseMatchesEatenInto.cleanedUp
    end matchesAndTheirSections

    val attempt = Try {
      val baseFilesByPath =
        base.filesByPathUtilising(
          matchesAndTheirSections.baseSections
        )
      val leftFilesByPath =
        left.filesByPathUtilising(
          matchesAndTheirSections.leftSections
        )
      val rightFilesByPath =
        right.filesByPathUtilising(
          matchesAndTheirSections.rightSections
        )

      val sectionsAndTheirMatches =
        matchesAndTheirSections.sectionsAndTheirMatches

      // Check the invariant that all matches that involve the same section must
      // be of the same kind...
      sectionsAndTheirMatches.sets.foreach { (section, matches) =>
        matches.head match
          case _: Match.AllSides[Section[Element]] =>
            matches.tail.foreach { anotherMatch =>
              require(anotherMatch.isInstanceOf[Match.AllSides[Element]])
            }
          case _: Match.BaseAndLeft[Section[Element]] =>
            matches.tail.foreach { anotherMatch =>
              require(anotherMatch.isInstanceOf[Match.BaseAndLeft[Element]])
            }
          case _: Match.BaseAndRight[Section[Element]] =>
            matches.tail.foreach { anotherMatch =>
              require(anotherMatch.isInstanceOf[Match.BaseAndRight[Element]])
            }
          case _: Match.LeftAndRight[Section[Element]] =>
            matches.tail.foreach { anotherMatch =>
              require(anotherMatch.isInstanceOf[Match.LeftAndRight[Element]])
            }
      }

      new CodeMotionAnalysis[Path, Element]:
        override def matchesFor(
            section: Section[Element]
        ): collection.Set[Match[Section[Element]]] =
          sectionsAndTheirMatches.get(section)

        override def base: Map[Path, File[Element]] = baseFilesByPath

        override def left: Map[Path, File[Element]] = leftFilesByPath

        override def right: Map[Path, File[Element]] = rightFilesByPath
      end new

    }

    if propagateExceptions then Right(attempt.get) else attempt.toEither
    end if
  end of

  // TODO - what happened?
  case object AmbiguousMatch extends RuntimeException
end CodeMotionAnalysis
