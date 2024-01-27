package com.sageserpent.kineticmerge.core

import cats.implicits.catsKernelOrderingForOrder
import cats.instances.seq.*
import cats.{Eq, Order}
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import com.google.common.hash.{Funnel, HashFunction}
import com.sageserpent.kineticmerge.core
import com.sageserpent.kineticmerge.core.Match.BaseAndLeft
import de.sciss.fingertree.RangedSeq
import monocle.syntax.all.*

import java.lang.Byte as JavaByte
import scala.annotation.tailrec
import scala.collection.decorators.mapDecorator
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

object CodeMotionAnalysis:
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
      1 max (minimumFileSizeAcrossAllFilesOverAllSides * thresholdSizeFractionForMatching).floor.toInt

    val maximumFileSizeAcrossAllFilesOverAllSides = fileSizes.max

    // This is the minimum window size that would be allowed in *all* files
    // across the sources.
    val minimumSureFireWindowSizeAcrossAllFilesOverAllSides =
      1 max (maximumFileSizeAcrossAllFilesOverAllSides * thresholdSizeFractionForMatching).floor.toInt

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
          .fold(ifEmpty = false)(_.includes(section.closedOpenInterval))

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
              .flatMap(sectionsAndTheirMatches.get)
              .exists {
                case _: Match.AllSides[Section[Element]] => true
                case _                                   => false
              }
          )

      private def subsumingPairwiseSections(
          sectionsAndTheirMatches: MatchedSections
      )(
          side: Sources[Path, Element],
          sectionsByPath: Map[Path, SectionsSeen]
      )(section: Section[Element]): Set[PairwiseMatch] =
        sectionsByPath
          .get(side.pathFor(section))
          .fold(ifEmpty = Set.empty)(
            _.filterIncludes(section.closedOpenInterval)
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

      private def maximumSizeOfCoalescedSections(
          firstSectionsByPath: Map[Path, SectionsSeen],
          secondSectionsByPath: Map[Path, SectionsSeen]
      ): Option[Int] =
        val maxima = firstSectionsByPath
          .rightOuterJoin(secondSectionsByPath)
          .flatMap { case (_, (possibleFirst, second)) =>
            val differences = possibleFirst
              .fold(ifEmpty = second.iterator.toSeq)(
                second.iterator.toSeq diff _.iterator.toSeq
              )
              .sortBy(
                _.startOffset
              )

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
              differences,
              putativeCoalescence = None,
              partialResult = None
            )
          }

        maxima.reduceOption(_ max _)
      end maximumSizeOfCoalescedSections

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
                stateAfterTryingCandidate
                  .withAllMatchesOfAtLeastTheSureFireWindowSize(
                    looseExclusiveUpperBoundOnMaximumMatchSize =
                      candidateWindowSize
                  )
              case Some(estimate) =>
                // We have an improvement, move the lower bound up and note the
                // improved state.
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
            fallbackImprovedState
          else
            // The optimal matches are in the fallback improved state; try
            // searching for the next lowest optimal size. This is necessary as
            // we may have *multiple* distinct optimal matches at a given window
            // size.
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
        val MatchingResult(stateAfterTryingCandidate, _, _) =
          this.matchesForWindowSize(candidateWindowSize)

        if candidateWindowSize > minimumWindowSizeAcrossAllFilesOverAllSides
        then
          stateAfterTryingCandidate.withAllSmallFryMatches(
            candidateWindowSize - 1
          )
        else stateAfterTryingCandidate
        end if
      end withAllSmallFryMatches

      def withPairwiseMatchesEatenInto: MatchesAndTheirSections =
        val allSidesMatches = sectionsAndTheirMatches.values.collect {
          case allSides: Match.AllSides[Section[Element]] => allSides
        }.toSet

        def pairwiseMatchesSubsumingOnBothSides(
            allSides: Match.AllSides[Section[Element]]
        ): Set[PairwiseMatch] =
          val subsumingOnBase =
            subsumingPairwiseSections(sectionsAndTheirMatches)(
              base,
              baseSectionsByPath
            )(
              allSides.baseElement
            )
          val subsumingOnLeft =
            subsumingPairwiseSections(sectionsAndTheirMatches)(
              left,
              leftSectionsByPath
            )(
              allSides.leftElement
            )
          val subsumingOnRight =
            subsumingPairwiseSections(sectionsAndTheirMatches)(
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

              pairwiseMatch match
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
              end match
          }

        withoutThosePairwiseMatches
          .withMatches(
            pairwiseMatchesFromLeftovers
              .asInstanceOf[collection.Set[Match[Section[Element]]]]
          )
          .matchesAndTheirSections
      end withPairwiseMatchesEatenInto

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
            .estimateOptimalMatchSizeInComparisonTo(this)
        )
      end withMatches

      private def withoutRedundantPairwiseMatchesIn(
          matches: collection.Set[Match[Section[Element]]]
      ): (MatchesAndTheirSections, collection.Set[Match[Section[Element]]]) =
        val (updatedSectionsAndTheirMatches, updatedMatches) =
          val isAnAllSidesMatch: Match[Section[Element]] => Boolean = {
            case _: Match.AllSides[Section[Element]] => true
            case _                                   => false
          }

          matches.foldLeft(sectionsAndTheirMatches -> matches) {
            case (
                  (sectionsAndTheirMatches, matches),
                  pairwiseMatch @ Match.BaseAndLeft(baseSection, leftSection)
                )
                if sectionsAndTheirMatches
                  .get(baseSection)
                  .intersect(sectionsAndTheirMatches.get(leftSection))
                  .exists(isAnAllSidesMatch) =>
              (
                sectionsAndTheirMatches - (baseSection -> pairwiseMatch) - (leftSection -> pairwiseMatch),
                matches - pairwiseMatch
              )
            case (
                  (sectionsAndTheirMatches, matches),
                  pairwiseMatch @ Match.BaseAndRight(baseSection, rightSection)
                )
                if sectionsAndTheirMatches
                  .get(baseSection)
                  .intersect(sectionsAndTheirMatches.get(rightSection))
                  .exists(isAnAllSidesMatch) =>
              (
                sectionsAndTheirMatches - (baseSection -> pairwiseMatch) - (rightSection -> pairwiseMatch),
                matches - pairwiseMatch
              )
            case (
                  (sectionsAndTheirMatches, matches),
                  pairwiseMatch @ Match.LeftAndRight(leftSection, rightSection)
                )
                if sectionsAndTheirMatches
                  .get(leftSection)
                  .intersect(sectionsAndTheirMatches.get(rightSection))
                  .exists(isAnAllSidesMatch) =>
              (
                sectionsAndTheirMatches - (leftSection -> pairwiseMatch) - (rightSection -> pairwiseMatch),
                matches - pairwiseMatch
              )

            case (partialResult, _) => partialResult
          }
        end val

        // NOTE: don't remove the sections, as by virtue of the pairwise matches
        // being redundant, there must by all-sides matches that involve those
        // sections.
        copy(sectionsAndTheirMatches =
          updatedSectionsAndTheirMatches
        ) -> updatedMatches
      end withoutRedundantPairwiseMatchesIn

      // TODO: it would be nicer to pass in the new matches and get the sections
      // directly from them, instead of inferring the new sections down in
      // `maximumSizeOfCoalescedSections`...
      private def estimateOptimalMatchSizeInComparisonTo(
          previous: MatchesAndTheirSections
      ): Option[Int] =
        val maximumSizeOfCoalescedBaseSections =
          maximumSizeOfCoalescedSections(
            previous.baseSectionsByPath,
            baseSectionsByPath
          )
        val maximumSizeOfCoalescedLeftSections =
          maximumSizeOfCoalescedSections(
            previous.leftSectionsByPath,
            leftSectionsByPath
          )
        val maximumSizeOfCoalescedRightSections =
          maximumSizeOfCoalescedSections(
            previous.rightSectionsByPath,
            rightSectionsByPath
          )

        Seq(
          maximumSizeOfCoalescedBaseSections,
          maximumSizeOfCoalescedLeftSections,
          maximumSizeOfCoalescedRightSections
        ).flatten.maxOption
      end estimateOptimalMatchSizeInComparisonTo

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

      private def matchFrom(
          baseSection: Section[Element],
          leftSection: Section[Element],
          rightSection: Section[Element]
      ): Option[Match[Section[Element]]] =
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
              Some(Match.AllSides(baseSection, leftSection, rightSection))
            case (false, false, true) =>
              Some(Match.BaseAndLeft(baseSection, leftSection))
            case (false, true, false) =>
              Some(Match.BaseAndRight(baseSection, rightSection))
            case (true, false, false) =>
              Some(Match.LeftAndRight(leftSection, rightSection))
            case _ => None
          end match
        }
      end matchFrom

      private def baseAndLeftMatchOf(
          baseSection: Section[Element],
          leftSection: Section[Element]
      ): Option[Match.BaseAndLeft[Section[Element]]] =
        val excluded =
          overlapsBaseSection(baseSection) || overlapsLeftSection(
            leftSection
          ) || subsumesBaseSection(baseSection) || subsumesLeftSection(
            leftSection
          )

        Option.unless(excluded)(
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
        val excluded =
          overlapsBaseSection(baseSection) || overlapsRightSection(
            rightSection
          ) || subsumesBaseSection(baseSection) || subsumesRightSection(
            rightSection
          )

        Option.unless(excluded)(
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
        val excluded =
          overlapsLeftSection(leftSection) || overlapsRightSection(
            rightSection
          ) || subsumesLeftSection(leftSection) || subsumesRightSection(
            rightSection
          )
        Option.unless(excluded)(
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

              if potentialMatchKeyOrder.eqv(
                  leftHead,
                  minimumFingerprint
                )
              then
                matchingFingerprintsAcrossSides(
                  baseFingerprints,
                  leftFingerprints.tail,
                  rightFingerprints,
                  matches
                )
              else if potentialMatchKeyOrder.eqv(
                  rightHead,
                  minimumFingerprint
                )
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
      ).withPairwiseMatchesEatenInto
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
