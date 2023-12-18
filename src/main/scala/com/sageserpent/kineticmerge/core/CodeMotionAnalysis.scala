package com.sageserpent.kineticmerge.core

import cats.instances.seq.*
import cats.{Eq, Order}
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import com.google.common.hash.{Funnel, HashFunction}
import com.sageserpent.americium.randomEnrichment.*
import com.sageserpent.kineticmerge.core.genetic.Evolution
import de.sciss.fingertree.RangedSeq

import java.lang.Byte as JavaByte
import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.collection.{SortedMultiDict, mutable}
import scala.util.Random

trait CodeMotionAnalysis[Path, Element]:
  def base: Map[Path, File[Element]]
  def left: Map[Path, File[Element]]
  def right: Map[Path, File[Element]]

  def matchForBaseSection(
      section: Section[Element]
  ): Option[Match[Section[Element]]]
  def matchForLeftSection(
      section: Section[Element]
  ): Option[Match[Section[Element]]]
  def matchForRightSection(
      section: Section[Element]
  ): Option[Match[Section[Element]]]
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
    * @param minimumSizeFractionForMotionDetection
    *   A section's size must be at least this fraction of its containing file's
    *   size to qualify for motion detection.
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
      minimumSizeFractionForMotionDetection: Double
  )(
      equality: Eq[Element],
      hashFunction: HashFunction,
      funnel: Funnel[Element]
  ): Either[AmbiguousMatch.type, CodeMotionAnalysis[Path, Element]] =
    require(0 <= minimumSizeFractionForMotionDetection)
    require(1 >= minimumSizeFractionForMotionDetection)

    given witness: Eq[Element] = equality

    val sequenceEquality: Eq[Seq[Element]] = Eq[Seq[Element]]

    // TODO: suppose all the sources are empty? Could this happen?
    val fileSizes = base.filesByPath.values.map(_.size) ++
      left.filesByPath.values.map(_.size) ++
      right.filesByPath.values.map(_.size)

    val totalContentSize = fileSizes.sum

    val minimumFileSizeAcrossAllFilesOverAllSides = fileSizes.min

    // This is the minimum window size that would be allowed in *some* file
    // across the sources.
    val minimumWindowSizeAcrossAllFilesOverAllSides =
      1 max (minimumFileSizeAcrossAllFilesOverAllSides * minimumSizeFractionForMotionDetection).ceil.toInt

    val maximumFileSizeAcrossAllFilesOverAllSides = fileSizes.max

    // This is the minimum window size that would be allowed in *all* files
    // across the sources.
    val minimumSureFireWindowSizeAcrossAllFilesOverAllSides =
      1 max (maximumFileSizeAcrossAllFilesOverAllSides * minimumSizeFractionForMotionDetection).ceil.toInt

    var looseExclusiveUpperBoundOnMaximumMatchSize =
      1 + maximumFileSizeAcrossAllFilesOverAllSides

    def dynamicValidWindowSizes: Range =
      minimumWindowSizeAcrossAllFilesOverAllSides until looseExclusiveUpperBoundOnMaximumMatchSize

    enum MatchGrade:
      case Triple
      case Pair
    end MatchGrade

    type MatchGroupKey                = (Int, MatchGrade)
    type WindowSizesInDescendingOrder = TreeSet[Int]
    type MatchGroupsInDescendingOrderOfKeys = Seq[
      (
          MatchGroupKey,
          Set[
            Match[Section[Element]]
          ]
      )
    ]

    object Chromosome:
      private val descendingWindowSizeOrdering = Ordering[Int].reverse
      private val noWindowSizes = TreeSet.empty(descendingWindowSizeOrdering)

      def initial: Chromosome =
        // Need the sure-fire size to make sure that larger matches stand a
        // chance of being partially matched at a smaller size, if the desired
        // match spans files of quite different sizes. Otherwise a potential
        // large match won't be partially matched with the absolute minimum
        // window size because that won't make the grade in the larger files
        // participating in the match; this would cause a Hamming wall because
        // it is unlikely for mutation to chance on a large enough window size
        // to get a successful chromosome.
        withWindowSizes(
          minimumSureFireWindowSizeAcrossAllFilesOverAllSides,
          minimumWindowSizeAcrossAllFilesOverAllSides
        )(dynamicValidWindowSizes)
      end initial

      private def withWindowSizes(windowSizes: Int*)(validWindowSizes: Range) =
        val windowSizesInDescendingOrder =
          TreeSet(windowSizes*)(descendingWindowSizeOrdering)

        Chromosome(
          windowSizesInDescendingOrder = windowSizesInDescendingOrder,
          validWindowSizes = validWindowSizes
        )
      end withWindowSizes
    end Chromosome

    case class Chromosome(
        windowSizesInDescendingOrder: WindowSizesInDescendingOrder,
        validWindowSizes: Range
    ):
      import Chromosome.*

      if windowSizesInDescendingOrder.nonEmpty then
        require(validWindowSizes contains windowSizesInDescendingOrder.head)
        require(validWindowSizes contains windowSizesInDescendingOrder.last)
      end if

      def mutate(using random: Random): Chromosome =
        enum Choice:
          case Grow
          case Contract
          case Replace
        end Choice

        val choices = Choice.values.filter {
          case Choice.Replace | Choice.Grow
              if validWindowSizes.size > windowSizesInDescendingOrder.size =>
            // NOTE: replacement has to find an *unused* window size to swap
            // in, so it has to have at least one free window size to proceed.
            true
          case Choice.Contract if 1 < windowSizesInDescendingOrder.size =>
            true
          case _ => false
        }

        if choices.nonEmpty then
          random.chooseOneOf(choices) match
            case Choice.Grow =>
              trimToSuitDynamicValidWindowSizes.grown
            case Choice.Contract =>
              trimToSuitDynamicValidWindowSizes.contracted
            case Choice.Replace =>
              trimToSuitDynamicValidWindowSizes.nudged
          end match
        else this
        end if
      end mutate

      def breedWith(another: Chromosome)(using random: Random): Chromosome =
        this.trimToSuitDynamicValidWindowSizes.breedWith_(
          another.trimToSuitDynamicValidWindowSizes
        )

      // This method trims the chromosome's window sizes so that the invariant
      // holds again against a snapshot of the dynamic valid window sizes. If
      // all of the chromosomes window sizes turn out to be invalid, we just
      // build a new chromosome using the highest possible valid one.
      private def trimToSuitDynamicValidWindowSizes: Chromosome =
        val validWindowSizesSnapshot = dynamicValidWindowSizes

        val windowSizesInDescendingOrder = validWindowSizesSnapshot.maxOption
          .fold(ifEmpty = noWindowSizes)(maximumValidWindowSize =>
            this.windowSizesInDescendingOrder.rangeFrom(maximumValidWindowSize)
          )

        if windowSizesInDescendingOrder.nonEmpty then
          Chromosome(
            windowSizesInDescendingOrder,
            validWindowSizes = validWindowSizesSnapshot
          )
        else
          Chromosome.withWindowSizes(
            validWindowSizesSnapshot.maxOption.toSeq*
          )(
            validWindowSizesSnapshot
          )
        end if
      end trimToSuitDynamicValidWindowSizes

      private def breedWith_(another: Chromosome)(using
          random: Random
      ): Chromosome =
        // PLAN: walk down the window sizes from both chromosomes, looking for
        // synchronization points where the sizes agree. Between these
        // synchronization points there will be runs of window sizes that belong
        // to one chromosome or the other; these will either lead directly to
        // the next synchronization point or will swap chromosomes. Where there
        // is a swap, choose either the preceding run or the following one.
        // Where there is just one run leading to a synchronization point,
        // choose to either include it or omit it. Synchronized window sizes go
        // through unconditionally, thus making breeding of identical
        // chromosomes stable.

        enum PickingState:
          case PickingFirst
          case SkippingFirst
          case PickingSecond
          case SkippingSecond
          case Synchronized
        end PickingState

        import PickingState.*

        @tailrec
        def pickWindowSizes(
            firstWindowSizesDescending: Iterable[Int],
            secondWindowSizesDescending: Iterable[Int]
        )(
            bredWindowSizes: WindowSizesInDescendingOrder,
            pickingState: PickingState,
            mandatoryState: Boolean
        ): WindowSizesInDescendingOrder =
          (
            firstWindowSizesDescending.headOption,
            secondWindowSizesDescending.headOption
          ) match
            case (Some(firstWindowSize), Some(secondWindowSize)) =>
              if firstWindowSize > secondWindowSize then
                // The first chromosome leads...
                pickingState match
                  case PickingFirst =>
                    pickWindowSizes(
                      firstWindowSizesDescending.tail,
                      secondWindowSizesDescending
                    )(
                      bredWindowSizes + firstWindowSize,
                      pickingState = PickingFirst,
                      mandatoryState = mandatoryState
                    )
                  case SkippingFirst =>
                    pickWindowSizes(
                      firstWindowSizesDescending.tail,
                      secondWindowSizesDescending
                    )(
                      bredWindowSizes,
                      pickingState = SkippingFirst,
                      mandatoryState = mandatoryState
                    )
                  case PickingSecond if !mandatoryState =>
                    pickWindowSizes(
                      firstWindowSizesDescending.tail,
                      secondWindowSizesDescending
                    )(
                      bredWindowSizes,
                      pickingState = SkippingFirst,
                      mandatoryState = true
                    )
                  case SkippingSecond if !mandatoryState =>
                    pickWindowSizes(
                      firstWindowSizesDescending.tail,
                      secondWindowSizesDescending
                    )(
                      bredWindowSizes + firstWindowSize,
                      pickingState = PickingFirst,
                      mandatoryState = true
                    )
                  case Synchronized | PickingSecond | SkippingSecond =>
                    if random.nextBoolean() then
                      pickWindowSizes(
                        firstWindowSizesDescending.tail,
                        secondWindowSizesDescending
                      )(
                        bredWindowSizes + firstWindowSize,
                        pickingState = PickingFirst,
                        mandatoryState = false
                      )
                    else
                      pickWindowSizes(
                        firstWindowSizesDescending.tail,
                        secondWindowSizesDescending
                      )(
                        bredWindowSizes,
                        pickingState = SkippingFirst,
                        mandatoryState = false
                      )
              else if firstWindowSize < secondWindowSize then
                // The second chromosome leads...
                pickingState match
                  case PickingSecond =>
                    pickWindowSizes(
                      firstWindowSizesDescending,
                      secondWindowSizesDescending.tail
                    )(
                      bredWindowSizes + secondWindowSize,
                      pickingState = PickingSecond,
                      mandatoryState = mandatoryState
                    )
                  case SkippingSecond =>
                    pickWindowSizes(
                      firstWindowSizesDescending,
                      secondWindowSizesDescending.tail
                    )(
                      bredWindowSizes,
                      pickingState = SkippingSecond,
                      mandatoryState = mandatoryState
                    )
                  case PickingFirst if !mandatoryState =>
                    pickWindowSizes(
                      firstWindowSizesDescending,
                      secondWindowSizesDescending.tail
                    )(
                      bredWindowSizes,
                      pickingState = SkippingSecond,
                      mandatoryState = true
                    )
                  case SkippingFirst if !mandatoryState =>
                    pickWindowSizes(
                      firstWindowSizesDescending,
                      secondWindowSizesDescending.tail
                    )(
                      bredWindowSizes + secondWindowSize,
                      pickingState = PickingSecond,
                      mandatoryState = true
                    )
                  case Synchronized | PickingFirst | SkippingFirst =>
                    if random.nextBoolean() then
                      pickWindowSizes(
                        firstWindowSizesDescending,
                        secondWindowSizesDescending.tail
                      )(
                        bredWindowSizes + secondWindowSize,
                        pickingState = PickingSecond,
                        mandatoryState = false
                      )
                    else
                      pickWindowSizes(
                        firstWindowSizesDescending,
                        secondWindowSizesDescending.tail
                      )(
                        bredWindowSizes,
                        pickingState = SkippingSecond,
                        mandatoryState = false
                      )
              else
                // Synchronized the two chromosomes...
                pickWindowSizes(
                  firstWindowSizesDescending.tail,
                  secondWindowSizesDescending.tail
                )(
                  bredWindowSizes = bredWindowSizes + firstWindowSize,
                  pickingState = Synchronized,
                  mandatoryState = false
                )
            case (Some(_), None) =>
              if bredWindowSizes.isEmpty || random.nextBoolean() then
                bredWindowSizes ++ firstWindowSizesDescending
              else bredWindowSizes
            case (None, Some(_)) =>
              if bredWindowSizes.isEmpty || random.nextBoolean() then
                bredWindowSizes ++ secondWindowSizesDescending
              else bredWindowSizes
            case (None, None) =>
              bredWindowSizes
          end match
        end pickWindowSizes

        val bredWindowSizes = pickWindowSizes(
          this.windowSizesInDescendingOrder,
          another.windowSizesInDescendingOrder
        )(
          bredWindowSizes = noWindowSizes,
          pickingState = Synchronized,
          mandatoryState = false
        )

        Chromosome(
          bredWindowSizes,
          validWindowSizes
        )
      end breedWith_

      private def nudged(using random: Random) =
        val numberOfFreeWindowSizes =
          validWindowSizes.size - windowSizesInDescendingOrder.size

        val numberOfFreeWindowSizesAboveTheCurrentMaximum =
          validWindowSizes.max - windowSizesInDescendingOrder.head

        val numberOfFreeWindowSizesBelowTheCurrentMaximum =
          numberOfFreeWindowSizes - numberOfFreeWindowSizesAboveTheCurrentMaximum

        val roomAvailableBeforeTheHighestWindowSize =
          numberOfFreeWindowSizes > numberOfFreeWindowSizesAboveTheCurrentMaximum

        if roomAvailableBeforeTheHighestWindowSize then
          // The new window size will either come before the current minimum
          // or will fit in a gap before the current maximum...

          val newWindowSizeIndex =
            random.chooseAnyNumberFromZeroToOneLessThan(
              numberOfFreeWindowSizesBelowTheCurrentMaximum
            )

          val (outgoingWindowSize, newWindowSize) =
            val gapBoundaries =
              LazyList.from(
                windowSizesInDescendingOrder.incl(
                  oneBeforeLowestValidWindowSize
                )
              )

            // NOTE: gaps are arranged to *descend* down window size, so larger
            // indices select smaller window sizes...

            val sizeGaps = gapBoundaries.zip(gapBoundaries.tail).filter {
              case (larger, smaller) => larger > 1 + smaller
            }

            val onePastIndexOfEachLowestFreeWindowSizePerGap = sizeGaps
              .scanLeft(0) { case (index, (larger, smaller)) =>
                val numberOfVacanciesInGap = larger - (1 + smaller)
                index + numberOfVacanciesInGap
              }
              .tail

            val (
              onePastIndexOfLowestFreeWindowSize,
              (
                _,
                lowerGapBoundary
              )
            ) =
              onePastIndexOfEachLowestFreeWindowSizePerGap
                .zip(sizeGaps)
                .dropWhile { case (onePastIndexOfLowestFreeWindowSize, _) =>
                  onePastIndexOfLowestFreeWindowSize <= newWindowSizeIndex
                }
                .head

            lowerGapBoundary -> (lowerGapBoundary + (onePastIndexOfLowestFreeWindowSize - newWindowSizeIndex))
          end val

          assert(
            windowSizesInDescendingOrder.contains(
              outgoingWindowSize
            ) || outgoingWindowSize == oneBeforeLowestValidWindowSize
          )
          assert(!windowSizesInDescendingOrder.contains(newWindowSize))
          assert(newWindowSize > outgoingWindowSize)

          for successor <- windowSizesInDescendingOrder.maxBefore(
              outgoingWindowSize
            )
          do assert(newWindowSize < successor)
          end for

          Chromosome(
            windowSizesInDescendingOrder =
              windowSizesInDescendingOrder + newWindowSize - outgoingWindowSize,
            validWindowSizes = validWindowSizes
          )
        else
          // Fall back to growing and contracting, possibly even
          // round-tripping the chromosome to the same state.
          grown.contracted
        end if
      end nudged

      // NOTE: the following helper is a method because it has a precondition
      // that there are valid window sizes.
      private def oneBeforeLowestValidWindowSize = validWindowSizes.min - 1

      private def grown(using random: Random) =
        val numberOfFreeWindowSizes =
          validWindowSizes.size - windowSizesInDescendingOrder.size

        val whereWillThisLand =
          random.chooseAnyNumberFromZeroToOneLessThan(numberOfFreeWindowSizes)

        val numberOfFreeWindowSizesAboveTheCurrentMaximum =
          windowSizesInDescendingOrder.maxOption.fold(ifEmpty =
            validWindowSizes.size
          )(validWindowSizes.max - _)

        val numberOfFreeWindowSizesBelowTheCurrentMaximum =
          numberOfFreeWindowSizes - numberOfFreeWindowSizesAboveTheCurrentMaximum

        val newWindowSize =
          if numberOfFreeWindowSizesBelowTheCurrentMaximum > whereWillThisLand
          then
            // This is subtle: `whereWillThisLand` should be thought of as
            // choosing an integer from either [0,
            // `numberOfFreeWindowSizesBelowTheCurrentMaximum`) - so choosing to
            // fill in a gap - or
            // [`numberOfFreeWindowSizesBelowTheCurrentMaximum`,
            // `numberOfFreeWindowSizes`) - so beating the current maximum. One
            // we decide to fill in a gap, we use the chosen integer as an index
            // in reversed sense, so zero selects from the highest gap.
            val newWindowSizeIndex = whereWillThisLand

            // The new window size will either come before the current minimum
            // or will fit in a gap before the current maximum...
            val gapBoundaries =
              LazyList.from(
                windowSizesInDescendingOrder.incl(
                  oneBeforeLowestValidWindowSize
                )
              )

            // NOTE: gaps are arranged to *descend* down window size, so larger
            // indices select smaller window sizes...

            val sizeGaps = gapBoundaries.zip(gapBoundaries.tail).filter {
              case (larger, smaller) => larger > 1 + smaller
            }

            val onePastIndexOfEachLowestFreeWindowSizePerGap = sizeGaps
              .scanLeft(0) { case (index, (larger, smaller)) =>
                val numberOfVacanciesInGap = larger - (1 + smaller)
                index + numberOfVacanciesInGap
              }
              .tail

            val (
              onePastIndexOfLowestFreeWindowSize,
              (
                higherGapBoundary,
                lowerGapBoundary
              )
            ) =
              onePastIndexOfEachLowestFreeWindowSizePerGap
                .zip(sizeGaps)
                .dropWhile { case (onePastIndexOfLowestFreeWindowSize, _) =>
                  onePastIndexOfLowestFreeWindowSize <= newWindowSizeIndex
                }
                .head

            lowerGapBoundary + (onePastIndexOfLowestFreeWindowSize - newWindowSizeIndex)
          else
            // Go beyond the maximum window size, but don't be too ambitious...
            val baselineWindowSize = windowSizesInDescendingOrder.maxOption
              .fold(ifEmpty = validWindowSizes.min)(1 + _)
            val geometricMean = Math
              .sqrt(
                baselineWindowSize * validWindowSizes.max
              )
              .ceil
              .toInt

            baselineWindowSize + random.chooseAnyNumberFromZeroToOneLessThan(
              1 + geometricMean - baselineWindowSize
            )
          end if
        end newWindowSize

        assert(!windowSizesInDescendingOrder.contains(newWindowSize))

        for
          predecessor <- windowSizesInDescendingOrder.minAfter(newWindowSize)
          successor   <- windowSizesInDescendingOrder.maxBefore(predecessor)
        do assert(newWindowSize < successor)
        end for

        Chromosome(
          windowSizesInDescendingOrder =
            windowSizesInDescendingOrder + newWindowSize,
          validWindowSizes = validWindowSizes
        )
      end grown

      private def contracted(using random: Random) =
        val deletedWindowSize = random.chooseOneOf(windowSizesInDescendingOrder)

        Chromosome(
          windowSizesInDescendingOrder =
            windowSizesInDescendingOrder - deletedWindowSize,
          validWindowSizes = validWindowSizes
        )
      end contracted
    end Chromosome

    case class Phenotype(
        chromosomeSize: Int,
        matchGroupsInDescendingOrderOfKeys: MatchGroupsInDescendingOrderOfKeys
    ):
      require(matchGroupsInDescendingOrderOfKeys.forall {
        case (_, matchGroup) =>
          matchGroup.nonEmpty
      })

      def baseSectionsAndTheirMatches
          : Map[Section[Element], Match[Section[Element]]] =
        matchGroupsInDescendingOrderOfKeys
          .map { case (_, matches) =>
            matches.view.collect {
              case aMatch @ Match.AllThree(baseSection, _, _) =>
                baseSection -> aMatch
              case aMatch @ Match.BaseAndLeft(baseSection, _) =>
                baseSection -> aMatch
              case aMatch @ Match.BaseAndRight(baseSection, _) =>
                baseSection -> aMatch
            }.toMap
          }
          .reduceOption(_ ++ _)
          .getOrElse(Map.empty)

      def leftSectionsAndTheirMatches
          : Map[Section[Element], Match[Section[Element]]] =
        matchGroupsInDescendingOrderOfKeys
          .map { case (_, matches) =>
            matches.view.collect {
              case aMatch @ Match.AllThree(_, leftSection, _) =>
                leftSection -> aMatch
              case aMatch @ Match.BaseAndLeft(_, leftSection) =>
                leftSection -> aMatch
              case aMatch @ Match.LeftAndRight(leftSection, _) =>
                leftSection -> aMatch
            }.toMap
          }
          .reduceOption(_ ++ _)
          .getOrElse(Map.empty)

      def rightSectionsAndTheirMatches
          : Map[Section[Element], Match[Section[Element]]] =
        matchGroupsInDescendingOrderOfKeys
          .map { case (_, matches) =>
            matches.view.collect {
              case aMatch @ Match.AllThree(_, _, rightSection) =>
                rightSection -> aMatch
              case aMatch @ Match.BaseAndRight(_, rightSection) =>
                rightSection -> aMatch
              case aMatch @ Match.LeftAndRight(_, rightSection) =>
                rightSection -> aMatch
            }.toMap
          }
          .reduceOption(_ ++ _)
          .getOrElse(Map.empty)

    end Phenotype

    given Order[MatchGrade] with
      override def compare(x: MatchGrade, y: MatchGrade): Int =
        // A triple beats a pair.
        (x, y) match
          case (MatchGrade.Pair, MatchGrade.Pair)     => 0
          case (MatchGrade.Pair, MatchGrade.Triple)   => -1
          case (MatchGrade.Triple, MatchGrade.Pair)   => 1
          case (MatchGrade.Triple, MatchGrade.Triple) => 0
    end given

    given Order[Phenotype] with
      override def compare(x: Phenotype, y: Phenotype): Int =
        // Do a sequential tie-breaker comparison of the group keys paired with
        // the group sizes, assuming these are in descending order of group
        // keys. The least chromosome size is the final tiebreaker.
        Order.compare(
          (
            x.matchGroupsInDescendingOrderOfKeys.map {
              case (matchGroupKey, matches) =>
                matchGroupKey -> matches.size
            },
            -x.chromosomeSize
          ),
          (
            y.matchGroupsInDescendingOrderOfKeys.map {
              case (matchGroupKey, matches) =>
                matchGroupKey -> matches.size
            },
            -y.chromosomeSize
          )
        )
    end given

    given Evolution[Chromosome, Phenotype] with
      private val phenotypeCache: Cache[Chromosome, Phenotype] =
        Caffeine.newBuilder().build()
      private val rollingHashFactoryCache: Cache[Int, RollingHash.Factory] =
        Caffeine.newBuilder().build()
      // TODO: review this one - a) it does not seem to add much of a
      // performance benefit because the previous fingerprinting cache cuts out
      // a much larger overhead and b) if we're going to use this we should
      // probably lambda-lift the cache population functions to conform to the
      // caching API's style.
      private val fingerprintSectionsCache
          : Cache[Int, FingerprintSectionsAcrossSides] =
        Caffeine.newBuilder().build()

      override def mutate(chromosome: Chromosome)(using
          random: Random
      ): Chromosome = chromosome.mutate

      override def breed(first: Chromosome, second: Chromosome)(using
          random: Random
      ): Chromosome = first.breedWith(second)

      override def initialChromosome: Chromosome = Chromosome.initial

      override def phenotype(chromosome: Chromosome): Phenotype =
        phenotypeCache.get(chromosome, phenotype_)

      private def phenotype_(chromosome: Chromosome): Phenotype =
        type SectionsSeen = RangedSeq[Section[Element], Int]

        case class SectionsSeenAcrossSides(
            baseSectionsByPath: Map[Path, SectionsSeen] = Map.empty,
            leftSectionsByPath: Map[Path, SectionsSeen] = Map.empty,
            rightSectionsByPath: Map[Path, SectionsSeen] = Map.empty
        ):
          val containsBaseSection: Section[Element] => Boolean =
            containsSection(base, baseSectionsByPath)
          val containsLeftSection: Section[Element] => Boolean =
            containsSection(left, leftSectionsByPath)
          val containsRightSection: Section[Element] => Boolean =
            containsSection(right, rightSectionsByPath)

          private val withBaseSection
              : Section[Element] => Map[Path, SectionsSeen] =
            withSection(base, baseSectionsByPath)
          private val withLeftSection
              : Section[Element] => Map[Path, SectionsSeen] =
            withSection(left, leftSectionsByPath)
          private val withRightSection
              : Section[Element] => Map[Path, SectionsSeen] =
            withSection(right, rightSectionsByPath)

          def withSectionsFrom(
              matches: Set[Match[Section[Element]]]
          ): SectionsSeenAcrossSides =
            matches.foldLeft(this)(_.withSectionsFrom(_))

          private def withSectionsFrom(
              aMatch: Match[Section[Element]]
          ): SectionsSeenAcrossSides =
            aMatch match
              case Match.AllThree(baseSection, leftSection, rightSection) =>
                copy(
                  baseSectionsByPath = withBaseSection(baseSection),
                  leftSectionsByPath = withLeftSection(leftSection),
                  rightSectionsByPath = withRightSection(rightSection)
                )
              case Match.BaseAndLeft(baseSection, leftSection) =>
                copy(
                  baseSectionsByPath = withBaseSection(baseSection),
                  leftSectionsByPath = withLeftSection(leftSection)
                )
              case Match.BaseAndRight(baseSection, rightSection) =>
                copy(
                  baseSectionsByPath = withBaseSection(baseSection),
                  rightSectionsByPath = withRightSection(rightSection)
                )
              case Match.LeftAndRight(leftSection, rightSection) =>
                copy(
                  leftSectionsByPath = withLeftSection(leftSection),
                  rightSectionsByPath = withRightSection(rightSection)
                )
            end match
          end withSectionsFrom

          private def containsSection(
              side: Sources[Path, Element],
              sectionsByPath: Map[Path, SectionsSeen]
          )(section: Section[Element]): Boolean =
            sectionsByPath
              .get(side.pathFor(section))
              .fold(ifEmpty = false)(
                _.includes(section.closedOpenInterval)
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
              case Some(sections) => Some(sections + section)
              case None =>
                Some(
                  RangedSeq(section)(_.closedOpenInterval, Ordering[Int])
                )
            }
        end SectionsSeenAcrossSides

        def matchesForWindowSize(
            partialResult: (
                SectionsSeenAcrossSides,
                MatchGroupsInDescendingOrderOfKeys
            ),
            windowSize: Int
        ): (SectionsSeenAcrossSides, MatchGroupsInDescendingOrderOfKeys) =
          val (sectionsSeenAcrossSides, matchGroupsInDescendingOrderOfKeys) =
            partialResult

          require(0 < windowSize)

          def fingerprintStartIndices(
              elements: IndexedSeq[Element]
          ): SortedMultiDict[BigInt, Int] =
            require(elements.size >= windowSize)

            val rollingHashFactory = rollingHashFactoryCache.get(
              windowSize,
              { (windowSize: Int) =>
                println(
                  s"looseExclusiveUpperBoundOnMaximumMatchSize: $looseExclusiveUpperBoundOnMaximumMatchSize, windowSize: $windowSize"
                )

                val fixedNumberOfBytesInElementHash =
                  hashFunction.bits / JavaByte.SIZE

                new RollingHash.Factory(
                  // Translate the window size from number of elements to number
                  // of bytes.
                  windowSize = fixedNumberOfBytesInElementHash * windowSize,
                  numberOfFingerprintsToBeTaken = totalContentSize
                )
              }
            )

            val rollingHash = rollingHashFactory()

            val accumulatingResults = mutable.SortedMultiDict.empty[BigInt, Int]

            def updateFingerprint(elementIndex: Int): Unit =
              val elementBytes =
                hashFunction
                  .newHasher()
                  .putObject(elements(elementIndex), funnel)
                  .hash()
                  .asBytes()

              elementBytes.foreach(rollingHash.pushByte)
            end updateFingerprint

            // NOTE: fingerprints are incrementally calculated walking *down*
            // the elements.
            val descendingIndices = elements.indices.reverse

            val (primingIndices, fingerprintingIndices) =
              descendingIndices.splitAt(windowSize - 1)

            // Prime to get ready for the first fingerprint...
            primingIndices.foreach(updateFingerprint)

            // ... henceforth, each pass records a new fingerprint, starting
            // with the first.
            fingerprintingIndices.foreach: fingerprintStartIndex =>
              updateFingerprint(fingerprintStartIndex)
              accumulatingResults.addOne(
                rollingHash.fingerprint -> fingerprintStartIndex
              )

            accumulatingResults
          end fingerprintStartIndices

          def fingerprintSections(
              sources: Sources[Path, Element]
          ): SortedMultiDict[BigInt, Section[Element]] =
            sources.filesByPath
              .filter { case (_, file) =>
                val fileSize = file.size
                val minimumWindowSize =
                  (minimumSizeFractionForMotionDetection * fileSize).ceil.toInt

                minimumWindowSize to fileSize contains windowSize
              }
              .map { case (path, file) =>
                fingerprintStartIndices(file.content).map(
                  (fingerprint, fingerprintStartIndex) =>
                    fingerprint -> sources
                      .section(path)(fingerprintStartIndex, windowSize)
                )
              }
              // This isn't quite the same as flat-mapping / flattening,
              // because we want the type of the result to be a
              // `SortedMultiDict`...
              .reduceOption(_ concat _)
              .getOrElse(SortedMultiDict.empty)
          end fingerprintSections

          val FingerprintSectionsAcrossSides(
            baseSectionsByFingerprint,
            leftSectionsByFingerprint,
            rightSectionsByFingerprint
          ) = fingerprintSectionsCache.get(
            windowSize,
            _ =>
              FingerprintSectionsAcrossSides(
                baseSectionsByFingerprint = fingerprintSections(base),
                leftSectionsByFingerprint = fingerprintSections(left),
                rightSectionsByFingerprint = fingerprintSections(right)
              )
          )

          @tailrec
          def matchingFingerprintsAcrossSides(
              baseFingerprints: Iterable[BigInt],
              leftFingerprints: Iterable[BigInt],
              rightFingerprints: Iterable[BigInt],
              matches: Set[Match[Section[Element]]],
              sectionsSeenAcrossSides: SectionsSeenAcrossSides
          ): (SectionsSeenAcrossSides, MatchGroupsInDescendingOrderOfKeys) =
            (
              baseFingerprints.headOption,
              leftFingerprints.headOption,
              rightFingerprints.headOption
            ) match
              case (Some(baseHead), Some(leftHead), Some(rightHead))
                  if baseHead == leftHead && baseHead == rightHead =>
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
                    baseSection  <- baseSections
                    leftSection  <- leftSections
                    rightSection <- rightSections
                    // Guard against false positives sharing the same
                    // fingerprint...
                    if sequenceEquality.eqv(
                      baseSection.content,
                      leftSection.content
                    ) && sequenceEquality.eqv(
                      baseSection.content,
                      rightSection.content
                    )
                    baseSubsumed = sectionsSeenAcrossSides
                      .containsBaseSection(
                        baseSection
                      )
                    leftSubsumed = sectionsSeenAcrossSides
                      .containsLeftSection(
                        leftSection
                      )
                    rightSubsumed = sectionsSeenAcrossSides
                      .containsRightSection(
                        rightSection
                      )
                    suppressed =
                      // In contrast with the pairwise matches below, we have
                      // the subtlety of a *replacement* of an all-three match
                      // with a pairwise match to consider, so this condition
                      // is more complex than the others...
                      baseSubsumed && leftSubsumed || baseSubsumed && rightSubsumed || leftSubsumed && rightSubsumed
                    if !suppressed
                  yield
                    if baseSubsumed then
                      Match.LeftAndRight(leftSection, rightSection)
                    else if leftSubsumed then
                      Match.BaseAndRight(baseSection, rightSection)
                    else if rightSubsumed then
                      Match.BaseAndLeft(baseSection, leftSection)
                    else
                      Match.AllThree(
                        baseSection,
                        leftSection,
                        rightSection
                      )
                  ).toSet
                end matchesForSynchronisedFingerprint

                matchingFingerprintsAcrossSides(
                  baseFingerprints.tail,
                  leftFingerprints.tail,
                  rightFingerprints.tail,
                  matches ++ matchesForSynchronisedFingerprint,
                  sectionsSeenAcrossSides
                    .withSectionsFrom(matchesForSynchronisedFingerprint)
                )

              case (Some(baseHead), Some(leftHead), Some(rightHead))
                  if baseHead == leftHead && baseHead > rightHead =>
                // Tentatively synchronised the fingerprints between the base
                // and left, need to advance the right to resolve ...
                matchingFingerprintsAcrossSides(
                  baseFingerprints,
                  leftFingerprints,
                  rightFingerprints.tail,
                  matches,
                  sectionsSeenAcrossSides
                )

              case (Some(baseHead), Some(leftHead), _)
                  if baseHead == leftHead =>
                // Synchronised the fingerprints between the base and left...
                val matchesForSynchronisedFingerprint
                    : Set[Match[Section[Element]]] =
                  val baseSections =
                    baseSectionsByFingerprint.get(baseHead)
                  val leftSections =
                    leftSectionsByFingerprint.get(leftHead)

                  (for
                    baseSection <- baseSections
                    leftSection <- leftSections
                    // Guard against false positives sharing the same
                    // fingerprint...
                    if sequenceEquality.eqv(
                      baseSection.content,
                      leftSection.content
                    )
                    suppressed = sectionsSeenAcrossSides.containsBaseSection(
                      baseSection
                    ) || sectionsSeenAcrossSides.containsLeftSection(
                      leftSection
                    )
                    if !suppressed
                  yield Match.BaseAndLeft(
                    baseSection,
                    leftSection
                  )).toSet
                end matchesForSynchronisedFingerprint

                matchingFingerprintsAcrossSides(
                  baseFingerprints.tail,
                  leftFingerprints.tail,
                  rightFingerprints,
                  matches ++ matchesForSynchronisedFingerprint,
                  sectionsSeenAcrossSides
                    .withSectionsFrom(matchesForSynchronisedFingerprint)
                )

              case (Some(baseHead), Some(leftHead), Some(rightHead))
                  if baseHead == rightHead && baseHead > leftHead =>
                // Tentatively synchronised the fingerprints between the base
                // and right, need to advance the left to resolve ...
                matchingFingerprintsAcrossSides(
                  baseFingerprints,
                  leftFingerprints.tail,
                  rightFingerprints,
                  matches,
                  sectionsSeenAcrossSides
                )

              case (Some(baseHead), _, Some(rightHead))
                  if baseHead == rightHead =>
                // Synchronised the fingerprints between the base and right...
                val matchesForSynchronisedFingerprint
                    : Set[Match[Section[Element]]] =
                  val baseSections =
                    baseSectionsByFingerprint.get(baseHead)
                  val rightSections =
                    rightSectionsByFingerprint.get(rightHead)

                  (for
                    baseSection  <- baseSections
                    rightSection <- rightSections
                    // Guard against false positives sharing the same
                    // fingerprint...
                    if sequenceEquality.eqv(
                      baseSection.content,
                      rightSection.content
                    )
                    suppressed = sectionsSeenAcrossSides.containsBaseSection(
                      baseSection
                    ) || sectionsSeenAcrossSides.containsRightSection(
                      rightSection
                    )
                    if !suppressed
                  yield Match.BaseAndRight(
                    baseSection,
                    rightSection
                  )).toSet
                end matchesForSynchronisedFingerprint

                matchingFingerprintsAcrossSides(
                  baseFingerprints.tail,
                  leftFingerprints,
                  rightFingerprints.tail,
                  matches ++ matchesForSynchronisedFingerprint,
                  sectionsSeenAcrossSides
                    .withSectionsFrom(matchesForSynchronisedFingerprint)
                )

              case (Some(baseHead), Some(leftHead), Some(rightHead))
                  if leftHead == rightHead && leftHead > baseHead =>
                // Tentatively synchronised the fingerprints between the left
                // and right, need to advance the base to resolve ...
                matchingFingerprintsAcrossSides(
                  baseFingerprints.tail,
                  leftFingerprints,
                  rightFingerprints,
                  matches,
                  sectionsSeenAcrossSides
                )

              case (_, Some(leftHead), Some(rightHead))
                  if leftHead == rightHead =>
                // Synchronised the fingerprints between the left and right...
                val matchesForSynchronisedFingerprint
                    : Set[Match[Section[Element]]] =
                  val leftSections =
                    leftSectionsByFingerprint.get(leftHead)
                  val rightSections =
                    rightSectionsByFingerprint.get(rightHead)

                  (for
                    leftSection  <- leftSections
                    rightSection <- rightSections
                    // Guard against false positives sharing the same
                    // fingerprint...
                    if sequenceEquality.eqv(
                      leftSection.content,
                      rightSection.content
                    )
                    suppressed = sectionsSeenAcrossSides.containsLeftSection(
                      leftSection
                    ) || sectionsSeenAcrossSides.containsRightSection(
                      rightSection
                    )
                    if !suppressed
                  yield Match.LeftAndRight(
                    leftSection,
                    rightSection
                  )).toSet
                end matchesForSynchronisedFingerprint

                matchingFingerprintsAcrossSides(
                  baseFingerprints,
                  leftFingerprints.tail,
                  rightFingerprints.tail,
                  matches ++ matchesForSynchronisedFingerprint,
                  sectionsSeenAcrossSides
                    .withSectionsFrom(matchesForSynchronisedFingerprint)
                )

              case (Some(baseHead), Some(leftHead), Some(rightHead)) =>
                // All the fingerprints disagree, so advance the side with the
                // minimum fingerprint to see if it can catch up and
                // synchronise...

                val minimumFingerprint = baseHead min leftHead min rightHead

                if leftHead == minimumFingerprint then
                  matchingFingerprintsAcrossSides(
                    baseFingerprints,
                    leftFingerprints.tail,
                    rightFingerprints,
                    matches,
                    sectionsSeenAcrossSides
                  )
                else if rightHead == minimumFingerprint then
                  matchingFingerprintsAcrossSides(
                    baseFingerprints,
                    leftFingerprints,
                    rightFingerprints.tail,
                    matches,
                    sectionsSeenAcrossSides
                  )
                else
                  matchingFingerprintsAcrossSides(
                    baseFingerprints.tail,
                    leftFingerprints,
                    rightFingerprints,
                    matches,
                    sectionsSeenAcrossSides
                  )
                end if

              case (Some(baseHead), Some(leftHead), None) =>
                // The base and left fingerprints disagree, so advance the
                // side with the minimum fingerprint to see if it can catch up
                // and synchronise...
                if baseHead < leftHead then
                  matchingFingerprintsAcrossSides(
                    baseFingerprints.tail,
                    leftFingerprints,
                    rightFingerprints,
                    matches,
                    sectionsSeenAcrossSides
                  )
                else
                  matchingFingerprintsAcrossSides(
                    baseFingerprints,
                    leftFingerprints.tail,
                    rightFingerprints,
                    matches,
                    sectionsSeenAcrossSides
                  )

              case (Some(baseHead), None, Some(rightHead)) =>
                // The base and right fingerprints disagree, so advance the
                // side with the minimum fingerprint to see if it can catch up
                // and synchronise...
                if baseHead < rightHead then
                  matchingFingerprintsAcrossSides(
                    baseFingerprints.tail,
                    leftFingerprints,
                    rightFingerprints,
                    matches,
                    sectionsSeenAcrossSides
                  )
                else
                  matchingFingerprintsAcrossSides(
                    baseFingerprints,
                    leftFingerprints,
                    rightFingerprints.tail,
                    matches,
                    sectionsSeenAcrossSides
                  )

              case (None, Some(leftHead), Some(rightHead)) =>
                // The left and right fingerprints disagree, so advance the
                // side with the minimum fingerprint to see if it can catch up
                // and synchronise...
                if leftHead < rightHead then
                  matchingFingerprintsAcrossSides(
                    baseFingerprints,
                    leftFingerprints.tail,
                    rightFingerprints,
                    matches,
                    sectionsSeenAcrossSides
                  )
                else
                  matchingFingerprintsAcrossSides(
                    baseFingerprints,
                    leftFingerprints,
                    rightFingerprints.tail,
                    matches,
                    sectionsSeenAcrossSides
                  )

              case _ =>
                // There are no more opportunities to match a full triple or
                // just a pair, so this terminates the recursion. Add the
                // triples first if we have any, then any pairs as we are
                // adding match groups in descending order of keys.

                if matches.isEmpty
                then
                  sectionsSeenAcrossSides -> matchGroupsInDescendingOrderOfKeys
                else
                  val (tripleMatches, pairMatches) = matches.partition {
                    case _: Match.AllThree[Section[Element]] => true
                    case _                                   => false
                  }

                  sectionsSeenAcrossSides -> (matchGroupsInDescendingOrderOfKeys ++ Seq(
                    (windowSize, MatchGrade.Triple) -> tripleMatches,
                    (windowSize, MatchGrade.Pair)   -> pairMatches
                  ).filter { case entry @ (_, matches) => matches.nonEmpty })
            end match
          end matchingFingerprintsAcrossSides

          matchingFingerprintsAcrossSides(
            baseSectionsByFingerprint.keySet,
            leftSectionsByFingerprint.keySet,
            rightSectionsByFingerprint.keySet,
            matches = Set.empty,
            sectionsSeenAcrossSides
          )

        end matchesForWindowSize

        val (
          _,
          matchGroupsInDescendingOrderOfKeys: MatchGroupsInDescendingOrderOfKeys
        ) =
          chromosome.windowSizesInDescendingOrder.foldLeft(
            SectionsSeenAcrossSides() -> Seq.empty
          )(
            matchesForWindowSize
          )

//        println(
//          s"Chromosome: ${chromosome.windowSizesInDescendingOrder}, matches: $matchGroupsInDescendingOrderOfKeys"
//        )

        val pointlessWindowSizes = matchGroupsInDescendingOrderOfKeys.headOption
          .fold(ifEmpty = chromosome.windowSizesInDescendingOrder) {
            case ((largestMatchingWindowSize, _), _) =>
              chromosome.windowSizesInDescendingOrder.rangeUntil(
                largestMatchingWindowSize
              )
          }
          .filter(_ >= minimumSureFireWindowSizeAcrossAllFilesOverAllSides)

        pointlessWindowSizes.minOption
          .foreach { lowestPointlessWindowSize =>
            looseExclusiveUpperBoundOnMaximumMatchSize =
              lowestPointlessWindowSize min
                looseExclusiveUpperBoundOnMaximumMatchSize
          }

        Phenotype(
          chromosomeSize = chromosome.windowSizesInDescendingOrder.size,
          matchGroupsInDescendingOrderOfKeys
        )
      end phenotype_

      private case class FingerprintSectionsAcrossSides(
          baseSectionsByFingerprint: SortedMultiDict[BigInt, Section[Element]],
          leftSectionsByFingerprint: SortedMultiDict[BigInt, Section[Element]],
          rightSectionsByFingerprint: SortedMultiDict[BigInt, Section[Element]]
      )
    end given

    val evolvedPhenotype =
      Evolution.of(
        maximumNumberOfRetries = 3,
        maximumPopulationSize = 10
      )

    println(s"Finally: -----> $evolvedPhenotype")

    val baseSectionsAndTheirMatches =
      evolvedPhenotype.baseSectionsAndTheirMatches
    val leftSectionsAndTheirMatches =
      evolvedPhenotype.leftSectionsAndTheirMatches
    val rightSectionsAndTheirMatches =
      evolvedPhenotype.rightSectionsAndTheirMatches

    val baseFilesByPath =
      base.filesByPathUtilising(baseSectionsAndTheirMatches.keySet)
    val leftFilesByPath =
      left.filesByPathUtilising(leftSectionsAndTheirMatches.keySet)
    val rightFilesByPath =
      right.filesByPathUtilising(rightSectionsAndTheirMatches.keySet)

    Right(
      new CodeMotionAnalysis[Path, Element]:
        override def matchForBaseSection(
            section: Section[Element]
        ): Option[Match[Section[Element]]] =
          baseSectionsAndTheirMatches.get(section)

        override def matchForLeftSection(
            section: Section[Element]
        ): Option[Match[Section[Element]]] =
          leftSectionsAndTheirMatches.get(section)

        override def matchForRightSection(
            section: Section[Element]
        ): Option[Match[Section[Element]]] =
          rightSectionsAndTheirMatches.get(section)

        override def base: Map[Path, File[Element]] = baseFilesByPath

        override def left: Map[Path, File[Element]] = leftFilesByPath

        override def right: Map[Path, File[Element]] = rightFilesByPath
    )
  end of

  // TODO - what happened?
  case object AmbiguousMatch
end CodeMotionAnalysis
