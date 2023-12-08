package com.sageserpent.kineticmerge.core

import cats.instances.seq.*
import cats.{Eq, Order}
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import com.google.common.hash.{Funnel, HashFunction}
import com.sageserpent.americium.RangeOfSlots
import com.sageserpent.americium.randomEnrichment.*
import com.sageserpent.kineticmerge.core.genetic.Evolution
import de.sciss.fingertree.RangedSeq
import org.rabinfingerprint.fingerprint.RabinFingerprintLongWindowed
import org.rabinfingerprint.polynomial.Polynomial

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
  )(
      polynomial: Polynomial
  ): Either[AmbiguousMatch.type, CodeMotionAnalysis[Path, Element]] =
    require(0 < minimumSizeFractionForMotionDetection)
    require(1 >= minimumSizeFractionForMotionDetection)

    given witness: Eq[Element] = equality

    val sequenceEquality: Eq[Seq[Element]] = Eq[Seq[Element]]

    // TODO: suppose all the sources are empty? Could this happen?
    val fileSizes = base.filesByPath.values.map(_.size) ++
      left.filesByPath.values.map(_.size) ++
      right.filesByPath.values.map(_.size)

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

    val validWindowSizes =
      minimumWindowSizeAcrossAllFilesOverAllSides to maximumFileSizeAcrossAllFilesOverAllSides

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
      private val allWindowSizesAreFree =
        RangeOfSlots.allSlotsAreVacant(validWindowSizes.size)

      private val descendingWindowSizeOrdering = Ordering[Int].reverse

      private val noWindowSizes = TreeSet.empty(descendingWindowSizeOrdering)

      def initial: Chromosome =
        val windowSizesInDescendingOrder = TreeSet(
          // Need the sure-fire size to make sure that larger matches stand a
          // chance of being partially matched at a smaller size, if the desired
          // match spans files of quite different sizes. Otherwise a potential
          // large match won't be partially matched with the absolute minimum
          // window size because that won't make the grade in the larger files
          // participating in the match.
          (minimumWindowSizeAcrossAllFilesOverAllSides to minimumSureFireWindowSizeAcrossAllFilesOverAllSides).reverse*
        )(descendingWindowSizeOrdering)

        Chromosome(
          windowSizesInDescendingOrder = windowSizesInDescendingOrder,
          windowSizeSlots = allWindowSizesAreFree.claimSlotsOnBehalfOf(
            windowSizesInDescendingOrder
          ),
          deletedWindowSizes = noWindowSizes
        )
      end initial
      extension (windowSizeSlots: RangeOfSlots)
        // NOTE: careful with this helper - because of its recursion invariant
        // it shouldn't be folded into a chain of calls.
        private def claimSlotsOnBehalfOf(
            windowSizesInDecreasingOrder: WindowSizesInDescendingOrder
        ) =
          windowSizesInDecreasingOrder.foldLeft(windowSizeSlots) {
            (windowSizeSlots, highestUnclaimedWindowSize) =>
              val (_, withTheHighestWindowSizeClaimed) =
                // As we work down the sizes, we maintain the invariant that
                // all the slots leading up to the one corresponding to the
                // size we want to claim are vacant. So the slot index we
                // need is just the size's ordinal number, taken zero
                // relative.
                windowSizeSlots.fillVacantSlotAtIndex(
                  highestUnclaimedWindowSize - validWindowSizes.min
                )
              withTheHighestWindowSizeClaimed
          }
    end Chromosome

    case class Chromosome(
        // NOTE: only the window sizes determine equality, the rest is baggage
        // used to allocate new sizes that don't conflict with sizes already in
        // use.
        windowSizesInDescendingOrder: WindowSizesInDescendingOrder,
        windowSizeSlots: RangeOfSlots,
        deletedWindowSizes: WindowSizesInDescendingOrder = noWindowSizes
    ):
      println(
        s"Constructing: $windowSizesInDescendingOrder, $deletedWindowSizes"
      )

      import Chromosome.*

      require(windowSizesInDescendingOrder.nonEmpty)
      require(
        windowSizesInDescendingOrder.intersect(deletedWindowSizes).isEmpty
      )

      // We have vacant slots for window sizes that have never been used, and an
      // additional pool of window sizes that have been used and then deleted.
      require(
        windowSizeSlots.numberOfFilledSlots == windowSizesInDescendingOrder.size + deletedWindowSizes.size
      )

      override def equals(another: Any): Boolean = another match
        case another: Chromosome =>
          this.windowSizesInDescendingOrder == another.windowSizesInDescendingOrder
        case _ => false

      override def hashCode(): Int = windowSizesInDescendingOrder.hashCode()

      def mutate(using random: Random): Chromosome =
        enum Choice:
          case Grow
          case Contract
          case Replace
        end Choice

        val choices = Choice.values.filter {
          case Choice.Replace | Choice.Grow
              if validWindowSizes.size > windowSizesInDescendingOrder.size =>
            // NOTE: replacement has to find an *unused* window size to swap in,
            // so it has to have at least one free window size to proceed.
            true
          case Choice.Contract if 1 < windowSizesInDescendingOrder.size =>
            true
          case _ => false
        }

        random.chooseOneOf(choices) match
          case Choice.Grow =>
            grown
          case Choice.Contract =>
            contracted

          case Choice.Replace =>
            // TODO: contraction can just remove the window size that was added
            // to grow the chromosome, but for now let's live with that as a low
            // probability occurrence.
            grown.contracted
        end match
      end mutate

      windowSizesInDescendingOrder.foreach { size =>
        require(
          validWindowSizes contains size
        )
      }

      private def grown(using random: Random) =
        val numberOfFreeWindowSizes =
          validWindowSizes.size - windowSizesInDescendingOrder.size

        val claimASlot =
          random.chooseAnyNumberFromZeroToOneLessThan(
            numberOfFreeWindowSizes
          ) < windowSizeSlots.numberOfVacantSlots

        if claimASlot then
          @tailrec
          def claimSlot(potentialSlotClaim: Int): (Int, RangeOfSlots) =
            if 1 + potentialSlotClaim == windowSizeSlots.numberOfVacantSlots || random
                .nextBoolean()
            then windowSizeSlots.fillVacantSlotAtIndex(potentialSlotClaim)
            else claimSlot(1 + potentialSlotClaim)

          val (claimedSlotIndex, windowSizeSlotsWithClaim) =
            claimSlot(potentialSlotClaim = 0)

          val claimedWindowSize = claimedSlotIndex + validWindowSizes.min

          Chromosome(
            windowSizesInDescendingOrder =
              windowSizesInDescendingOrder + claimedWindowSize,
            windowSizeSlots = windowSizeSlotsWithClaim,
            deletedWindowSizes = deletedWindowSizes
          )
        else
          val reclaimedWindowSize = random.chooseOneOf(deletedWindowSizes)

          Chromosome(
            windowSizesInDescendingOrder =
              windowSizesInDescendingOrder + reclaimedWindowSize,
            windowSizeSlots = windowSizeSlots,
            deletedWindowSizes = deletedWindowSizes - reclaimedWindowSize
          )
        end if
      end grown

      private def contracted(using random: Random) =
        val deletedWindowSize =
          random.chooseOneOf(windowSizesInDescendingOrder)

        Chromosome(
          windowSizesInDescendingOrder =
            windowSizesInDescendingOrder - deletedWindowSize,
          windowSizeSlots = windowSizeSlots,
          deletedWindowSizes = deletedWindowSizes + deletedWindowSize
        )
      end contracted

      def breedWith(another: Chromosome)(using random: Random): Chromosome =
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
          bredWindowSizes = TreeSet.empty(descendingWindowSizeOrdering),
          pickingState = Synchronized,
          mandatoryState = false
        )

        val deletedWindowSizes =
          (this.deletedWindowSizes ++ another.deletedWindowSizes)
            .removedAll(bredWindowSizes)

        val windowSizeSlots = allWindowSizesAreFree
          // NOTE: don't change this into two successive calls of
          // `claimSlotsOnBehalfOf`, because that helper assumes the claimed
          // window sizes are presented in descending order, so can't be called
          // more than once with sizes that are contained in non-overlapping
          // intervals.
          .claimSlotsOnBehalfOf(bredWindowSizes ++ deletedWindowSizes)

        Chromosome(bredWindowSizes, windowSizeSlots, deletedWindowSizes)
      end breedWith
    end Chromosome

    case class Phenotype(
        chromosomeSize: Int,
        matchGroupsInReverseOrder: MatchGroupsInDescendingOrderOfKeys
    ):
      require(matchGroupsInReverseOrder.forall { case (_, matchGroup) =>
        matchGroup.nonEmpty
      })

      def baseSectionsAndTheirMatches
          : Map[Section[Element], Match[Section[Element]]] =
        matchGroupsInReverseOrder
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
        matchGroupsInReverseOrder
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
        matchGroupsInReverseOrder
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
            x.matchGroupsInReverseOrder.map { case (matchGroupKey, matches) =>
              matchGroupKey -> matches.size
            },
            -x.chromosomeSize
          ),
          (
            y.matchGroupsInReverseOrder.map { case (matchGroupKey, matches) =>
              matchGroupKey -> matches.size
            },
            -y.chromosomeSize
          )
        )
    end given

    given Evolution[Chromosome, Phenotype] with
      private val phenotypeCache: Cache[Chromosome, Phenotype] =
        Caffeine.newBuilder().maximumSize(100).build()
      private val fingerprintingCache
          : Cache[Int, RabinFingerprintLongWindowed] =
        Caffeine.newBuilder().maximumSize(100).build()
      // TODO: review this one - a) it does not seem to add much of a
      // performance benefit because the previous fingerprinting cache cuts out
      // a much larger overhead and b) if we're going to use this we should
      // probably lambda-lift the cache population functions to conform to the
      // caching API's style.
      private val fingerprintSectionsCache
          : Cache[Int, FingerprintSectionsAcrossSides] =
        Caffeine.newBuilder().maximumSize(100).build()

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

          if validWindowSizes contains windowSize then
            require(0 < windowSize)

            def fingerprintStartIndices(
                elements: IndexedSeq[Element]
            ): SortedMultiDict[Long, Int] =
              require(elements.size >= windowSize)

              val fingerprinting = fingerprintingCache.get(
                windowSize,
                { (windowSize: Int) =>
                  val fixedNumberOfBytesInElementHash =
                    hashFunction.bits / JavaByte.SIZE

                  new RabinFingerprintLongWindowed(
                    polynomial,
                    fixedNumberOfBytesInElementHash * windowSize
                  )
                }
              )

              // Fingerprinting is imperative, so go with that style local to
              // this helper function...
              fingerprinting.reset()

              val accumulatingResults = mutable.SortedMultiDict.empty[Long, Int]

              def updateFingerprint(elementIndex: Int): Unit =
                val elementBytes =
                  hashFunction
                    .newHasher()
                    .putObject(elements(elementIndex), funnel)
                    .hash()
                    .asBytes()

                elementBytes.foreach(fingerprinting.pushByte)
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
                  fingerprinting.getFingerprintLong -> fingerprintStartIndex
                )

              accumulatingResults
            end fingerprintStartIndices

            def fingerprintSections(
                sources: Sources[Path, Element]
            ): SortedMultiDict[Long, Section[Element]] =
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
                baseFingerprints: Iterable[Long],
                leftFingerprints: Iterable[Long],
                rightFingerprints: Iterable[Long],
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
                    ).filter { case (_, matches) => matches.nonEmpty })
              end match
            end matchingFingerprintsAcrossSides

            matchingFingerprintsAcrossSides(
              baseSectionsByFingerprint.keySet,
              leftSectionsByFingerprint.keySet,
              rightSectionsByFingerprint.keySet,
              matches = Set.empty,
              sectionsSeenAcrossSides
            )
          else sectionsSeenAcrossSides -> matchGroupsInDescendingOrderOfKeys
          end if
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

        println(
          s"Chromosome: ${chromosome.windowSizesInDescendingOrder}, matches: $matchGroupsInDescendingOrderOfKeys"
        )

        Phenotype(
          chromosomeSize = chromosome.windowSizesInDescendingOrder.size,
          matchGroupsInDescendingOrderOfKeys
        )
      end phenotype_

      private case class FingerprintSectionsAcrossSides(
          baseSectionsByFingerprint: SortedMultiDict[Long, Section[Element]],
          leftSectionsByFingerprint: SortedMultiDict[Long, Section[Element]],
          rightSectionsByFingerprint: SortedMultiDict[Long, Section[Element]]
      )
    end given

    val evolvedPhenotype =
      Evolution.of(
        maximumNumberOfRetries = 10 /*100*/,
        maximumPopulationSize = 10 /*100*/
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
