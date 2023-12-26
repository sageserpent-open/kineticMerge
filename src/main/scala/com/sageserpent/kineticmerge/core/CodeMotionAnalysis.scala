package com.sageserpent.kineticmerge.core

import cats.implicits.catsKernelOrderingForOrder
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

  // TODO: why do we need to build a map for each side? Surely sections from
  // different sides can't be equal?
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
      order: Order[Element],
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
      1 max (minimumFileSizeAcrossAllFilesOverAllSides * minimumSizeFractionForMotionDetection).floor.toInt

    val maximumFileSizeAcrossAllFilesOverAllSides = fileSizes.max

    // This is the minimum window size that would be allowed in *all* files
    // across the sources.
    val minimumSureFireWindowSizeAcrossAllFilesOverAllSides =
      1 max (maximumFileSizeAcrossAllFilesOverAllSides * minimumSizeFractionForMotionDetection).floor.toInt

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

      private val withBaseSection: Section[Element] => Map[Path, SectionsSeen] =
        withSection(base, baseSectionsByPath)
      private val withLeftSection: Section[Element] => Map[Path, SectionsSeen] =
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
        val path = side.pathFor(section)
        sectionsByPath.updatedWith(
          path
        ) {
          case Some(sections) =>
            // If the section overlaps one or more already present, we may as
            // well condense them together.
            val overlaps =
              sections.filterOverlaps(section.closedOpenInterval).toSeq

            Some(if overlaps.nonEmpty then
              val minimumStartOffset =
                overlaps.map(_.startOffset).min min section.startOffset
              val onePastMaximumEndOffset = overlaps
                .map(_.onePastEndOffset)
                .max max section.onePastEndOffset

              val condensedSection = side.section(path)(
                minimumStartOffset,
                onePastMaximumEndOffset - minimumStartOffset
              )

              overlaps.foldLeft(sections)(_ - _) + condensedSection
            else sections + section)
          case None =>
            Some(
              // NOTE: don't use `Ordering[Int]` as while that is valid, it will
              // pull in a Cats `Order[Int]` which round-trips back to an
              // `Ordering`. That makes profiling difficult.
              RangedSeq(section)(_.closedOpenInterval, Ordering.Int)
            )
        }
      end withSection
    end SectionsSeenAcrossSides

    given potentialMatchKeyOrder: Order[PotentialMatchKey] =
      given Order[Element] = order

      // Use an explicit implementation as Cats evaluates tuple ordering
      // strictly on both parts of the tuple.
      (x: PotentialMatchKey, y: PotentialMatchKey) =>
        val firstRankComparison = Order.compare(x.fingerprint, y.fingerprint)

        if 0 == firstRankComparison then
          // NOTE: need the pesky type ascriptions because `Order` is
          // invariant on its type parameter.
          Order.compare(
            x.impliedContent.content: Seq[Element],
            y.impliedContent.content: Seq[Element]
          )
        else firstRankComparison
        end if
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

    val phenotypeCache: Cache[Chromosome, Phenotype] =
      Caffeine.newBuilder().build()
    val rollingHashFactoryCache: Cache[Int, RollingHash.Factory] =
      Caffeine.newBuilder().build()
    // TODO: review this one - a) it does not seem to add much of a
    // performance benefit because the previous fingerprinting cache cuts out
    // a much larger overhead and b) if we're going to use this we should
    // probably lambda-lift the cache population functions to conform to the
    // caching API's style.
    val fingerprintSectionsCache: Cache[Int, FingerprintSectionsAcrossSides] =
      Caffeine.newBuilder().build()

    object MatchCalculationState:
      lazy val empty = MatchCalculationState(
        sectionsSeenAcrossSides = SectionsSeenAcrossSides(),
        matchGroupsInDescendingOrderOfKeys = Seq.empty,
        looseExclusiveUpperBoundOnMaximumMatchSize =
          Some(1 + maximumFileSizeAcrossAllFilesOverAllSides)
      )
    end MatchCalculationState

    case class MatchCalculationState(
        sectionsSeenAcrossSides: SectionsSeenAcrossSides,
        matchGroupsInDescendingOrderOfKeys: MatchGroupsInDescendingOrderOfKeys,
        // If `None`, don't bother tracking whether a match was made at a given
        // window size.
        looseExclusiveUpperBoundOnMaximumMatchSize: Option[Int]
    ):
      @tailrec
      final def withAllMatchesOfAtLeastTheSureFireWindowSize(
          bestMatchSize: Int =
            minimumSureFireWindowSizeAcrossAllFilesOverAllSides - 1
      ): MatchCalculationState =
        // TODO: tidy up the messy treatment of
        // `looseExclusiveUpperBoundOnMaximumMatchSize`. Could we make
        // `MatchCalculationState` into a trait?
        require(looseExclusiveUpperBoundOnMaximumMatchSize.nonEmpty)

        if 1 + bestMatchSize < looseExclusiveUpperBoundOnMaximumMatchSize.get
        then
          val candidateWindowSize =
            (bestMatchSize + looseExclusiveUpperBoundOnMaximumMatchSize.get) / 2

          val stateAfterTryingCandidate @ MatchCalculationState(
            _,
            _,
            Some(possiblyContractedExclusiveUpperBound)
          ) = this.matchesForWindowSize(candidateWindowSize): @unchecked

          if possiblyContractedExclusiveUpperBound == candidateWindowSize then
            // Failed to improve the match size, try again with the contracted
            // upper bound.
            stateAfterTryingCandidate
              .withAllMatchesOfAtLeastTheSureFireWindowSize(
                bestMatchSize
              )
          else
            // We have an improvement, move the lower bound up.
            this
              .withAllMatchesOfAtLeastTheSureFireWindowSize(candidateWindowSize)
          end if
        else if minimumSureFireWindowSizeAcrossAllFilesOverAllSides == looseExclusiveUpperBoundOnMaximumMatchSize.get
        then this
        else
          (if minimumSureFireWindowSizeAcrossAllFilesOverAllSides <= bestMatchSize
           then
             this
               .matchesForWindowSize(bestMatchSize)
           else this)
            .copy(looseExclusiveUpperBoundOnMaximumMatchSize =
              Some(bestMatchSize)
            )
            .withAllMatchesOfAtLeastTheSureFireWindowSize()
        end if
      end withAllMatchesOfAtLeastTheSureFireWindowSize

      def matchesForWindowSize(
          windowSize: Int
      ): MatchCalculationState =
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
                (minimumSizeFractionForMotionDetection * fileSize).floor.toInt

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
        ) = fingerprintSectionsCache.get(
          windowSize,
          _ =>
            FingerprintSectionsAcrossSides(
              baseSectionsByFingerprint = fingerprintSections(base),
              leftSectionsByFingerprint = fingerprintSections(left),
              rightSectionsByFingerprint = fingerprintSections(right)
            )
        )

        val maximumNumberOfAmbiguousMatches =
          if minimumSureFireWindowSizeAcrossAllFilesOverAllSides > windowSize
          then
            // Assume that we are down amongst the small fry - we don't want
            // lots of trivial single and double-element ambiguous matches, so
            // don't collect them. We do want to track interesting matches where
            // some special element(s) have moved in one match, so leave that as
            // a possibility.
            1
          else
            // If we are at or above the sure-fire size, then we honour the
            // user's desire to see *all* matches at that threshold.
            Int.MaxValue

        @tailrec
        def matchingFingerprintsAcrossSides(
            baseFingerprints: Iterable[PotentialMatchKey],
            leftFingerprints: Iterable[PotentialMatchKey],
            rightFingerprints: Iterable[PotentialMatchKey],
            matches: Set[Match[Section[Element]]]
        ): MatchCalculationState =
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
                    // In contrast with the pairwise matches below, we have the
                    // subtlety of a *replacement* of an all-three match with a
                    // pairwise match to consider, so this condition is more
                    // complex than the others...
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
                ).take(maximumNumberOfAmbiguousMatches).toSet
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

                  suppressed = sectionsSeenAcrossSides.containsBaseSection(
                    baseSection
                  ) || sectionsSeenAcrossSides.containsLeftSection(
                    leftSection
                  )
                  if !suppressed
                yield Match.BaseAndLeft(
                  baseSection,
                  leftSection
                )).take(maximumNumberOfAmbiguousMatches).toSet
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

                  suppressed = sectionsSeenAcrossSides.containsBaseSection(
                    baseSection
                  ) || sectionsSeenAcrossSides.containsRightSection(
                    rightSection
                  )
                  if !suppressed
                yield Match.BaseAndRight(
                  baseSection,
                  rightSection
                )).take(maximumNumberOfAmbiguousMatches).toSet
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

                  suppressed = sectionsSeenAcrossSides.containsLeftSection(
                    leftSection
                  ) || sectionsSeenAcrossSides.containsRightSection(
                    rightSection
                  )
                  if !suppressed
                yield Match.LeftAndRight(
                  leftSection,
                  rightSection
                )).take(maximumNumberOfAmbiguousMatches).toSet
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

              if matches.isEmpty
              then
                MatchCalculationState(
                  sectionsSeenAcrossSides,
                  matchGroupsInDescendingOrderOfKeys,
                  looseExclusiveUpperBoundOnMaximumMatchSize =
                    looseExclusiveUpperBoundOnMaximumMatchSize.map(
                      _ min windowSize
                    )
                )
              else
                println(
                  s"Matches discovered at window size: $windowSize number: ${matches.size}"
                )

                // Add the triples first if we have any, then any pairs as we
                // are adding match groups in descending order of keys.
                val (tripleMatches, pairMatches) = matches.partition {
                  case _: Match.AllThree[Section[Element]] => true
                  case _                                   => false
                }

                val withDiscoveredMatchTriplesAndPairs =
                  matchGroupsInDescendingOrderOfKeys ++ Seq(
                    (windowSize, MatchGrade.Triple) -> tripleMatches,
                    (windowSize, MatchGrade.Pair)   -> pairMatches
                  ).filter { case entry @ (_, matches) => matches.nonEmpty }

                MatchCalculationState(
                  if 1 < windowSize then
                    sectionsSeenAcrossSides.withSectionsFrom(matches)
                  else
                    // If match is of size 1, then it sections won't overlap any
                    // others, nor will they subsume any others - so don't
                    // bother noting them.
                    sectionsSeenAcrossSides
                  ,
                  matchGroupsInDescendingOrderOfKeys =
                    withDiscoveredMatchTriplesAndPairs,
                  looseExclusiveUpperBoundOnMaximumMatchSize =
                    looseExclusiveUpperBoundOnMaximumMatchSize
                )
              end if
          end match
        end matchingFingerprintsAcrossSides

        matchingFingerprintsAcrossSides(
          baseSectionsByFingerprint.keySet,
          leftSectionsByFingerprint.keySet,
          rightSectionsByFingerprint.keySet,
          matches = Set.empty
        )
      end matchesForWindowSize
    end MatchCalculationState

    object Chromosome:
      private val descendingWindowSizeOrdering = Ordering[Int].reverse
      private val noWindowSizes = TreeSet.empty(descendingWindowSizeOrdering)

      def initial: Chromosome =
        withWindowSizes(
          minimumWindowSizeAcrossAllFilesOverAllSides
        )(
          minimumWindowSizeAcrossAllFilesOverAllSides until minimumSureFireWindowSizeAcrossAllFilesOverAllSides
        )
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
              grown
            case Choice.Contract =>
              contracted
            case Choice.Replace =>
              nudged
          end match
        else this
        end if
      end mutate

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
          bredWindowSizes = noWindowSizes,
          pickingState = Synchronized,
          mandatoryState = false
        )

        Chromosome(
          bredWindowSizes,
          validWindowSizes
        )
      end breedWith

      // NOTE: the following helper is a method because it has a precondition
      // that there are valid window sizes.
      private def oneBeforeLowestValidWindowSize = validWindowSizes.min - 1

      private def contracted(using random: Random) =
        val deletedWindowSize = random.chooseOneOf(windowSizesInDescendingOrder)

        Chromosome(
          windowSizesInDescendingOrder =
            windowSizesInDescendingOrder - deletedWindowSize,
          validWindowSizes = validWindowSizes
        )
      end contracted

      private def newWindowSizeInGapOrBeforeLowest(newWindowSizeIndex: Int) =
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
            _,
            lowerGapBoundary // NOTE: this will be `oneBeforeLowestValidWindowSize` if the new window size comes before the current minimum.
          )
        ) =
          onePastIndexOfEachLowestFreeWindowSizePerGap
            .zip(sizeGaps)
            .dropWhile { case (onePastIndexOfLowestFreeWindowSize, _) =>
              onePastIndexOfLowestFreeWindowSize <= newWindowSizeIndex
            }
            .head

        val newWindowSize =
          lowerGapBoundary + (onePastIndexOfLowestFreeWindowSize - newWindowSizeIndex)

        newWindowSize -> lowerGapBoundary
      end newWindowSizeInGapOrBeforeLowest

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
          val newWindowSizeIndex =
            random.chooseAnyNumberFromZeroToOneLessThan(
              numberOfFreeWindowSizesBelowTheCurrentMaximum
            )

          val (newWindowSize, outgoingWindowSize) =
            newWindowSizeInGapOrBeforeLowest(newWindowSizeIndex)
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

      private def grown(using random: Random) =
        val numberOfFreeWindowSizes =
          validWindowSizes.size - windowSizesInDescendingOrder.size

        val whereWillThisLand =
          random.chooseAnyNumberFromZeroToOneLessThan(numberOfFreeWindowSizes)

        val numberOfFreeWindowSizesAboveTheCurrentMaximum =
          windowSizesInDescendingOrder.headOption.fold(ifEmpty =
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

            newWindowSizeInGapOrBeforeLowest(newWindowSizeIndex)._1
          else
            // Go beyond the maximum window size...
            val baselineWindowSize = windowSizesInDescendingOrder.headOption
              .fold(ifEmpty = validWindowSizes.min)(1 + _)

            baselineWindowSize + random.chooseAnyNumberFromZeroToOneLessThan(
              1 + validWindowSizes.max - baselineWindowSize
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
    end Chromosome

    case class Phenotype(
        chromosomeSize: Int,
        matchGroupsInDescendingOrderOfKeys: MatchGroupsInDescendingOrderOfKeys
    ):
      require(matchGroupsInDescendingOrderOfKeys.forall {
        case (_, matchGroup) =>
          matchGroup.nonEmpty
      })

      // TODO: why do we need to build a map for each side? Surely sections from
      // different sides can't be equal?

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

    val withAllMatchesOfAtLeastTheSureFireWindowSize =
      // We will be exploring the low window sizes below
      // `minimumSureFireWindowSizeAcrossAllFilesOverAllSides`; in this
      // situation, sizes below per-file thresholds lead to no matches,
      // this leads to gaps in validity as a size exceeds the largest
      // match it could participate in, but fails to meet the next highest
      // per-file threshold. Disable tracking of the exclusive upper
      // bound.
      MatchCalculationState.empty
        .withAllMatchesOfAtLeastTheSureFireWindowSize()
        .copy(looseExclusiveUpperBoundOnMaximumMatchSize = None)

    given Evolution[Chromosome, Phenotype] with
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
        val MatchCalculationState(
          _,
          matchGroupsInDescendingOrderOfKeys,
          _
        ) =
          chromosome.windowSizesInDescendingOrder.foldLeft(
            withAllMatchesOfAtLeastTheSureFireWindowSize
          )(_ matchesForWindowSize _)

        Phenotype(
          chromosomeSize = chromosome.windowSizesInDescendingOrder.size,
          matchGroupsInDescendingOrderOfKeys
        )
      end phenotype_
    end given

    val evolvedPhenotype =
      if minimumSureFireWindowSizeAcrossAllFilesOverAllSides > minimumWindowSizeAcrossAllFilesOverAllSides
      then
        println(
          s"Genetic end-game, $minimumWindowSizeAcrossAllFilesOverAllSides, $minimumSureFireWindowSizeAcrossAllFilesOverAllSides"
        )
        Evolution.of(
          maximumNumberOfRetries = 3,
          maximumPopulationSize = 10
        )
      else
        println(
          s"No genetic end-game, $minimumWindowSizeAcrossAllFilesOverAllSides, $minimumSureFireWindowSizeAcrossAllFilesOverAllSides"
        )
        Phenotype(
          chromosomeSize = 0,
          matchGroupsInDescendingOrderOfKeys =
            withAllMatchesOfAtLeastTheSureFireWindowSize.matchGroupsInDescendingOrderOfKeys
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
