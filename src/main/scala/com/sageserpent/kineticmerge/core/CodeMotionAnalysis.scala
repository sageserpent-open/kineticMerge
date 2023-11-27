package com.sageserpent.kineticmerge.core

import cats.instances.seq.*
import cats.{Eq, Order}
import com.google.common.hash.{Funnel, HashFunction}
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

    val minimumFileSize =
      base.filesByPath.values.map(_.size).minOption.getOrElse(0) min
        left.filesByPath.values.map(_.size).minOption.getOrElse(0) min
        right.filesByPath.values.map(_.size).minOption.getOrElse(0)

    val minimumWindowSize =
      1 max (minimumFileSize * minimumSizeFractionForMotionDetection).ceil.toInt

    val maximumWindowSize =
      base.filesByPath.values.map(_.size).maxOption.getOrElse(0) max
        left.filesByPath.values.map(_.size).maxOption.getOrElse(0) max
        right.filesByPath.values.map(_.size).maxOption.getOrElse(0)

    var exclusiveUpperBoundOnWindowSize: Int = 1 + maximumWindowSize

    def validWindowSizes =
      minimumWindowSize until exclusiveUpperBoundOnWindowSize

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

    case class Chromosome(
        windowSizesInDescendingOrder: WindowSizesInDescendingOrder
    ):
      require(windowSizesInDescendingOrder.nonEmpty)

      windowSizesInDescendingOrder.foreach { size =>
        // NOTE: there is a subtlety: ideally we would like the invariant to be
        // that all sizes are contained in `validWindowSizes`, but the latter
        // changes dynamically as evolution proceeds, so we would end up with
        // chromosomes being constructed with the invariant holding, but being
        // rendered invalid later! We compromise and tolerate chromosomes with
        // invalid window sizes; they will be weeded out as evolution proceeds,
        // as they carry an additional burden of useless sizes that don't match.
        require((minimumWindowSize to maximumWindowSize) contains size)
      }
    end Chromosome

    case class Phenotype(
        chromosomeSize: Int,
        matchGroupsInReverseOrder: MatchGroupsInDescendingOrderOfKeys
    ):
      require(matchGroupsInReverseOrder.forall { case (_, matchGroup) =>
        matchGroup.nonEmpty
      })

      def matchesByTheirSections
          : Map[Section[Element], Match[Section[Element]]] =
        matchGroupsInReverseOrder.flatMap { case (_, matches) =>
          matches.flatMap(aMatch =>
            aMatch match
              case Match.AllThree(baseSection, leftSection, rightSection) =>
                Seq(
                  baseSection  -> aMatch,
                  leftSection  -> aMatch,
                  rightSection -> aMatch
                )
              case Match.BaseAndLeft(baseSection, leftSection) =>
                Seq(
                  baseSection -> aMatch,
                  leftSection -> aMatch
                )
              case Match.BaseAndRight(baseSection, rightSection) =>
                Seq(
                  baseSection  -> aMatch,
                  rightSection -> aMatch
                )
              case Match.LeftAndRight(leftSection, rightSection) =>
                Seq(
                  leftSection  -> aMatch,
                  rightSection -> aMatch
                )
          )
        }.toMap

      def baseSections: Set[Section[Element]] = matchGroupsInReverseOrder
        .map { case (_, matches) =>
          matches.collect {
            case Match.AllThree(baseSection, _, _) =>
              baseSection
            case Match.BaseAndLeft(baseSection, _) =>
              baseSection
            case Match.BaseAndRight(baseSection, _) =>
              baseSection
          }
        }
        .reduce(_ ++ _)

      def leftSections: Set[Section[Element]] = matchGroupsInReverseOrder
        .map { case (_, matches) =>
          matches.collect {
            case Match.AllThree(_, leftSection, _) =>
              leftSection
            case Match.BaseAndLeft(_, leftSection) =>
              leftSection
            case Match.LeftAndRight(leftSection, _) =>
              leftSection
          }
        }
        .reduce(_ ++ _)

      def rightSections: Set[Section[Element]] = matchGroupsInReverseOrder
        .map { case (_, matches) =>
          matches.collect {
            case Match.AllThree(_, _, rightSection) =>
              rightSection
            case Match.BaseAndRight(_, rightSection) =>
              rightSection
            case Match.LeftAndRight(_, rightSection) =>
              rightSection
          }
        }
        .reduce(_ ++ _)

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
      override def mutate(chromosome: Chromosome)(using
          random: Random
      ): Chromosome =
        val validWindowSizesSnapshot = validWindowSizes

        // May as well prune any window sizes from the original chromosome that
        // have become invalid.
        val weededWindowSizesInDescendingOrder =
          chromosome.windowSizesInDescendingOrder filter validWindowSizesSnapshot.contains

        val contractionIsPossible =
          1 < weededWindowSizesInDescendingOrder.size

        val expansionIsPossible =
          validWindowSizesSnapshot.size > weededWindowSizesInDescendingOrder.size

        if expansionIsPossible && (!contractionIsPossible || random
            .nextBoolean())
        then
          Chromosome {
            val additionalSize =
              random.chooseOneOf(
                validWindowSizesSnapshot.filterNot(
                  weededWindowSizesInDescendingOrder.contains
                )
              )

            weededWindowSizesInDescendingOrder + additionalSize
          }
        else
          Chromosome {
            val victim = random.chooseOneOf(weededWindowSizesInDescendingOrder)

            val contractedSizes = weededWindowSizesInDescendingOrder - victim

            if contractedSizes.isEmpty then
              // Add the new size on to the empty set to pick up the existing
              // reverse ordering.
              contractedSizes + random
                .chooseOneOf(
                  validWindowSizesSnapshot.filterNot(
                    weededWindowSizesInDescendingOrder.contains
                  )
                )
            else contractedSizes
            end if
          }
        end if
      end mutate

      override def breed(first: Chromosome, second: Chromosome)(using
          random: Random
      ): Chromosome =
        // TODO: actually mix up some genetic material!
        if random.nextBoolean() then first else second

      override def initialChromosome: Chromosome = Chromosome(
        TreeSet(minimumWindowSize)(Ordering[Int].reverse)
      )

      override def phenotype(chromosome: Chromosome): Phenotype =
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

        // TODO: caching!
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

            val fixedNumberOfBytesInElementHash =
              hashFunction.bits / JavaByte.SIZE

            val fingerprinting =
              new RabinFingerprintLongWindowed(
                polynomial,
                fixedNumberOfBytesInElementHash * windowSize
              )

            def fingerprintStartIndices(
                elements: IndexedSeq[Element]
            ): SortedMultiDict[Long, Int] =
              require(elements.size >= windowSize)

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
              // TODO: more caching!
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

            val baseSectionsByFingerprint  = fingerprintSections(base)
            val leftSectionsByFingerprint  = fingerprintSections(left)
            val rightSectionsByFingerprint = fingerprintSections(right)

            @tailrec
            def matchingFingerprintsAcrossSides(
                baseFingerprints: Iterable[Long],
                leftFingerprints: Iterable[Long],
                rightFingerprints: Iterable[Long],
                // TODO: tighten the type signature to use `Match.AllThree`
                tripleMatches: Set[Match[Section[Element]]],
                // TODO: tighten the type signature to use a union type of the
                // other match kinds.
                pairMatches: Set[Match[Section[Element]]],
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
                  val allSidesMatches: Set[Match[Section[Element]]] =
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
                      suppressed = sectionsSeenAcrossSides.containsBaseSection(
                        baseSection
                      ) || sectionsSeenAcrossSides.containsLeftSection(
                        leftSection
                      ) || sectionsSeenAcrossSides.containsRightSection(
                        rightSection
                      )
                      if !suppressed
                    yield Match.AllThree(
                      baseSection,
                      leftSection,
                      rightSection
                    )).toSet
                  end allSidesMatches

                  matchingFingerprintsAcrossSides(
                    baseFingerprints.tail,
                    leftFingerprints.tail,
                    rightFingerprints.tail,
                    tripleMatches ++ allSidesMatches,
                    pairMatches,
                    sectionsSeenAcrossSides
                      .withSectionsFrom(allSidesMatches)
                  )

                case (Some(baseHead), Some(leftHead), _)
                    if baseHead == leftHead =>
                  // Synchronised the fingerprints between the base and left...
                  val baseLeftMatches: Set[Match[Section[Element]]] =
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
                  end baseLeftMatches

                  matchingFingerprintsAcrossSides(
                    baseFingerprints.tail,
                    leftFingerprints.tail,
                    rightFingerprints,
                    tripleMatches,
                    pairMatches ++ baseLeftMatches,
                    sectionsSeenAcrossSides
                      .withSectionsFrom(baseLeftMatches)
                  )

                case (Some(baseHead), _, Some(rightHead))
                    if baseHead == rightHead =>
                  // Synchronised the fingerprints between the base and right...
                  val baseRightMatches: Set[Match[Section[Element]]] =
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
                  end baseRightMatches

                  matchingFingerprintsAcrossSides(
                    baseFingerprints.tail,
                    leftFingerprints,
                    rightFingerprints.tail,
                    tripleMatches,
                    pairMatches ++ baseRightMatches,
                    sectionsSeenAcrossSides
                      .withSectionsFrom(baseRightMatches)
                  )

                case (_, Some(leftHead), Some(rightHead))
                    if leftHead == rightHead =>
                  // Synchronised the fingerprints between the left and right...
                  val leftRightMatches: Set[Match[Section[Element]]] =
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
                  end leftRightMatches

                  matchingFingerprintsAcrossSides(
                    baseFingerprints,
                    leftFingerprints.tail,
                    rightFingerprints.tail,
                    tripleMatches,
                    pairMatches ++ leftRightMatches,
                    sectionsSeenAcrossSides
                      .withSectionsFrom(leftRightMatches)
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
                      tripleMatches,
                      pairMatches,
                      sectionsSeenAcrossSides
                    )
                  else if rightHead == minimumFingerprint then
                    matchingFingerprintsAcrossSides(
                      baseFingerprints,
                      leftFingerprints,
                      rightFingerprints.tail,
                      tripleMatches,
                      pairMatches,
                      sectionsSeenAcrossSides
                    )
                  else
                    matchingFingerprintsAcrossSides(
                      baseFingerprints.tail,
                      leftFingerprints,
                      rightFingerprints,
                      tripleMatches,
                      pairMatches,
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
                      tripleMatches,
                      pairMatches,
                      sectionsSeenAcrossSides
                    )
                  else
                    matchingFingerprintsAcrossSides(
                      baseFingerprints,
                      leftFingerprints.tail,
                      rightFingerprints,
                      tripleMatches,
                      pairMatches,
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
                      tripleMatches,
                      pairMatches,
                      sectionsSeenAcrossSides
                    )
                  else
                    matchingFingerprintsAcrossSides(
                      baseFingerprints,
                      leftFingerprints,
                      rightFingerprints.tail,
                      tripleMatches,
                      pairMatches,
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
                      tripleMatches,
                      pairMatches,
                      sectionsSeenAcrossSides
                    )
                  else
                    matchingFingerprintsAcrossSides(
                      baseFingerprints,
                      leftFingerprints,
                      rightFingerprints.tail,
                      tripleMatches,
                      pairMatches,
                      sectionsSeenAcrossSides
                    )

                case _ =>
                  // There are no more opportunities to match a full triple or
                  // just a pair, so this terminates the recursion. Add the
                  // triples first if we have any, then any pairs as we are
                  // adding match groups in descending order of keys.

                  if tripleMatches.isEmpty && pairMatches.isEmpty then
                    // There is no point looking for matches with a larger
                    // window size if we could not find any at this size.
                    assume(windowSize < exclusiveUpperBoundOnWindowSize)
                    exclusiveUpperBoundOnWindowSize = windowSize

                    sectionsSeenAcrossSides -> matchGroupsInDescendingOrderOfKeys
                  else
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
              tripleMatches = Set.empty,
              pairMatches = Set.empty,
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
          s"Chromosome: $chromosome, matchGroupsInDescendingOrderOfKeys: $matchGroupsInDescendingOrderOfKeys"
        )

        Phenotype(
          chromosomeSize = chromosome.windowSizesInDescendingOrder.size,
          matchGroupsInDescendingOrderOfKeys
        )
      end phenotype
    end given

    val baseFilesByPath =
      base.filesByPath
    val leftFilesByPath =
      left.filesByPath
    val rightFilesByPath =
      right.filesByPath

    Right(
      new CodeMotionAnalysis[Path, Element]:
        override def matchFor(
            section: Section[Element]
        ): Option[Match[Section[Element]]] = None

        override def base: Map[Path, File[Element]] = baseFilesByPath

        override def left: Map[Path, File[Element]] = leftFilesByPath

        override def right: Map[Path, File[Element]] = rightFilesByPath
    )
  end of

  // TODO - what happened?
  case object AmbiguousMatch
end CodeMotionAnalysis

trait CodeMotionAnalysis[Path, Element]:
  def base: Map[Path, File[Element]]
  def left: Map[Path, File[Element]]
  def right: Map[Path, File[Element]]

  def matchFor(section: Section[Element]): Option[Match[Section[Element]]]
end CodeMotionAnalysis
