package com.sageserpent.kineticmerge.core

import alleycats.std.set.*
import cats.instances.seq.*
import cats.syntax.traverse.*
import cats.{Eq, Order}
import com.google.common.hash.{Funnel, HashFunction}
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
      base.filesByPath.values.map(_.size).min min
        left.filesByPath.values.map(_.size).min min
        right.filesByPath.values.map(_.size).min

    val minimumWindowSize =
      (minimumFileSize * minimumSizeFractionForMotionDetection).ceil.toInt

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

    // TODO: consider integrating the cleanup directly in to the matching
    // process, as we can work with sections directly, thus avoiding the
    // decomposition of matches.
    def cleanUp(
        matchGroupsInDescendingOrderOfKeys: MatchGroupsInDescendingOrderOfKeys
    ): MatchGroupsInDescendingOrderOfKeys =
      // 1. Remove matches that involve one or more sections that are subsumed
      // within larger sections belonging to other matches...

      val noSections: RangedSeq[Section[Element], Int] = RangedSeq.empty(
        section => section.startOffset -> section.onePastEndOffset,
        Ordering[Int]
      )

      val noSectionsAcrossAllSides = (noSections, noSections, noSections)

      val withoutRedundantMatches: MatchGroupsInDescendingOrderOfKeys =
        matchGroupsInDescendingOrderOfKeys
          .mapAccumulate(noSectionsAcrossAllSides) {
            case (sectionsAcrossAllSidesForKey, (matchGroupKey, matches)) =>
              val (updatedSectionsAcrossAllSides, cleanedMatchesWithHoles) =
                matches.mapAccumulate(sectionsAcrossAllSidesForKey) {
                  case (
                        sectionsAcrossAllSidesForMatch @ (
                          baseSections,
                          leftSections,
                          rightSections
                        ),
                        aMatch
                      ) =>
                    aMatch match
                      case aMatch @ Match.AllThree(
                            baseSection,
                            leftSection,
                            rightSection
                          ) =>
                        if baseSections.includes(
                            baseSection.closedOpenInterval
                          ) || leftSections
                            .includes(
                              leftSection.closedOpenInterval
                            ) || rightSections.includes(
                            rightSection.closedOpenInterval
                          )
                        then sectionsAcrossAllSidesForMatch -> None
                        else
                          (
                            baseSections + baseSection,
                            leftSections + leftSection,
                            rightSections + rightSection
                          ) -> Some(aMatch)
                        end if

                      case aMatch @ Match.BaseAndLeft(
                            baseSection,
                            leftSection
                          ) =>
                        if baseSections.includes(
                            baseSection.closedOpenInterval
                          ) || leftSections
                            .includes(
                              leftSection.closedOpenInterval
                            )
                        then sectionsAcrossAllSidesForMatch -> None
                        else
                          (
                            baseSections + baseSection,
                            leftSections + leftSection,
                            rightSections
                          ) -> Some(aMatch)
                        end if

                      case aMatch @ Match.BaseAndRight(
                            baseSection,
                            rightSection
                          ) =>
                        if baseSections.includes(
                            baseSection.closedOpenInterval
                          ) || rightSections.includes(
                            rightSection.closedOpenInterval
                          )
                        then sectionsAcrossAllSidesForMatch -> None
                        else
                          (
                            baseSections + baseSection,
                            leftSections,
                            rightSections + rightSection
                          ) -> Some(aMatch)
                        end if

                      case aMatch @ Match.LeftAndRight(
                            leftSection,
                            rightSection
                          ) =>
                        if leftSections
                            .includes(
                              leftSection.closedOpenInterval
                            ) || rightSections.includes(
                            rightSection.closedOpenInterval
                          )
                        then sectionsAcrossAllSidesForMatch -> None
                        else
                          (
                            baseSections,
                            leftSections + leftSection,
                            rightSections + rightSection
                          ) -> Some(aMatch)
                        end if
                    end match
                }

              updatedSectionsAcrossAllSides -> (matchGroupKey, cleanedMatchesWithHoles.flatten)
          }
          ._2

      // 2. Remove zero size match groups.
      withoutRedundantMatches.filter { case (_, matches) => matches.isEmpty }
    end cleanUp

    case class Chromosome(
        windowSizesInDescendingOrder: WindowSizesInDescendingOrder
    )

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
      ): Chromosome = ???

      override def breed(first: Chromosome, second: Chromosome)(using
          random: Random
      ): Chromosome = ???

      override def initialChromosome: Chromosome = Chromosome(
        TreeSet(minimumWindowSize)(Ordering[Int].reverse)
      )

      override def phenotype(chromosome: Chromosome): Phenotype =
        // TODO: caching!
        def matchesForWindowSize(
            matchGroupsInDescendingOrderOfKeys: MatchGroupsInDescendingOrderOfKeys,
            windowSize: Int
        ): MatchGroupsInDescendingOrderOfKeys =
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
            // Fingerprinting is imperative, so go with that style local to this
            // helper function...
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
              // This isn't quite the same as flat-mapping / flattening, because
              // we want the type of the result to be a `SortedMultiDict`...
              .reduce(_ concat _)
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
              pairMatches: Set[Match[Section[Element]]]
          ): MatchGroupsInDescendingOrderOfKeys =
            // TODO: this termination condition is too loose - we may still be
            // able to pair-wise match when only one side has run out of
            // fingerprints. Consider pattern matching...
            if baseFingerprints.isEmpty || leftFingerprints.isEmpty || rightFingerprints.isEmpty
            then
              // Add the triples first if we have any, then any pairs as we are
              // adding match groups in descending order of keys.
              matchGroupsInDescendingOrderOfKeys ++ Seq(
                (windowSize, MatchGrade.Triple) -> tripleMatches,
                (windowSize, MatchGrade.Pair)   -> pairMatches
              ).filter { case (_, matches) => matches.nonEmpty }
            else
              val baseHead  = baseFingerprints.head
              val leftHead  = leftFingerprints.head
              val rightHead = rightFingerprints.head

              val maximumFingerprint = baseHead max leftHead max rightHead
              val minimumFingerprint = baseHead min leftHead min rightHead

              if maximumFingerprint == minimumFingerprint then
                // Synchronised the fingerprints across all three sides - so all
                // match...

                val allSidesMatches: Set[Match[Section[Element]]] =
                  val baseSections =
                    baseSectionsByFingerprint.get(maximumFingerprint)
                  val leftSections =
                    leftSectionsByFingerprint.get(maximumFingerprint)
                  val rightSections =
                    rightSectionsByFingerprint.get(maximumFingerprint)

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
                  pairMatches
                )
              else
                // If we have two sides sharing the *minimum* fingerprint, this
                // means that there is no further chance of synchronising on
                // that fingerprint across all three sides, so we declare a
                // match on two sides...

                // ...if all three sides have differing fingerprints, advance on
                // the side with the minimum fingerprint, this is conservative
                // in terms of finding all-sides matches, but makes sure we
                // don't miss two-sided matches.

                matchingFingerprintsAcrossSides(???, ???, ???, ???, ???)
              end if
            end if
          end matchingFingerprintsAcrossSides

          matchingFingerprintsAcrossSides(
            baseSectionsByFingerprint.keySet,
            leftSectionsByFingerprint.keySet,
            rightSectionsByFingerprint.keySet,
            tripleMatches = Set.empty,
            pairMatches = Set.empty
          )
        end matchesForWindowSize

        val matchGroupsInDescendingOrderOfKeys
            : MatchGroupsInDescendingOrderOfKeys =
          chromosome.windowSizesInDescendingOrder.foldLeft(Seq.empty)(
            matchesForWindowSize
          )

        Phenotype(
          chromosomeSize = chromosome.windowSizesInDescendingOrder.size,
          matchGroupsInDescendingOrderOfKeys
        )
      end phenotype
    end given

    val evolvedPhenotype =
      Evolution.of(maximumNumberOfRetries = 100, maximumPopulationSize = 100)

    val matchesByTheirSections = evolvedPhenotype.matchesByTheirSections

    val baseFilesByPath =
      base.filesByPathUtilising(evolvedPhenotype.baseSections)
    val leftFilesByPath =
      left.filesByPathUtilising(evolvedPhenotype.leftSections)
    val rightFilesByPath =
      right.filesByPathUtilising(evolvedPhenotype.rightSections)

    Right(
      new CodeMotionAnalysis[Path, Element]:
        override def matchFor(
            section: Section[Element]
        ): Option[Match[Section[Element]]] = matchesByTheirSections.get(section)

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
