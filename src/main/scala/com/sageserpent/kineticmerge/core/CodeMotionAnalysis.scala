package com.sageserpent.kineticmerge.core

import alleycats.std.set.*
import cats.instances.seq.*
import cats.syntax.traverse.*
import cats.{Eq, Order}
import com.google.common.hash.{Funnel, HashFunction}
import com.sageserpent.kineticmerge.core.genetic.Evolution
import com.softwaremill.tagging.*
import de.sciss.fingertree.RangedSeq

import scala.collection.immutable.TreeSet
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
  ): Either[AmbiguousMatch.type, CodeMotionAnalysis[Path, Element]] =
    require(0 < minimumSizeFractionForMotionDetection)
    require(1 >= minimumSizeFractionForMotionDetection)

    val baseFilesByPath =
      base.filesByPathUtilising(Set.empty)

    val leftFilesByPath =
      left.filesByPathUtilising(Set.empty)

    val rightFilesByPath =
      right.filesByPathUtilising(Set.empty)

    val minimumFileSize = baseFilesByPath.map(_._2.size).min min leftFilesByPath
      .map(_._2.size)
      .min min rightFilesByPath.map(_._2.size).min

    val minimumWindowSize =
      (minimumFileSize * minimumSizeFractionForMotionDetection).ceil.toInt
        .taggedWith[Tags.WindowSize]

    object Tags:
      trait WindowSize
    end Tags

    enum MatchGrade:
      case Triple
      case Pair
    end MatchGrade

    type MatchGroupKey = (Int @@ Tags.WindowSize, MatchGrade)
    type Chromosome    = TreeSet[Int @@ Tags.WindowSize]
    type MatchGroupsInDescendingOrderOfKeys = Seq[
      (
          MatchGroupKey,
          Set[
            Match[Section[Element]]
          ]
      )
    ]

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

    case class Phenotype(
        chromosomeSize: Int,
        matchGroupsInReverseOrder: MatchGroupsInDescendingOrderOfKeys
    ):
      require(matchGroupsInReverseOrder.forall { case (_, matchGroup) =>
        !matchGroup.isEmpty
      })

    end Phenotype

    // Have to be prolix with the explicit generic type substitutions to avoid
    // confusing the Scala compiler.
    given Order[Int @@ Tags.WindowSize] =
      Order.by[Int @@ Tags.WindowSize, Int](_.eraseTag)

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

      override def initialChromosome: Chromosome = TreeSet(minimumWindowSize)

      override def phenotype(chromosome: Chromosome): Phenotype =
        // For each sources....
        // For each path....
        // For each window size compatible with the file size (>= minimum size
        // fraction for that file and <= file size)...
        // Build Rabin fingerprints....

        ???
    end given

    val mysteriousAnswer =
      Evolution.of(maximumNumberOfRetries = 100, maximumPopulationSize = 100)

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
