package com.sageserpent.kineticmerge.core

import cats.kernel.{Eq, Order}
import com.google.common.hash.{Hashing, PrimitiveSink}
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.java.CasesLimitStrategy
import com.sageserpent.americium.junit5.*
import com.sageserpent.americium.{Trials, TrialsApi}
import com.sageserpent.kineticmerge.NoProgressRecording
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisTest.*
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import org.junit.jupiter.api.{Order as _, *}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

class CodeMotionAnalysisTest:

  @TestFactory
  def sourcesCanBeReconstructedFromTheAnalysis: DynamicTests =
    extension (results: Map[Path, File[Element]])
      private infix def matches(sources: FakeSources): Unit =
        assert(results.keys == sources.filesByPathUtilising(Set.empty).keys)

        results.foreach { case (path, result) =>
          assert(
            result.content == sources
              .filesByPathUtilising(Set.empty)(path)
              .content
          )
        }
      end matches
    end extension

    val minimumSizeFractionTrials: Trials[Double] =
      trialsApi.doubles(0.1, 1)

    val contentTrials: Trials[Vector[Element]] = trialsApi
      .integers(0, 100)
      .flatMap(textSize =>
        trialsApi
          .integers(lowerBound = 1, upperBound = 20)
          .lotsOfSize[Vector[Path]](textSize)
      )

    val pathTrials: Trials[Path] = trialsApi
      .integers(1, 1000)

    val sourcesTrials: Trials[FakeSources] =
      pathTrials
        .maps(contentTrials)
        .filter(_.nonEmpty)
        .map(
          new FakeSources(_, "unlabelled")
            with SourcesContracts[
              Path,
              Element
            ]
        )

    (sourcesTrials.map(
      _.copy(label = "base")
    ) and sourcesTrials.map(
      _.copy(label = "left")
    ) and sourcesTrials.map(
      _.copy(label = "right")
    ) and minimumSizeFractionTrials)
      .withStrategy(_ =>
        CasesLimitStrategy.timed(Duration.apply(1, TimeUnit.MINUTES))
      )
      .dynamicTests(
        (
            base: FakeSources,
            left: FakeSources,
            right: FakeSources,
            minimumSizeFraction: Double
        ) =>
          pprintCustomised.pprintln((base, left, right, minimumSizeFraction))

          try
            val Right(
              analysis: CodeMotionAnalysis[
                Path,
                Element
              ]
            ) =
              CodeMotionAnalysis.of(base, left, right)(
                minimumMatchSize = 2,
                thresholdSizeFractionForMatching = minimumSizeFraction,
                minimumAmbiguousMatchSize = 0
              )(
                elementEquality = Eq[Element],
                elementOrder = Order[Element],
                elementFunnel = elementFunnel,
                hashFunction = Hashing.murmur3_32_fixed()
              )(progressRecording = NoProgressRecording): @unchecked

            analysis.base matches base
            analysis.left matches left
            analysis.right matches right
          catch
            case overlappingSections: FakeSources#OverlappingSections =>
              pprintCustomised.pprintln(overlappingSections)
              Trials.reject()
          end try
      )
  end sourcesCanBeReconstructedFromTheAnalysis

  @TestFactory
  def matchingSectionsAreFound(): DynamicTests =
    case class TestPlan(
        minimumPossibleExpectedMatchSize: Int,
        commonToAllThreeSides: IndexedSeq[Vector[Element]],
        commonToBaseAndLeft: IndexedSeq[Vector[Element]],
        commonToBaseAndRight: IndexedSeq[Vector[Element]],
        commonToLeftAndRight: IndexedSeq[Vector[Element]],
        uniqueToBase: IndexedSeq[Vector[Element]],
        uniqueToLeft: IndexedSeq[Vector[Element]],
        uniqueToRight: IndexedSeq[Vector[Element]],
        baseSources: FakeSources,
        leftSources: FakeSources,
        rightSources: FakeSources,
        adjacentCommonSequencesArePossibleOnBase: Boolean,
        adjacentCommonSequencesArePossibleOnLeft: Boolean,
        adjacentCommonSequencesArePossibleOnRight: Boolean
    ):
      commonToAllThreeSides.foreach { sectionContent =>
        require(baseSources.contains(sectionContent))
        require(leftSources.contains(sectionContent))
        require(rightSources.contains(sectionContent))
      }

      commonToBaseAndLeft.foreach { sectionContent =>
        require(baseSources.contains(sectionContent))
        require(leftSources.contains(sectionContent))
      }

      commonToBaseAndRight.foreach { sectionContent =>
        require(baseSources.contains(sectionContent))
        require(rightSources.contains(sectionContent))
      }

      commonToLeftAndRight.foreach { sectionContent =>
        require(leftSources.contains(sectionContent))
        require(rightSources.contains(sectionContent))
      }

      uniqueToBase.foreach { sectionContent =>
        require(baseSources.contains(sectionContent))
      }
      uniqueToLeft.foreach { sectionContent =>
        require(leftSources.contains(sectionContent))
      }
      uniqueToRight.foreach { sectionContent =>
        require(rightSources.contains(sectionContent))
      }

      def minimumSizeFractionForMotionDetection: Double =
        def minimumNumberOfElementsCoveredBy(
            commonSequenceGroups: IndexedSeq[Vector[Element]]*
        ): Int =
          commonSequenceGroups
            .collect(Function.unlift(_.map(_.size).minOption))
            .minOption
            .getOrElse(0)

        val minimumMatchableSize = minimumNumberOfElementsCoveredBy(
          commonToAllThreeSides,
          commonToBaseAndLeft,
          commonToBaseAndRight,
          commonToLeftAndRight
        )

        minimumMatchableSize.toDouble / (baseSources.maximumContentsSize max leftSources.maximumContentsSize max rightSources.maximumContentsSize)
      end minimumSizeFractionForMotionDetection

    end TestPlan

    val maximumGlyphThatMightBeShared = 8

    val alphabet = 1 to maximumGlyphThatMightBeShared

    val testPlans =
      trialsApi.integers(4, 6).flatMap { minimumPossibleExpectedMatchSize =>
        // Test plan synthesis: start with a set of sequences, so these cannot
        // match each other ...
        val sequences =
          trialsApi
            .integers(minimumPossibleExpectedMatchSize, 200)
            .flatMap(sequenceLength =>
              trialsApi
                .choose(alphabet)
                .lotsOfSize[Vector[Element]](sequenceLength)
            )

        val setsOfSequences = trialsApi
          .integers(4, 30)
          .flatMap(numberOfSequences =>
            sequences
              .lotsOfSize[Array[Vector[Element]]](numberOfSequences)
              .map(
                _.zipWithIndex
                  .map { case (core, index) =>
                    // Each sequence has a unique first and last element amongst
                    // the set of sequences; this prevents accidental overrun of
                    // the expected matches.
                    core
                      .prepended(
                        1 + (2 * index) + maximumGlyphThatMightBeShared
                      )
                      .appended(
                        (2 * (1 + index)) + maximumGlyphThatMightBeShared
                      )
                  }
                  .toSet
              )
          )

        setsOfSequences.flatMap(sequences =>
          trialsApi
            // ... split into seven sets ...
            .partitioning(sequences.toIndexedSeq, numberOfPartitions = 7)
            .flatMap {
              case Seq(
                    // ... the first four sets are of sequences that are
                    // expected to be matched across three or just two sides ...
                    commonToAllThreeSides,
                    commonToBaseAndLeft,
                    commonToBaseAndRight,
                    commonToLeftAndRight,
                    // ... and the last three are specific to the three sides,
                    // so they are unmatchable.
                    uniqueToBase,
                    uniqueToLeft,
                    uniqueToRight
                  ) =>
                val common =
                  commonToAllThreeSides ++ commonToBaseAndLeft ++ commonToBaseAndRight ++ commonToLeftAndRight

                // Sanity check: it is possible to have accidental matches where
                // a common sequence is *embedded* within a unique sequence...
                if uniqueToBase.exists(unique =>
                    (commonToAllThreeSides ++ commonToLeftAndRight)
                      .exists(unique.containsSlice)
                  )
                then trialsApi.impossible
                else if uniqueToLeft.exists(unique =>
                    (commonToAllThreeSides ++ commonToBaseAndRight)
                      .exists(unique.containsSlice)
                  )
                then trialsApi.impossible
                else if uniqueToRight.exists(unique =>
                    (commonToAllThreeSides ++ commonToBaseAndLeft)
                      .exists(unique.containsSlice)
                  )
                then trialsApi.impossible
                // ... it is also possible to have accidental matches where a
                // common sequence for one match is *embedded* within a common
                // sequence for another match.
                else if common.exists(commonSequence =>
                    common.exists(anotherCommonSequence =>
                      anotherCommonSequence != commonSequence && commonSequence
                        .containsSlice(anotherCommonSequence)
                    )
                  )
                then trialsApi.impossible
                else
                  def sourcesForASide(label: String)(
                      commonToAllThreeSides: IndexedSeq[
                        Vector[Element]
                      ],
                      commonToOnePairOfSides: IndexedSeq[
                        Vector[Element]
                      ],
                      commonToTheOtherPairOfSides: IndexedSeq[
                        Vector[Element]
                      ],
                      sideUniqueSequences: IndexedSeq[Vector[Element]]
                  ): Trials[(FakeSources, Boolean)] =
                    val sequenceMixtures: Trials[
                      (IndexedSeq[Vector[Element]], Boolean)
                    ] =
                      if commonToAllThreeSides.nonEmpty || commonToOnePairOfSides.nonEmpty || commonToTheOtherPairOfSides.nonEmpty
                      then
                        // Mix up the all-sides and pairwise matches....
                        val sideCommonSequencesRearrangements =
                          trialsApi.pickAlternatelyFrom(
                            shrinkToRoundRobin = false,
                            commonToAllThreeSides,
                            commonToOnePairOfSides,
                            commonToTheOtherPairOfSides
                          )

                        if sideUniqueSequences.nonEmpty then
                          // ...intersperse unique sequences between chunks of
                          // common sequences; a unique sequence never abuts
                          // another unique sequence...
                          sideCommonSequencesRearrangements
                            .flatMap(sideCommonSequencesRearrangement =>
                              if sideCommonSequencesRearrangement.size >= sideUniqueSequences.size
                              then
                                val adjacentCommonSequencesArePossible =
                                  // Pigeonhole principle: we have too many
                                  // common sequences, so some must share the
                                  // same slot that will be paired with a unique
                                  // sequence.
                                  sideCommonSequencesRearrangement.size > sideUniqueSequences.size
                                trialsApi
                                  .nonEmptyPartitioning(
                                    sideCommonSequencesRearrangement,
                                    numberOfPartitions =
                                      sideUniqueSequences.size
                                  )
                                  .map(commonSequencesInChunks =>
                                    commonSequencesInChunks
                                      .zip(sideUniqueSequences)
                                      .flatMap {
                                        case (
                                              chunkOfCommonSequences,
                                              uniqueSequence
                                            ) =>
                                          chunkOfCommonSequences :+ uniqueSequence
                                      }
                                      .toIndexedSeq -> adjacentCommonSequencesArePossible
                                  )
                              else trialsApi.impossible
                              end if
                            )
                        else
                          // We don't have any unique sequences to put between
                          // the common sequences, so the latter may be adjacent
                          // if there's more than one of them.
                          sideCommonSequencesRearrangements.map(
                            commonSequences =>
                              commonSequences -> (1 < commonSequences.size)
                          )
                        end if
                      else
                        // We don't have any common sequences at all, so none
                        // are adjacent.
                        trialsApi.only(sideUniqueSequences -> false)

                    // ... finally, allocate the sequences in groups to paths.

                    sequenceMixtures
                      .flatMap {
                        case (
                              sequenceMixture,
                              adjacentCommonSequencesArePossible
                            ) =>
                          val pathContentAllocations =
                            if sequenceMixture.nonEmpty then
                              trialsApi
                                .integers(1, sequenceMixture.size)
                                .flatMap(numberOfPaths =>
                                  trialsApi.nonEmptyPartitioning(
                                    sequenceMixture,
                                    numberOfPartitions = numberOfPaths
                                  )
                                )
                            else
                              // Sometimes we generate an empty side with no
                              // files.
                              trialsApi.only(Seq.empty)

                          pathContentAllocations
                            .map(_.zipWithIndex)
                            .map(_.map { case (pathContentAllocation, path) =>
                              val fileContents = pathContentAllocation.flatten
                              path -> fileContents
                            }.toMap)
                            .map(contentsByPath =>
                              new FakeSources(contentsByPath, label)
                                with SourcesContracts[
                                  Path,
                                  Element
                                ] -> adjacentCommonSequencesArePossible
                            )
                      }
                  end sourcesForASide

                  for
                    (baseSources, adjacentCommonSequencesArePossibleOnBase) <-
                      sourcesForASide(label = "base")(
                        commonToAllThreeSides,
                        commonToBaseAndLeft,
                        commonToBaseAndRight,
                        uniqueToBase
                      )
                    (leftSources, adjacentCommonSequencesArePossibleOnLeft) <-
                      sourcesForASide(label = "left")(
                        commonToAllThreeSides,
                        commonToBaseAndLeft,
                        commonToLeftAndRight,
                        uniqueToLeft
                      )
                    (rightSources, adjacentCommonSequencesArePossibleOnRight) <-
                      sourcesForASide(label = "right")(
                        commonToAllThreeSides,
                        commonToBaseAndRight,
                        commonToLeftAndRight,
                        uniqueToRight
                      )
                  yield TestPlan(
                    minimumPossibleExpectedMatchSize,
                    commonToAllThreeSides,
                    commonToBaseAndLeft,
                    commonToBaseAndRight,
                    commonToLeftAndRight,
                    uniqueToBase,
                    uniqueToLeft,
                    uniqueToRight,
                    baseSources,
                    leftSources,
                    rightSources,
                    adjacentCommonSequencesArePossibleOnBase,
                    adjacentCommonSequencesArePossibleOnLeft,
                    adjacentCommonSequencesArePossibleOnRight
                  )
                  end for
                end if
            }
        )
      }

    testPlans
      .withStrategy(caseSupplyCycle =>
        if caseSupplyCycle.isInitial then
          CasesLimitStrategy.timed(Duration.apply(5, TimeUnit.MINUTES))
        else CasesLimitStrategy.counted(200, 3.0)
      )
      .dynamicTests { testPlan =>

        import testPlan.*

        println(
          s"Minimum size fraction for motion detection: $minimumSizeFractionForMotionDetection"
        )
        println("Sizes of common to all three sides...")
        pprintCustomised.pprintln(commonToAllThreeSides.map(_.size))
        println("Sizes of common to base and left...")
        pprintCustomised.pprintln(commonToBaseAndLeft.map(_.size))
        println("Sizes of common to base and right...")
        pprintCustomised.pprintln(commonToBaseAndRight.map(_.size))
        println("Sizes of common to left and right...")
        pprintCustomised.pprintln(commonToLeftAndRight.map(_.size))

        try
          val Right(
            analysis: CodeMotionAnalysis[Path, Element]
          ) =
            CodeMotionAnalysis.of(
              baseSources,
              leftSources,
              rightSources
            )(
              minimumMatchSize = minimumPossibleExpectedMatchSize,
              thresholdSizeFractionForMatching =
                minimumSizeFractionForMotionDetection,
              minimumAmbiguousMatchSize = 0
            )(
              elementEquality = Eq[Element],
              elementOrder = Order[Element],
              elementFunnel = elementFunnel,
              hashFunction = Hashing.murmur3_32_fixed()
            )(progressRecording = NoProgressRecording): @unchecked
          end val

          // Check that all matches are consistent with the base sections...
          analysis.base.values.flatMap(_.sections).foreach { baseSection =>
            val matches = analysis.matchesFor(baseSection)

            matches foreach {
              case Match.AllSides(
                    matchedBaseSection,
                    matchedLeftSection,
                    matchedRightSection
                  ) =>
                assert(matchedBaseSection == baseSection)
                assert(matchedLeftSection.content == matchedBaseSection.content)
                assert(
                  matchedRightSection.content == matchedBaseSection.content
                )

              case Match.BaseAndLeft(matchedBaseSection, matchedLeftSection) =>
                assert(matchedBaseSection == baseSection)
                assert(matchedLeftSection.content == matchedBaseSection.content)

              case Match
                    .BaseAndRight(matchedBaseSection, matchedRightSection) =>
                assert(matchedBaseSection == baseSection)
                assert(
                  matchedRightSection.content == matchedBaseSection.content
                )

              case Match.LeftAndRight(_, _) =>
            }
          }

          // Check that all matches are consistent with the left sections...
          analysis.left.values.flatMap(_.sections).foreach { leftSection =>
            val matches = analysis.matchesFor(leftSection)

            matches foreach {
              case Match.AllSides(
                    matchedBaseSection,
                    matchedLeftSection,
                    matchedRightSection
                  ) =>
                assert(matchedLeftSection == leftSection)
                assert(matchedBaseSection.content == matchedLeftSection.content)
                assert(
                  matchedRightSection.content == matchedLeftSection.content
                )

              case Match.BaseAndLeft(matchedBaseSection, matchedLeftSection) =>
                assert(matchedLeftSection == leftSection)
                assert(matchedBaseSection.content == matchedLeftSection.content)

              case Match.BaseAndRight(_, _) =>

              case Match
                    .LeftAndRight(matchedLeftSection, matchedRightSection) =>
                assert(matchedLeftSection == leftSection)
                assert(
                  matchedRightSection.content == matchedLeftSection.content
                )
            }
          }

          // Check that all matches are consistent with the right sections...
          analysis.right.values.flatMap(_.sections).foreach { rightSection =>
            val matches = analysis.matchesFor(rightSection)

            matches foreach {
              case Match.AllSides(
                    matchedBaseSection,
                    matchedLeftSection,
                    matchedRightSection
                  ) =>
                assert(matchedRightSection == rightSection)
                assert(
                  matchedBaseSection.content == matchedRightSection.content
                )
                assert(
                  matchedLeftSection.content == matchedRightSection.content
                )

              case Match.BaseAndLeft(_, _) =>

              case Match
                    .BaseAndRight(matchedBaseSection, matchedRightSection) =>
                assert(matchedRightSection == rightSection)
                assert(
                  matchedBaseSection.content == matchedRightSection.content
                )

              case Match
                    .LeftAndRight(matchedLeftSection, matchedRightSection) =>
                assert(matchedRightSection == rightSection)
                assert(
                  matchedLeftSection.content == matchedRightSection.content
                )
            }
          }

          // Over all paths on the base side, the concatenations of all matched
          // sections should contain all the relevant common sequences...
          {
            val survivorsCommonToAllThreeSides =
              collection.mutable.Set[IndexedSeq[Element]](
                commonToAllThreeSides*
              )
            val survivorsCommonToBaseAndLeft =
              collection.mutable.Set[IndexedSeq[Element]](commonToBaseAndLeft*)
            val survivorsCommonToBaseAndRight =
              collection.mutable.Set[IndexedSeq[Element]](commonToBaseAndRight*)

            analysis.base.foreach { case (_, file) =>
              val matchedBaseSectionsContent: IndexedSeq[Element] =
                file.sections
                  .filter(analysis.matchesFor(_).nonEmpty)
                  .map(_.content)
                  .foldLeft(IndexedSeq.empty)(_ ++ _)

              survivorsCommonToAllThreeSides.filterInPlace(
                !matchedBaseSectionsContent.containsSlice(_)
              )
              survivorsCommonToBaseAndLeft.filterInPlace(
                !matchedBaseSectionsContent.containsSlice(_)
              )
              survivorsCommonToBaseAndRight.filterInPlace(
                !matchedBaseSectionsContent.containsSlice(_)
              )
            }

            assert(survivorsCommonToAllThreeSides.isEmpty)
            assert(survivorsCommonToBaseAndLeft.isEmpty)
            assert(survivorsCommonToBaseAndRight.isEmpty)
          }

          // Over all paths on the left side, the concatenations of all matched
          // sections should contain all the relevant common sequences...
          {
            val survivorsCommonToAllThreeSides =
              collection.mutable.Set[IndexedSeq[Element]](
                commonToAllThreeSides*
              )
            val survivorsCommonToBaseAndLeft =
              collection.mutable.Set[IndexedSeq[Element]](commonToBaseAndLeft*)
            val survivorsCommonToLeftAndRight =
              collection.mutable.Set[IndexedSeq[Element]](commonToLeftAndRight*)

            analysis.left.foreach { case (_, file) =>
              val matchedLeftSectionsContent: IndexedSeq[Element] =
                file.sections
                  .filter(analysis.matchesFor(_).nonEmpty)
                  .map(_.content)
                  .foldLeft(IndexedSeq.empty)(_ ++ _)

              survivorsCommonToAllThreeSides.filterInPlace(
                !matchedLeftSectionsContent.containsSlice(_)
              )
              survivorsCommonToBaseAndLeft.filterInPlace(
                !matchedLeftSectionsContent.containsSlice(_)
              )
              survivorsCommonToLeftAndRight.filterInPlace(
                !matchedLeftSectionsContent.containsSlice(_)
              )
            }

            assert(survivorsCommonToAllThreeSides.isEmpty)
            assert(survivorsCommonToBaseAndLeft.isEmpty)
            assert(survivorsCommonToLeftAndRight.isEmpty)
          }

          // Over all paths on the right side, the concatenations of all matched
          // sections should contain all the relevant common sequences...
          {
            val survivorsCommonToAllThreeSides =
              collection.mutable.Set[IndexedSeq[Element]](
                commonToAllThreeSides*
              )
            val survivorsCommonToBaseAndRight =
              collection.mutable.Set[IndexedSeq[Element]](commonToBaseAndRight*)
            val survivorsCommonToLeftAndRight =
              collection.mutable.Set[IndexedSeq[Element]](commonToLeftAndRight*)

            analysis.right.foreach { case (_, file) =>
              val matchedRightSectionsContent: IndexedSeq[Element] =
                file.sections
                  .filter(analysis.matchesFor(_).nonEmpty)
                  .map(_.content)
                  .foldLeft(IndexedSeq.empty)(_ ++ _)

              survivorsCommonToAllThreeSides.filterInPlace(
                !matchedRightSectionsContent.containsSlice(_)
              )
              survivorsCommonToBaseAndRight.filterInPlace(
                !matchedRightSectionsContent.containsSlice(_)
              )
              survivorsCommonToLeftAndRight.filterInPlace(
                !matchedRightSectionsContent.containsSlice(_)
              )
            }

            assert(survivorsCommonToAllThreeSides.isEmpty)
            assert(survivorsCommonToBaseAndRight.isEmpty)
            assert(survivorsCommonToLeftAndRight.isEmpty)
          }

        catch
          case overlappingSections: FakeSources#OverlappingSections =>
            pprintCustomised.pprintln(overlappingSections)
            Trials.reject()
        end try
      }
  end matchingSectionsAreFound

  @Test
  def anAmbiguousAllSidesMatchSubsumedOnOneSideByALargerAllSidesMatchIsEliminatedCompletely
      : Unit =
    val prefix                        = 0 until 10
    val suffix                        = 30 until 40
    val smallAmbiguousAllSidesContent = 10 until 20
    val bigAllSidesContent =
      prefix ++ smallAmbiguousAllSidesContent ++ suffix

    // We have ambiguous all-sides matches because
    // `smallAmbiguousAllSidesContent` sits inside `bigAllSidesContent` on all
    // three sides, followed by another match made of just
    // `smallAmbiguousAllSidesContent` on all three sides - in addition to all
    // the other Cartesian product combinations across the sides.

    // This leads amongst other things to there being a partial suppression of
    // the all-sides match made from the *second* occurrences of
    // `smallAmbiguousAllSidesContent` on the base and left with the *first*
    // occurrence on the right, because of the larger all-sides match making
    // claim on the right.

    // That partial suppression should lead to a redundant pairwise match of the
    // *second* occurrences of `smallAmbiguousAllSidesContent` on the base and
    // left; but these are already contained within the obvious all-sides match
    // of the second occurrences of `smallAmbiguousAllSidesContent` across all
    // three sides.

    // All of the other ambiguous all-sides matches should be either suppressed
    // completely or are similarly redundant.

    val baseSources = new FakeSources(
      Map(
        1 -> (bigAllSidesContent ++ Vector(8, 4, 6, 7, 8,
          2) ++ smallAmbiguousAllSidesContent)
      ),
      "base"
    ) with SourcesContracts[Path, Element]

    val leftSources = new FakeSources(
      Map(
        1 -> (bigAllSidesContent ++ Vector(2, 5, 9, 6,
          3) ++ smallAmbiguousAllSidesContent)
      ),
      "left"
    ) with SourcesContracts[Path, Element]

    val rightSources = new FakeSources(
      Map(
        1 -> (bigAllSidesContent ++ Vector(0, 3, 4, 5, 6, 6,
          4) ++ smallAmbiguousAllSidesContent)
      ),
      "right"
    ) with SourcesContracts[Path, Element]

    val Right(
      analysis: CodeMotionAnalysis[Path, Element]
    ) =
      CodeMotionAnalysis.of(
        baseSources,
        leftSources,
        rightSources
      )(
        minimumMatchSize = 10,
        thresholdSizeFractionForMatching = 0,
        minimumAmbiguousMatchSize = 0
      )(
        elementEquality = Eq[Element],
        elementOrder = Order[Element],
        elementFunnel = elementFunnel,
        hashFunction = Hashing.murmur3_32_fixed()
      )(progressRecording = NoProgressRecording): @unchecked
    end val

    val matches =
      (analysis.base.values.flatMap(_.sections) ++ analysis.left.values.flatMap(
        _.sections
      ) ++ analysis.right.values.flatMap(_.sections))
        .map(analysis.matchesFor)
        .reduce(_ union _)

    // There should be no redundant pairwise matches.
    assert(matches.forall {
      case _: Match.AllSides[Section[Element]] => true
      case _                                   => false
    })

    // There should only be a big match and a little match.
    assert(matches.size == 2)

    // The contents should be what we started with.
    assert(
      matches.map(_.dominantElement.content) == Set(
        bigAllSidesContent,
        smallAmbiguousAllSidesContent
      )
    )
  end anAmbiguousAllSidesMatchSubsumedOnOneSideByALargerAllSidesMatchIsEliminatedCompletely

  @Test
  def eatenPairwiseMatchesMayBeSuppressedByACompetingAmbiguousPairwiseMatch
      : Unit =
    // This is a pathological situation - we have a pairwise match that subsumes
    // a smaller all-sides match; thus the pairwise match would be eaten into to
    // yield smaller leftover pairwise matches that would flank the all-sides
    // match.

    // So far, so good, only the pairwise match is ambiguous with another
    // pairwise match that only intrudes on the all-sides match on one side;
    // this means the other pairwise match is not eaten into by that all-sides
    // match. What would usually happen is that there would be an implied
    // ambiguous all-sides match that would be subsumed by the second pairwise
    // match; thus causing that one to be eaten into, and thus additional
    // leftover pairwise matches ambiguous with the first lot. More complex, but
    // still OK thus far...

    // The final twist that makes it pathological is that the second ambiguous
    // pairwise match involves a file on the side not intruding on the all-sides
    // match that is distinct from the one for the first pairwise match. In
    // addition, this file is large enough for the matching threshold to forbid
    // the implied all-sides match.

    // What would result is the first pairwise match being eaten into to make
    // leftovers along with the all-sides match, but an ambiguous pairwise match
    // that intrudes on both the leftovers and the all-sides match. Having one
    // match's section subsume another match's is not tolerated, so the
    // expectation is for the second ambiguous pairwise match to suppress both
    // the leftovers and the all-sides match; in addition, the original
    // ambiguous pairwise match is allowed to stand as-is.

    val prefix               = 0 until 10
    val suffix               = 30 until 40
    val smallAllSidesContent = 10 until 20
    val bigAmbiguousBaseAndLeftContent =
      prefix ++ smallAllSidesContent ++ suffix

    val baseSources = new FakeSources(
      Map(
        1 -> bigAmbiguousBaseAndLeftContent,
        2 -> (Vector(2, 5, 9, 6, 3, 7, 3, 6, 9, 4, 6, 1, 3, 4, 9, 0,
          2) ++ bigAmbiguousBaseAndLeftContent)
      ),
      "base"
    ) with SourcesContracts[Path, Element]

    val leftSources = new FakeSources(
      Map(1 -> bigAmbiguousBaseAndLeftContent),
      "left"
    ) with SourcesContracts[Path, Element]

    val rightSources = new FakeSources(
      Map(1 -> smallAllSidesContent),
      "right"
    ) with SourcesContracts[Path, Element]

    val Right(
      analysis: CodeMotionAnalysis[Path, Element]
    ) =
      CodeMotionAnalysis.of(
        baseSources,
        leftSources,
        rightSources
      )(
        minimumMatchSize = 10,
        // Low enough to allow the all-sides match to be considered, except in
        // path 2 on the base side...
        thresholdSizeFractionForMatching = 0.3,
        minimumAmbiguousMatchSize = 0
      )(
        elementEquality = Eq[Element],
        elementOrder = Order[Element],
        elementFunnel = elementFunnel,
        hashFunction = Hashing.murmur3_32_fixed()
      )(progressRecording = NoProgressRecording): @unchecked
    end val

    val matches =
      (analysis.base.values.flatMap(_.sections) ++ analysis.left.values.flatMap(
        _.sections
      ) ++ analysis.right.values.flatMap(_.sections))
        .map(analysis.matchesFor)
        .reduce(_ union _)

    // There only be base and left matches.
    assert(matches.forall {
      case _: Match.BaseAndLeft[Section[Element]] => true
      case _                                      => false
    })

    // There should be two ambiguous matches.
    assert(matches.size == 2)

    // The contents should be the same; we have ambiguous matches.
    assert(
      matches.map(_.dominantElement.content) == Set(
        bigAmbiguousBaseAndLeftContent
      )
    )
  end eatenPairwiseMatchesMayBeSuppressedByACompetingAmbiguousPairwiseMatch

  @Test
  def eatenPairwiseMatchesMayBeSuppressedByACompetingOverlappingAllSidesMatch
      : Unit =
    // This is a even more pathological situation - we have overlapping matches
    // of the same size, one of which is a pairwise match and the other an
    // all-sides match. This in itself is permitted (but will result in an
    // admissible downstream exception due to the overlap when
    // `CodeMotionAnalysis.of` builds up its files from the matches).

    // The twist is when there is another smaller all-sides that is subsumed by
    // the pairwise match but does *not* overlap with the larger all-sides
    // match, and thus does not interfere with the overlap between the pairwise
    // match and the larger all-sides match on the relevant sides.

    // Consequently, the smaller all-sides match will eat into the larger
    // pairwise match to cleave off a *smaller* pairwise match that is now
    // subsumed (and not just overlapped) by the larger all-sides match. It's OK
    // to have the smaller all-sides match because it doesn't overlap with the
    // larger all-sides match, but the leftover pairwise match should be
    // suppressed.

    val prefix                    = 0 until 10
    val suffix                    = 30 until 40
    val overlap                   = 10 until 20
    val bigAllSidesContent        = prefix ++ overlap
    val equallyBigPairwiseContent = overlap ++ suffix
    val smallAllSidesContent      = suffix

    val baseSources = new FakeSources(
      Map(1 -> (prefix ++ overlap ++ suffix)),
      "base"
    ) with SourcesContracts[Path, Element]

    val leftSources = new FakeSources(
      Map(
        1 -> (equallyBigPairwiseContent ++ Vector(8, 7, 3, 4, 7, 8, 4, 5, 3, 8,
          7, 3, 4) ++ bigAllSidesContent)
      ),
      "left"
    ) with SourcesContracts[Path, Element]

    val rightSources = new FakeSources(
      Map(
        1 -> (smallAllSidesContent ++ Vector(2, 1, 5, 6, 5, 6,
          2) ++ bigAllSidesContent)
      ),
      "right"
    ) with SourcesContracts[Path, Element]

    val Right(
      analysis: CodeMotionAnalysis[Path, Element]
    ) =
      CodeMotionAnalysis.of(
        baseSources,
        leftSources,
        rightSources
      )(
        minimumMatchSize = 10,
        thresholdSizeFractionForMatching = 0,
        minimumAmbiguousMatchSize = 0
      )(
        elementEquality = Eq[Element],
        elementOrder = Order[Element],
        elementFunnel = elementFunnel,
        hashFunction = Hashing.murmur3_32_fixed()
      )(progressRecording = NoProgressRecording): @unchecked
    end val

    val matches =
      (analysis.base.values.flatMap(_.sections) ++ analysis.left.values.flatMap(
        _.sections
      ) ++ analysis.right.values.flatMap(_.sections))
        .map(analysis.matchesFor)
        .reduce(_ union _)

    // There only be all-sides matches.
    assert(matches.forall {
      case _: Match.AllSides[Section[Element]] => true
      case _                                   => false
    })

    // There should be two matches.
    assert(matches.size == 2)

    // The contents should be that of the two all-sides matches
    assert(
      matches.map(_.dominantElement.content) == Set(
        bigAllSidesContent,
        smallAllSidesContent
      )
    )
  end eatenPairwiseMatchesMayBeSuppressedByACompetingOverlappingAllSidesMatch

end CodeMotionAnalysisTest

object CodeMotionAnalysisTest:
  type Path    = Int
  type Element = Int

  private def elementFunnel(
      element: Element,
      primitiveSink: PrimitiveSink
  ): Unit =
    primitiveSink.putInt(element)
  end elementFunnel

  extension (fakeSources: FakeSources)
    /** Specific to testing, not part of the [[Sources]] API implementation.
      */
    def contains(sectionContent: IndexedSeq[Element]): Boolean =
      fakeSources.contentsByPath.values.exists(_ containsSlice sectionContent)

    /** Specific to testing, not part of the [[Sources]] API implementation.
      */
    def maximumContentsSize: Int =
      fakeSources.contentsByPath.values.map(_.size).maxOption.getOrElse(0)
  end extension

  extension (trialsApi: TrialsApi)
    def nonEmptyPartitioning[Element](
        things: IndexedSeq[Element],
        numberOfPartitions: Int
    ): Trials[Seq[IndexedSeq[Element]]] =
      require(0 < numberOfPartitions)

      val chunkSizeVectors =
        val numberOfThings = things.size

        val partitionPointIndexVectors = trialsApi
          .indexCombinations(
            numberOfIndices = numberOfThings - 1,
            combinationSize = numberOfPartitions - 1
          )
          .map(_.map(1 + _))

        partitionPointIndexVectors.map(partitionPointIndices =>
          (0 +: partitionPointIndices)
            .zip(partitionPointIndices :+ numberOfThings)
            .map { case (partitionStartIndex, onePastPartitionEndIndex) =>
              onePastPartitionEndIndex - partitionStartIndex
            }
        )
      end chunkSizeVectors

      chunkSizeVectors.map { chunkSizes =>
        assume(chunkSizes.sum == things.size)
        assume(chunkSizes.size == numberOfPartitions)
        assume(chunkSizes.forall(0 < _))
        thingsInChunks(chunkSizes, things)
      }
    end nonEmptyPartitioning

    def partitioning[Element](
        things: IndexedSeq[Element],
        numberOfPartitions: Int
    ): Trials[Seq[IndexedSeq[Element]]] =
      require(0 < numberOfPartitions)

      val chunkSizeVectors =
        val numberOfThings = things.size

        val partitionPointIndexVectors =
          if 0 < numberOfThings then
            trialsApi
              .integers(lowerBound = 0, upperBound = numberOfThings)
              .lotsOfSize[Vector[Int]](numberOfPartitions - 1)
              .map(_.sorted)
          else trialsApi.only(Vector.fill(numberOfPartitions - 1)(0))

        partitionPointIndexVectors.map(partitionPointIndices =>
          (0 +: partitionPointIndices)
            .zip(partitionPointIndices :+ numberOfThings)
            .map { case (partitionStartIndex, onePastPartitionEndIndex) =>
              onePastPartitionEndIndex - partitionStartIndex
            }
        )
      end chunkSizeVectors

      chunkSizeVectors.map { chunkSizes =>
        assume(chunkSizes.sum == things.size)
        assume(chunkSizes.size == numberOfPartitions)
        thingsInChunks(chunkSizes, things)
      }
    end partitioning

    private def thingsInChunks[Element](
        chunkSizes: Seq[Int],
        things: IndexedSeq[Element]
    ): Seq[IndexedSeq[Element]] =
      chunkSizes match
        case Seq(leadingChunkSize, chunkSizesTail*) =>
          val (leadingChunk, remainder) = things.splitAt(leadingChunkSize)

          leadingChunk +: thingsInChunks(chunkSizesTail, remainder)
        case Nil =>
          assume(things.isEmpty)
          Seq.empty
    end thingsInChunks
  end extension

  trait SourcesContracts[Path, Element] extends Sources[Path, Element]:
    abstract override def section(
        path: SourcesContracts.this.Path
    )(startOffset: Int, size: Int): Section[SourcesContracts.this.Element] =
      require(0 <= startOffset)
      require(0 < size)

      val result = super.section(path)(startOffset, size)

      assert(pathFor(result) == path)

      assert(result.size == size)

      result
    end section

    abstract override def filesByPathUtilising(
        mandatorySections: Set[Section[SourcesContracts.this.Element]]
    ): Map[SourcesContracts.this.Path, File[Element]] =
      val result = super.filesByPathUtilising(mandatorySections)

      assert(result.keys == paths)

      mandatorySections.foreach(mandatorySection =>
        assert(
          result(pathFor(mandatorySection)).sections.contains(mandatorySection)
        )
      )

      // If we have mandatory sections, we are definitely not initialising
      // `filesByPath`, so it is safe to check consistency against it.
      if mandatorySections.nonEmpty then
        assert(filesByPath.values.map(_.size) == result.values.map(_.size))
      end if

      result
    end filesByPathUtilising
  end SourcesContracts

  case class FakeSources(
      override val contentsByPath: Map[Path, IndexedSeq[Element]],
      override val label: String
  ) extends MappedContentSources[Path, Element]
end CodeMotionAnalysisTest
