package com.sageserpent.kineticmerge.core

import cats.kernel.Eq
import com.google.common.hash.{Funnel, HashFunction, Hashing, PrimitiveSink}
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.java.CasesLimitStrategy
import com.sageserpent.americium.junit5.*
import com.sageserpent.americium.{Trials, TrialsApi}
import com.sageserpent.kineticmerge.core.CodeMotionAnalysis.{
  AdmissibleFailure,
  Configuration
}
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisTest.{*, given}
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
      trialsApi.doubles(0, 0.2)

    val contentTrials: Trials[Vector[Element]] = trialsApi
      .integers(0, 100)
      .flatMap(textSize =>
        trialsApi
          .integers(lowerBound = 1, upperBound = 10)
          .lotsOfSize[Vector[Path]](textSize)
      )

    val pathTrials: Trials[Path] = trialsApi
      .integers(1, 100)

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
      .withStrategy(cycle =>
        CasesLimitStrategy.timed(
          Duration.apply(if cycle.isInitial then 1 else 3, TimeUnit.MINUTES)
        )
      )
      .dynamicTests(
        (
            base: FakeSources,
            left: FakeSources,
            right: FakeSources,
            minimumSizeFraction: Double
        ) =>
          pprintCustomised.pprintln((base, left, right, minimumSizeFraction))

          val configuration = Configuration(
            minimumMatchSize = 2,
            thresholdSizeFractionForMatching = minimumSizeFraction,
            minimumAmbiguousMatchSize = 0,
            ambiguousMatchesThreshold = 10
          )

          CodeMotionAnalysis.of(base, left, right)(
            configuration
          ) match
            case Right(analysis) =>
              analysis.base matches base
              analysis.left matches left
              analysis.right matches right

            case Left(overlappingSections: AdmissibleFailure) =>
              pprintCustomised.pprintln(overlappingSections)
              Trials.reject()

            case Left(unexpectedException) => throw unexpectedException
          end match
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
        else CasesLimitStrategy.counted(1000, 3.0)
      )
      .dynamicTests { testPlan =>
        import testPlan.*
        // Scalafmt 3.8.5 will wreck this block of code if it isn't protected by
        // braces; it seems it doesn't play well with the preceding import
        // statement.
        {
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

          val configuration = Configuration(
            minimumMatchSize = minimumPossibleExpectedMatchSize,
            thresholdSizeFractionForMatching =
              minimumSizeFractionForMotionDetection,
            minimumAmbiguousMatchSize = 0,
            ambiguousMatchesThreshold = 10
          )

          CodeMotionAnalysis.of(
            baseSources,
            leftSources,
            rightSources
          )(
            configuration,
            // NOTE: the test cases can exhibit matches with overlapping
            // sections
            // that intrude on the content the test is checking, so rather than
            // quietly suppressing the matches, we let admissible failures for
            // overlapping sections occur and reject the test case.
            suppressMatchesInvolvingOverlappingSections = false
          ) match
            case Right(analysis) =>
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
                    assert(
                      matchedLeftSection.content == matchedBaseSection.content
                    )
                    assert(
                      matchedRightSection.content == matchedBaseSection.content
                    )

                  case Match
                        .BaseAndLeft(matchedBaseSection, matchedLeftSection) =>
                    assert(matchedBaseSection == baseSection)
                    assert(
                      matchedLeftSection.content == matchedBaseSection.content
                    )

                  case Match
                        .BaseAndRight(
                          matchedBaseSection,
                          matchedRightSection
                        ) =>
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
                    assert(
                      matchedBaseSection.content == matchedLeftSection.content
                    )
                    assert(
                      matchedRightSection.content == matchedLeftSection.content
                    )

                  case Match
                        .BaseAndLeft(matchedBaseSection, matchedLeftSection) =>
                    assert(matchedLeftSection == leftSection)
                    assert(
                      matchedBaseSection.content == matchedLeftSection.content
                    )

                  case Match.BaseAndRight(_, _) =>

                  case Match
                        .LeftAndRight(
                          matchedLeftSection,
                          matchedRightSection
                        ) =>
                    assert(matchedLeftSection == leftSection)
                    assert(
                      matchedRightSection.content == matchedLeftSection.content
                    )
                }
              }

              // Check that all matches are consistent with the right
              // sections...
              analysis.right.values.flatMap(_.sections).foreach {
                rightSection =>
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
                          .BaseAndRight(
                            matchedBaseSection,
                            matchedRightSection
                          ) =>
                      assert(matchedRightSection == rightSection)
                      assert(
                        matchedBaseSection.content == matchedRightSection.content
                      )

                    case Match
                          .LeftAndRight(
                            matchedLeftSection,
                            matchedRightSection
                          ) =>
                      assert(matchedRightSection == rightSection)
                      assert(
                        matchedLeftSection.content == matchedRightSection.content
                      )
                  }
              }

              // NOTE: the reason that matched sections are concatenated in the
              // three following blocks is to allow looseness in the
              // expectations
              // for the generated matches. This test starts off by making
              // content
              // matches across two or all sides, but it also encourages the
              // possibility of having subsumption of one content match within
              // another larger one, either on just one side or two. This means
              // that the original content matches from the test can't be taken
              // literally as what we expect to see from the SUT; instead we
              // expect to encounter the occasional situation where the original
              // content match is broken down by the SUT into several smaller
              // matches that abut on each side. However, the sections are
              // ordered
              // so as to reproduce the source file's content, thus we know that
              // concatenation will place any such smaller matches together in
              // the
              // correct order for a correctly functioning SUT, so each of the
              // original matches should show up in the concatenation.

              // Over all paths on the base side, the concatenations of all
              // matched sections should contain all the relevant common
              // sequences...
              {
                val survivorsCommonToAllThreeSides =
                  collection.mutable.Set[IndexedSeq[Element]](
                    commonToAllThreeSides*
                  )
                val survivorsCommonToBaseAndLeft =
                  collection.mutable.Set[IndexedSeq[Element]](
                    commonToBaseAndLeft*
                  )
                val survivorsCommonToBaseAndRight =
                  collection.mutable.Set[IndexedSeq[Element]](
                    commonToBaseAndRight*
                  )

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

              // Over all paths on the left side, the concatenations of all
              // matched sections should contain all the relevant common
              // sequences...
              {
                val survivorsCommonToAllThreeSides =
                  collection.mutable.Set[IndexedSeq[Element]](
                    commonToAllThreeSides*
                  )
                val survivorsCommonToBaseAndLeft =
                  collection.mutable.Set[IndexedSeq[Element]](
                    commonToBaseAndLeft*
                  )
                val survivorsCommonToLeftAndRight =
                  collection.mutable.Set[IndexedSeq[Element]](
                    commonToLeftAndRight*
                  )

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

              // Over all paths on the right side, the concatenations of all
              // matched sections should contain all the relevant common
              // sequences...
              {
                val survivorsCommonToAllThreeSides =
                  collection.mutable.Set[IndexedSeq[Element]](
                    commonToAllThreeSides*
                  )
                val survivorsCommonToBaseAndRight =
                  collection.mutable.Set[IndexedSeq[Element]](
                    commonToBaseAndRight*
                  )
                val survivorsCommonToLeftAndRight =
                  collection.mutable.Set[IndexedSeq[Element]](
                    commonToLeftAndRight*
                  )

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

            case Left(overlappingSections: AdmissibleFailure) =>
              pprintCustomised.pprintln(overlappingSections)
              Trials.reject()

            case Left(unexpectedException) => throw unexpectedException
          end match
        }
      }
  end matchingSectionsAreFound

  @Test
  def anAmbiguousAllSidesMatchSubsumedOnOneSideByALargerAllSidesMatchIsEliminatedCompletely()
      : Unit =
    val configuration = Configuration(
      minimumMatchSize = 10,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    val prefix                        = 0 until 10
    val suffix                        = 30 until 40
    val smallAmbiguousAllSidesContent = 10 until 20
    val bigAllSidesContent            =
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

    // All the other ambiguous all-sides matches should be either suppressed
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
      )(configuration): @unchecked
    end val

    val matches =
      (analysis.base.values.flatMap(_.sections) ++ analysis.left.values.flatMap(
        _.sections
      ) ++ analysis.right.values.flatMap(_.sections))
        .map(analysis.matchesFor)
        .reduce(_ union _)

    // There should be no redundant pairwise matches.
    assert(matches.forall(_.isAnAllSidesMatch))

    // There should only be a big match and a little match.
    assert(matches.size == 2)

    // The contents should be what we started with.
    assert(
      matches.map(_.content) == Set(
        bigAllSidesContent,
        smallAmbiguousAllSidesContent
      )
    )
  end anAmbiguousAllSidesMatchSubsumedOnOneSideByALargerAllSidesMatchIsEliminatedCompletely

  @Test
  def competingAmbiguousPairwiseMatchesCanBeEatenIntoByCompetingAmbiguousAllSidesMatches()
      : Unit =
    // We have a pairwise match that subsumes a smaller all-sides match; thus
    // the pairwise match would be eaten into to yield smaller leftover pairwise
    // matches that would flank the all-sides match.

    // So far, so good, only the pairwise match is ambiguous with another
    // pairwise match that only intrudes on the all-sides match on one side;
    // this means the other pairwise match is not eaten into by that all-sides
    // match. What then happens is that there would be an implied ambiguous
    // all-sides match that would be subsumed by the second pairwise match; thus
    // causing that one to be eaten into, and thus additional leftover pairwise
    // matches ambiguous with the first lot.

    val prefix                         = 0 until 10
    val suffix                         = 30 until 40
    val smallAllSidesContent           = 10 until 20
    val bigAmbiguousBaseAndLeftContent =
      prefix ++ smallAllSidesContent ++ suffix

    val baseSources = new FakeSources(
      Map(
        1 -> bigAmbiguousBaseAndLeftContent
      ),
      "base"
    ) with SourcesContracts[Path, Element]

    val leftSources = new FakeSources(
      Map(
        1 -> bigAmbiguousBaseAndLeftContent,
        2 -> bigAmbiguousBaseAndLeftContent
      ),
      "left"
    ) with SourcesContracts[Path, Element]

    val rightSources = new FakeSources(
      Map(1 -> smallAllSidesContent),
      "right"
    ) with SourcesContracts[Path, Element]

    val configuration = Configuration(
      minimumMatchSize = 10,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    val Right(
      analysis: CodeMotionAnalysis[Path, Element]
    ) =
      CodeMotionAnalysis.of(
        baseSources,
        leftSources,
        rightSources
      )(configuration): @unchecked
    end val

    val matches =
      (analysis.base.values.flatMap(_.sections) ++ analysis.left.values.flatMap(
        _.sections
      ) ++ analysis.right.values.flatMap(_.sections))
        .map(analysis.matchesFor)
        .reduce(_ union _)

    // There only be base and left matches and all-sides matches.
    assert(matches.forall {
      case _: (Match.BaseAndLeft[Section[Element]] |
            Match.AllSides[Section[Element]]) =>
        true
      case _ => false
    })

    val (allSidesMatches, baseAndLeftMatches) =
      matches.partition(_.isAnAllSidesMatch)

    // There should be two all-sides matches.
    assert(allSidesMatches.size == 2)

    // The contents should be the same; we have ambiguous matches.
    assert(
      allSidesMatches.map(_.content) == Set(
        smallAllSidesContent
      )
    )

    // There should be four base and left matches.
    assert(baseAndLeftMatches.size == 4)

    // We have ambiguous matches of either the prefix or the suffix.
    assert(
      baseAndLeftMatches.map(_.content) == Set(prefix, suffix)
    )
  end competingAmbiguousPairwiseMatchesCanBeEatenIntoByCompetingAmbiguousAllSidesMatches

  @Test
  def eatenPairwiseMatchesMayBeSuppressedByACompetingAmbiguousPairwiseMatch()
      : Unit =
    // This is a pathological situation that extends
    // `CodeMotionAnalysisTest.competingAmbiguousPairwiseMatchesCanBeEatenIntoByCompetingAmbiguousAllSidesMatches`
    // - we have a pairwise match that subsumes a smaller all-sides match; thus
    // the pairwise match would be eaten into to yield smaller leftover pairwise
    // matches that would flank the all-sides match.

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

    val prefix                         = 0 until 10
    val suffix                         = 30 until 40
    val smallAllSidesContent           = 10 until 20
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

    val configuration = Configuration(
      minimumMatchSize = 10,
      // Low enough to allow the all-sides match to be considered, except in
      // path 2 on the base side...
      thresholdSizeFractionForMatching = 0.3,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    val Right(
      analysis: CodeMotionAnalysis[Path, Element]
    ) =
      CodeMotionAnalysis.of(
        baseSources,
        leftSources,
        rightSources
      )(configuration): @unchecked
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

    // We have ambiguous matches of the same content.
    assert(
      matches.map(_.content) == Set(
        bigAmbiguousBaseAndLeftContent
      )
    )
  end eatenPairwiseMatchesMayBeSuppressedByACompetingAmbiguousPairwiseMatch

  @Test
  def eatenPairwiseMatchesMayBeSuppressedByACompetingOverlappingAllSidesMatch()
      : Unit =
    val configuration = Configuration(
      minimumMatchSize = 10,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    // This is an even more pathological situation - we have overlapping matches
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
      )(configuration): @unchecked
    end val

    val matches =
      (analysis.base.values.flatMap(_.sections) ++ analysis.left.values.flatMap(
        _.sections
      ) ++ analysis.right.values.flatMap(_.sections))
        .map(analysis.matchesFor)
        .reduce(_ union _)

    // There only be all-sides matches.
    assert(matches.forall(_.isAnAllSidesMatch))

    // There should be two matches.
    assert(matches.size == 2)

    // The contents should be that of the two all-sides matches.
    assert(
      matches.map(_.content) == Set(
        bigAllSidesContent,
        smallAllSidesContent
      )
    )
  end eatenPairwiseMatchesMayBeSuppressedByACompetingOverlappingAllSidesMatch

  @Test
  def notEatingIntoTwoPairwiseMatchesEvenThoughBothHaveCommonContent(): Unit =
    // Here, we have a base-left match and a base-right match - both matches
    // share a smaller run of common content. Ordinarily, the common content
    // would form two ambiguous all-sides matches (obviously the pairwise
    // matches must differ in content to not constitute an all-side match
    // overall).

    // The twist is that the matching threshold forbids matching of the common
    // content in the base side of the base-left match, so there is no ambiguous
    // all-sides match to eat into that base-left match. In turn, that means
    // that the putative all-sides match that would have eaten into the
    // base-right match is blocked by the subsuming base-left match, so in the
    // end there is no eating into either pairwise match.

    val largePrefix           = 0 until 10
    val largeSuffix           = 30 until 40
    val smallAllSidesContent  = 10 until 20
    val bigBaseAndLeftContent =
      largePrefix ++ smallAllSidesContent ++ largeSuffix
    val smallPrefix            = 40 until 45
    val smallSuffix            = 50 until 55
    val bigBaseAndRightContent =
      smallPrefix ++ smallAllSidesContent ++ smallSuffix

    val baseSources = new FakeSources(
      Map(
        1 -> bigBaseAndLeftContent,
        2 -> bigBaseAndRightContent
      ),
      "base"
    ) with SourcesContracts[Path, Element]

    val leftSources = new FakeSources(
      Map(1 -> bigBaseAndLeftContent),
      "left"
    ) with SourcesContracts[Path, Element]

    val rightSources = new FakeSources(
      Map(2 -> bigBaseAndRightContent),
      "right"
    ) with SourcesContracts[Path, Element]

    val configuration = Configuration(
      minimumMatchSize = 10,
      // Low enough to allow the all-sides match to be considered, except in
      // path 1 on the base side...
      thresholdSizeFractionForMatching = 0.4,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    val Right(
      analysis: CodeMotionAnalysis[Path, Element]
    ) =
      CodeMotionAnalysis.of(
        baseSources,
        leftSources,
        rightSources
      )(configuration): @unchecked
    end val

    val matches =
      (analysis.base.values.flatMap(_.sections) ++ analysis.left.values.flatMap(
        _.sections
      ) ++ analysis.right.values.flatMap(_.sections))
        .map(analysis.matchesFor)
        .reduce(_ union _)

    // There should be a base-left and a base-right match.
    assert(matches.forall {
      case _: Match.BaseAndLeft[Section[Element]] |
          _: Match.BaseAndRight[Section[Element]] =>
        true
      case _ => false
    })

    // There should only be two big matches.
    assert(matches.size == 2)

    // The contents should be what we started with.
    assert(
      matches.map(_.content) == Set(
        bigBaseAndLeftContent,
        bigBaseAndRightContent
      )
    )
  end notEatingIntoTwoPairwiseMatchesEvenThoughBothHaveCommonContent

  @Test
  def eatingIntoTwoPairwiseMatchesWhenBothHaveCommonContent(): Unit =
    // Here, we have a base-left match and a base-right match - both matches
    // share a smaller run of common content. Ordinarily, the common content
    // would not form two ambiguous all-sides matches, because each pairwise
    // match partially subsumes the all-sides match contributed by the other
    // pairwise match.
    //
    // We then sneak in an all-sides match that has the common content all by
    // itself on just the right side - it can escape partial subsumption by the
    // base-left and eats into that pairwise match. Consequently, the common
    // content that was part of the base-left match is liberated, allowing a
    // fresh all-sides match to be considered that eats into the base-right
    // match.

    // The end result is that there are two ambiguous all-sides matches and
    // fragments from both pairwise matches.

    val largePrefix           = 0 until 10
    val largeSuffix           = 30 until 40
    val smallAllSidesContent  = 10 until 20
    val bigBaseAndLeftContent =
      largePrefix ++ smallAllSidesContent ++ largeSuffix
    val smallPrefix            = 40 until 45
    val smallSuffix            = 50 until 55
    val bigBaseAndRightContent =
      smallPrefix ++ smallAllSidesContent ++ smallSuffix

    val baseSources = new FakeSources(
      Map(
        1 -> bigBaseAndLeftContent,
        2 -> bigBaseAndRightContent
      ),
      "base"
    ) with SourcesContracts[Path, Element]

    val leftSources = new FakeSources(
      Map(1 -> bigBaseAndLeftContent),
      "left"
    ) with SourcesContracts[Path, Element]

    val rightSources = new FakeSources(
      Map(1 -> smallAllSidesContent, 2 -> bigBaseAndRightContent),
      "right"
    ) with SourcesContracts[Path, Element]

    val configuration = Configuration(
      minimumMatchSize = 10,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    val Right(
      analysis: CodeMotionAnalysis[Path, Element]
    ) =
      CodeMotionAnalysis.of(
        baseSources,
        leftSources,
        rightSources
      )(configuration): @unchecked
    end val

    val matches =
      (analysis.base.values.flatMap(_.sections) ++ analysis.left.values.flatMap(
        _.sections
      ) ++ analysis.right.values.flatMap(_.sections))
        .map(analysis.matchesFor)
        .reduce(_ union _)

    // There should be base-left, base-right and all-sides matches.
    assert(matches.map(_.ordinal).size == 3)

    // There should be two base-left matches.
    assert((matches count {
      case _: Match.BaseAndLeft[Section[Element]] => true
      case _                                      => false
    }) == 2)

    // There should be two base-right matches.
    assert((matches count {
      case _: Match.BaseAndRight[Section[Element]] => true
      case _                                       => false
    }) == 2)

    // There should be four all-sides matches.
    assert((matches count {
      case _: Match.AllSides[Section[Element]] => true
      case _                                   => false
    }) == 4)

    // The contents should be broken down.
    assert(
      matches.map(_.content) == Set(
        largePrefix,
        largeSuffix,
        smallPrefix,
        smallSuffix,
        smallAllSidesContent
      )
    )
  end eatingIntoTwoPairwiseMatchesWhenBothHaveCommonContent

  @Test
  def matchesWithOverlappingSections(): Unit =
    val someContentThatMatchesWithoutOverlap  = 0 until 10
    val otherContentThatMatchesWithoutOverlap = 10 until 20
    val overlappingMatchesTargetContent       = -1 to -50 by -1
    val overlappingMatchesSources             =
      (-1 to -41 by -1).flatMap(start => start until start - 10 by -1)

    val baseSources = new FakeSources(
      Map(
        1 -> (someContentThatMatchesWithoutOverlap ++ otherContentThatMatchesWithoutOverlap ++ overlappingMatchesSources)
      ),
      "base"
    ) with SourcesContracts[Path, Element]

    val leftSources = new FakeSources(
      Map(
        1 -> (someContentThatMatchesWithoutOverlap ++ overlappingMatchesTargetContent ++ otherContentThatMatchesWithoutOverlap)
      ),
      "left"
    ) with SourcesContracts[Path, Element]

    val rightSources = new FakeSources(
      Map(
        1 -> (overlappingMatchesSources ++ someContentThatMatchesWithoutOverlap ++ otherContentThatMatchesWithoutOverlap)
      ),
      "right"
    ) with SourcesContracts[Path, Element]

    val configuration = Configuration(
      minimumMatchSize = 10,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 1
    )

    val Right(
      analysis: CodeMotionAnalysis[Path, Element]
    ) =
      Assertions.assertDoesNotThrow(() =>
        CodeMotionAnalysis.of(
          baseSources,
          leftSources,
          rightSources
        )(configuration, suppressMatchesInvolvingOverlappingSections = true)
      ): @unchecked
    end val

    val someMatchedContentBaseSection = baseSources.section(1)(
      startOffset = 0,
      size = someContentThatMatchesWithoutOverlap.size
    )
    val someMatchedContentLeftSection = leftSources.section(1)(
      startOffset = 0,
      size = someContentThatMatchesWithoutOverlap.size
    )
    val someMatchedContentRightSection = rightSources.section(1)(
      startOffset = overlappingMatchesSources.size,
      size = someContentThatMatchesWithoutOverlap.size
    )

    val otherMatchedContentBaseSection = baseSources.section(1)(
      startOffset = someContentThatMatchesWithoutOverlap.size,
      size = otherContentThatMatchesWithoutOverlap.size
    )
    val otherMatchedContentLeftSection = leftSources.section(1)(
      startOffset =
        someContentThatMatchesWithoutOverlap.size + overlappingMatchesTargetContent.size,
      size = otherContentThatMatchesWithoutOverlap.size
    )
    val otherMatchedContentRightSection = rightSources.section(1)(
      startOffset =
        overlappingMatchesSources.size + someContentThatMatchesWithoutOverlap.size,
      size = otherContentThatMatchesWithoutOverlap.size
    )

    assert(
      Set(
        Match.AllSides(
          someMatchedContentBaseSection,
          someMatchedContentLeftSection,
          someMatchedContentRightSection
        )
      ) == analysis.matchesFor(
        someMatchedContentBaseSection
      )
    )

    assert(
      Set(
        Match.AllSides(
          otherMatchedContentBaseSection,
          otherMatchedContentLeftSection,
          otherMatchedContentRightSection
        )
      ) == analysis.matchesFor(
        otherMatchedContentBaseSection
      )
    )

    val Left(exception) =
      CodeMotionAnalysis.of(
        baseSources,
        leftSources,
        rightSources
      )(
        configuration,
        suppressMatchesInvolvingOverlappingSections = false
      ): @unchecked

    assert(exception.isInstanceOf[AdmissibleFailure])

  end matchesWithOverlappingSections

  @Test
  def overlappingSmallerAllSidesMatchesCanEatIntoALargerPairwiseMatchWithoutLeavingAnyFragments()
      : Unit =
    // Here, we set up a larger pairwise match and eat into it via four smaller,
    // ambiguous *and* overlapping all-sides matches. The overlapping causes the
    // collective all-sides matches to eat into all of the content of the
    // pairwise matches, so apart from the all-sides matches (which are
    // suppressed later by virtue of overlapping each other), there are no
    // fragmented pairwise matches left over.

    val configuration = Configuration(
      minimumMatchSize = 2,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    val largeIdenticalContentRun = Vector.fill(3)(5)
    val smallIdenticalContentRun = largeIdenticalContentRun.drop(1)

    val baseSources = new FakeSources(
      Map(1 -> ((1 +: largeIdenticalContentRun) :+ 2)),
      "base"
    ) with SourcesContracts[Path, Element]

    val leftSources = new FakeSources(
      Map(1 -> ((3 +: largeIdenticalContentRun) :+ 4)),
      "left"
    ) with SourcesContracts[Path, Element]

    val rightSources = new FakeSources(
      Map(1 -> ((6 +: smallIdenticalContentRun) :+ 7)),
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
        configuration,
        suppressMatchesInvolvingOverlappingSections = true
      ): @unchecked
    end val

    val matches =
      (analysis.base.values.flatMap(_.sections) ++ analysis.left.values.flatMap(
        _.sections
      ) ++ analysis.right.values.flatMap(_.sections))
        .map(analysis.matchesFor)
        .reduce(_ union _)

    assert(matches.isEmpty)
  end overlappingSmallerAllSidesMatchesCanEatIntoALargerPairwiseMatchWithoutLeavingAnyFragments

  @Test
  def problematicSituation(): Unit =
    val configuration = Configuration(
      minimumMatchSize = 2,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    val alpha   = 1
    val beta    = 2
    val gamma   = 3
    val delta   = 4
    val epsilon = 5

    val baseSources = new FakeSources(
      Map(
        1 -> Vector(
          gamma,
          delta,
          epsilon
        )
      ),
      "base"
    ) with SourcesContracts[Path, Element]

    val leftSources = new FakeSources(
      Map(
        1 -> Vector(
          alpha,
          delta,
          epsilon,
          -1,
          beta,
          gamma,
          delta
        )
      ),
      "left"
    ) with SourcesContracts[Path, Element]

    val rightSources = new FakeSources(
      Map(
        1 -> Vector(
          alpha,
          delta,
          epsilon,
          -2,
          beta,
          gamma,
          delta,
          epsilon
        )
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
      )(configuration): @unchecked
    end val

    val matches =
      (analysis.base.values.flatMap(_.sections) ++ analysis.left.values.flatMap(
        _.sections
      ) ++ analysis.right.values.flatMap(_.sections))
        .map(analysis.matchesFor)
        .reduce(_ union _)

    // There should be just left-right matches.
    assert(matches.map(_.ordinal).size == 1)

    // There should be two left-right matches.
    assert((matches count {
      case _: Match.LeftAndRight[Section[Element]] => true
      case _                                       => false
    }) == 2)

    // The contents should be broken down.
    assert(
      matches.map(_.content) == Set(
        Vector(alpha),
        Vector(beta)
      )
    )
  end problematicSituation

  @Test
  def complexFragmentationOfMultiplePairwiseMatches(): Unit =
    val configuration = Configuration(
      minimumMatchSize = 2,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    val alpha = -1
    val beta  = -2
    val gamma = -3
    val delta = -4

    val baseSources = new FakeSources(
      Map(1 -> Vector(1, beta, gamma, delta, 2, beta, gamma, 3)),
      "base"
    ) with SourcesContracts[Path, Element]

    val leftSources = new FakeSources(
      Map(1 -> Vector(4, alpha, beta, gamma, 5)),
      "left"
    ) with SourcesContracts[Path, Element]

    val rightSources = new FakeSources(
      Map(1 -> Vector(6, alpha, beta, gamma, 7, alpha, beta, gamma, delta, 8)),
      "right"
    ) with SourcesContracts[Path, Element]

    val Right(
      analysis: CodeMotionAnalysis[Path, Element]
    ) =
      CodeMotionAnalysis.of(
        baseSources,
        leftSources,
        rightSources
      )(configuration): @unchecked
    end val

    val matches =
      (analysis.base.values.flatMap(_.sections) ++ analysis.left.values.flatMap(
        _.sections
      ) ++ analysis.right.values.flatMap(_.sections))
        .map(analysis.matchesFor)
        .reduce(_ union _)

    // There should be base-right, left-right and all-sides matches.
    assert(matches.map(_.ordinal).size == 3)

    // There should be one base-right match.
    assert((matches count {
      case _: Match.BaseAndRight[Section[Element]] => true
      case _                                       => false
    }) == 1)

    // There should be two left-right matches.
    assert((matches count {
      case _: Match.LeftAndRight[Section[Element]] => true
      case _                                       => false
    }) == 2)

    // There should be four all-sides matches.
    assert((matches count {
      case _: Match.AllSides[Section[Element]] => true
      case _                                   => false
    }) == 4)

    // The contents should be broken down.
    assert(
      matches.map(_.content) == Set(
        Vector(alpha),
        Vector(beta, gamma),
        Vector(delta)
      )
    )
  end complexFragmentationOfMultiplePairwiseMatches

  @Test
  def complexFragmentationOfASinglePairwiseMatch(): Unit =
    val configuration = Configuration(
      minimumMatchSize = 2,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    val alpha = -1
    val beta  = -2
    val gamma = -3

    val baseSources = new FakeSources(
      Map(1 -> Vector(1, alpha, beta, gamma, 2, alpha, beta, 3)),
      "base"
    ) with SourcesContracts[Path, Element]

    val leftSources = new FakeSources(
      Map(1 -> Vector(4, alpha, beta, gamma, 5)),
      "left"
    ) with SourcesContracts[Path, Element]

    val rightSources = new FakeSources(
      Map(1 -> Vector(6, alpha, beta, 7, alpha, beta, 8)),
      "right"
    ) with SourcesContracts[Path, Element]

    val Right(
      analysis: CodeMotionAnalysis[Path, Element]
    ) =
      CodeMotionAnalysis.of(
        baseSources,
        leftSources,
        rightSources
      )(configuration): @unchecked
    end val

    val matches =
      (analysis.base.values.flatMap(_.sections) ++ analysis.left.values.flatMap(
        _.sections
      ) ++ analysis.right.values.flatMap(_.sections))
        .map(analysis.matchesFor)
        .reduce(_ union _)

    // There should be base-left and all-sides matches.
    assert(matches.map(_.ordinal).size == 2)

    // There should be one base-left match.
    assert((matches count {
      case _: Match.BaseAndLeft[Section[Element]] => true
      case _                                      => false
    }) == 1)

    // There should be four all-sides matches.
    assert((matches count {
      case _: Match.AllSides[Section[Element]] => true
      case _                                   => false
    }) == 4)

    // The contents should be broken down.
    assert(
      matches.map(_.content) == Set(
        Vector(alpha, beta),
        Vector(gamma)
      )
    )
  end complexFragmentationOfASinglePairwiseMatch

end CodeMotionAnalysisTest

object CodeMotionAnalysisTest:
  type Path    = Int
  type Element = Int

  given HashFunction = Hashing.murmur3_32_fixed()

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
      val result =
        super.filesByPathUtilising(mandatorySections)

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

  extension [Element](thisMatch: Match[Section[Element]])
    def content: IndexedSeq[Element] = thisMatch match
      case Match.BaseAndLeft(baseElement, leftElement) =>
        val result = baseElement.content
        assert(result == leftElement.content)
        result
      case Match.BaseAndRight(baseElement, rightElement) =>
        val result = baseElement.content
        assert(result == rightElement.content)
        result
      case Match.LeftAndRight(leftElement, rightElement) =>
        val result = leftElement.content
        assert(result == rightElement.content)
        result
      case Match.AllSides(baseElement, leftElement, rightElement) =>
        val result = baseElement.content
        assert(
          result == leftElement.content && result == rightElement.content
        )
        result
  end extension

  case class FakeSources(
      override val contentsByPath: Map[Path, IndexedSeq[Element]],
      override val label: String
  ) extends MappedContentSources[Path, Element]

  given Funnel[Element] with
    override def funnel(element: Element, primitiveSink: PrimitiveSink): Unit =
      primitiveSink.putInt(element)
  end given
end CodeMotionAnalysisTest
