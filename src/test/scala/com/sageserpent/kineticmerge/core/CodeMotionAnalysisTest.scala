package com.sageserpent.kineticmerge.core

import cats.kernel.{Eq, Order}
import com.google.common.hash.{Hashing, PrimitiveSink}
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.java.CasesLimitStrategy
import com.sageserpent.americium.junit5.*
import com.sageserpent.americium.{Trials, TrialsApi}
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
          pprint.pprintln((base, left, right, minimumSizeFraction))

          try
            val Right(
              analysis: CodeMotionAnalysis[
                Path,
                Element
              ]
            ) =
              CodeMotionAnalysis.of(base, left, right)(
                minimumMatchSize = 2,
                thresholdSizeFractionForMatching = minimumSizeFraction
              )(
                elementEquality = Eq[Element],
                elementOrder = Order[Element],
                elementFunnel = elementFunnel,
                hashFunction = Hashing.murmur3_32_fixed()
              ): @unchecked

            analysis.base matches base
            analysis.left matches left
            analysis.right matches right
          catch
            case overlappingSections: FakeSources#OverlappingSections =>
              pprint.pprintln(overlappingSections)
              Trials.reject()
          end try
      )
  end sourcesCanBeReconstructedFromTheAnalysis

  @TestFactory
  def matchingSectionsAreFound(): DynamicTests =
    case class TestPlan(
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

    // Test plan synthesis: start with a set of sequences, so these cannot match
    // each other ...
    val sequences =
      trialsApi
        .integers(1, 1000)
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
                // Each sequence has a unique first and last element amongst the
                // set of sequences; this prevents accidental overrun of the
                // expected matches.
                core
                  .prepended(1 + (2 * index) + maximumGlyphThatMightBeShared)
                  .appended((2 * (1 + index)) + maximumGlyphThatMightBeShared)
              }
              .toSet
          )
      )

    val testPlans: Trials[TestPlan] = setsOfSequences.flatMap(sequences =>
      trialsApi
        // ... split into seven sets ...
        .partitioning(sequences.toIndexedSeq, numberOfPartitions = 7)
        .flatMap {
          case Seq(
                // ... the first four sets are of sequences that are expected to
                // be matched across three or just two sides ...
                commonToAllThreeSides,
                commonToBaseAndLeft,
                commonToBaseAndRight,
                commonToLeftAndRight,
                // ... and the last three are specific to the three sides, so
                // they are unmatchable.
                uniqueToBase,
                uniqueToLeft,
                uniqueToRight
              ) =>
            val common =
              commonToAllThreeSides ++ commonToBaseAndLeft ++ commonToBaseAndRight ++ commonToLeftAndRight

            // Sanity check: it is possible to have accidental matches where a
            // common sequence is *embedded* within a unique sequence...
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
            // ... it is also possible to have accidental matches where a common
            // sequence for one match is *embedded* within a common sequence for
            // another match.
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
                      // common sequences; a unique sequence never abuts another
                      // unique sequence...
                      sideCommonSequencesRearrangements
                        .flatMap(sideCommonSequencesRearrangement =>
                          if sideCommonSequencesRearrangement.size >= sideUniqueSequences.size
                          then
                            val adjacentCommonSequencesArePossible =
                              // Pigeonhole principle: we have too many common
                              // sequences, so some must share the same slot
                              // that will be paired with a unique sequence.
                              sideCommonSequencesRearrangement.size > sideUniqueSequences.size
                            trialsApi
                              .nonEmptyPartitioning(
                                sideCommonSequencesRearrangement,
                                numberOfPartitions = sideUniqueSequences.size
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
                      // We don't have any unique sequences to put between the
                      // common sequences, so the latter may be adjacent if
                      // there's more than one of them.
                      sideCommonSequencesRearrangements.map(commonSequences =>
                        commonSequences -> (1 < commonSequences.size)
                      )
                    end if
                  else
                    // We don't have any common sequences at all, so none are
                    // adjacent.
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
                          // Sometimes we generate an empty side with no files.
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

    testPlans
      .withStrategy(caseSupplyCycle =>
        if caseSupplyCycle.isInitial then
          CasesLimitStrategy.timed(Duration.apply(15, TimeUnit.MINUTES))
        else CasesLimitStrategy.counted(200, 3.0)
      )
      .dynamicTests { testPlan =>

        import testPlan.*

        println(
          s"Minimum size fraction for motion detection: $minimumSizeFractionForMotionDetection"
        )
        println("Sizes of common to all three sides...")
        pprint.pprintln(commonToAllThreeSides.map(_.size))
        println("Sizes of common to base and left...")
        pprint.pprintln(commonToBaseAndLeft.map(_.size))
        println("Sizes of common to base and right...")
        pprint.pprintln(commonToBaseAndRight.map(_.size))
        println("Sizes of common to left and right...")
        pprint.pprintln(commonToLeftAndRight.map(_.size))

        try
          val Right(
            analysis: CodeMotionAnalysis[Path, Element]
          ) =
            CodeMotionAnalysis.of(
              baseSources,
              leftSources,
              rightSources
            )(
              minimumMatchSize = 5,
              thresholdSizeFractionForMatching =
                minimumSizeFractionForMotionDetection
            )(
              elementEquality = Eq[Element],
              elementOrder = Order[Element],
              elementFunnel = elementFunnel,
              hashFunction = Hashing.murmur3_32_fixed()
            ): @unchecked
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
            pprint.pprintln(overlappingSections)
            Trials.reject()
        end try
      }
  end matchingSectionsAreFound
end CodeMotionAnalysisTest

object CodeMotionAnalysisTest:
  type Path        = Int
  type Element     = Int
  type FakeSources = MappedContentSources[Path, Element]

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

  private def elementFunnel(
      element: Element,
      primitiveSink: PrimitiveSink
  ): Unit =
    primitiveSink.putInt(element)
  end elementFunnel

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
end CodeMotionAnalysisTest
