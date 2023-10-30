package com.sageserpent.kineticmerge.core

import com.google.common.hash.{Hashing, PrimitiveSink}
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.java.junit5.ConfiguredTrialsTest
import com.sageserpent.americium.java.{CasesLimitStrategy, TrialsScaffolding as JavaTrialsScaffolding}
import com.sageserpent.americium.junit5.*
import com.sageserpent.americium.{Trials, TrialsApi, TrialsScaffolding}
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisTest.{FakeSources, funnel, nonEmptyPartitioning, partitioning}
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import org.junit.jupiter.api.*
import org.junit.jupiter.api.DynamicTest.dynamicTest
import org.junit.jupiter.api.TestInstance.Lifecycle
import org.opentest4j.TestAbortedException

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.jdk.CollectionConverters.*

class CodeMotionAnalysisTest:
  val minimumSizeFractionTrials: Trials[Double] =
    trialsApi.doubles(0.1, 1)

  val sourcesTrials: Trials[FakeSources] =
    for
      textSize <- trialsApi.integers(0, 10000)
      textsByPath <- trialsApi
        .integers(1, 1000)
        .maps(
          trialsApi
            .integers(lowerBound = 1, upperBound = 20)
            .lotsOfSize[Vector[Int]](textSize)
        )
        .filter(_.nonEmpty)
    yield FakeSources(textsByPath)

  @TestFactory
  def sourcesCanBeReconstructedFromTheAnalysis: DynamicTests =
    extension (results: Map[FakeSources#Path, File[FakeSources#Element]])
      private infix def matches(sources: FakeSources): Unit =
        assert(results.keys == sources.filesByPath.keys)

        results.foreach { case (path, result) =>
          assert(result.content == sources.filesByPath(path).content)
        }
      end matches
    end extension

    (sourcesTrials and sourcesTrials and sourcesTrials and minimumSizeFractionTrials)
      .withLimit(100)
      .dynamicTests(
        (
            base: FakeSources,
            left: FakeSources,
            right: FakeSources,
            minimumSizeFraction: Double
        ) =>
          val Right(
            analysis: CodeMotionAnalysis[FakeSources#Path, FakeSources#Element]
          ) =
            CodeMotionAnalysis.of(base, left, right)(
              minimumSizeFraction
            )(
              equality = _ == _,
              hashFunction = Hashing.murmur3_32_fixed(),
              funnel = funnel
            ): @unchecked

          analysis.base matches base
          analysis.left matches left
          analysis.right matches right
      )
  end sourcesCanBeReconstructedFromTheAnalysis

  @TestFactory
  def matchingSectionsAreFound(): DynamicTests =
    case class TestPlan(
        commonToAllThreeSides: IndexedSeq[Vector[FakeSources#Element]],
        commonToBaseAndLeft: IndexedSeq[Vector[FakeSources#Element]],
        commonToBaseAndRight: IndexedSeq[Vector[FakeSources#Element]],
        commonToLeftAndRight: IndexedSeq[Vector[FakeSources#Element]],
        uniqueToBase: IndexedSeq[Vector[FakeSources#Element]],
        uniqueToLeft: IndexedSeq[Vector[FakeSources#Element]],
        uniqueToRight: IndexedSeq[Vector[FakeSources#Element]],
        baseSources: FakeSources,
        leftSources: FakeSources,
        rightSources: FakeSources
    ):
      commonToAllThreeSides.foreach { section =>
        require(baseSources.contains(section))
        require(leftSources.contains(section))
        require(rightSources.contains(section))
      }

      commonToBaseAndLeft.foreach { section =>
        require(baseSources.contains(section))
        require(leftSources.contains(section))
      }

      commonToBaseAndRight.foreach { section =>
        require(baseSources.contains(section))
        require(rightSources.contains(section))
      }

      commonToLeftAndRight.foreach { section =>
        require(leftSources.contains(section))
        require(rightSources.contains(section))
      }

      uniqueToBase.foreach { section => require(baseSources.contains(section)) }
      uniqueToLeft.foreach { section => require(leftSources.contains(section)) }
      uniqueToRight.foreach { section =>
        require(rightSources.contains(section))
      }
    end TestPlan

    val alphabet = 1 to 20

    // Test plan synthesis: start with a set of sequences, so these cannot match
    // each other ...
    val sequences =
      trialsApi
        .integers(1, 10)
        .flatMap(sequenceLength =>
          trialsApi
            .choose(alphabet)
            .lotsOfSize[Vector[FakeSources#Element]](sequenceLength)
        )

    val setsOfSequences = trialsApi
      .integers(4, 30)
      .flatMap(numberOfSequences =>
        sequences
          .lotsOfSize[Set[Vector[FakeSources#Element]]](numberOfSequences)
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
            def sourcesForASide(
                commonToAllThreeSides: IndexedSeq[Vector[FakeSources#Element]],
                commonToOnePairOfSides: IndexedSeq[Vector[FakeSources#Element]],
                commonToTheOtherPairOfSides: IndexedSeq[
                  Vector[FakeSources#Element]
                ],
                sideUniqueSequences: IndexedSeq[Vector[FakeSources#Element]]
            ): Trials[FakeSources] =
              val sequenceMixtures
                  : Trials[IndexedSeq[Vector[FakeSources#Element]]] =
                if commonToAllThreeSides.nonEmpty || commonToOnePairOfSides.nonEmpty || commonToTheOtherPairOfSides.nonEmpty
                then
                  // Mix up the three-side and two-side matches....
                  val sideCommonSequencesRearrangements =
                    trialsApi.pickAlternatelyFrom(
                      shrinkToRoundRobin = false,
                      commonToAllThreeSides ++ commonToOnePairOfSides ++ commonToTheOtherPairOfSides
                    )

                  if sideUniqueSequences.nonEmpty then
                    // ...intersperse unique sequences between chunks of common
                    // sequences; a unique sequence never abuts another unique
                    // sequence...
                    sideCommonSequencesRearrangements
                      .flatMap(sideCommonSequencesRearrangement =>
                        if sideCommonSequencesRearrangement.size >= sideUniqueSequences.size
                        then
                          trialsApi.nonEmptyPartitioning(
                            sideCommonSequencesRearrangement,
                            numberOfPartitions = sideUniqueSequences.size
                          )
                        else trialsApi.impossible
                      )
                      .map(commonSequencesInChunks =>
                        commonSequencesInChunks
                          .zip(sideUniqueSequences)
                          .flatMap {
                            case (chunkOfCommonSequences, uniqueSequence) =>
                              chunkOfCommonSequences :+ uniqueSequence
                          }
                          .toIndexedSeq
                      )
                  else sideCommonSequencesRearrangements
                  end if
                else trialsApi.only(sideUniqueSequences)

              // ... finally, allocate the sequences in groups to paths.

              sequenceMixtures
                .flatMap(sequenceMixture =>
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
                )
                .map(_.zipWithIndex)
                .map(_.map { case (sequences, path) =>
                  val fileContents = sequences.flatten
                  path -> fileContents
                }.toMap)
                .map(FakeSources.apply)
            end sourcesForASide

            for
              baseSources <- sourcesForASide(
                commonToAllThreeSides,
                commonToBaseAndLeft,
                commonToBaseAndRight,
                uniqueToBase
              )
              leftSources <- sourcesForASide(
                commonToAllThreeSides,
                commonToBaseAndLeft,
                commonToLeftAndRight,
                uniqueToLeft
              )
              rightSources <- sourcesForASide(
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
              rightSources
            )
            end for
        }
    )

    testPlans
      .withLimit(200)
      .dynamicTests(testPlan => pprint.pprintln(testPlan))
  end matchingSectionsAreFound
end CodeMotionAnalysisTest

object CodeMotionAnalysisTest:
  type Path    = Int
  type Element = Int

  private def funnel(element: Int, primitiveSink: PrimitiveSink): Unit =
    primitiveSink.putInt(element)
  end funnel

  extension (trialsApi: TrialsApi)
    def nonEmptyPartitioning[Element](
        things: IndexedSeq[Element],
        numberOfPartitions: Int
    ): Trials[Seq[IndexedSeq[Element]]] =
      require(0 < numberOfPartitions)

      val chunkSizeVectors =
        val numberOfThings = things.size

        val partitionPointIndexVectors = trialsApi.indexCombinations(
          numberOfIndices = numberOfThings,
          combinationSize = numberOfPartitions - 1
        )

        partitionPointIndexVectors.map(partitionPointIndices =>
          (0 +: partitionPointIndices)
            .zip(partitionPointIndices :+ numberOfThings)
            .map { case (partitionStartIndex, onePastPartitionEndIndex) =>
              onePastPartitionEndIndex - partitionStartIndex
            }
        )
      end chunkSizeVectors

      chunkSizeVectors.map(chunkSizes => thingsInChunks(chunkSizes, things))
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
              .integers(lowerBound = 0, upperBound = numberOfThings - 1)
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

  case class FakeSources(contentsByPath: Map[Path, IndexedSeq[Element]])
      extends Sources[Path, Element]:
    override def filesByPath: Map[Path, File[Element]] =
      contentsByPath.map { case (path, content) =>
        path -> File(
          Vector(
            SectionImplementation(
              path = path,
              startOffset = 0,
              size = content.length
            )
          )
        )
      }

    def contains(section: IndexedSeq[Element]): Boolean =
      contentsByPath.values.exists(_ containsSlice section)

    case class SectionImplementation(
        path: Path,
        override val startOffset: Int,
        override val size: Int
    ) extends Section[Element]:
      override def content: IndexedSeq[Element] =
        contentsByPath(path).slice(startOffset, onePastEndOffset)
    end SectionImplementation

  end FakeSources

end CodeMotionAnalysisTest
