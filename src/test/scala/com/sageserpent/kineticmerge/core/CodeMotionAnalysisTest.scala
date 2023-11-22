package com.sageserpent.kineticmerge.core

import com.google.common.hash.{Hashing, PrimitiveSink}
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.americium.{Trials, TrialsApi}
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisTest.*
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import org.junit.jupiter.api.*
import org.rabinfingerprint.polynomial.Polynomial

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
            )(Polynomial.createIrreducible(15)): @unchecked

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

      def minimumSizeFractionForMotionDetection: Double =
        def numberOfElementsCoveredBy(
            allocations: IndexedSeq[Vector[FakeSources#Element]]*
        ): Int =
          allocations.map(_.map(_.size).sum).sum

        def minimumNumberOfElementsCoveredBy(
            allocations: IndexedSeq[Vector[FakeSources#Element]]*
        ): Int =
          allocations
            .map(_.map(_.size).minOption.getOrElse(1))
            .minOption
            .getOrElse(1)

        val minimumMatchableSize = minimumNumberOfElementsCoveredBy(
          commonToAllThreeSides,
          commonToBaseAndLeft,
          commonToBaseAndRight,
          commonToLeftAndRight
        )

        minimumMatchableSize.toDouble / (baseSources.maximumContentsSize max leftSources.maximumContentsSize max rightSources.maximumContentsSize)
      end minimumSizeFractionForMotionDetection

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
                .map(contentsByPath =>
                  new FakeSources(contentsByPath)
                    with SourcesContracts[Path, Element]
                )
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
      .dynamicTests { testPlan =>
        pprint.pprintln(testPlan)

        import testPlan.*

        val Right(
          analysis: CodeMotionAnalysis[FakeSources#Path, FakeSources#Element]
        ) =
          CodeMotionAnalysis.of(
            baseSources,
            leftSources,
            rightSources
          )(minimumSizeFractionForMotionDetection)(
            equality = _ == _,
            hashFunction = Hashing.murmur3_32_fixed(),
            funnel = funnel
          )(Polynomial.createIrreducible(15)): @unchecked
        end val

        def matches(sideFilesByPath: Map[Path, File[Element]]) =
          sideFilesByPath.values
            .flatMap(_.sections)
            .collect(Function.unlift(analysis.matchFor))

        def verify(
            matches: Iterable[Match[Section[FakeSources#Element]]]
        ): Unit =
          // NOTE: all of the cases below have to consider the possibility that
          // juxtaposition of sequences may create incidental match
          // opportunities that don't necessarily encompass the expected
          // matches. If so, we just reject the trial as inconclusive, but not
          // before we have done some mandatory validation beforehand.
          matches.foreach {
            case Match.AllThree(baseSection, leftSection, rightSection) =>
              assert(analysis.base.values.exists(_ == baseSection))
              assert(analysis.left.values.exists(_ == leftSection))
              assert(analysis.right.values.exists(_ == rightSection))

              assert(leftSection == baseSection)
              assert(rightSection == baseSection)

              if !commonToAllThreeSides
                  .exists(baseSection.content containsSlice _)
              then Trials.reject()
              end if

            case Match.BaseAndLeft(baseSection, leftSection) =>
              assert(analysis.base.values.exists(_ == baseSection))
              assert(analysis.left.values.exists(_ == leftSection))

              assert(leftSection == baseSection)

              if !commonToBaseAndLeft.exists(
                  baseSection.content containsSlice _
                )
              then Trials.reject()
              end if

            case Match.BaseAndRight(baseSection, rightSection) =>
              assert(analysis.base.values.exists(_ == baseSection))
              assert(analysis.right.values.exists(_ == rightSection))

              assert(rightSection == baseSection)

              if !commonToBaseAndRight.exists(
                  baseSection.content containsSlice _
                )
              then Trials.reject()
              end if

            case Match.LeftAndRight(leftSection, rightSection) =>
              assert(analysis.left.values.exists(_ == leftSection))
              assert(analysis.right.values.exists(_ == rightSection))

              assert(rightSection == leftSection)

              if !commonToBaseAndLeft.exists(
                  leftSection.content containsSlice _
                )
              then Trials.reject()
              end if
          }

        val baseMatches: Iterable[Match[Section[FakeSources#Element]]] =
          matches(analysis.base)

        if commonToAllThreeSides.nonEmpty || commonToBaseAndLeft.nonEmpty || commonToBaseAndRight.nonEmpty
        then assert(baseMatches.nonEmpty)
        end if

        verify(baseMatches)

        val leftMatches: Iterable[Match[Section[FakeSources#Element]]] =
          matches(analysis.left)

        if commonToAllThreeSides.nonEmpty || commonToBaseAndLeft.nonEmpty || commonToLeftAndRight.nonEmpty
        then assert(leftMatches.nonEmpty)
        end if

        verify(leftMatches)

        val rightMatches: Iterable[Match[Section[FakeSources#Element]]] =
          matches(analysis.right)

        if commonToAllThreeSides.nonEmpty || commonToBaseAndRight.nonEmpty || commonToLeftAndRight.nonEmpty
        then assert(rightMatches.nonEmpty)
        end if

        verify(rightMatches)
      }
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

  trait SourcesContracts[Path, Element] extends Sources[Path, Element]:
    abstract override def section(
        path: SourcesContracts.this.Path
    )(startOffset: Int, size: Int): Section[SourcesContracts.this.Element] =
      val result = super.section(path)(startOffset, size)

      assert(pathFor(result) == path)

      result
    end section

    abstract override def filesByPathUtilising(
        sections: Set[Section[SourcesContracts.this.Element]]
    ): Map[SourcesContracts.this.Path, File[Element]] =
      val result = super.filesByPathUtilising(sections)

      sections.foreach(section =>
        assert(result(pathFor(section)).sections.exists(section == _))
      )

      assert(result.keys == paths)

      result
    end filesByPathUtilising
  end SourcesContracts

  case class FakeSources(contentsByPath: Map[Path, IndexedSeq[Element]])
      extends Sources[Path, Element]:
    override def paths: Set[Path] = contentsByPath.keySet

    override def section(path: Path)(
        startOffset: CodeMotionAnalysisTest.Path,
        size: CodeMotionAnalysisTest.Path
    ): Section[Element] = ???

    override def pathFor(section: Section[Element]): Path = ???

    override def filesByPathUtilising(
        sections: Set[Section[Element]]
    ): Map[Path, File[Element]] =
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

    def maximumContentsSize: Int =
      contentsByPath.values.map(_.size).maxOption.getOrElse(0)

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
