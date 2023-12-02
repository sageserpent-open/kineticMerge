package com.sageserpent.kineticmerge.core

import com.google.common.hash.{Hashing, PrimitiveSink}
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.java.CasesLimitStrategy
import com.sageserpent.americium.junit5.*
import com.sageserpent.americium.{Trials, TrialsApi}
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisTest.*
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import org.junit.jupiter.api.*
import org.rabinfingerprint.polynomial.Polynomial

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

class CodeMotionAnalysisTest:
  val minimumSizeFractionTrials: Trials[Double] =
    trialsApi.doubles(0.1, 1)

  val contentTrials: Trials[Vector[FakeSources#Element]] = trialsApi
    .integers(0, 100 /*TODO: reinstate the old value of: 10000*/ )
    .flatMap(textSize =>
      trialsApi
        .integers(lowerBound = 1, upperBound = 20)
        .lotsOfSize[Vector[Path]](textSize)
    )

  val pathTrials: Trials[FakeSources#Path] = trialsApi
    .integers(1, 1000)

  val sourcesTrials: Trials[FakeSources] =
    pathTrials
      .maps(contentTrials)
      .filter(_.nonEmpty)
      .map(FakeSources.apply)

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
        pprint.pprintln(
          testPlan -> testPlan.minimumSizeFractionForMotionDetection
        )

        import testPlan.*

        try
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
            )(irreduciblePolynomial()): @unchecked
          end val

          extension (file: File[Element])
            def contains(section: Section[Element]) =
              file.sections.contains(section)

          val baseMatches: Iterable[Match[Section[FakeSources#Element]]] =
            analysis.base.values
              .flatMap(_.sections)
              .collect(Function.unlift(analysis.matchForBaseSection))

          if commonToAllThreeSides.nonEmpty || commonToBaseAndLeft.nonEmpty || commonToBaseAndRight.nonEmpty
          then
            assert(baseMatches.nonEmpty)

            val survivorsCommonToAllThreeSides =
              collection.mutable.Set[IndexedSeq[Element]](
                commonToAllThreeSides*
              )
            val survivorsCommonToBaseAndLeft =
              collection.mutable.Set[IndexedSeq[Element]](commonToBaseAndLeft*)
            val survivorsCommonToBaseAndRight =
              collection.mutable.Set[IndexedSeq[Element]](commonToBaseAndRight*)

            baseMatches.foreach {
              case Match.AllThree(baseSection, leftSection, rightSection) =>
                assert(analysis.base.values.exists(_ contains baseSection))
                assert(analysis.left.values.exists(_ contains leftSection))
                assert(analysis.right.values.exists(_ contains rightSection))

                assert(leftSection.content == baseSection.content)
                assert(rightSection.content == baseSection.content)

                survivorsCommonToAllThreeSides.filterInPlace(candidate =>
                  !(baseSection.content containsSlice candidate)
                )

              case Match.BaseAndLeft(baseSection, leftSection) =>
                assert(analysis.base.values.exists(_ contains baseSection))
                assert(analysis.left.values.exists(_ contains leftSection))

                assert(leftSection.content == baseSection.content)

                survivorsCommonToBaseAndLeft.filterInPlace(candidate =>
                  !(baseSection.content containsSlice candidate)
                )

              case Match.BaseAndRight(baseSection, rightSection) =>
                assert(analysis.base.values.exists(_ contains baseSection))
                assert(analysis.right.values.exists(_ contains rightSection))

                assert(rightSection.content == baseSection.content)

                survivorsCommonToBaseAndRight.filterInPlace(candidate =>
                  !(baseSection.content containsSlice candidate)
                )

              case Match.LeftAndRight(leftSection, rightSection) =>
            }

            assert(survivorsCommonToAllThreeSides.isEmpty)
            assert(survivorsCommonToBaseAndLeft.isEmpty)
            assert(survivorsCommonToBaseAndRight.isEmpty)

          end if

          val leftMatches: Iterable[Match[Section[FakeSources#Element]]] =
            analysis.left.values
              .flatMap(_.sections)
              .collect(Function.unlift(analysis.matchForLeftSection))

          if commonToAllThreeSides.nonEmpty || commonToBaseAndLeft.nonEmpty || commonToLeftAndRight.nonEmpty
          then
            assert(leftMatches.nonEmpty)

            val survivorsCommonToAllThreeSides =
              collection.mutable.Set[IndexedSeq[Element]](
                commonToAllThreeSides*
              )
            val survivorsCommonToBaseAndLeft =
              collection.mutable.Set[IndexedSeq[Element]](commonToBaseAndLeft*)
            val survivorsCommonToLeftAndRight =
              collection.mutable.Set[IndexedSeq[Element]](commonToLeftAndRight*)

            leftMatches.foreach {
              case Match.AllThree(baseSection, leftSection, rightSection) =>
                assert(analysis.base.values.exists(_ contains baseSection))
                assert(analysis.left.values.exists(_ contains leftSection))
                assert(analysis.right.values.exists(_ contains rightSection))

                assert(leftSection.content == baseSection.content)
                assert(rightSection.content == baseSection.content)

                survivorsCommonToAllThreeSides.filterInPlace(candidate =>
                  !(baseSection.content containsSlice candidate)
                )

              case Match.BaseAndLeft(baseSection, leftSection) =>
                assert(analysis.base.values.exists(_ contains baseSection))
                assert(analysis.left.values.exists(_ contains leftSection))

                assert(leftSection.content == baseSection.content)

                survivorsCommonToBaseAndLeft.filterInPlace(candidate =>
                  !(baseSection.content containsSlice candidate)
                )

              case Match.BaseAndRight(baseSection, rightSection) =>

              case Match.LeftAndRight(leftSection, rightSection) =>
                assert(analysis.left.values.exists(_ contains leftSection))
                assert(analysis.right.values.exists(_ contains rightSection))

                assert(rightSection.content == leftSection.content)

                survivorsCommonToLeftAndRight.filterInPlace(candidate =>
                  !(leftSection.content containsSlice candidate)
                )
            }

            assert(survivorsCommonToAllThreeSides.isEmpty)
            assert(survivorsCommonToBaseAndLeft.isEmpty)
            assert(survivorsCommonToLeftAndRight.isEmpty)
          end if

          val rightMatches: Iterable[Match[Section[FakeSources#Element]]] =
            analysis.right.values
              .flatMap(_.sections)
              .collect(Function.unlift(analysis.matchForRightSection))

          if commonToAllThreeSides.nonEmpty || commonToBaseAndRight.nonEmpty || commonToLeftAndRight.nonEmpty
          then
            assert(rightMatches.nonEmpty)

            val survivorsCommonToAllThreeSides =
              collection.mutable.Set[IndexedSeq[Element]](
                commonToAllThreeSides*
              )
            val survivorsCommonToBaseAndRight =
              collection.mutable.Set[IndexedSeq[Element]](commonToBaseAndRight*)
            val survivorsCommonToLeftAndRight =
              collection.mutable.Set[IndexedSeq[Element]](commonToLeftAndRight*)

            rightMatches.foreach {
              case Match.AllThree(baseSection, leftSection, rightSection) =>
                assert(analysis.base.values.exists(_ contains baseSection))
                assert(analysis.left.values.exists(_ contains leftSection))
                assert(analysis.right.values.exists(_ contains rightSection))

                assert(leftSection.content == baseSection.content)
                assert(rightSection.content == baseSection.content)

                survivorsCommonToAllThreeSides.filterInPlace(candidate =>
                  !(baseSection.content containsSlice candidate)
                )

              case Match.BaseAndLeft(baseSection, leftSection) =>

              case Match.BaseAndRight(baseSection, rightSection) =>
                assert(analysis.base.values.exists(_ contains baseSection))
                assert(analysis.right.values.exists(_ contains rightSection))

                assert(rightSection.content == baseSection.content)

                survivorsCommonToBaseAndRight.filterInPlace(candidate =>
                  !(baseSection.content containsSlice candidate)
                )

              case Match.LeftAndRight(leftSection, rightSection) =>
                assert(analysis.left.values.exists(_ contains leftSection))
                assert(analysis.right.values.exists(_ contains rightSection))

                assert(rightSection.content == leftSection.content)

                survivorsCommonToLeftAndRight.filterInPlace(candidate =>
                  !(leftSection.content containsSlice candidate)
                )
            }

            assert(survivorsCommonToAllThreeSides.isEmpty)
            assert(survivorsCommonToBaseAndRight.isEmpty)
            assert(survivorsCommonToLeftAndRight.isEmpty)
          end if

        catch
          case overlappingSections: OverlappingSections =>
            pprint.pprintln(overlappingSections)
            Trials.reject()
        end try
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

  class OverlappingSections(
      path: Path,
      first: Section[Element],
      second: Section[Element]
  ) extends RuntimeException(
        s"Overlapping section detected at path: $path: $first (content: ${first.content}) overlaps with start of section: $second (content: ${second.content})."
      )

  case class FakeSources(contentsByPath: Map[Path, IndexedSeq[Element]])
      extends Sources[Path, Element]:
    override def filesByPathUtilising(
        mandatorySections: Set[Section[Element]]
    ): Map[Path, File[Element]] =
      val sectionsByPath = mandatorySections.groupBy(pathFor)

      contentsByPath.map { case (path, content) =>
        val pertinentSections = sectionsByPath.get(path).getOrElse(Set.empty)

        path -> File(
          if pertinentSections.nonEmpty then
            val sectionsInStartOffsetOrder =
              pertinentSections.toSeq.sortBy(_.startOffset)

            sectionsInStartOffsetOrder
              .zip(sectionsInStartOffsetOrder.tail)
              .foreach((first, second) =>
                if first.onePastEndOffset > second.startOffset then
                  throw new OverlappingSections(path, first, second)
              )

            val (onePastLastEndOffset, contiguousSections) =
              sectionsInStartOffsetOrder.foldLeft(
                0 -> Vector.empty[Section[Element]]
              ) { case ((onePastLastEndOffset, partialResult), section) =>
                section.onePastEndOffset ->
                  ((if onePastLastEndOffset < section.startOffset then
                      partialResult :+ this.section(path)(
                        onePastLastEndOffset,
                        section.startOffset - onePastLastEndOffset
                      )
                    else partialResult) :+ section)
              }

            if content.size > onePastLastEndOffset then
              contiguousSections :+ section(path)(
                onePastLastEndOffset,
                content.size - onePastLastEndOffset
              )
            else contiguousSections
            end if
          else
            Vector(
              SectionImplementation(
                path = path,
                startOffset = 0,
                size = content.length
              )
            )
        )
      }
    end filesByPathUtilising

    override def section(path: Path)(
        startOffset: CodeMotionAnalysisTest.Path,
        size: CodeMotionAnalysisTest.Path
    ): Section[Element] = SectionImplementation(path, startOffset, size)

    override def pathFor(section: Section[Element]): Path =
      section match
        // If the section implementation does not come from this `FakeSources`,
        // then it can't be accepted, it's up to the client to be consistent.
        case SectionImplementation(path, _, _)
            if contentsByPath.contains(path) =>
          path

    override def paths: Set[Path] = contentsByPath.keySet

    /** Specific to testing, not part of the [[Sources]] API implementation.
      */
    def contains(sectionContent: IndexedSeq[Element]): Boolean =
      contentsByPath.values.exists(_ containsSlice sectionContent)

    /** Specific to testing, not part of the [[Sources]] API implementation.
      */
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
