package com.sageserpent.kineticmerge.core

import cats.Order
import cats.syntax.functor.*
import com.google.common.hash.{HashFunction, Hashing}
import com.sageserpent.americium.Trials
import com.sageserpent.americium.junit5.{DynamicTests, Syntax}
import com.sageserpent.kineticmerge.core.BlockDuplicationAndCondensationTests.{
  *,
  given
}
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.{
  Contribution,
  Sized,
  defaultElementSize
}
import com.sageserpent.kineticmerge.core.MatchAnalysis.Configuration
import com.sageserpent.kineticmerge.core.SectionedCodeExtension.longestCommonSubsequenceOf
import com.sageserpent.kineticmerge.core.SectionedCodeTest.{
  Element,
  FakeSources,
  Path,
  given_Funnel_Element
}
import com.sageserpent.kineticmerge.{NoProgressRecording, ProgressRecording}
import org.junit.jupiter.api.{Disabled, TestFactory}

object BlockDuplicationAndCondensationTests:
  given HashFunction = Hashing.murmur3_32_fixed()

  given ProgressRecording       = NoProgressRecording
  given Order[Section[Element]] =
    Order.by[Section[Element], Seq[Element]](_.content)

  given Sized[Section[Element]] = defaultElementSize

  extension [X](longestCommonSubsequence: LongestCommonSubsequence[X])
    def adaptedForMirroring(mirrored: Boolean): LongestCommonSubsequence[X] =
      if mirrored then longestCommonSubsequence.mirror
      else longestCommonSubsequence
  end extension

  extension (contributions: IndexedSeq[Contribution[Section[Element]]])
    def asElementContributions: IndexedSeq[Contribution[IndexedSeq[Element]]] =
      contributions.map(_.map(_.content))
  end extension
end BlockDuplicationAndCondensationTests

class BlockDuplicationAndCondensationTests:
  @TestFactory
  def aBlockIsDuplicatedOnOneSide(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 1,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val placeholderPath: Path = 1

      val blockContent = Vector(1, 2, 3)

      val baseElements: IndexedSeq[Element] = blockContent

      val baseSources = FakeSources(
        contentsByPath = Map(placeholderPath -> baseElements),
        label = "base"
      )

      val elementsOnSideWithoutChanges: IndexedSeq[Element]  = baseElements
      val elementsOnSideWithDuplication: IndexedSeq[Element] =
        blockContent ++ blockContent

      val leftSources = FakeSources(
        contentsByPath = Map(
          placeholderPath -> (if mirrorImage then elementsOnSideWithDuplication
                              else elementsOnSideWithoutChanges)
        ),
        label = "left"
      )

      val rightSources = FakeSources(
        contentsByPath = Map(
          placeholderPath -> (if mirrorImage then elementsOnSideWithoutChanges
                              else elementsOnSideWithDuplication)
        ),
        label = "right"
      )

      val Right(sectionedCode) = SectionedCode.of(
        baseSources = baseSources,
        leftSources = leftSources,
        rightSources = rightSources
      )(configuration): @unchecked

      val LongestCommonSubsequence(
        baseContributions,
        contributionsOnSideWithoutChanges,
        contributionsOnSideWithDuplication,
        _,
        _,
        _,
        _
      ) = sectionedCode
        .longestCommonSubsequenceOf(path = placeholderPath)
        .adaptedForMirroring(mirrorImage)

      println(s"Base contributions: ${pprintCustomised(baseContributions)}")
      println(
        s"Side without changes contributions: ${pprintCustomised(contributionsOnSideWithoutChanges)}"
      )
      println(
        s"Side with duplication contributions: ${pprintCustomised(contributionsOnSideWithDuplication)}"
      )

      assert(
        Vector(
          Contribution.Common(blockContent)
        ) == baseContributions.asElementContributions
      )
      assert(
        Vector(
          Contribution.Common(blockContent)
        ) == contributionsOnSideWithoutChanges.asElementContributions
      )
      assert(
        Vector(
          Contribution.Common(blockContent),
          Contribution.Difference(blockContent)
        ) == contributionsOnSideWithDuplication.asElementContributions
      )
    }
  end aBlockIsDuplicatedOnOneSide

  @TestFactory
  def aBlockIsDuplicatedOnTwoSides(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 1,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val placeholderPath: Path = 1

      val blockContent = Vector(1, 2, 3)

      val baseElements: IndexedSeq[Element] = blockContent

      val baseSources = FakeSources(
        contentsByPath = Map(placeholderPath -> baseElements),
        label = "base"
      )

      val elementsOnSidesWithDuplication: IndexedSeq[Element] =
        blockContent ++ blockContent

      val leftSources = FakeSources(
        contentsByPath = Map(
          placeholderPath -> elementsOnSidesWithDuplication
        ),
        label = "left"
      )

      val rightSources = FakeSources(
        contentsByPath = Map(
          placeholderPath -> elementsOnSidesWithDuplication
        ),
        label = "right"
      )

      val Right(sectionedCode) = SectionedCode.of(
        baseSources = baseSources,
        leftSources = leftSources,
        rightSources = rightSources
      )(configuration): @unchecked

      val LongestCommonSubsequence(
        baseContributions,
        contributionsOnOneSideWithDuplication,
        contributionsOnTheOtherSideWithDuplication,
        _,
        _,
        _,
        _
      ) = sectionedCode
        .longestCommonSubsequenceOf(path = placeholderPath)
        .adaptedForMirroring(mirrorImage)

      println(s"Base contributions: ${pprintCustomised(baseContributions)}")
      println(
        s"One side with duplication contributions: ${pprintCustomised(contributionsOnOneSideWithDuplication)}"
      )
      println(
        s"The other side with duplication contributions: ${pprintCustomised(contributionsOnTheOtherSideWithDuplication)}"
      )

      assert(
        Vector(
          Contribution.Common(blockContent)
        ) == baseContributions.asElementContributions
      )
      assert(
        Vector(
          Contribution.Common(blockContent),
          Contribution.CommonToLeftAndRightOnly(blockContent)
        ) == contributionsOnOneSideWithDuplication.asElementContributions
      )
      assert(
        Vector(
          Contribution.Common(blockContent),
          Contribution.CommonToLeftAndRightOnly(blockContent)
        ) == contributionsOnTheOtherSideWithDuplication.asElementContributions
      )
    }
  end aBlockIsDuplicatedOnTwoSides

  @TestFactory
  def duplicateBlocksAreMergedOnOneSide(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 1,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val placeholderPath: Path = 1

      val blockContent = Vector(1, 2, 3)

      val elementsOnBaseWithDuplication: IndexedSeq[Element] =
        blockContent ++ blockContent

      val baseElements: IndexedSeq[Element] = elementsOnBaseWithDuplication

      val baseSources = FakeSources(
        contentsByPath = Map(placeholderPath -> baseElements),
        label = "base"
      )

      val elementsOnSideWithoutChanges: IndexedSeq[Element] =
        elementsOnBaseWithDuplication
      val elementsOnSideWithoutDuplication: IndexedSeq[Element] = blockContent

      val leftSources = FakeSources(
        contentsByPath = Map(
          placeholderPath -> (if mirrorImage then
                                elementsOnSideWithoutDuplication
                              else elementsOnSideWithoutChanges)
        ),
        label = "left"
      )

      val rightSources = FakeSources(
        contentsByPath = Map(
          placeholderPath -> (if mirrorImage then elementsOnSideWithoutChanges
                              else elementsOnSideWithoutDuplication)
        ),
        label = "right"
      )

      val Right(sectionedCode) = SectionedCode.of(
        baseSources = baseSources,
        leftSources = leftSources,
        rightSources = rightSources
      )(configuration): @unchecked

      val LongestCommonSubsequence(
        baseContributionsWithDuplication,
        contributionsOnSideWithoutChanges,
        contributionsOnTheOtherSideWithoutDuplication,
        _,
        _,
        _,
        _
      ) = sectionedCode
        .longestCommonSubsequenceOf(path = placeholderPath)
        .adaptedForMirroring(mirrorImage)

      println(
        s"Base contributions: ${pprintCustomised(baseContributionsWithDuplication)}"
      )
      println(
        s"Side without changes contributions: ${pprintCustomised(contributionsOnSideWithoutChanges)}"
      )
      println(
        s"The other side with merged contributions: ${pprintCustomised(contributionsOnTheOtherSideWithoutDuplication)}"
      )

      assert(
        Vector(
          Contribution.Common(blockContent),
          Contribution.CommonToBaseAndLeftOnly(blockContent)
        ) == baseContributionsWithDuplication.asElementContributions
      )
      assert(
        Vector(
          Contribution.Common(blockContent),
          Contribution.CommonToBaseAndLeftOnly(blockContent)
        ) == contributionsOnSideWithoutChanges.asElementContributions
      )
      assert(
        Vector(
          Contribution.Common(blockContent)
        ) == contributionsOnTheOtherSideWithoutDuplication.asElementContributions
      )
    }
  end duplicateBlocksAreMergedOnOneSide

  @TestFactory
  def overlappingBlocksAreSeparatedOnOneSide(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 1,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val placeholderPath: Path = 1

      val leadingContent  = Vector(1, 2)
      val overlapContent  = Vector(3, 4)
      val trailingContent = Vector(5, 6)

      val baseElements: IndexedSeq[Element] =
        leadingContent ++ overlapContent ++ trailingContent

      val baseSources = FakeSources(
        contentsByPath = Map(placeholderPath -> baseElements),
        label = "base"
      )

      val elementsOnSideWithoutChanges: IndexedSeq[Element] = baseElements
      val elementsOnSideWithSeparation: IndexedSeq[Element] =
        leadingContent ++ overlapContent ++ overlapContent ++ trailingContent

      val leftSources = FakeSources(
        contentsByPath = Map(
          placeholderPath -> (if mirrorImage then elementsOnSideWithSeparation
                              else elementsOnSideWithoutChanges)
        ),
        label = "left"
      )

      val rightSources = FakeSources(
        contentsByPath = Map(
          placeholderPath -> (if mirrorImage then elementsOnSideWithoutChanges
                              else elementsOnSideWithSeparation)
        ),
        label = "right"
      )

      val Right(sectionedCode) = SectionedCode.of(
        baseSources = baseSources,
        leftSources = leftSources,
        rightSources = rightSources
      )(configuration): @unchecked

      val LongestCommonSubsequence(
        baseContributions,
        contributionsOnSideWithoutChanges,
        contributionsOnSideWithSeparation,
        _,
        _,
        _,
        _
      ) = sectionedCode
        .longestCommonSubsequenceOf(path = placeholderPath)
        .adaptedForMirroring(mirrorImage)

      println(
        s"Base overlapped contributions: ${pprintCustomised(baseContributions)}"
      )
      println(
        s"Side without changes contributions: ${pprintCustomised(contributionsOnSideWithoutChanges)}"
      )
      println(
        s"Side with separated contributions: ${pprintCustomised(contributionsOnSideWithSeparation)}"
      )

      assert(
        Vector(
          Contribution.Common(leadingContent),
          Contribution.Common(overlapContent),
          Contribution.Common(trailingContent)
        ) == baseContributions.asElementContributions
      )
      assert(
        Vector(
          Contribution.Common(leadingContent),
          Contribution.Common(overlapContent),
          Contribution.Common(trailingContent)
        ) == contributionsOnSideWithoutChanges.asElementContributions
      )
      assert(
        Vector(
          Contribution.Common(leadingContent),
          Contribution.Common(overlapContent),
          Contribution.Difference(overlapContent),
          Contribution.Common(trailingContent)
        ) == contributionsOnSideWithSeparation.asElementContributions
      )
    }
  end overlappingBlocksAreSeparatedOnOneSide

  @TestFactory
  def overlappingBlocksAreSeparatedAndSwappedAroundOnOneSide(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 1,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val placeholderPath: Path = 1

      val leadingContent  = Vector(1, 2)
      val overlapContent  = Vector(3, 4)
      val trailingContent = Vector(5, 6)

      val baseElements: IndexedSeq[Element] =
        leadingContent ++ overlapContent ++ trailingContent

      val baseSources = FakeSources(
        contentsByPath = Map(placeholderPath -> baseElements),
        label = "base"
      )

      val elementsOnSideWithoutChanges: IndexedSeq[Element] = baseElements
      val elementsOnSideWithSeparationAndSwapping: IndexedSeq[Element] =
        overlapContent ++ trailingContent ++ leadingContent ++ overlapContent

      val leftSources = FakeSources(
        contentsByPath = Map(
          placeholderPath -> (if mirrorImage then
                                elementsOnSideWithSeparationAndSwapping
                              else elementsOnSideWithoutChanges)
        ),
        label = "left"
      )

      val rightSources = FakeSources(
        contentsByPath = Map(
          placeholderPath -> (if mirrorImage then elementsOnSideWithoutChanges
                              else elementsOnSideWithSeparationAndSwapping)
        ),
        label = "right"
      )

      val Right(sectionedCode) = SectionedCode.of(
        baseSources = baseSources,
        leftSources = leftSources,
        rightSources = rightSources
      )(configuration): @unchecked

      val LongestCommonSubsequence(
        baseContributions,
        contributionsOnSideWithoutChanges,
        contributionsOnSideWithSeparationAndSwapping,
        _,
        _,
        _,
        _
      ) = sectionedCode
        .longestCommonSubsequenceOf(path = placeholderPath)
        .adaptedForMirroring(mirrorImage)

      println(
        s"Base overlapped contributions: ${pprintCustomised(baseContributions)}"
      )
      println(
        s"Side without changes contributions: ${pprintCustomised(contributionsOnSideWithoutChanges)}"
      )
      println(
        s"Side with separated contributions: ${pprintCustomised(contributionsOnSideWithSeparationAndSwapping)}"
      )

      // NOTE: in contrast with `overlappingBlocksAreSeparatedOnOneSide`, the
      // assertions have to looser because the two blocks can't both align in
      // the merge due to the swapping around on one side. Which one wins the
      // alignment depends on the handedness of the merge inputs, and that is
      // a valid situation - both alignments are just as good.

      val baseContent = baseContributions.asElementContributions
      assert(
        Vector(
          Contribution.Common(leadingContent),
          Contribution.Common(overlapContent),
          Contribution.CommonToBaseAndLeftOnly(trailingContent)
        ) == baseContent || Vector(
          Contribution.CommonToBaseAndLeftOnly(leadingContent),
          Contribution.Common(overlapContent),
          Contribution.Common(trailingContent)
        ) == baseContent
      )

      val unchangedSideContent =
        contributionsOnSideWithoutChanges.asElementContributions
      assert(
        Vector(
          Contribution.Common(leadingContent),
          Contribution.Common(overlapContent),
          Contribution.CommonToBaseAndLeftOnly(trailingContent)
        ) == unchangedSideContent || Vector(
          Contribution.CommonToBaseAndLeftOnly(leadingContent),
          Contribution.Common(overlapContent),
          Contribution.Common(trailingContent)
        ) == unchangedSideContent
      )

      val separationAndSwappingSideContent =
        contributionsOnSideWithSeparationAndSwapping.asElementContributions
      assert(
        Vector(
          Contribution.Difference(overlapContent),
          Contribution.Difference(trailingContent),
          Contribution.Common(leadingContent),
          Contribution.Common(overlapContent)
        ) == separationAndSwappingSideContent || Vector(
          Contribution.Common(overlapContent),
          Contribution.Common(trailingContent),
          Contribution.Difference(leadingContent),
          Contribution.Difference(overlapContent)
        ) == separationAndSwappingSideContent
      )
    }
  end overlappingBlocksAreSeparatedAndSwappedAroundOnOneSide

  @TestFactory
  def aBlockIsTriplicatedOnTwoSides(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 1,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val placeholderPath: Path = 1

      val blockContent = Vector(1, 2, 3)

      val baseElements: IndexedSeq[Element] = blockContent

      val baseSources = FakeSources(
        contentsByPath = Map(placeholderPath -> baseElements),
        label = "base"
      )

      val elementsWithTriplication: IndexedSeq[Element] =
        blockContent ++ blockContent ++ blockContent

      val leftSources = FakeSources(
        contentsByPath = Map(
          placeholderPath -> elementsWithTriplication
        ),
        label = "left"
      )

      val rightSources = FakeSources(
        contentsByPath = Map(
          placeholderPath -> elementsWithTriplication
        ),
        label = "right"
      )

      val Right(sectionedCode) = SectionedCode.of(
        baseSources = baseSources,
        leftSources = leftSources,
        rightSources = rightSources
      )(configuration): @unchecked

      val LongestCommonSubsequence(
        baseContributions,
        contributionsOnOneSideWithTriplication,
        contributionsOnTheOtherSideWithTriplication,
        _,
        _,
        _,
        _
      ) = sectionedCode
        .longestCommonSubsequenceOf(path = placeholderPath)
        .adaptedForMirroring(mirrorImage)

      println(s"Base contributions: ${pprintCustomised(baseContributions)}")
      println(
        s"One side with triplication contributions: ${pprintCustomised(contributionsOnOneSideWithTriplication)}"
      )
      println(
        s"The other side with triplication contributions: ${pprintCustomised(contributionsOnTheOtherSideWithTriplication)}"
      )

      assert(
        Vector(
          Contribution.Common(blockContent)
        ) == baseContributions.asElementContributions
      )

      val expectedTriplicatedSide = Vector(
        Contribution.Common(blockContent),
        Contribution.CommonToLeftAndRightOnly(blockContent),
        Contribution.CommonToLeftAndRightOnly(blockContent)
      )

      assert(
        expectedTriplicatedSide == contributionsOnOneSideWithTriplication.map(
          _.map(_.content)
        )
      )
      assert(
        expectedTriplicatedSide == contributionsOnTheOtherSideWithTriplication.asElementContributions
      )
    }
  end aBlockIsTriplicatedOnTwoSides

  @Disabled("Fails due to block merge alignment asymmetry under edits (Issue #403)")
  @TestFactory
  def duplicateBlocksWithAnEditAreMerged(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 1,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val placeholderPath: Path = 1

      val baseElements: IndexedSeq[Element] =
        Vector(1, 100, 2, 100, 3, 100, 1, 100, 2, 100, 3, 100, 4)

      val baseSources = FakeSources(
        contentsByPath = Map(placeholderPath -> baseElements),
        label = "base"
      )

      val leftElements: IndexedSeq[Element] =
        Vector(1, 101, 2, 101, 3, 101, 4, 101, 1, 101, 2, 100, 3)

      val leftSources = FakeSources(
        contentsByPath = Map(
          placeholderPath -> leftElements
        ),
        label = "left"
      )

      val rightElements: IndexedSeq[Element] =
        Vector(1, 102, 2, 102, 3)

      val rightSources = FakeSources(
        contentsByPath = Map(
          placeholderPath -> rightElements
        ),
        label = "right"
      )

      val Right(sectionedCode) = SectionedCode.of(
        baseSources = baseSources,
        leftSources = leftSources,
        rightSources = rightSources
      )(configuration): @unchecked

      val LongestCommonSubsequence(
        baseContributions,
        leftContributions,
        rightContributions,
        _,
        _,
        _,
        _
      ) = sectionedCode
        .longestCommonSubsequenceOf(path = placeholderPath)

      println(s"Base contributions: ${pprintCustomised(baseContributions)}")
      println(s"Left contributions: ${pprintCustomised(leftContributions)}")
      println(s"Right contributions: ${pprintCustomised(rightContributions)}")

      println(s"Groups of parallel matches: ${pprintCustomised(sectionedCode.groupsOfParallelMatches)}")
      println(s"Base blocks: ${pprintCustomised(sectionedCode.baseBlocksFor(placeholderPath))}")
      println(s"Left blocks: ${pprintCustomised(sectionedCode.leftBlocksFor(placeholderPath))}")
      println(s"Right blocks: ${pprintCustomised(sectionedCode.rightBlocksFor(placeholderPath))}")

      // IDEAL EXPECTATIONS (under correct block-level merge alignment):
      // Because of the bug (asymmetry in Order[Block[Element]]), Group 3 (for element 4)
      // and Group 2/7 (for 2, 100, 3) fail to align correctly, causing them to be classified
      // as Differences instead of CommonToBaseAndLeftOnly.
      assert(
        Vector(
          Contribution.Common(Vector(1)),
          Contribution.Difference(Vector(100)),
          Contribution.CommonToBaseAndLeftOnly(Vector(2, 100, 3)),
          Contribution.Difference(Vector(100)),
          Contribution.Common(Vector(1)),
          Contribution.Difference(Vector(100)),
          Contribution.CommonToBaseAndLeftOnly(Vector(2, 100, 3)),
          Contribution.Difference(Vector(100)),
          Contribution.CommonToBaseAndLeftOnly(Vector(4))
        ) == baseContributions.asElementContributions
      )

      assert(
        Vector(
          Contribution.Common(Vector(1)),
          Contribution.Difference(Vector(101)),
          Contribution.CommonToLeftAndRightOnly(Vector(2)),
          Contribution.Difference(Vector(101)),
          Contribution.CommonToLeftAndRightOnly(Vector(3)),
          Contribution.Difference(Vector(101)),
          Contribution.CommonToBaseAndLeftOnly(Vector(4)),
          Contribution.Difference(Vector(101)),
          Contribution.Common(Vector(1)),
          Contribution.Difference(Vector(101)),
          Contribution.CommonToBaseAndLeftOnly(Vector(2, 100, 3))
        ) == leftContributions.asElementContributions
      )

      assert(
        Vector(
          Contribution.Common(Vector(1)),
          Contribution.Difference(Vector(102)),
          Contribution.Difference(Vector(2)),
          Contribution.Difference(Vector(102)),
          Contribution.Difference(Vector(3))
        ) == rightContributions.asElementContributions
      )
    }
  end duplicateBlocksWithAnEditAreMerged

end BlockDuplicationAndCondensationTests
