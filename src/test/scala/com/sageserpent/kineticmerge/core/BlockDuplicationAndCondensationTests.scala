package com.sageserpent.kineticmerge.core

import cats.Eq
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
import org.junit.jupiter.api.TestFactory

object BlockDuplicationAndCondensationTests:
  given HashFunction = Hashing.murmur3_32_fixed()

  given ProgressRecording    = NoProgressRecording
  given Eq[Section[Element]] =
    given Eq[IndexedSeq[Element]] = Eq.fromUniversalEquals

    Eq.by(_.content)
  end given

  given Sized[Section[Element]] = defaultElementSize

  extension [X](longestCommonSubsequence: LongestCommonSubsequence[X])
    def adaptedForMirroring(mirrored: Boolean): LongestCommonSubsequence[X] =
      if mirrored then longestCommonSubsequence.mirror
      else longestCommonSubsequence
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
        Vector(Contribution.Common(blockContent)) == baseContributions
          .map(_.map(_.content))
      )
      assert(
        Vector(
          Contribution.Common(blockContent)
        ) == contributionsOnSideWithoutChanges.map(_.map(_.content))
      )
      assert(
        Vector(
          Contribution.Common(blockContent),
          Contribution.Difference(blockContent)
        ) == contributionsOnSideWithDuplication.map(_.map(_.content))
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
        Vector(Contribution.Common(blockContent)) == baseContributions
          .map(_.map(_.content))
      )
      assert(
        Vector(
          Contribution.Common(blockContent),
          Contribution.CommonToLeftAndRightOnly(blockContent)
        ) == contributionsOnOneSideWithDuplication.map(_.map(_.content))
      )
      assert(
        Vector(
          Contribution.Common(blockContent),
          Contribution.CommonToLeftAndRightOnly(blockContent)
        ) == contributionsOnTheOtherSideWithDuplication.map(_.map(_.content))
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
        ) == baseContributionsWithDuplication.map(_.map(_.content))
      )
      assert(
        Vector(
          Contribution.Common(blockContent),
          Contribution.CommonToBaseAndLeftOnly(blockContent)
        ) == contributionsOnSideWithoutChanges.map(_.map(_.content))
      )
      assert(
        Vector(
          Contribution.Common(blockContent)
        ) == contributionsOnTheOtherSideWithoutDuplication.map(_.map(_.content))
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
        ) == baseContributions
          .map(_.map(_.content))
      )
      assert(
        Vector(
          Contribution.Common(leadingContent),
          Contribution.Common(overlapContent),
          Contribution.Common(trailingContent)
        ) == contributionsOnSideWithoutChanges.map(_.map(_.content))
      )
      assert(
        Vector(
          Contribution.Common(leadingContent),
          Contribution.Common(overlapContent),
          Contribution.Difference(overlapContent),
          Contribution.Common(trailingContent)
        ) == contributionsOnSideWithSeparation.map(_.map(_.content))
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

      val baseContent = baseContributions
        .map(_.map(_.content))
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
        contributionsOnSideWithoutChanges.map(_.map(_.content))
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
        contributionsOnSideWithSeparationAndSwapping.map(_.map(_.content))
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
        Vector(Contribution.Common(blockContent)) == baseContributions
          .map(_.map(_.content))
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
        expectedTriplicatedSide == contributionsOnTheOtherSideWithTriplication
          .map(_.map(_.content))
      )
    }
  end aBlockIsTriplicatedOnTwoSides

end BlockDuplicationAndCondensationTests
