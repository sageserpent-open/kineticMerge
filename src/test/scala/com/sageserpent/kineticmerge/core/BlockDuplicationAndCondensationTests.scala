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
      ambiguousMatchesThreshold = 4
    )

    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val placeholderPath: Path = 1

      val blockContent = Vector(1, 2, 3)

      val baseElements: IndexedSeq[Element] = blockContent

      val baseSources = FakeSources(
        contentsByPath = Map(placeholderPath -> baseElements),
        label = "base"
      )

      val elementsOnSideWithoutChanges: IndexedSeq[Element]  = blockContent
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

      // TODO: calling `SectionedCodeExtension.longestCommonSubsequenceOf` is
      // made awkward because the path and sections have to be consistent. Fix
      // this.
      val LongestCommonSubsequence(
        baseContributions,
        contributionsOnSideWithoutChanges,
        contributionsOnSideWithDuplication,
        _,
        _,
        _,
        _
      ) = sectionedCode
        .longestCommonSubsequenceOf(
          baseSections = sectionedCode.base(placeholderPath).sections,
          leftSections = sectionedCode.left(placeholderPath).sections,
          rightSections = sectionedCode.right(placeholderPath).sections
        )(path = placeholderPath)
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
      ambiguousMatchesThreshold = 4
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

      // TODO: calling `SectionedCodeExtension.longestCommonSubsequenceOf` is
      // made awkward because the path and sections have to be consistent. Fix
      // this.
      val LongestCommonSubsequence(
        baseContributions,
        contributionsOnOneSideWithDuplication,
        contributionsOnTheOtherSideWithDuplication,
        _,
        _,
        _,
        _
      ) = sectionedCode
        .longestCommonSubsequenceOf(
          baseSections = sectionedCode.base(placeholderPath).sections,
          leftSections = sectionedCode.left(placeholderPath).sections,
          rightSections = sectionedCode.right(placeholderPath).sections
        )(path = placeholderPath)
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
          Contribution.Difference(blockContent)
        ) == contributionsOnOneSideWithDuplication.map(_.map(_.content))
      )
      assert(
        Vector(
          Contribution.Common(blockContent),
          Contribution.Difference(blockContent)
        ) == contributionsOnTheOtherSideWithDuplication.map(_.map(_.content))
      )
    }
  end aBlockIsDuplicatedOnTwoSides

  @TestFactory
  def aBlockIsCondensedOnOneSide(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 1,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 4
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

      // TODO: calling `SectionedCodeExtension.longestCommonSubsequenceOf` is
      // made awkward because the path and sections have to be consistent. Fix
      // this.
      val LongestCommonSubsequence(
        baseContributionsWithDuplication,
        contributionsOnSideWithoutChanges,
        contributionsOnTheOtherSideWithoutDuplication,
        _,
        _,
        _,
        _
      ) = sectionedCode
        .longestCommonSubsequenceOf(
          baseSections = sectionedCode.base(placeholderPath).sections,
          leftSections = sectionedCode.left(placeholderPath).sections,
          rightSections = sectionedCode.right(placeholderPath).sections
        )(path = placeholderPath)
        .adaptedForMirroring(mirrorImage)

      println(
        s"Base contributions: ${pprintCustomised(baseContributionsWithDuplication)}"
      )
      println(
        s"Side without changes contributions: ${pprintCustomised(contributionsOnSideWithoutChanges)}"
      )
      println(
        s"The other side with duplication contributions: ${pprintCustomised(contributionsOnTheOtherSideWithoutDuplication)}"
      )

      assert(
        Vector(
          Contribution.Common(blockContent),
          Contribution.Difference(blockContent)
        ) == baseContributionsWithDuplication.map(_.map(_.content))
      )
      assert(
        Vector(
          Contribution.Common(blockContent),
          Contribution.Difference(blockContent)
        ) == contributionsOnSideWithoutChanges.map(_.map(_.content))
      )
      assert(
        Vector(
          Contribution.Common(blockContent)
        ) == contributionsOnTheOtherSideWithoutDuplication.map(_.map(_.content))
      )
    }
  end aBlockIsCondensedOnOneSide

end BlockDuplicationAndCondensationTests
