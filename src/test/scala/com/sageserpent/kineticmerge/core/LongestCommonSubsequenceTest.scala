package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.java.CasesLimitStrategy
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.{CommonSubsequenceSize, Contribution, defaultElementSize}
import com.sageserpent.kineticmerge.core.LongestCommonSubsequenceTest.{Element, TestCase, maximumSize, testCases}
import org.junit.jupiter.api.TestFactory

class LongestCommonSubsequenceTest:
  @TestFactory
  def theResultsCorrespondToTheOriginalSequences(): DynamicTests =
    testCases
      .withLimit(100)
      .dynamicTests(
        (
          testCase: TestCase
        ) =>
          val LongestCommonSubsequence(base, left, right, _, _, _, _) =
            LongestCommonSubsequence
              .of(testCase.base, testCase.left, testCase.right)(
                equality = _ == _,
                elementSize = defaultElementSize
              )

          assert(base.map(_.element) == testCase.base)
          assert(left.map(_.element) == testCase.left)
          assert(right.map(_.element) == testCase.right)
      )

  end theResultsCorrespondToTheOriginalSequences

  @TestFactory
  def theLongestCommonSubsequenceUnderpinsAllThreeResults(): DynamicTests =
    testCases
      .withLimit(500)
      .dynamicTests(
        (
          testCase: TestCase
        ) =>


          val coreSize = testCase.core.size

          extension (sequence: IndexedSeq[Contribution[Element]])
            private def verifyLongestCommonSubsequence(
                elements: IndexedSeq[Element], commonSubsequenceSize: Int
            ): Unit =
              // Part #1 - verify the commonality - all sides should agree...

              val indexedCommonParts: IndexedSeq[(Int, Element)] =
                sequence.zipWithIndex.collect:
                  case (common: Contribution.Common[Element], index) =>
                    index -> common.element

              val commonSubsequence = indexedCommonParts.map(_._2)

              val _ = commonSubsequence isSubsequenceOf testCase.base
              val _ = commonSubsequence isSubsequenceOf testCase.left
              val _ = commonSubsequence isSubsequenceOf testCase.right

              assert(commonSubsequence.size == commonSubsequenceSize)

              // Part #2 - verify the length is maximal - anything else should cause a difference somewhere...

              def verifyDifference(viveLaDifférence: IndexedSeq[Element]): Unit =
                if elements != testCase.base then
                  val _ = viveLaDifférence isNotSubsequenceOf testCase.base
                end  if

                if elements != testCase.left then
                    val _ = viveLaDifférence isNotSubsequenceOf testCase.left
                end if

                if elements != testCase.right then
                    val _ = viveLaDifférence isNotSubsequenceOf testCase.right
                end if
              end verifyDifference
              
              def verifyCommonBaseAndLeft(viveLaDifférence: IndexedSeq[Element]): Unit =
                if elements != testCase.right then
                  viveLaDifférence isNotSubsequenceOf testCase.right
                end if
              end verifyCommonBaseAndLeft

              def verifyCommonBaseAndRight(viveLaDifférence: IndexedSeq[Element]): Unit =
                if elements != testCase.left then
                  viveLaDifférence isNotSubsequenceOf testCase.left
                end if
              end verifyCommonBaseAndRight

              def verifyCommonLeftAndRight(viveLaDifférence: IndexedSeq[Element]): Unit =
                if elements != testCase.base then
                  viveLaDifférence isNotSubsequenceOf testCase.base
                end if
              end verifyCommonLeftAndRight

              def insert(contribution: Contribution[Element], contributionIndex: Int, background: IndexedSeq[(Int, Element)]) =
                val (leadingCommonIndices, trailingCommonIndices) =
                  background.span { case (commonIndex, _) =>
                    contributionIndex > commonIndex
                  }

                leadingCommonIndices.map(
                  _._2
                ) ++ (contribution.element +: trailingCommonIndices.map(_._2))
              end insert

              val indexedCommonPartsAndPartialAgreements: IndexedSeq[(Int, Element)] =
                sequence.zipWithIndex.collect:
                  case (common: Contribution.Common[Element], index) =>
                    index -> common.element
                  case (common: Contribution.CommonToLeftAndRightOnly[Element], index) =>
                    index -> common.element
                  case (common: Contribution.CommonToBaseAndLeftOnly[Element], index) =>
                    index -> common.element
                  case (common: Contribution.CommonToBaseAndRightOnly[Element], index) =>
                    index -> common.element

              // Partial agreements in their own right are no problem - they mop up corner
              // cases where a straight assertion on differences would fail. What we have
              // to worry about is when an element that is different in `sequence` wrt to
              // other two sides matches further up or down on one of those sides because
              // it is duplicated in more than one position on that side - when this
              // happens, at least one of the duplicates will have been partially matched
              // (think about it). Including the partial matches in with the common backbone
              // refines the position of the differing element so we don't get spurious
              // subsequence matches.
              val havePartialAgreementsThatNeedToBeTreatedAsCommon = sequence.exists {
                case _: Contribution.CommonToLeftAndRightOnly[Element] => true
                case _: Contribution.CommonToBaseAndLeftOnly[Element] => true
                case _: Contribution.CommonToBaseAndRightOnly[Element] => true
                case _ => false}

              sequence.zipWithIndex.foreach{
                case (
                  difference: Contribution.Difference[Element],
                  index
                  ) if havePartialAgreementsThatNeedToBeTreatedAsCommon => verifyDifference(insert(difference, index, indexedCommonPartsAndPartialAgreements))
                case (
                  difference: Contribution.Difference[Element],
                  index
                  ) if !havePartialAgreementsThatNeedToBeTreatedAsCommon => verifyDifference(insert(difference, index, indexedCommonParts))
                case (
                  difference: Contribution.CommonToBaseAndLeftOnly[Element],
                  index
                  ) => verifyCommonBaseAndLeft(insert(difference, index, indexedCommonParts))
                case (
                  difference: Contribution.CommonToBaseAndRightOnly[Element],
                  index
                  ) => verifyCommonBaseAndRight(insert(difference, index, indexedCommonParts))
                case (
                  difference: Contribution.CommonToLeftAndRightOnly[Element],
                  index
                  ) => verifyCommonLeftAndRight(insert(difference, index, indexedCommonParts))
                case _ =>
              }
          end extension

          val LongestCommonSubsequence(base, left, right, CommonSubsequenceSize(commonSubsequenceLength, _), _, _, _) =
            LongestCommonSubsequence
              .of(testCase.base, testCase.left, testCase.right)(
                equality = _ == _,
                elementSize = defaultElementSize
              )

          // NOTE: the common subsequence aspect is checked against the
          // corresponding sequence it was derived from, *not* against the core.
          // This is because the interleaves for the base, left and right
          // sequences may add elements that form an alternative longest common
          // subsequence that contradicts the core one, but has at least the
          // same size. All the core sequence does is to guarantee that there
          // will be *some* common subsequence.
          // NASTY HACK: placate IntelliJ with these underscore bindings.
          val _ = base.verifyLongestCommonSubsequence(testCase.base, commonSubsequenceLength)
          val _ = left.verifyLongestCommonSubsequence(testCase.left, commonSubsequenceLength)
          val _ = right verifyLongestCommonSubsequence(testCase.right, commonSubsequenceLength)

          // NOTE: The reason for the lower bound on size (rather than strict
          // equality) is because the interleaves for the base, left and right
          // sequences may either augment the core sequence by coincidence, or
          // form an alternative one that is longer.
          assert(commonSubsequenceLength >= coreSize)
      )
  end theLongestCommonSubsequenceUnderpinsAllThreeResults

  @TestFactory
  def theLargestElementSizeSumIsTheTiebreakForLongestCommonSubsequencesOfTheSameLength(): DynamicTests =
    enum MissingSide:
      case None
      case Base
      case Left
      case Right

    val testCases = for
      size <- trialsApi.integers(1, maximumSize)
      lowerCaseSequence <- trialsApi.choose('a' to 'z').lotsOfSize[Vector[Element]](size)
      upperCaseSequence <- trialsApi.choose('A' to 'Z').lotsOfSize[Vector[Element]](size)
      missingSide <- trialsApi.choose(MissingSide.values)
      baseSequence <- if missingSide == MissingSide.Base then trialsApi.only(Vector.empty) else trialsApi.pickAlternatelyFrom(shrinkToRoundRobin = true, lowerCaseSequence, upperCaseSequence)
      leftSequence <- if missingSide == MissingSide.Left then trialsApi.only(Vector.empty) else trialsApi.pickAlternatelyFrom(shrinkToRoundRobin = true, lowerCaseSequence, upperCaseSequence)
      rightSequence <- if missingSide == MissingSide.Right then trialsApi.only(Vector.empty) else trialsApi.pickAlternatelyFrom(shrinkToRoundRobin = true, lowerCaseSequence, upperCaseSequence)
      if baseSequence != leftSequence || baseSequence != rightSequence || leftSequence != baseSequence
    yield TestCase(core = upperCaseSequence, base = baseSequence, left = leftSequence, right = rightSequence) -> missingSide

    testCases.withStrategy(_ => CasesLimitStrategy.counted(30, 50)).dynamicTests{(testCase, missingSide) =>
      val guaranteeUpperCaseElementSumWins = 1 + 'z' - 'a'

      def elementSize(element: Element): Int =
        if element.isUpper then guaranteeUpperCaseElementSumWins + element - 'A' else element - 'a'

      val LongestCommonSubsequence(base, left, right, _, _, _, _) =
        LongestCommonSubsequence
          .of(testCase.base, testCase.left, testCase.right)(
              equality = _ == _,
              elementSize = elementSize
            )
        
      def commonParts(sequence: IndexedSeq[Contribution[Element]]): IndexedSeq[Element] =
        sequence.collect{
          case Contribution.Common[Element](element) => element
          case Contribution.CommonToBaseAndLeftOnly[Element](element) => element
          case Contribution.CommonToBaseAndRightOnly[Element](element) => element
          case Contribution.CommonToLeftAndRightOnly[Element](element) => element
        }

      val baseCommonParts = commonParts(base)
      val leftCommonParts = commonParts(left)
      val rightCommonParts = commonParts(right)

      val upperCaseSequence = testCase.core

      if baseCommonParts.size > upperCaseSequence.size ||
        leftCommonParts.size > upperCaseSequence.size ||
        rightCommonParts.size > upperCaseSequence.size then Trials.reject()

      // We expect the longest common subsequence to contain at least the
      // upper case sequence because it can beat both the lower case sequence
      // and any mixture of contributions from the two, barring when the
      // mixture includes all of the upper case sequence anyway.

      if missingSide != MissingSide.Base
      then upperCaseSequence isSubsequenceOf baseCommonParts
      end if
      if missingSide != MissingSide.Left
      then upperCaseSequence isSubsequenceOf leftCommonParts
      end if
      if missingSide != MissingSide.Right
      then upperCaseSequence isSubsequenceOf rightCommonParts
      end if
    }
  end theLargestElementSizeSumIsTheTiebreakForLongestCommonSubsequencesOfTheSameLength
end LongestCommonSubsequenceTest

object LongestCommonSubsequenceTest:
  type Element = Char

  val coreElements: Trials[Element] = trialsApi.choose('a' to 'z')
  val additionalElements: Trials[Element] = trialsApi.choose('A' to 'Z')
  val maximumSize = 30
  val testCases: Trials[TestCase] = for
    core <- sizes(maximumSize)
      .filter(2 < _)
      .flatMap(coreElements.lotsOfSize[Vector[Element]])

    interleaveForBase <- sizes(maximumSize).flatMap(
      additionalElements.lotsOfSize[Vector[Element]]
    )
    base <- trialsApi.pickAlternatelyFrom(
      shrinkToRoundRobin = true,
      core,
      interleaveForBase
    )
    interleaveForLeft <- sizes(maximumSize).flatMap(
      additionalElements.lotsOfSize[Vector[Element]]
    )
    left <- trialsApi.pickAlternatelyFrom(
      shrinkToRoundRobin = true,
      core,
      interleaveForLeft
    )
    interleaveForRight <- sizes(maximumSize).flatMap(
      additionalElements.lotsOfSize[Vector[Element]]
    )
    right <- trialsApi.pickAlternatelyFrom(
      shrinkToRoundRobin = true,
      core,
      interleaveForRight
    )
    if core != base || core != left || core != right
  yield TestCase(core, base, left, right)

  def sizes(maximumSize: Int): Trials[Int] = trialsApi.alternateWithWeights(
    1 -> trialsApi.only(0),
    10 -> trialsApi.integers(1, maximumSize)
  )

  case class TestCase(
      core: Vector[Element],
      base: Vector[Element],
      left: Vector[Element],
      right: Vector[Element]
  )
end LongestCommonSubsequenceTest


