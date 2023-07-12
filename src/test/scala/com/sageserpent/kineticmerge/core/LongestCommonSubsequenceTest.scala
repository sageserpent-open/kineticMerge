package com.sageserpent.kineticmerge.core

import cats.data.{EitherT, OptionT, Writer}
import com.eed3si9n.expecty.Expecty
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Contribution
import com.sageserpent.kineticmerge.core.LongestCommonSubsequenceTest.{Element, TestCase, assert}
import org.junit.jupiter.api.Assertions.fail
import org.junit.jupiter.api.TestInstance.Lifecycle
import org.junit.jupiter.api.{DynamicTest, Test, TestFactory, TestInstance}

import scala.annotation.tailrec
import scala.util.Try

class LongestCommonSubsequenceTest:
  val coreValues: Trials[Element] = trialsApi.choose('a' to 'z')

  val additionalValues: Trials[Element] = trialsApi.choose('A' to 'Z')
  val maximumSize                       = 30
  val testCases: Trials[TestCase] = for
    core <- sizes(maximumSize)
      .filter(2 < _)
      .flatMap(coreValues.lotsOfSize[Vector[Element]])

    interleaveForBase <- sizes(maximumSize).flatMap(
      additionalValues.lotsOfSize[Vector[Element]]
    )
    base <- trialsApi.pickAlternatelyFrom(
      shrinkToRoundRobin = true,
      core,
      interleaveForBase
    )
    interleaveForLeft <- sizes(maximumSize).flatMap(
      additionalValues.lotsOfSize[Vector[Element]]
    )
    left <- trialsApi.pickAlternatelyFrom(
      shrinkToRoundRobin = true,
      core,
      interleaveForLeft
    )
    interleaveForRight <- sizes(maximumSize).flatMap(
      additionalValues.lotsOfSize[Vector[Element]]
    )
    right <- trialsApi.pickAlternatelyFrom(
      shrinkToRoundRobin = true,
      core,
      interleaveForRight
    )
    if core != base || core != left || core != right
  yield TestCase(core, base, left, right)

  def sizes(maximumSize: Int): Trials[Int] = trialsApi.alternateWithWeights(
    1  -> trialsApi.only(0),
    10 -> trialsApi.integers(1, maximumSize)
  )

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
                _ == _
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
                elements: IndexedSeq[Element]
            ): Unit =
              val indexedCommonParts: IndexedSeq[(Int, Element)] =
                sequence.zipWithIndex.collect:
                  case (common: Contribution.Common[Element], index) =>
                    index -> common.element

              val commonSubsequence = indexedCommonParts.map(_._2)

              val _ = commonSubsequence isSubsequenceOf testCase.base
              val _ = commonSubsequence isSubsequenceOf testCase.left
              val _ = commonSubsequence isSubsequenceOf testCase.right

              assert(commonSubsequence.size >= coreSize)

              def verifyDifference(viveLaDifférence: IndexedSeq[Element]): Unit =
                if elements != testCase.base then
                  viveLaDifférence isNotSubsequenceOf testCase.base
                end  if

                if elements != testCase.left then
                    viveLaDifférence isNotSubsequenceOf testCase.left
                end if

                if elements != testCase.right then
                    viveLaDifférence isNotSubsequenceOf testCase.right
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

              def insert(contribution: Contribution[Element], contributionIndex: Int) =
                val (leadingCommonIndices, trailingCommonIndices) =
                  indexedCommonParts.span { case (commonIndex, _) =>
                    contributionIndex > commonIndex
                  }

                leadingCommonIndices.map(
                  _._2
                ) ++ (contribution.element +: trailingCommonIndices.map(_._2))

              sequence.zipWithIndex.foreach{
                case (
                  difference: Contribution.Difference[Element],
                  index
                  ) => /*verifyDifference(insert(difference, index))*/ // TODO - reinstate this assertion when the coincidence problems have been sorted out!

                case (
                  difference: Contribution.CommonToBaseAndLeftOnly[Element],
                  index
                  ) => verifyCommonBaseAndLeft(insert(difference, index))
                case (
                  difference: Contribution.CommonToBaseAndRightOnly[Element],
                  index
                  ) => verifyCommonBaseAndRight(insert(difference, index))
                case (
                  difference: Contribution.CommonToLeftAndRightOnly[Element],
                  index
                  ) => verifyCommonLeftAndRight(insert(difference, index))
                case _ =>
              }
          end extension

          val LongestCommonSubsequence(base, left, right, commonSubsequenceSize, _, _, _) =
            LongestCommonSubsequence
              .of(testCase.base, testCase.left, testCase.right)(
                _ == _
              )

          // NOTE: the common subsequence aspect is checked against the
          // corresponding sequence it was derived from, *not* against the core.
          // This is because the interleaves for the base, left and right
          // sequences may add elements that form an alternative longest common
          // subsequence that contradicts the core one, but has at least the
          // same size. All the core sequence does is to guarantee that there
          // will be *some* common subsequence.
          // NASTY HACK: placate IntelliJ with these underscore bindings.
          val _ =
            base verifyLongestCommonSubsequence testCase.base
          val _ =
            left verifyLongestCommonSubsequence testCase.left
          val _ =
            right verifyLongestCommonSubsequence testCase.right

          // NOTE: The reason for the lower bound on size (rather than strict
          // equality) is because the interleaves for the base, left and right
          // sequences may either augment the core sequence by coincidence, or
          // form an alternative one that is longer.
          assert(commonSubsequenceSize >= coreSize)
      )
  end theLongestCommonSubsequenceUnderpinsAllThreeResults

end LongestCommonSubsequenceTest

object LongestCommonSubsequenceTest:
  type Element = Char

  val assert: Expecty = new Expecty:
    override val showLocation: Boolean = true
    override val showTypes: Boolean    = true
  end assert
  case class TestCase(
      core: Vector[Element],
      base: Vector[Element],
      left: Vector[Element],
      right: Vector[Element]
  )
end LongestCommonSubsequenceTest

extension [Element](sequence: Seq[Element])
  /* Replacement for ```should contain inOrderElementsOf```; as I'm not sure if
   * that actually detects subsequences correctly in the presence of duplicates. */
  def isSubsequenceOf(
      anotherSequence: Seq[? >: Element]
  ): Unit =
    isSubsequenceOf(anotherSequence, negated = false)

  def isNotSubsequenceOf(
      anotherSequence: Seq[? >: Element]
  ): Unit =
    isSubsequenceOf(anotherSequence, negated = true)

  private def isSubsequenceOf[ElementSupertype >: Element](
      anotherSequence: Seq[ElementSupertype],
      negated: Boolean
  ): Unit =
    @tailrec
    def verify(
        sequenceRemainder: Seq[Element],
        anotherSequenceRemainder: Seq[ElementSupertype],
        matchingPrefix: Seq[Element]
    ): Unit =
      if sequenceRemainder.isEmpty then
        if negated then
          fail(
            s"Assertion failed because $sequence is a subsequence of $anotherSequence."
          )
      else if anotherSequenceRemainder.isEmpty then
        if !negated then
          if matchingPrefix.isEmpty then
            fail(
              s"Assertion failed because $sequence is not a subsequence of $anotherSequence - no prefix matches found, either."
            )
          else
            fail(
              s"Assertion failed because $sequence is not a subsequence of $anotherSequence, matched prefix $matchingPrefix but failed to find the remaining $sequenceRemainder."
            )
      else if sequenceRemainder.head == anotherSequenceRemainder.head then
        verify(
          sequenceRemainder.tail,
          anotherSequenceRemainder.tail,
          matchingPrefix :+ sequenceRemainder.head
        )
      else
        verify(
          sequenceRemainder,
          anotherSequenceRemainder.tail,
          matchingPrefix
        )
      end if
    end verify

    verify(sequence, anotherSequence, sequence.empty)
  end isSubsequenceOf
end extension
