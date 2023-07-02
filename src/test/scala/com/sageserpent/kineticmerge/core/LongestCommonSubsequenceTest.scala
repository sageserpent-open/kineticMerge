package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Contribution
import com.sageserpent.kineticmerge.core.LongestCommonSubsequenceTest.TestCase
import org.junit.jupiter.api.Assertions.fail
import org.junit.jupiter.api.TestInstance.Lifecycle
import org.junit.jupiter.api.{DynamicTest, Test, TestFactory, TestInstance}
import utest.*

import _root_.java.util.Iterator as JavaIterator
import scala.annotation.tailrec

class LongestCommonSubsequenceTest:
  val coreValues: Trials[Int] = trialsApi.choose(1 to 10)

  val additionalValues: Trials[Int] = trialsApi.choose(11 to 20)

  def sizes(maximumSize: Int): Trials[Int] = trialsApi.alternateWithWeights(
    1  -> trialsApi.only(0),
    10 -> trialsApi.integers(1, maximumSize)
  )

  val maximumSize = 30

  val testCases: Trials[TestCase] = (for
    core <- sizes(maximumSize)
      .filter(2 < _)
      .flatMap(coreValues.lotsOfSize[Vector[Int]])

    interleaveForBase <- sizes(maximumSize).flatMap(
      additionalValues.lotsOfSize[Vector[Int]]
    )
    base <- trialsApi.pickAlternatelyFrom(
      shrinkToRoundRobin = true,
      core,
      interleaveForBase
    )
    interleaveForLeft <- sizes(maximumSize).flatMap(
      additionalValues.lotsOfSize[Vector[Int]]
    )
    left <- trialsApi.pickAlternatelyFrom(
      shrinkToRoundRobin = true,
      core,
      interleaveForLeft
    )
    interleaveForRight <- sizes(maximumSize).flatMap(
      additionalValues.lotsOfSize[Vector[Int]]
    )
    right <- trialsApi.pickAlternatelyFrom(
      shrinkToRoundRobin = true,
      core,
      interleaveForRight
    )
    if core != base || core != left || core != right
  yield TestCase(core, base, left, right))

  @TestFactory
  def theResultsCorrespondToTheOriginalSequences(): JavaIterator[DynamicTest] =
    testCases
      .withLimit(100)
      .dynamicTests(
        (
          testCase: TestCase
        ) =>
          val LongestCommonSubsequence(base, left, right, size) =
            LongestCommonSubsequence
              .of(testCase.base, testCase.left, testCase.right)(
                _ == _
              )

          extension (sequence: IndexedSeq[Contribution])
            private def reconstituteAgainst(
                elements: IndexedSeq[Int]
            ): IndexedSeq[Int] =
              sequence.map:
                case Contribution.Common(index)     => elements(index)
                case Contribution.Difference(index) => elements(index)
          end extension

          assert(base.reconstituteAgainst(testCase.base) == testCase.base)
          assert(left.reconstituteAgainst(testCase.left) == testCase.left)
          assert(right.reconstituteAgainst(testCase.right) == testCase.right)
      )

  end theResultsCorrespondToTheOriginalSequences

  @TestFactory
  def theLongestCommonSubsequenceUnderpinsAllThreeResults()
      : JavaIterator[DynamicTest] =
    testCases
      .withLimit(500)
      .dynamicTests(
        (
          testCase: TestCase
        ) =>
          val coreSize = testCase.core.size

          extension (sequence: IndexedSeq[Contribution])
            private def verifyLongestCommonSubsequence(
                elements: IndexedSeq[Int]
            ): Unit =
              val commonIndices = sequence.collect:
                case Contribution.Common(index) => index

              val commonSubsequence = commonIndices.map(elements.apply)

              val _ = commonSubsequence isSubsequenceOf testCase.base
              val _ = commonSubsequence isSubsequenceOf testCase.left
              val _ = commonSubsequence isSubsequenceOf testCase.right

              assert(commonSubsequence.size >= coreSize)

              val differenceIndices = sequence.collect:
                case Contribution.Difference(index) => index

              for differenceIndex <- differenceIndices do
                val (leadingCommonIndices, trailingCommonIndices) =
                  commonIndices.span(differenceIndex > _)

                val viveLaDifférence =
                  leadingCommonIndices :+ differenceIndex :+ trailingCommonIndices

                val _ = viveLaDifférence isNotSubsequenceOf testCase.base
                val _ = viveLaDifférence isNotSubsequenceOf testCase.left
                val _ = viveLaDifférence isNotSubsequenceOf testCase.right
              end for

              if commonSubsequence != elements then
                assert(differenceIndices.nonEmpty)
              end if
          end extension

          val LongestCommonSubsequence(base, left, right, size) =
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
          assert(size >= coreSize)
      )
  end theLongestCommonSubsequenceUnderpinsAllThreeResults

end LongestCommonSubsequenceTest

object LongestCommonSubsequenceTest:
  case class TestCase(
      core: Vector[Int],
      base: Vector[Int],
      left: Vector[Int],
      right: Vector[Int]
  )
end LongestCommonSubsequenceTest

extension [Element](sequence: Seq[Element])
  /* Replacement for ```should contain inOrderElementsOf```; as I'm not sure if
   * that actually detects subsequences correctly in the presence of duplicates. */
  def isSubsequenceOf(anotherSequence: Seq[Element]): Unit =
    isSubsequenceOf(anotherSequence, negated = false)

  def isNotSubsequenceOf(anotherSequence: Seq[Element]): Unit =
    isSubsequenceOf(anotherSequence, negated = true)

  private def isSubsequenceOf(
      anotherSequence: Seq[Element],
      negated: Boolean
  ): Unit =
    @tailrec
    def verify(
        sequenceRemainder: Seq[Element],
        anotherSequenceRemainder: Seq[Element],
        matchingPrefix: Seq[Element]
    ): Unit =
      if sequenceRemainder.isEmpty then
        if negated then fail(s"$sequence is a subsequence of $anotherSequence.")
      else if anotherSequenceRemainder.isEmpty then
        if !negated then
          if matchingPrefix.isEmpty then
            fail(
              s"$sequence is not a subsequence of $anotherSequence - no prefix matches found, either."
            )
          else
            fail(
              s"$sequence is not a subsequence of $anotherSequence, matched prefix $matchingPrefix but failed to find the remaining $sequenceRemainder."
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
