package com.sageserpent.kineticmerge.core

import com.eed3si9n.expecty.Expecty
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Contribution
import com.sageserpent.kineticmerge.core.LongestCommonSubsequenceTest.{
  Element,
  TestCase,
  assert
}
import org.junit.jupiter.api.Assertions.fail
import org.junit.jupiter.api.TestInstance.Lifecycle
import org.junit.jupiter.api.{DynamicTest, Test, TestFactory, TestInstance}

import scala.annotation.tailrec
import scala.util.Try

class LongestCommonSubsequenceTest:
  val coreValues: Trials[Element] = trialsApi.choose('a' to 'z')

  val additionalValues: Trials[Element] = trialsApi.choose('A' to 'Z')
  val maximumSize                       = 30
  val testCases: Trials[TestCase] = (for
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
  yield TestCase(core, base, left, right))

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
          val LongestCommonSubsequence(base, left, right, size) =
            LongestCommonSubsequence
              .of(testCase.base, testCase.left, testCase.right)(
                _ == _
              )

          assert(base.reconstituteAgainst(testCase.base) == testCase.base)
          assert(left.reconstituteAgainst(testCase.left) == testCase.left)
          assert(right.reconstituteAgainst(testCase.right) == testCase.right)
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

          extension (sequence: IndexedSeq[Contribution])
            private def verifyLongestCommonSubsequence(
                elements: IndexedSeq[Element]
            ): Unit =
              val commonParts: IndexedSeq[Contribution.Common] =
                sequence.collect:
                  case common: Contribution.Common => common

              val commonSubsequence: IndexedSeq[Element] =
                commonParts reconstituteAgainst elements

              val _ = commonSubsequence isSubsequenceOf testCase.base
              val _ = commonSubsequence isSubsequenceOf testCase.left
              val _ = commonSubsequence isSubsequenceOf testCase.right

              assert(commonSubsequence.size >= coreSize)

              val differences: IndexedSeq[Contribution.Difference] =
                sequence.collect:
                  case difference: Contribution.Difference => difference

              for difference <- differences do
                val (leadingCommonIndices, trailingCommonIndices) =
                  commonParts.span(
                    difference.indexInContributor > _.indexInContributor
                  )

                val viveLaDifférence: IndexedSeq[Element] =
                  (leadingCommonIndices ++ (difference +: trailingCommonIndices)) reconstituteAgainst elements

                Try {
                  viveLaDifférence isNotSubsequenceOf testCase.base
                }.toEither
                  .orElse(Try {
                    viveLaDifférence isNotSubsequenceOf testCase.left
                  }.toEither)
                  .orElse(Try {
                    viveLaDifférence isNotSubsequenceOf testCase.right
                  }.toEither)
                  .left
                  .foreach(fail _)
              end for

              if commonSubsequence != elements then assert(differences.nonEmpty)
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

extension (sequence: IndexedSeq[Contribution])
  private def reconstituteAgainst(
      elements: IndexedSeq[Element]
  ): IndexedSeq[Element] =
    sequence.map:
      case Contribution.Common(index)     => elements(index)
      case Contribution.Difference(index) => elements(index)
end extension

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
