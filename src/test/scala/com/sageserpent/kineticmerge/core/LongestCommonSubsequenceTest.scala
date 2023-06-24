package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.java.TrialsTest
import com.sageserpent.americium.{Trials, java}
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Contribution
import com.sageserpent.kineticmerge.core.LongestCommonSubsequenceTest.TestCase
import org.junit.jupiter.api.TestInstance.Lifecycle
import org.junit.jupiter.api.{Test, TestInstance}
import org.scalatest.Assertion
import org.scalatest.Assertions.*
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit5.AssertionsForJUnit

import scala.annotation.tailrec

@TestInstance(Lifecycle.PER_CLASS)
class LongestCommonSubsequenceTest extends AssertionsForJUnit with Matchers:
  val coreValues: Trials[Int] = trialsApi.choose(1 to 10)

  val additionalValues: Trials[Int] = trialsApi.choose(11 to 20)

  def sizes(maximumSize: Int): Trials[Int] = trialsApi.alternateWithWeights(
    1  -> trialsApi.only(0),
    10 -> trialsApi.integers(1, maximumSize)
  )

  val maximumSize = 30

  val testCases: java.Trials[TestCase] = (for
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
  yield TestCase(core, base, left, right)).javaTrials

  @TrialsTest(trials = Array("testCases"), casesLimit = 500)
  def theLongestCommonSubsequenceUnderpinsAllThreeResults(
      testCase: TestCase
  ): Unit =
    val coreSize = testCase.core.size

    extension (sequence: IndexedSeq[Contribution])
      private def verifyCommonSubsequence(
          elements: IndexedSeq[Int]
      ): Assertion =
        val commonSubsequence = sequence.collect:
          case Contribution.Common(index) =>
            elements(index)

        val _ = commonSubsequence isSubsequenceOf testCase.base
        val _ = commonSubsequence isSubsequenceOf testCase.left
        val _ = commonSubsequence isSubsequenceOf testCase.right

        commonSubsequence.size should be >= coreSize

    val LongestCommonSubsequence(base, left, right, size) =
      LongestCommonSubsequence.of(testCase.base, testCase.left, testCase.right)(
        _ == _
      )

    // NOTE: the common subsequence aspect is checked against the corresponding
    // sequence it was derived from, *not* against the core. This is because the
    // interleaves for the base, left and right sequences may add elements that
    // form an alternative longest common subsequence that contradicts the core
    // one, but has at least the same size. All the core sequence does is to
    // guarantee that there will be *some* common subsequence.
    // NASTY HACK: placate IntelliJ with these underscore bindings.
    val _ =
      base verifyCommonSubsequence testCase.base
    val _ =
      left verifyCommonSubsequence testCase.left
    val _ =
      right verifyCommonSubsequence testCase.right

    // NOTE: The reason for the lower bound on size (rather than strict
    // equality) is because the interleaves for the base, left and right
    // sequences may either augment the core sequence by coincidence, or form an
    // alternative one that is longer.
    size should be >= coreSize
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
  def isSubsequenceOf(anotherSequence: Seq[Element]): Assertion =
    @tailrec
    def verify(
        sequenceRemainder: Seq[Element],
        anotherSequenceRemainder: Seq[Element],
        matchingPrefix: Seq[Element]
    ): Assertion =
      if sequenceRemainder.isEmpty then succeed
      else if anotherSequenceRemainder.isEmpty then
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
end extension
