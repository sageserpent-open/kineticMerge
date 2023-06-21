package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.java.TrialsTest
import com.sageserpent.americium.{Trials, java}
import com.sageserpent.kineticmerge.core.LongestCommonSubsequenceTest.{TestCase, withFilter}
import org.junit.jupiter.api.TestInstance.Lifecycle
import org.junit.jupiter.api.{Test, TestInstance}
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit5.AssertionsForJUnit

@TestInstance(Lifecycle.PER_CLASS)
class LongestCommonSubsequenceTest extends AssertionsForJUnit with Matchers:
  val coreValues: Trials[Int] = trialsApi.choose(1 to 10)

  val additionalValues: Trials[Int] = trialsApi.choose(11 to 20)

  def sizes(maximumSize: Int): Trials[Int] = trialsApi.alternateWithWeights(
    1  -> trialsApi.only(0),
    10 -> trialsApi.integers(1, maximumSize)
  )

  val testCases: java.Trials[TestCase] = (for
    core <- coreValues.several[Vector[Int]].filter(2 < _.size)

    coreSize = core.size

    interleaveForBase <- sizes(coreSize).flatMap(
      additionalValues.lotsOfSize[Vector[Int]]
    )
    base <- trialsApi.pickAlternatelyFrom(
      shrinkToRoundRobin = true,
      core,
      interleaveForBase
    )
    interleaveForLeft <- sizes(coreSize).flatMap(
      additionalValues.lotsOfSize[Vector[Int]]
    )
    left <- trialsApi.pickAlternatelyFrom(
      shrinkToRoundRobin = true,
      core,
      interleaveForLeft
    )
    interleaveForRight <- sizes(coreSize).flatMap(
      additionalValues.lotsOfSize[Vector[Int]]
    )
    right <- trialsApi.pickAlternatelyFrom(
      shrinkToRoundRobin = true,
      core,
      interleaveForRight
    )
    if core != base || core != left || core != right
  yield TestCase(core, base, left, right)).javaTrials

  @TrialsTest(trials = Array("testCases"), casesLimit = 100)
  def justExamineTheTestCases(testCase: TestCase): Unit =
    println(testCase)

end LongestCommonSubsequenceTest

object LongestCommonSubsequenceTest:
  extension [Case](trials: Trials[Case])
    def withFilter(predicate: Case => Boolean): Trials[Case] =
      trials.filter(predicate)

  case class TestCase(
      core: Vector[Int],
      base: Vector[Int],
      left: Vector[Int],
      right: Vector[Int]
  )
end LongestCommonSubsequenceTest
