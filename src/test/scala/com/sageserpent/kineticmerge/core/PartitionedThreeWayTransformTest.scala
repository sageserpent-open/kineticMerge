package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.LongestCommonSubsequenceTest.testCases
import com.sageserpent.kineticmerge.core.PartitionedThreeWayTransform.Input
import com.sageserpent.kineticmerge.core.PartitionedThreeWayTransformTest.partitionSizeFraction
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.TestFactory
import pprint.pprintln

class PartitionedThreeWayTransformTest:
  @TestFactory
  def inputsContentContributesToTheResult(): DynamicTests =
    testCases
      .withLimit(1000)
      .dynamicTests:
        case LongestCommonSubsequenceTest.TestCase(_, base, left, right) =>
          val reconstitutedBase =
            PartitionedThreeWayTransform(base, left, right)(
              partitionSizeFraction,
              _ == _
            )(_.base, _ ++ _)

          assert(reconstitutedBase == base)

          val reconstitutedLeft =
            PartitionedThreeWayTransform(base, left, right)(
              partitionSizeFraction,
              _ == _
            )(_.left, _ ++ _)

          assert(reconstitutedLeft == left)

          val reconstitutedRight =
            PartitionedThreeWayTransform(base, left, right)(
              partitionSizeFraction,
              _ == _
            )(_.right, _ ++ _)

          assert(reconstitutedRight == right)
  end inputsContentContributesToTheResult

  @TestFactory
  def selectingOnlyTheCommonPartitionsYieldsACommonSubsequence(): DynamicTests =
    testCases
      .withLimit(100)
      .dynamicTests:
        case LongestCommonSubsequenceTest.TestCase(core, base, left, right) =>
          ???
  end selectingOnlyTheCommonPartitionsYieldsACommonSubsequence

end PartitionedThreeWayTransformTest

object PartitionedThreeWayTransformTest:
  private val partitionSizeFraction = 0.2
end PartitionedThreeWayTransformTest
