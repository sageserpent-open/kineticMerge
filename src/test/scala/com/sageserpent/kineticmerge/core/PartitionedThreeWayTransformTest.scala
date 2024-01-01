package com.sageserpent.kineticmerge.core

import com.google.common.hash.{Hashing, PrimitiveSink}
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.PartitionedThreeWayTransform.Input
import com.sageserpent.kineticmerge.core.PartitionedThreeWayTransformTest.{funnel, partitionedThreeWayTransform, testCases}
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.TestFactory

class PartitionedThreeWayTransformTest:
  @TestFactory
  def inputsContentContributesToTheResult(): DynamicTests =
    testCases
      .withLimit(200)
      .dynamicTests:
        case (
              LongestCommonSubsequenceTest.TestCase(_, base, left, right),
              targetCommonPartitionSize
            ) =>
          val reconstitutedBase =
            partitionedThreeWayTransform(base, left, right)(
              targetCommonPartitionSize,
              elementEquality = _ == _,
              elementFunnel = funnel,
              hashFunction = Hashing.murmur3_32_fixed()
            )(_.base, _ ++ _)

          assert(reconstitutedBase == base)

          val reconstitutedLeft =
            partitionedThreeWayTransform(base, left, right)(
              targetCommonPartitionSize,
              elementEquality = _ == _,
              elementFunnel = funnel,
              hashFunction = Hashing.murmur3_32_fixed()
            )(_.left, _ ++ _)

          assert(reconstitutedLeft == left)

          val reconstitutedRight =
            partitionedThreeWayTransform(base, left, right)(
              targetCommonPartitionSize,
              elementEquality = _ == _,
              elementFunnel = funnel,
              hashFunction = Hashing.murmur3_32_fixed()
            )(_.right, _ ++ _)

          assert(reconstitutedRight == right)
  end inputsContentContributesToTheResult

  @TestFactory
  def selectingOnlyTheCommonPartitionsYieldsACommonSubsequence(): DynamicTests =
    testCases
      .withLimit(200)
      .dynamicTests:
        case (
              LongestCommonSubsequenceTest.TestCase(_, base, left, right),
              targetCommonPartitionSize
            ) =>
          println("*******************")

          val commonSubsequenceViaBase =
            partitionedThreeWayTransform(base, left, right)(
              targetCommonPartitionSize,
              elementEquality = _ == _,
              elementFunnel = funnel,
              hashFunction = Hashing.murmur3_32_fixed()
            )(
              {
                case Input(base, _, _, true) => base
                case Input(_, _, _, false)   => Vector.empty
              },
              _ ++ _
            )

          println(commonSubsequenceViaBase.mkString)

          commonSubsequenceViaBase isSubsequenceOf base
          commonSubsequenceViaBase isSubsequenceOf left
          commonSubsequenceViaBase isSubsequenceOf right

          val commonSubsequenceViaLeft =
            partitionedThreeWayTransform(base, left, right)(
              targetCommonPartitionSize,
              elementEquality = _ == _,
              elementFunnel = funnel,
              hashFunction = Hashing.murmur3_32_fixed()
            )(
              {
                case Input(_, left, _, true) => left
                case Input(_, _, _, false)   => Vector.empty
              },
              _ ++ _
            )

          println(commonSubsequenceViaLeft.mkString)

          commonSubsequenceViaLeft isSubsequenceOf base
          commonSubsequenceViaLeft isSubsequenceOf left
          commonSubsequenceViaLeft isSubsequenceOf right

          val commonSubsequenceViaRight =
            partitionedThreeWayTransform(base, left, right)(
              targetCommonPartitionSize,
              elementEquality = _ == _,
              elementFunnel = funnel,
              hashFunction = Hashing.murmur3_32_fixed()
            )(
              {
                case Input(_, _, right, true) => right
                case Input(_, _, _, false)    => Vector.empty
              },
              _ ++ _
            )

          println(commonSubsequenceViaRight.mkString)

          commonSubsequenceViaRight isSubsequenceOf base
          commonSubsequenceViaRight isSubsequenceOf left
          commonSubsequenceViaRight isSubsequenceOf right

          assert(commonSubsequenceViaBase == commonSubsequenceViaLeft)
          assert(commonSubsequenceViaBase == commonSubsequenceViaRight)

          if commonSubsequenceViaBase.isEmpty then Trials.reject()
          end if

  end selectingOnlyTheCommonPartitionsYieldsACommonSubsequence

end PartitionedThreeWayTransformTest

object PartitionedThreeWayTransformTest:
  private val partitionedThreeWayTransform = new PartitionedThreeWayTransform

  private val testCases =
    for
      sequences <- LongestCommonSubsequenceTest.testCases
      targetCommonPartitionSize <- trialsApi.integers(
        0,
        sequences.base.size min sequences.left.size min sequences.right.size
      )
    yield sequences -> targetCommonPartitionSize

  private def funnel(element: Char, primitiveSink: PrimitiveSink): Unit =
    primitiveSink.putChar(element)
  end funnel

end PartitionedThreeWayTransformTest
