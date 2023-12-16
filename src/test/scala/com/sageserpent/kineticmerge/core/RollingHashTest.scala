package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import org.junit.jupiter.api.TestFactory

class RollingHashTest:
  @TestFactory
  def subsequencesYieldTheSameFingerprintsRegardlessOfTheirOrderInTheContainingSequence()
      : DynamicTests =
    case class TestCase(
        windowSize: Int,
        concatenatedSubsequences: Vector[Byte],
        concatenatedPermutedSubsequences: Vector[Byte]
    )

    val testCases = for
      windowSize <- trialsApi.alternate(
        trialsApi.integers(11, 10000),
        trialsApi.integers(1, 10)
      )

      subsequences = trialsApi.bytes
        .lotsOfSize[Vector[Byte]](windowSize)

      subsequenceArrangement <- subsequences
        .several[Vector[Vector[Byte]]]
        .filter(_.nonEmpty)
        .map(_.distinct)
      permutation <- trialsApi
        .indexPermutations(subsequenceArrangement.size)
      permutedSubsequences = permutation.map(subsequenceArrangement.apply)
    yield TestCase(
      windowSize,
      concatenatedSubsequences = subsequenceArrangement.reduce(_ ++ _),
      concatenatedPermutedSubsequences = permutedSubsequences.reduce(_ ++ _)
    )

    testCases.withLimit(200).dynamicTests { testCase =>
      import testCase.*

      val factory = RollingHash.Factory(windowSize)

      def fingerprints(bigSequence: Vector[Byte]): Vector[BigInt] =
        val rollingHash = factory()

        val (priming, remainder) = bigSequence.splitAt(windowSize - 1)

        priming.foreach { byte =>
          rollingHash.pushByte(byte)
          assert(!rollingHash.isPrimed)
        }

        remainder
          .map { byte =>
            rollingHash.pushByte(byte)
            rollingHash.fingerprint
          }
          .grouped(windowSize)
          // Each group starts with a fingerprint from one of the subsequences
          // without any intermixing from its neighbouring subsequences.
          .map(_.head)
          .toVector
          .sorted
      end fingerprints

      val firstLotOfFingerprints = fingerprints(concatenatedSubsequences)
      val secondLotOfFingerprints =
        fingerprints(concatenatedPermutedSubsequences)

      assert(secondLotOfFingerprints == firstLotOfFingerprints)
    }
  end subsequencesYieldTheSameFingerprintsRegardlessOfTheirOrderInTheContainingSequence
end RollingHashTest
