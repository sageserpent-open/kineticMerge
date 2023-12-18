package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import org.junit.jupiter.api.TestFactory

import scala.collection.immutable.SortedMultiDict

class RollingHashTest:
  @TestFactory
  def fingerprintCollisionsShouldBeVeryRare(): DynamicTests =
    case class TestCase(
        windowSize: Int,
        distinctSequences: Vector[Vector[Byte]]
    )

    val testCases =
      for
        windowSize <- trialsApi.alternate(
          trialsApi.integers(11, 10000),
          trialsApi.integers(1, 10)
        )

        subsequences = trialsApi.bytes.lotsOfSize[Vector[Byte]](windowSize)

        distinctSequences <- subsequences
          .several[Vector[Vector[Byte]]]
          .filter(_.nonEmpty)
          .map(_.distinct)
      yield TestCase(windowSize, distinctSequences)

    testCases.withLimit(4000).dynamicTests { testCase =>
      import testCase.*

      // NOTE: although we spin up a new instance of `RollingHash` for each byte
      // sequence, all the instances share the same range of fingerprint values.
      val factory = RollingHash.Factory(
        windowSize = windowSize,
        numberOfFingerprintsToBeTaken = distinctSequences.size * windowSize
      )

      val fingerprintedByteSequences =
        SortedMultiDict.from(distinctSequences.iterator.map { byteSequence =>
          val rollingHash = factory()

          byteSequence.foreach(rollingHash.pushByte)

          rollingHash.fingerprint -> byteSequence
        })

      fingerprintedByteSequences.keySet.foreach { fingerprint =>
        val expectedSingletonSet = fingerprintedByteSequences.get(fingerprint)

        assert(
          1 == expectedSingletonSet.size,
          s"Collision detected on fingerprint: $fingerprint, colliding sequences are: $expectedSingletonSet"
        )
      }
    }
  end fingerprintCollisionsShouldBeVeryRare

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

      distinctSubsequences <- subsequences
        .several[Vector[Vector[Byte]]]
        .filter(_.nonEmpty)
        .map(_.distinct)
      permutation <- trialsApi
        .indexPermutations(distinctSubsequences.size)
      permutedSubsequences = permutation.map(distinctSubsequences.apply)
    yield TestCase(
      windowSize,
      concatenatedSubsequences = distinctSubsequences.reduce(_ ++ _),
      concatenatedPermutedSubsequences = permutedSubsequences.reduce(_ ++ _)
    )

    testCases.withLimit(200).dynamicTests { testCase =>
      import testCase.*

      val factory = RollingHash.Factory(
        windowSize = windowSize,
        numberOfFingerprintsToBeTaken = concatenatedSubsequences.size
      )

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
