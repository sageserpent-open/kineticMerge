package com.sageserpent.kineticmerge.core

import scala.util.Random

trait RollingHash:
  def pushByte(byte: Byte): Unit

  def isPrimed: Boolean

  def fingerprint: BigInt
end RollingHash

object RollingHash:
  private val magicConstantForBirthdayParadoxAvoidanceAtOnePercentProbabilityOfCollision =
    // Approximation for `Math.log(Math.pow(1 / (1 - 0.01)), 2)`.
    Math.log(1 + 2 * 0.01)

  private def biasByteAsPositiveBigInt(byte: Byte): BigInt =
    BigInt(byte) - Byte.MinValue

  trait RollingHashContracts extends RollingHash:
    abstract override def fingerprint: BigInt =
      require(isPrimed)
      super.fingerprint
    end fingerprint
  end RollingHashContracts

  class Factory(windowSize: Int, numberOfFingerprintsToBeTaken: Int):
    private val seed = 8458945L

    private val scale =
      1 + BigInt(1 + Byte.MaxValue.toInt - Byte.MinValue.toInt)

    // Kudos to this article
    // (https://web.cs.unlv.edu/larmore/Courses/CSC477/F14/Assignments/horners.pdf)
    // for the awareness that this could be a problem.
    private val numberOfDistinctFingerprintsToAvoidBirthdayParadoxCollision =
      BigInt(
        ((numberOfFingerprintsToBeTaken.toDouble * numberOfFingerprintsToBeTaken.toDouble)
          / magicConstantForBirthdayParadoxAvoidanceAtOnePercentProbabilityOfCollision).ceil.toLong
      )

    private val primeModulusMustBeLargerThanThis =
      numberOfDistinctFingerprintsToAvoidBirthdayParadoxCollision max scale

    // Clearing `scale` should guarantee this.
    assume(1 <= primeModulusMustBeLargerThanThis.bitLength)

    private val primeModulus =
      BigInt.probablePrime(
        1 + primeModulusMustBeLargerThanThis.bitLength,
        new Random(seed)
      )

    private val highestScalePower = scale.pow(windowSize - 1) mod primeModulus

    def apply(): RollingHash =
      trait RollingHashImplementation extends RollingHash:
        private val bytesInRollingWindowAsRingBuffer =
          Array.ofDim[Byte](windowSize)
        private var polynomialLength: Int           = 0
        private var polynomialValue: BigInt         = BigInt(0)
        private var ringBufferIndexForNextPush: Int = 0

        override def pushByte(byte: Byte): Unit =
          val prefixPolynomialValue =
            if windowSize == polynomialLength then
              // The ring buffer is full, so the outgoing byte will lie at the
              // index of the next push.
              val byteLeftBehindByWindow = bytesInRollingWindowAsRingBuffer(
                ringBufferIndexForNextPush
              )
              val contributionToHashFromOutgoingByte = biasByteAsPositiveBigInt(
                byteLeftBehindByWindow
              ) * highestScalePower

              // NOTE: use `mod` and *not* `%` to keep values positive.
              (polynomialValue - contributionToHashFromOutgoingByte) mod primeModulus
            else
              polynomialLength += 1
              polynomialValue
            end if
          end prefixPolynomialValue

          polynomialValue =
            (scale * prefixPolynomialValue + biasByteAsPositiveBigInt(
              byte
            )) mod primeModulus

          bytesInRollingWindowAsRingBuffer(ringBufferIndexForNextPush) = byte

          ringBufferIndexForNextPush =
            (1 + ringBufferIndexForNextPush) % windowSize
        end pushByte

        def isPrimed: Boolean = windowSize == polynomialLength

        override def fingerprint: BigInt = polynomialValue
      end RollingHashImplementation

      new RollingHashImplementation with RollingHashContracts
    end apply
  end Factory
end RollingHash
