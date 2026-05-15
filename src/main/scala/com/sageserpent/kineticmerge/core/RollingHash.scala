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

  private def biasByteAsPositiveLong(byte: Byte): Long =
    byte.toLong - Byte.MinValue.toLong

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

    // We must ensure that:
    // 1. `primeModulus` fits into a `Long`.
    // 2. The maximum possible value for `polynomialValue` (which is `primeModulus - 1`)
    //    multiplied by `scale` fits into a `Long` to avoid overflow in the next `polynomialValue` update.
    // 3. The maximum possible value for `biasByteAsPositiveLong(...)` (which is 255)
    //    multiplied by `highestScalePowerLong` (which is at most `primeModulus - 1`) fits into a `Long`.
    private val useLongOptimization =
      primeModulus.isValidLong &&
        (primeModulus * scale).isValidLong &&
        (highestScalePower * 255).isValidLong

    private val primeModulusLong      = if useLongOptimization then primeModulus.toLong else 0L
    private val highestScalePowerLong = if useLongOptimization then highestScalePower.toLong else 0L
    private val scaleLong             = if useLongOptimization then scale.toLong else 0L

    def apply(): RollingHash =
      if useLongOptimization then
        class OptimizedRollingHashImplementation extends RollingHash:
          private val bytesInRollingWindowAsRingBuffer =
            Array.ofDim[Byte](windowSize)
          private var polynomialLength: Int           = 0
          private var polynomialValue: Long           = 0L
          private var ringBufferIndexForNextPush: Int = 0

          override def pushByte(byte: Byte): Unit =
            val prefixPolynomialValue =
              if windowSize == polynomialLength then
                // The ring buffer is full, so the outgoing byte will lie at the
                // index of the next push.
                val byteLeftBehindByWindow = bytesInRollingWindowAsRingBuffer(
                  ringBufferIndexForNextPush
                )
                val contributionToHashFromOutgoingByte =
                  biasByteAsPositiveLong(
                    byteLeftBehindByWindow
                  ) * highestScalePowerLong

                // NOTE: use `Math.floorMod` and *not* `%` to keep values positive.
                Math.floorMod(
                  polynomialValue - contributionToHashFromOutgoingByte,
                  primeModulusLong
                )
              else
                polynomialLength += 1
                polynomialValue
              end if
            end prefixPolynomialValue

            polynomialValue =
              Math.floorMod(
                scaleLong * prefixPolynomialValue + biasByteAsPositiveLong(
                  byte
                ),
                primeModulusLong
              )

            bytesInRollingWindowAsRingBuffer(ringBufferIndexForNextPush) = byte

            ringBufferIndexForNextPush =
              (1 + ringBufferIndexForNextPush) % windowSize
          end pushByte

          def isPrimed: Boolean = windowSize == polynomialLength

          override def fingerprint: BigInt = BigInt(polynomialValue)
        end OptimizedRollingHashImplementation

        new OptimizedRollingHashImplementation with RollingHashContracts
      else
        class GenericRollingHashImplementation extends RollingHash:
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
                val contributionToHashFromOutgoingByte =
                  biasByteAsPositiveBigInt(
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
        end GenericRollingHashImplementation

        new GenericRollingHashImplementation with RollingHashContracts
      end if
    end apply
  end Factory
end RollingHash
