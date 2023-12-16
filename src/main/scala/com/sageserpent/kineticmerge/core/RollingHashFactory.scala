package com.sageserpent.kineticmerge.core

import scala.util.Random

trait RollingHash:
  def pushByte(byte: Byte): Unit

  def isPrimed: Boolean

  def fingerprint: BigInt
end RollingHash

object RollingHash:
  private def biasByteAsPositiveBigInt(byte: Byte): BigInt =
    BigInt(byte) - Byte.MinValue

  trait RollingHashContracts extends RollingHash:
    abstract override def fingerprint: BigInt =
      require(isPrimed)
      super.fingerprint
    end fingerprint
  end RollingHashContracts

  class Factory(numberOfBytes: Int):
    private val seed = 8458945L
    def apply(): RollingHash =
      val logarithmToTheBaseTwoOfSquare =
        Integer.SIZE - Integer.numberOfLeadingZeros(
          numberOfBytes * numberOfBytes - 1
        )

      val primeModulusLargerThanSquareOfNumberOfBytes =
        if 1 <= logarithmToTheBaseTwoOfSquare then
          BigInt.probablePrime(
            1 + logarithmToTheBaseTwoOfSquare,
            new Random(seed)
          )
        else BigInt(3)

      assert(
        primeModulusLargerThanSquareOfNumberOfBytes > numberOfBytes * numberOfBytes
      )

      val scale = BigInt(1) + numberOfBytes

      val highestScalePower = scale.pow(numberOfBytes - 1)

      trait RollingHashImplementation extends RollingHash:
        private val bytesInRollingWindowAsRingBuffer =
          Array.ofDim[Byte](numberOfBytes)
        private var polynomialLength: Int           = 0
        private var polynomialValue: BigInt         = BigInt(0)
        private var ringBufferIndexForNextPush: Int = 0

        override def pushByte(byte: Byte): Unit =
          val prefixPolynomialValue =
            if numberOfBytes == polynomialLength then
              // The ring buffer is full, so the outgoing byte will lie at the
              // index of the next push.
              val byteLeftBehindByWindow = bytesInRollingWindowAsRingBuffer(
                ringBufferIndexForNextPush
              )
              val contributionToHashFromOutgoingByte = biasByteAsPositiveBigInt(
                byteLeftBehindByWindow
              ) * highestScalePower

              // NOTE: use `mod` and *not* `/` to keep values positive.
              (polynomialValue - contributionToHashFromOutgoingByte) mod primeModulusLargerThanSquareOfNumberOfBytes
            else
              polynomialLength += 1
              polynomialValue
            end if
          end prefixPolynomialValue

          polynomialValue =
            (scale * prefixPolynomialValue + biasByteAsPositiveBigInt(
              byte
            )) % primeModulusLargerThanSquareOfNumberOfBytes

          bytesInRollingWindowAsRingBuffer(ringBufferIndexForNextPush) = byte

          ringBufferIndexForNextPush =
            (1 + ringBufferIndexForNextPush) % numberOfBytes
        end pushByte

        def isPrimed: Boolean = numberOfBytes == polynomialLength

        override def fingerprint: BigInt = polynomialValue
      end RollingHashImplementation

      new RollingHashImplementation with RollingHashContracts
    end apply
  end Factory
end RollingHash
