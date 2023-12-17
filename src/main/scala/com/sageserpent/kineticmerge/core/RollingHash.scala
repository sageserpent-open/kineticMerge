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

  class Factory(windowSize: Int):
    private val seed = 8458945L
    def apply(): RollingHash =
      val scale = 1 + BigInt(1 + Byte.MaxValue.toInt - Byte.MinValue.toInt)

      val primeModulesMustBeLargerThanThis =
        BigInt(100000) max (BigInt(windowSize) * windowSize) max scale

      val primeModulus =
        if 1 <= primeModulesMustBeLargerThanThis.bitLength then
          BigInt.probablePrime(
            1 + primeModulesMustBeLargerThanThis.bitLength,
            new Random(seed)
          )
        else BigInt(3)

      assert(
        primeModulus > ((BigInt(windowSize) * windowSize) max scale)
      )

      val highestScalePower = scale.pow(windowSize - 1)

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

              // NOTE: use `mod` and *not* `/` to keep values positive.
              (polynomialValue - contributionToHashFromOutgoingByte) mod primeModulus
            else
              polynomialLength += 1
              polynomialValue
            end if
          end prefixPolynomialValue

          polynomialValue =
            (scale * prefixPolynomialValue + biasByteAsPositiveBigInt(
              byte
            )) % primeModulus

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
