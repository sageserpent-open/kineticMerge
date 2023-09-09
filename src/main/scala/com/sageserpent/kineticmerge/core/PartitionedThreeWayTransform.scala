package com.sageserpent.kineticmerge.core

import cats.Eq
import org.rabinfingerprint.fingerprint.{
  RabinFingerprintLong,
  RabinFingerprintLongWindowed
}
import org.rabinfingerprint.polynomial.Polynomial

import scala.annotation.tailrec
import scala.collection.{SortedMap, mutable}

object PartitionedThreeWayTransform:
  private val polynomial = Polynomial.createIrreducible(15)

  /** Partition the sequences {@code base}, {@code left} and {@code right} by a
    * common partition; each of the sequences is split into two (possibly empty)
    * parts before and after the partition.
    *
    * The partitioning is applied recursively to the parts bracketing the common
    * partition until no more partitions can be found.
    *
    * The result is assembled from the decomposed parts and common partitions
    * using {@code threeWayTransform} and {@code reduction} to stitch the pieces
    * together across the base, left and right contributions.
    *
    * The order of appearance of the parts in the overall sequences is
    * respected; if {@code threeWayTransform} simply picks from one side of
    * {@code base}, {@code left} and {@code right} and {@code reduction}
    * concatenates, then this will reconstitute the given side.
    *
    * Ignoring the inputs that are not common partitions provides a
    * quick-and-dirty approximation to the longest common subsequence.
    *
    * @param base
    * @param left
    * @param right
    * @param partitionSizeFraction
    * @param equality
    * @param threeWayTransform
    * @param reduction
    * @tparam Element
    * @tparam Result
    * @return
    */
  def apply[Element, Result](
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  )(
      partitionSizeFraction: Double,
      equality: Eq[Element],
      hash: Element => Array[Byte]
  )(
      threeWayTransform: Input[Element] => Result,
      reduction: (Result, Result) => Result
  ): Result =
    require(0 <= partitionSizeFraction)
    require(1 >= partitionSizeFraction)

    def transformThroughPartitions(
        base: IndexedSeq[Element],
        left: IndexedSeq[Element],
        right: IndexedSeq[Element]
    ): Result =
      val fingerprintSize = Math
        .round(
          partitionSizeFraction * (base.length min left.length min right.length)
        )
        .toInt

      if 0 == fingerprintSize then
        threeWayTransform(
          Input(
            base = base,
            left = left,
            right = right,
            isCommonPartition = false
          )
        )
      else
        val fingerprinting =
          // NOTE: the window size is really an underestimate, as each element
          // yields a byte array.
          new RabinFingerprintLongWindowed(
            polynomial,
            /*NASTY HACK ...*/ 4 * /*... END OF NASTY HACK*/ fingerprintSize
          )

        def fingerprintStartIndices(
            elements: IndexedSeq[Element]
        ): SortedMap[Long, Int] =
          // Fingerprinting is imperative, so go with that style local to this
          // helper function...
          val accumulatingResults = mutable.TreeMap.empty[Long, Int]

          def updateFingerprint(elementIndex: Int): Unit =
            val elementBytes = hash(elements(elementIndex))

            elementBytes.foreach(fingerprinting.pushByte)
          end updateFingerprint

          // Prime to get ready for the first fingerprint...
          0 until fingerprintSize foreach updateFingerprint

          // ... henceforth, each pass records a new fingerprint, starting with
          // the first.
          0 until (elements.size - fingerprintSize) foreach:
            fingerprintStartIndex =>
              accumulatingResults.addOne(
                fingerprinting.getFingerprintLong -> fingerprintStartIndex
              )

              updateFingerprint(fingerprintSize + fingerprintStartIndex)

          accumulatingResults
        end fingerprintStartIndices

        val baseFingerprintStartIndices  = fingerprintStartIndices(base)
        val leftFingerprintStartIndices  = fingerprintStartIndices(left)
        val rightFingerprintStartIndices = fingerprintStartIndices(right)

        case class Partitions(
            preceding: IndexedSeq[Element],
            common: IndexedSeq[Element],
            succeeding: IndexedSeq[Element]
        )
        case class PartitionedSides(
            base: Partitions,
            left: Partitions,
            right: Partitions
        ):
          assume(base.common == left.common)
          assume(base.common == right.common)
        end PartitionedSides

        def matchingFingerprintAcrossSides(
            baseFingerprintStartIndices: SortedMap[Long, Int],
            leftFingerprintStartIndices: SortedMap[Long, Int],
            rightFingerprintStartIndices: SortedMap[Long, Int]
        ): Option[PartitionedSides] =
          @tailrec
          def matchingFingerprintAcrossSides(
              baseFingerprints: Iterable[Long],
              leftFingerprints: Iterable[Long],
              rightFingerprints: Iterable[Long]
          ): Option[PartitionedSides] =
            if baseFingerprints.isEmpty || leftFingerprints.isEmpty || rightFingerprints.isEmpty
            then None
            else
              val baseHead  = baseFingerprints.head
              val leftHead  = leftFingerprints.head
              val rightHead = rightFingerprints.head

              val maximumFingerprint = baseHead max leftHead max rightHead

              val minimumFingerprint = baseHead min leftHead min rightHead

              if maximumFingerprint == minimumFingerprint then
                def partitionElements(
                    elements: IndexedSeq[Element],
                    fingerprintStartIndex: Int
                ): Partitions =
                  val (precedingPartition, tail) =
                    elements.splitAt(fingerprintStartIndex)
                  val (commonPartition, succeedingPartition) =
                    tail.splitAt(fingerprintSize)

                  Partitions(
                    precedingPartition,
                    commonPartition,
                    succeedingPartition
                  )
                end partitionElements

                val basePartitions = partitionElements(
                  base,
                  baseFingerprintStartIndices(maximumFingerprint)
                )
                val leftPartitions = partitionElements(
                  left,
                  leftFingerprintStartIndices(maximumFingerprint)
                )
                val rightPartitions = partitionElements(
                  right,
                  rightFingerprintStartIndices(maximumFingerprint)
                )

                // Have to perform a check of the common partitions, because we
                // might have fingerprint collisions either within the same side
                // or across sides...
                Option.when(
                  basePartitions.common == leftPartitions.common && basePartitions.common == rightPartitions.common
                )(
                  PartitionedSides(
                    base = basePartitions,
                    left = leftPartitions,
                    right = rightPartitions
                  )
                )
              else
                matchingFingerprintAcrossSides(
                  if maximumFingerprint == baseHead then baseFingerprints
                  else baseFingerprints.tail,
                  if maximumFingerprint == leftHead then leftFingerprints
                  else leftFingerprints.tail,
                  if maximumFingerprint == rightHead then rightFingerprints
                  else rightFingerprints.tail
                )
              end if
            end if
          end matchingFingerprintAcrossSides

          matchingFingerprintAcrossSides(
            baseFingerprintStartIndices.keys,
            leftFingerprintStartIndices.keys,
            rightFingerprintStartIndices.keys
          )
        end matchingFingerprintAcrossSides

        matchingFingerprintAcrossSides(
          baseFingerprintStartIndices,
          leftFingerprintStartIndices,
          rightFingerprintStartIndices
        ) match
          case Some(
                PartitionedSides(
                  Partitions(
                    basePrecedingPartition,
                    baseCommonPartition,
                    baseSucceedingPartition
                  ),
                  Partitions(
                    leftPrecedingPartition,
                    leftCommonPartition,
                    leftSucceedingPartition
                  ),
                  Partitions(
                    rightPrecedingPartition,
                    rightCommonPartition,
                    rightSucceedingPartition
                  )
                )
              ) =>
            val resultFromPrecedingPartition = transformThroughPartitions(
              basePrecedingPartition,
              leftPrecedingPartition,
              rightPrecedingPartition
            )

            val resultFromCommonPartition = threeWayTransform(
              Input(
                baseCommonPartition,
                leftCommonPartition,
                rightCommonPartition,
                isCommonPartition = true
              )
            )

            val resultFromSucceedingPartition = transformThroughPartitions(
              baseSucceedingPartition,
              leftSucceedingPartition,
              rightSucceedingPartition
            )

            reduction(
              reduction(
                resultFromPrecedingPartition,
                resultFromCommonPartition
              ),
              resultFromSucceedingPartition
            )

          case None =>
            threeWayTransform(
              Input(
                base = base,
                left = left,
                right = right,
                isCommonPartition = false
              )
            )
        end match
      end if
    end transformThroughPartitions

    transformThroughPartitions(base, left, right)
  end apply

  /** @param isCommonPartition
    *   True if the three sides refer to a common partition.
    * @note
    *   Even a common partition may have distinct base, left and right values -
    *   they just have to be equivalent from the point of view of [[apply]].
    */
  case class Input[Element](
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element],
      isCommonPartition: Boolean
  )

end PartitionedThreeWayTransform
