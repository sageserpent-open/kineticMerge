package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Contribution
import monocle.syntax.all.*

import scala.collection.mutable

case class LongestCommonSubsequence[Element] private (
    base: IndexedSeq[Contribution[Element]],
    left: IndexedSeq[Contribution[Element]],
    right: IndexedSeq[Contribution[Element]],
    commonSubsequenceSize: Int,
    commonToLeftAndRightOnlySize: Int,
    commonToBaseAndLeftOnlySize: Int,
    commonToBaseAndRightOnlySize: Int
):
  def addBaseDifference(
      baseElement: Element
  ): LongestCommonSubsequence[Element] =
    this
      .focus(_.base)
      .modify(_ :+ Contribution.Difference(baseElement))
  def addLeftDifference(
      leftElement: Element
  ): LongestCommonSubsequence[Element] =
    this
      .focus(_.left)
      .modify(_ :+ Contribution.Difference(leftElement))
  def addRightDifference(
      rightElement: Element
  ): LongestCommonSubsequence[Element] =
    this
      .focus(_.right)
      .modify(_ :+ Contribution.Difference(rightElement))

  def addCommonBaseAndLeft(
      baseElement: Element,
      leftElement: Element
  ): LongestCommonSubsequence[Element] =
    this
      .focus(_.base)
      .modify(
        _ :+ Contribution.CommonToBaseAndLeftOnly(baseElement)
      )
      .focus(_.left)
      .modify(
        _ :+ Contribution.CommonToBaseAndLeftOnly(leftElement)
      )
      .focus(_.commonToBaseAndLeftOnlySize)
      .modify(1 + _)

  def addCommonBaseAndRight(
      baseElement: Element,
      rightElement: Element
  ): LongestCommonSubsequence[Element] =
    this
      .focus(_.base)
      .modify(
        _ :+ Contribution.CommonToBaseAndRightOnly(baseElement)
      )
      .focus(_.right)
      .modify(
        _ :+ Contribution.CommonToBaseAndRightOnly(rightElement)
      )
      .focus(_.commonToBaseAndRightOnlySize)
      .modify(1 + _)

  def addCommonLeftAndRight(
      leftElement: Element,
      rightElement: Element
  ): LongestCommonSubsequence[Element] =
    this
      .focus(_.left)
      .modify(
        _ :+ Contribution.CommonToLeftAndRightOnly(leftElement)
      )
      .focus(_.right)
      .modify(
        _ :+ Contribution.CommonToLeftAndRightOnly(rightElement)
      )
      .focus(_.commonToLeftAndRightOnlySize)
      .modify(1 + _)

  def addCommon(
      baseElement: Element,
      leftElement: Element,
      rightElement: Element
  ): LongestCommonSubsequence[Element] =
    this
      .focus(_.base)
      .modify(_ :+ Contribution.Common(baseElement))
      .focus(_.left)
      .modify(_ :+ Contribution.Common(leftElement))
      .focus(_.right)
      .modify(_ :+ Contribution.Common(rightElement))
      .focus(_.commonSubsequenceSize)
      .modify(1 + _)

  def size: (Int, Int) =
    commonSubsequenceSize -> (commonToLeftAndRightOnlySize + commonToBaseAndLeftOnlySize + commonToBaseAndRightOnlySize)
end LongestCommonSubsequence

object LongestCommonSubsequence:
  private val sentinelIndexForExhaustion = -1

  // TODO - there could be more than one solution - could something be done to
  // choose between them? Should we yield a set instead?
  def of[Element](
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  )(equality: Eq[Element]): LongestCommonSubsequence[Element] =
    val orderBySize =
      Ordering[(Int, Int)].on[LongestCommonSubsequence[Element]](_.size)

    val partialResultsCache
        : mutable.Map[(Int, Int, Int), LongestCommonSubsequence[Element]] =
      mutable.Map.empty

    def of(
        onePastBaseIndex: Int,
        onePastLeftIndex: Int,
        onePastRightIndex: Int
    ): LongestCommonSubsequence[Element] =
      assume(0 <= onePastBaseIndex)
      assume(0 <= onePastLeftIndex)
      assume(0 <= onePastRightIndex)

      val baseIsExhausted  = 0 == onePastBaseIndex
      val leftIsExhausted  = 0 == onePastLeftIndex
      val rightIsExhausted = 0 == onePastRightIndex

      val numberOfNonEmptySides =
        Seq(baseIsExhausted, leftIsExhausted, rightIsExhausted).count(!_)

      numberOfNonEmptySides match
        case 0 | 1 =>
          LongestCommonSubsequence(
            base = Vector.tabulate(onePastBaseIndex)(index =>
              Contribution.Difference(base(index))
            ),
            left = Vector.tabulate(onePastLeftIndex)(index =>
              Contribution.Difference(left(index))
            ),
            right = Vector.tabulate(onePastRightIndex)(index =>
              Contribution.Difference(right(index))
            ),
            commonSubsequenceSize = 0,
            commonToLeftAndRightOnlySize = 0,
            commonToBaseAndLeftOnlySize = 0,
            commonToBaseAndRightOnlySize = 0
          )
        case 2 | 3 =>
          if baseIsExhausted then
            val leftIndex  = onePastLeftIndex - 1
            val rightIndex = onePastRightIndex - 1

            val leftElement  = left(leftIndex)
            val rightElement = right(rightIndex)

            partialResultsCache.getOrElseUpdate(
              (sentinelIndexForExhaustion, leftIndex, rightIndex), {
                val leftEqualsRight = equality.eqv(leftElement, rightElement)

                if leftEqualsRight then
                  of(onePastBaseIndex = 0, leftIndex, rightIndex)
                    .addCommonLeftAndRight(leftElement, rightElement)
                else
                  val resultDroppingTheEndOfTheLeft =
                    of(onePastBaseIndex = 0, leftIndex, onePastRightIndex)
                      .addLeftDifference(leftElement)

                  val resultDroppingTheEndOfTheRight =
                    of(onePastBaseIndex = 0, onePastLeftIndex, rightIndex)
                      .addRightDifference(rightElement)

                  orderBySize.max(
                    resultDroppingTheEndOfTheLeft,
                    resultDroppingTheEndOfTheRight
                  )
                end if
              }
            )
          else if leftIsExhausted then
            val baseIndex  = onePastBaseIndex - 1
            val rightIndex = onePastRightIndex - 1

            val baseElement  = base(baseIndex)
            val rightElement = right(rightIndex)

            partialResultsCache.getOrElseUpdate(
              (baseIndex, sentinelIndexForExhaustion, rightIndex), {
                val baseEqualsRight = equality.eqv(baseElement, rightElement)

                if baseEqualsRight then
                  of(baseIndex, onePastLeftIndex = 0, rightIndex)
                    .addCommonBaseAndRight(baseElement, rightElement)
                else
                  val resultDroppingTheEndOfTheBase =
                    of(baseIndex, onePastLeftIndex = 0, onePastRightIndex)
                      .addBaseDifference(baseElement)

                  val resultDroppingTheEndOfTheRight =
                    of(onePastBaseIndex, onePastLeftIndex = 0, rightIndex)
                      .addRightDifference(rightElement)

                  orderBySize.max(
                    resultDroppingTheEndOfTheBase,
                    resultDroppingTheEndOfTheRight
                  )
                end if
              }
            )
          else if rightIsExhausted then
            val baseIndex = onePastBaseIndex - 1
            val leftIndex = onePastLeftIndex - 1

            val baseElement = base(baseIndex)
            val leftElement = left(leftIndex)

            partialResultsCache.getOrElseUpdate(
              (baseIndex, leftIndex, sentinelIndexForExhaustion), {
                val baseEqualsLeft = equality.eqv(baseElement, leftElement)

                if baseEqualsLeft then
                  of(baseIndex, leftIndex, onePastRightIndex = 0)
                    .addCommonBaseAndLeft(baseElement, leftElement)
                else
                  val resultDroppingTheEndOfTheBase =
                    of(baseIndex, onePastLeftIndex, onePastRightIndex = 0)
                      .addBaseDifference(baseElement)

                  val resultDroppingTheEndOfTheLeft =
                    of(onePastBaseIndex, leftIndex, onePastRightIndex = 0)
                      .addLeftDifference(leftElement)

                  orderBySize.max(
                    resultDroppingTheEndOfTheBase,
                    resultDroppingTheEndOfTheLeft
                  )
                end if
              }
            )
          else
            val baseIndex  = onePastBaseIndex - 1
            val leftIndex  = onePastLeftIndex - 1
            val rightIndex = onePastRightIndex - 1

            val baseElement  = base(baseIndex)
            val leftElement  = left(leftIndex)
            val rightElement = right(rightIndex)

            partialResultsCache.getOrElseUpdate(
              (baseIndex, leftIndex, rightIndex), {
                val baseEqualsLeft  = equality.eqv(baseElement, leftElement)
                val baseEqualsRight = equality.eqv(baseElement, rightElement)

                if baseEqualsLeft && baseEqualsRight
                then
                  of(baseIndex, leftIndex, rightIndex)
                    .addCommon(baseElement, leftElement, rightElement)
                else
                  val resultDroppingTheBaseAndLeft = Option.when(
                    baseEqualsLeft
                  ) {
                    of(baseIndex, leftIndex, onePastRightIndex)
                      .addCommonBaseAndLeft(baseElement, leftElement)
                  }

                  val resultDroppingTheBaseAndRight = Option.when(
                    baseEqualsRight
                  ) {
                    of(baseIndex, onePastLeftIndex, rightIndex)
                      .addCommonBaseAndRight(baseElement, rightElement)
                  }

                  val leftEqualsRight = equality.eqv(leftElement, rightElement)

                  val resultDroppingTheLeftAndRight = Option.when(
                    leftEqualsRight
                  ) {
                    of(onePastBaseIndex, leftIndex, rightIndex)
                      .addCommonLeftAndRight(leftElement, rightElement)
                  }

                  val resultDroppingTheEndOfTheBase =
                    of(baseIndex, onePastLeftIndex, onePastRightIndex)
                      .addBaseDifference(baseElement)
                  val resultDroppingTheEndOfTheLeft =
                    of(onePastBaseIndex, leftIndex, onePastRightIndex)
                      .addLeftDifference(leftElement)
                  val resultDroppingTheEndOfTheRight =
                    of(onePastBaseIndex, onePastLeftIndex, rightIndex)
                      .addRightDifference(rightElement)

                  Iterator(
                    resultDroppingTheBaseAndLeft,
                    resultDroppingTheBaseAndRight,
                    resultDroppingTheLeftAndRight,
                    Some(resultDroppingTheEndOfTheBase),
                    Some(resultDroppingTheEndOfTheLeft),
                    Some(resultDroppingTheEndOfTheRight)
                  ).flatten.maxBy(_.size)
                end if
              }
            )
          end if
      end match
    end of

    of(
      base.size,
      left.size,
      right.size
    )

  end of

  enum Contribution[Element]:
    case Common(
        element: Element
    )
    case Difference(
        element: Element
    )
    case CommonToBaseAndLeftOnly(
        element: Element
    )
    case CommonToBaseAndRightOnly(
        element: Element
    )
    case CommonToLeftAndRightOnly(
        element: Element
    )

    def element: Element
  end Contribution
end LongestCommonSubsequence
