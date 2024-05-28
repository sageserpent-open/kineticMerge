package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.{CommonSubsequenceSize, Contribution}
import monocle.syntax.all.*

import scala.collection.mutable

case class LongestCommonSubsequence[Element] private (
    base: IndexedSeq[Contribution[Element]],
    left: IndexedSeq[Contribution[Element]],
    right: IndexedSeq[Contribution[Element]],
    commonSubsequenceSize: CommonSubsequenceSize,
    commonToLeftAndRightOnlySize: CommonSubsequenceSize,
    commonToBaseAndLeftOnlySize: CommonSubsequenceSize,
    commonToBaseAndRightOnlySize: CommonSubsequenceSize
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
  )(elementSize: Element => Int): LongestCommonSubsequence[Element] =
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
      .modify(
        _.addCostOfASingleContribution(
          elementSize(baseElement) max elementSize(leftElement)
        )
      )

  def addCommonBaseAndRight(
      baseElement: Element,
      rightElement: Element
  )(elementSize: Element => Int): LongestCommonSubsequence[Element] =
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
      .modify(
        _.addCostOfASingleContribution(
          elementSize(baseElement) max elementSize(rightElement)
        )
      )

  def addCommonLeftAndRight(
      leftElement: Element,
      rightElement: Element
  )(elementSize: Element => Int): LongestCommonSubsequence[Element] =
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
      .modify(
        _.addCostOfASingleContribution(
          elementSize(leftElement) max elementSize(rightElement)
        )
      )

  def addCommon(
      baseElement: Element,
      leftElement: Element,
      rightElement: Element
  )(elementSize: Element => Int): LongestCommonSubsequence[Element] =
    this
      .focus(_.base)
      .modify(_ :+ Contribution.Common(baseElement))
      .focus(_.left)
      .modify(_ :+ Contribution.Common(leftElement))
      .focus(_.right)
      .modify(_ :+ Contribution.Common(rightElement))
      .focus(_.commonSubsequenceSize)
      .modify(
        _.addCostOfASingleContribution(
          elementSize(baseElement) max elementSize(leftElement) max elementSize(
            rightElement
          )
        )
      )

  def size: (CommonSubsequenceSize, CommonSubsequenceSize) =
    commonSubsequenceSize -> (commonToLeftAndRightOnlySize plus commonToBaseAndLeftOnlySize plus commonToBaseAndRightOnlySize)
end LongestCommonSubsequence

object LongestCommonSubsequence:
  private val sentinelIndexForExhaustion = -1

  def defaultElementSize[Element](irrelevant: Element): Int = 1

  def of[Element: Eq: Sized](
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  ): LongestCommonSubsequence[Element] =
    given orderBySize: Ordering[LongestCommonSubsequence[Element]] =
      given Ordering[CommonSubsequenceSize] =
        Ordering.by(size => size.length -> size.elementSizeSumTiebreaker)

      Ordering.by(_.size)
    end orderBySize

    val equality = summon[Eq[Element]]
    val sized    = summon[Sized[Element]]

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
            commonSubsequenceSize = CommonSubsequenceSize.zero,
            commonToLeftAndRightOnlySize = CommonSubsequenceSize.zero,
            commonToBaseAndLeftOnlySize = CommonSubsequenceSize.zero,
            commonToBaseAndRightOnlySize = CommonSubsequenceSize.zero
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
                    .addCommonLeftAndRight(leftElement, rightElement)(
                      sized.sizeOf
                    )
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
                    .addCommonBaseAndRight(baseElement, rightElement)(
                      sized.sizeOf
                    )
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
                    .addCommonBaseAndLeft(baseElement, leftElement)(
                      sized.sizeOf
                    )
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
                    .addCommon(baseElement, leftElement, rightElement)(
                      sized.sizeOf
                    )
                else
                  lazy val resultDroppingTheEndOfTheBase =
                    of(baseIndex, onePastLeftIndex, onePastRightIndex)
                      .addBaseDifference(baseElement)

                  lazy val resultDroppingTheEndOfTheLeft =
                    of(onePastBaseIndex, leftIndex, onePastRightIndex)
                      .addLeftDifference(leftElement)

                  lazy val resultDroppingTheEndOfTheRight =
                    of(onePastBaseIndex, onePastLeftIndex, rightIndex)
                      .addRightDifference(rightElement)

                  val resultDroppingTheBaseAndLeft =
                    if baseEqualsLeft then
                      of(baseIndex, leftIndex, onePastRightIndex)
                        .addCommonBaseAndLeft(baseElement, leftElement)(
                          sized.sizeOf
                        )
                    else
                      orderBySize.max(
                        resultDroppingTheEndOfTheBase,
                        resultDroppingTheEndOfTheLeft
                      )
                    end if
                  end resultDroppingTheBaseAndLeft

                  val resultDroppingTheBaseAndRight =
                    if baseEqualsRight then
                      of(baseIndex, onePastLeftIndex, rightIndex)
                        .addCommonBaseAndRight(baseElement, rightElement)(
                          sized.sizeOf
                        )
                    else
                      orderBySize.max(
                        resultDroppingTheEndOfTheBase,
                        resultDroppingTheEndOfTheRight
                      )
                    end if
                  end resultDroppingTheBaseAndRight

                  val leftEqualsRight = equality.eqv(leftElement, rightElement)

                  val resultDroppingTheLeftAndRight =
                    if leftEqualsRight then
                      of(onePastBaseIndex, leftIndex, rightIndex)
                        .addCommonLeftAndRight(leftElement, rightElement)(
                          sized.sizeOf
                        )
                    else
                      orderBySize.max(
                        resultDroppingTheEndOfTheLeft,
                        resultDroppingTheEndOfTheRight
                      )
                    end if
                  end resultDroppingTheLeftAndRight

                  Iterator(
                    resultDroppingTheBaseAndLeft,
                    resultDroppingTheBaseAndRight,
                    resultDroppingTheLeftAndRight
                  ).max(orderBySize)
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

  trait Sized[Element]:
    def sizeOf(element: Element): Int
  end Sized

  case class CommonSubsequenceSize(length: Int, elementSizeSumTiebreaker: Int):
    def addCostOfASingleContribution(size: Int): CommonSubsequenceSize = this
      .focus(_.length)
      .modify(1 + _)
      .focus(_.elementSizeSumTiebreaker)
      .modify(size + _)

    def plus(that: CommonSubsequenceSize): CommonSubsequenceSize =
      CommonSubsequenceSize(
        length = this.length + that.length,
        elementSizeSumTiebreaker =
          this.elementSizeSumTiebreaker + that.elementSizeSumTiebreaker
      )
  end CommonSubsequenceSize

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

  object CommonSubsequenceSize:
    val zero = CommonSubsequenceSize(length = 0, elementSizeSumTiebreaker = 0)
  end CommonSubsequenceSize
end LongestCommonSubsequence
