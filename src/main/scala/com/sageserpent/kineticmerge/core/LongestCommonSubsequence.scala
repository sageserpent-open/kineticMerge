package com.sageserpent.kineticmerge.core

import cats.data.StateT
import cats.syntax.all.{catsSyntaxTuple2Semigroupal, catsSyntaxTuple3Semigroupal}
import cats.{Eq, Id}
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.{CommonSubsequenceSize, Contribution}
import monocle.syntax.all.*

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
  def defaultElementSize[Element](irrelevant: Element): Int = 1

  def of[Element: Eq: Sized](
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  ): LongestCommonSubsequence[Element] =
    given orderBySize: Ordering[LongestCommonSubsequence[Element]] =
      given Ordering[CommonSubsequenceSize] =
        Ordering.by(size => size.elementSizeSum)

      Ordering.by(_.size)
    end orderBySize

    val equality = summon[Eq[Element]]
    val sized    = summon[Sized[Element]]

    object mutualRecursionWorkaround:
      type PartialResultKey = (Int, Int, Int)

      type PartialResultsCache =
        Map[PartialResultKey, LongestCommonSubsequence[Element]]

      type EvalWithPartialResultState[X] = StateT[Id, PartialResultsCache, X]

      def of(
          onePastBaseIndex: Int,
          onePastLeftIndex: Int,
          onePastRightIndex: Int
      ): EvalWithPartialResultState[LongestCommonSubsequence[Element]] =
        StateT.get.flatMap { partialResultsCache =>
          val cachedResult = partialResultsCache.get(
            (onePastBaseIndex, onePastLeftIndex, onePastRightIndex)
          )

          cachedResult.fold(
            ifEmpty = for
              computedResult <- _of(
                onePastBaseIndex,
                onePastLeftIndex,
                onePastRightIndex
              )
              _ <- StateT.modify[Id, PartialResultsCache](
                _ + ((
                  onePastBaseIndex,
                  onePastLeftIndex,
                  onePastRightIndex
                ) -> computedResult)
              )
            yield computedResult
          )(StateT.pure)
        }
      end of

      def _of(
          onePastBaseIndex: Int,
          onePastLeftIndex: Int,
          onePastRightIndex: Int
      ): EvalWithPartialResultState[LongestCommonSubsequence[Element]] =
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
            StateT.pure(
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
            )
          case 2 | 3 =>
            if baseIsExhausted then
              val leftIndex  = onePastLeftIndex - 1
              val rightIndex = onePastRightIndex - 1

              val leftElement  = left(leftIndex)
              val rightElement = right(rightIndex)

              val leftEqualsRight = equality.eqv(leftElement, rightElement)

              if leftEqualsRight then
                of(onePastBaseIndex = 0, leftIndex, rightIndex).map(
                  _.addCommonLeftAndRight(leftElement, rightElement)(
                    sized.sizeOf
                  )
                )
              else
                for
                  resultDroppingTheEndOfTheLeft <-
                    of(onePastBaseIndex = 0, leftIndex, onePastRightIndex)
                      .map(_.addLeftDifference(leftElement))

                  resultDroppingTheEndOfTheRight <-
                    of(onePastBaseIndex = 0, onePastLeftIndex, rightIndex)
                      .map(_.addRightDifference(rightElement))
                yield orderBySize.max(
                  resultDroppingTheEndOfTheLeft,
                  resultDroppingTheEndOfTheRight
                )
              end if
            else if leftIsExhausted then
              val baseIndex  = onePastBaseIndex - 1
              val rightIndex = onePastRightIndex - 1

              val baseElement  = base(baseIndex)
              val rightElement = right(rightIndex)

              val baseEqualsRight = equality.eqv(baseElement, rightElement)

              if baseEqualsRight then
                of(baseIndex, onePastLeftIndex = 0, rightIndex)
                  .map(
                    _.addCommonBaseAndRight(baseElement, rightElement)(
                      sized.sizeOf
                    )
                  )
              else
                for
                  resultDroppingTheEndOfTheBase <-
                    of(baseIndex, onePastLeftIndex = 0, onePastRightIndex)
                      .map(_.addBaseDifference(baseElement))

                  resultDroppingTheEndOfTheRight <-
                    of(onePastBaseIndex, onePastLeftIndex = 0, rightIndex)
                      .map(_.addRightDifference(rightElement))
                yield orderBySize.max(
                  resultDroppingTheEndOfTheBase,
                  resultDroppingTheEndOfTheRight
                )
              end if
            else if rightIsExhausted then
              val baseIndex = onePastBaseIndex - 1
              val leftIndex = onePastLeftIndex - 1

              val baseElement = base(baseIndex)
              val leftElement = left(leftIndex)

              val baseEqualsLeft = equality.eqv(baseElement, leftElement)

              if baseEqualsLeft then
                of(baseIndex, leftIndex, onePastRightIndex = 0)
                  .map(
                    _.addCommonBaseAndLeft(baseElement, leftElement)(
                      sized.sizeOf
                    )
                  )
              else
                for
                  resultDroppingTheEndOfTheBase <-
                    of(baseIndex, onePastLeftIndex, onePastRightIndex = 0)
                      .map(_.addBaseDifference(baseElement))

                  resultDroppingTheEndOfTheLeft <-
                    of(onePastBaseIndex, leftIndex, onePastRightIndex = 0)
                      .map(_.addLeftDifference(leftElement))
                yield orderBySize.max(
                  resultDroppingTheEndOfTheBase,
                  resultDroppingTheEndOfTheLeft
                )
              end if
            else
              val baseIndex  = onePastBaseIndex - 1
              val leftIndex  = onePastLeftIndex - 1
              val rightIndex = onePastRightIndex - 1

              val baseElement  = base(baseIndex)
              val leftElement  = left(leftIndex)
              val rightElement = right(rightIndex)

              val baseEqualsLeft  = equality.eqv(baseElement, leftElement)
              val baseEqualsRight = equality.eqv(baseElement, rightElement)

              if baseEqualsLeft && baseEqualsRight
              then
                of(baseIndex, leftIndex, rightIndex)
                  .map(
                    _.addCommon(baseElement, leftElement, rightElement)(
                      sized.sizeOf
                    )
                  )
              else
                val leftEqualsRight = equality.eqv(leftElement, rightElement)

                // NOTE: at this point, we can't have any two of
                // `baseEqualsLeft`, `baseEqualsRight` or `leftEqualsRight`
                // being true - because by transitive equality, that would imply
                // all three sides are equal, and thus we should be following
                // other branch. So we have to use all the next three bindings
                // one way or the other...

                val resultDroppingTheEndOfTheBase =
                  of(baseIndex, onePastLeftIndex, onePastRightIndex)
                    .map(_.addBaseDifference(baseElement))

                val resultDroppingTheEndOfTheLeft =
                  of(onePastBaseIndex, leftIndex, onePastRightIndex)
                    .map(_.addLeftDifference(leftElement))

                val resultDroppingTheEndOfTheRight =
                  of(onePastBaseIndex, onePastLeftIndex, rightIndex)
                    .map(_.addRightDifference(rightElement))

                val resultDroppingTheBaseAndLeft =
                  if baseEqualsLeft then
                    of(baseIndex, leftIndex, onePastRightIndex)
                      .map(
                        _.addCommonBaseAndLeft(baseElement, leftElement)(
                          sized.sizeOf
                        )
                      )
                  else
                    (
                      resultDroppingTheEndOfTheBase,
                      resultDroppingTheEndOfTheLeft
                    ).mapN(orderBySize.max)
                  end if
                end resultDroppingTheBaseAndLeft

                val resultDroppingTheBaseAndRight =
                  if baseEqualsRight then
                    of(baseIndex, onePastLeftIndex, rightIndex)
                      .map(
                        _.addCommonBaseAndRight(baseElement, rightElement)(
                          sized.sizeOf
                        )
                      )
                  else
                    (
                      resultDroppingTheEndOfTheBase,
                      resultDroppingTheEndOfTheRight
                    ).mapN(orderBySize.max)
                  end if
                end resultDroppingTheBaseAndRight

                val resultDroppingTheLeftAndRight =
                  if leftEqualsRight then
                    of(onePastBaseIndex, leftIndex, rightIndex)
                      .map(
                        _.addCommonLeftAndRight(leftElement, rightElement)(
                          sized.sizeOf
                        )
                      )
                  else
                    (
                      resultDroppingTheEndOfTheLeft,
                      resultDroppingTheEndOfTheRight
                    ).mapN(orderBySize.max)
                  end if
                end resultDroppingTheLeftAndRight

                (
                  resultDroppingTheBaseAndLeft,
                  resultDroppingTheBaseAndRight,
                  resultDroppingTheLeftAndRight
                ).mapN((first, second, third) =>
                  orderBySize.max(orderBySize.max(first, second), third)
                )
              end if
            end if
        end match
      end _of
    end mutualRecursionWorkaround

    val (partialResultsCache, result) = mutualRecursionWorkaround
      .of(
        base.size,
        left.size,
        right.size
      )
      .run(Map.empty)

    // TODO: this is just debugging cruft, remove it...
    println(
      partialResultsCache.size -> (1L + base.size) * (1L + left.size) * (1L + right.size)
    )

    result
  end of

  trait Sized[Element]:
    def sizeOf(element: Element): Int
  end Sized

  /** @todo
    *   The parameter [[length]] needs review - its only use is by
    *   [[LongestCommonSubsequenceTest.theLongestCommonSubsequenceUnderpinsAllThreeResults]].
    *   It's great that said test passes, but could it be recast to not use this
    *   parameter?
    * @param length
    * @param elementSizeSum
    */
  case class CommonSubsequenceSize(
      length: Int,
      elementSizeSum: Int
  ):
    def addCostOfASingleContribution(size: Int): CommonSubsequenceSize = this
      .focus(_.length)
      .modify(1 + _)
      .focus(_.elementSizeSum)
      .modify(size + _)

    def plus(that: CommonSubsequenceSize): CommonSubsequenceSize =
      CommonSubsequenceSize(
        length = this.length + that.length,
        elementSizeSum = this.elementSizeSum + that.elementSizeSum
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
    val zero = CommonSubsequenceSize(length = 0, elementSizeSum = 0)
  end CommonSubsequenceSize
end LongestCommonSubsequence
