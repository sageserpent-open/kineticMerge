package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Contribution
import monocle.syntax.all.*

import scala.collection.mutable

case class LongestCommonSubsequence[Element] private (
    base: IndexedSeq[Contribution[Element]],
    left: IndexedSeq[Contribution[Element]],
    right: IndexedSeq[Contribution[Element]],
    commonSubsequenceSize: Int
):

end LongestCommonSubsequence

object LongestCommonSubsequence:
  // TODO - there could be more than one solution - could something be done to
  // choose between them? Should we yield a set instead?
  def of[Element](
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  )(equality: Eq[Element]): LongestCommonSubsequence[Element] =
    val partialResultsCache
        : mutable.Map[(Int, Int, Int), LongestCommonSubsequence[Element]] =
      mutable.Map.empty

    def of(
        onePastBaseIndex: Int,
        onePastLeftIndex: Int,
        onePastRightIndex: Int
    ): LongestCommonSubsequence[Element] =
      val minimumIndex =
        onePastBaseIndex min onePastLeftIndex min onePastRightIndex

      assume(0 <= minimumIndex)

      if 0 == minimumIndex then
        LongestCommonSubsequence(
          base = Vector.tabulate(onePastBaseIndex)(index =>
            Contribution.ThreeWayDifference(base(index))
          ),
          left = Vector.tabulate(onePastLeftIndex)(index =>
            Contribution.ThreeWayDifference(left(index))
          ),
          right = Vector.tabulate(onePastRightIndex)(index =>
            Contribution.ThreeWayDifference(right(index))
          ),
          0
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
            if equality.eqv(baseElement, leftElement) && equality
                .eqv(baseElement, rightElement)
            then
              of(
                baseIndex,
                leftIndex,
                rightIndex
              ).focus(_.base)
                .modify(_ :+ Contribution.Common(baseElement))
                .focus(_.left)
                .modify(_ :+ Contribution.Common(leftElement))
                .focus(_.right)
                .modify(_ :+ Contribution.Common(rightElement))
                .focus(_.commonSubsequenceSize)
                .modify(1 + _)
            else
              val resultDroppingTheEndOfTheBase = of(
                baseIndex,
                onePastLeftIndex,
                onePastRightIndex
              ).focus(_.base).modify(_ :+ Contribution.ThreeWayDifference(baseElement))
              val resultDroppingTheEndOfTheLeft = of(
                onePastBaseIndex,
                leftIndex,
                onePastRightIndex
              ).focus(_.left).modify(_ :+ Contribution.ThreeWayDifference(leftElement))
              val resultDroppingTheEndOfTheRight = of(
                onePastBaseIndex,
                onePastLeftIndex,
                rightIndex
              ).focus(_.right)
                .modify(_ :+ Contribution.ThreeWayDifference(rightElement))

              Seq(
                resultDroppingTheEndOfTheBase,
                resultDroppingTheEndOfTheLeft,
                resultDroppingTheEndOfTheRight
              ).maxBy(_.commonSubsequenceSize)
            end if
          }
        )

      end if

    end of

    of(
      base.size,
      left.size,
      right.size
    )

  end of

  /** NOTE: the index is taken in the context of the original base, left and
    * right elements.
    */
  enum Contribution[Element]:
    case Common(
        element: Element
    ) // The element belongs to the longest common subsequence across the base, left and right.
    case ThreeWayDifference(
        element: Element
    ) // The element is different across the base, left and right.
    case BaseDifferenceOnly(
        element: Element
    ) // The element is common to the left and right only.
    case LeftDifferenceOnly(
        element: Element
    ) // The element is common to the base and right only.
    case RightDifferenceOnly(
        element: Element
    ) // The element is common to the base and left only.

    def element: Element
  end Contribution
end LongestCommonSubsequence
