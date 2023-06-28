package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Contribution
import monocle.syntax.all.*

import scala.collection.mutable

// TODO: why use an `IndexedSeq[Contribution]` to model an externally-imposed
// order of contributions when `Contribution` is already ordered by virtue of
// its index? Either drop the indices and use the external ordering, or use a
// sorted set. In fact, why not simply use `IndexedSeq[Element]`?
case class LongestCommonSubsequence private (
    base: IndexedSeq[Contribution],
    left: IndexedSeq[Contribution],
    right: IndexedSeq[Contribution],
    commonSubsequenceSize: Int
):

end LongestCommonSubsequence

object LongestCommonSubsequence:
  /** NOTE: the index is taken in the context of the original base, left and
    * right elements.
    */
  enum Contribution:
    case Common(
        indexInContributor: Int
    ) // The indexed element belongs to the longest common subsequence across the base, left and right.
    case Difference(
        indexInContributor: Int
    ) // The indexed element has been added with respect to the longest common subsequence across the base, left and right.
  end Contribution

  // TODO - there could be more than one solution - could something be done to
  // choose between them? Should we yield a set instead?
  def of[Element](
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  )(equality: Eq[Element]): LongestCommonSubsequence =
    val partialResultsCache
        : mutable.Map[(Int, Int, Int), LongestCommonSubsequence] =
      mutable.Map.empty

    def of(
        onePastBaseIndex: Int,
        onePastLeftIndex: Int,
        onePastRightIndex: Int
    ): LongestCommonSubsequence =
      val minimumIndex =
        onePastBaseIndex min onePastLeftIndex min onePastRightIndex

      assume(0 <= minimumIndex)

      if 0 == minimumIndex then
        LongestCommonSubsequence(
          base = Vector.tabulate(onePastBaseIndex)(
            Contribution.Difference.apply
          ),
          left = Vector.tabulate(onePastLeftIndex)(
            Contribution.Difference.apply
          ),
          right = Vector.tabulate(onePastRightIndex)(
            Contribution.Difference.apply
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
                .modify(_ :+ Contribution.Common(baseIndex))
                .focus(_.left)
                .modify(_ :+ Contribution.Common(leftIndex))
                .focus(_.right)
                .modify(_ :+ Contribution.Common(rightIndex))
                .focus(_.commonSubsequenceSize)
                .modify(1 + _)
            else
              val resultDroppingTheEndOfTheBase = of(
                baseIndex,
                onePastLeftIndex,
                onePastRightIndex
              ).focus(_.base).modify(_ :+ Contribution.Difference(baseIndex))
              val resultDroppingTheEndOfTheLeft = of(
                onePastBaseIndex,
                leftIndex,
                onePastRightIndex
              ).focus(_.left).modify(_ :+ Contribution.Difference(leftIndex))
              val resultDroppingTheEndOfTheRight = of(
                onePastBaseIndex,
                onePastLeftIndex,
                rightIndex
              ).focus(_.right).modify(_ :+ Contribution.Difference(rightIndex))

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
end LongestCommonSubsequence
