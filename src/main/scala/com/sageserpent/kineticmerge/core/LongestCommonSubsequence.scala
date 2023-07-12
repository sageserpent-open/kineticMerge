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
  def size: (Int, Int) =
    commonSubsequenceSize -> (commonToLeftAndRightOnlySize + commonToBaseAndLeftOnlySize + commonToBaseAndRightOnlySize)
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
      assume(0 <= onePastBaseIndex)
      assume(0 <= onePastLeftIndex)
      assume(0 <= onePastRightIndex)

      val numberOfNonZeroIndices =
        Seq(onePastBaseIndex, onePastLeftIndex, onePastRightIndex).count(0 < _)

      numberOfNonZeroIndices match
        case 0 | 1 | 2 => // TODO - remove the `2` case...
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
//        case 2 =>
//          if 0 == onePastRightIndex then
//            ??? // TODO: recurse to find matches between the base and left...
//          else if 0 == onePastLeftIndex then
//            ??? // TODO: recurse to find matches between the base and right...
//          else
//            ??? // TODO: recurse to find matches between the left and right...
        case 3 =>
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
                  .focus(_.base)
                  .modify(_ :+ Contribution.Common(baseElement))
                  .focus(_.left)
                  .modify(_ :+ Contribution.Common(leftElement))
                  .focus(_.right)
                  .modify(_ :+ Contribution.Common(rightElement))
                  .focus(_.commonSubsequenceSize)
                  .modify(1 + _)
              else
                val resultDroppingTheBaseAndLeft = Option.when(
                  baseEqualsLeft
                ) {
                  of(baseIndex, leftIndex, onePastRightIndex)
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
                }

                val resultDroppingTheBaseAndRight = Option.when(
                  baseEqualsRight
                ) {
                  of(baseIndex, onePastLeftIndex, rightIndex)
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
                }

                val leftEqualsRight = equality.eqv(leftElement, rightElement)

                val resultDroppingTheLeftAndRight = Option.when(
                  leftEqualsRight
                ) {
                  of(onePastBaseIndex, leftIndex, rightIndex)
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
                }

                val resultDroppingTheEndOfTheBase =
                  of(baseIndex, onePastLeftIndex, onePastRightIndex)
                    .focus(_.base)
                    .modify(_ :+ Contribution.Difference(baseElement))
                val resultDroppingTheEndOfTheLeft =
                  of(onePastBaseIndex, leftIndex, onePastRightIndex)
                    .focus(_.left)
                    .modify(_ :+ Contribution.Difference(leftElement))
                val resultDroppingTheEndOfTheRight =
                  of(onePastBaseIndex, onePastLeftIndex, rightIndex)
                    .focus(_.right)
                    .modify(_ :+ Contribution.Difference(rightElement))

                Seq(
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
      end match
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
