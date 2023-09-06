package com.sageserpent.kineticmerge.core

import cats.syntax.all.{catsSyntaxTuple2Semigroupal, toTraverseOps}
import cats.{Eq, Eval}
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
  private type PartialResultsCacheKey = (Int, Int, Int)

  private type PartialResultsCache[Element] =
    mutable.Map[PartialResultsCacheKey, LongestCommonSubsequence[Element]]

  private type Evaluation[Element] = Eval[LongestCommonSubsequence[Element]]

  private val sentinelIndexForExhaustion = -1

  // TODO - there could be more than one solution - could something be done to
  // choose between them? Should we yield a set instead?
  def of[Element](
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  )(equality: Eq[Element]): LongestCommonSubsequence[Element] =

    val partialResultsCache: PartialResultsCache[Element] = mutable.Map.empty

    def getOrElseUpdate(
        partialResultsCacheKey: PartialResultsCacheKey,
        evaluation: => Evaluation[Element]
    ): Evaluation[Element] = for
      cachedResult <- Eval.now(
        partialResultsCache.get(partialResultsCacheKey)
      )
      result <- cachedResult.fold(ifEmpty = for
        evaluated <- evaluation
        _: None.type = partialResultsCache
          .put(partialResultsCacheKey, evaluated): @unchecked
      yield evaluated)(Eval.now)
    yield result

    def of(
        onePastBaseIndex: Int,
        onePastLeftIndex: Int,
        onePastRightIndex: Int
    ): Evaluation[Element] =
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
          Eval.now(
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
          )
        case 2 | 3 =>
          if baseIsExhausted then
            val leftIndex  = onePastLeftIndex - 1
            val rightIndex = onePastRightIndex - 1

            val leftElement  = left(leftIndex)
            val rightElement = right(rightIndex)

            getOrElseUpdate(
              (sentinelIndexForExhaustion, leftIndex, rightIndex), {
                val leftEqualsRight = equality.eqv(leftElement, rightElement)

                if leftEqualsRight then
                  of(onePastBaseIndex = 0, leftIndex, rightIndex).map(
                    _.focus(_.left)
                      .modify(
                        _ :+ Contribution.CommonToLeftAndRightOnly(leftElement)
                      )
                      .focus(_.right)
                      .modify(
                        _ :+ Contribution.CommonToLeftAndRightOnly(rightElement)
                      )
                      .focus(_.commonToLeftAndRightOnlySize)
                      .modify(1 + _)
                  )
                else
                  val resultDroppingTheEndOfTheLeft =
                    of(onePastBaseIndex = 0, leftIndex, onePastRightIndex).map(
                      _.focus(_.left)
                        .modify(_ :+ Contribution.Difference(leftElement))
                    )

                  val resultDroppingTheEndOfTheRight =
                    of(onePastBaseIndex = 0, onePastLeftIndex, rightIndex).map(
                      _.focus(_.right)
                        .modify(_ :+ Contribution.Difference(rightElement))
                    )

                  (
                    resultDroppingTheEndOfTheLeft,
                    resultDroppingTheEndOfTheRight
                  ).mapN(Seq(_, _).maxBy(_.size))
                end if
              }
            )
          else if leftIsExhausted then
            val baseIndex  = onePastBaseIndex - 1
            val rightIndex = onePastRightIndex - 1

            val baseElement  = base(baseIndex)
            val rightElement = right(rightIndex)

            getOrElseUpdate(
              (baseIndex, sentinelIndexForExhaustion, rightIndex), {
                val baseEqualsRight = equality.eqv(baseElement, rightElement)

                if baseEqualsRight then
                  of(baseIndex, onePastLeftIndex = 0, rightIndex).map(
                    _.focus(_.base)
                      .modify(
                        _ :+ Contribution.CommonToBaseAndRightOnly(baseElement)
                      )
                      .focus(_.right)
                      .modify(
                        _ :+ Contribution.CommonToBaseAndRightOnly(rightElement)
                      )
                      .focus(_.commonToBaseAndRightOnlySize)
                      .modify(1 + _)
                  )
                else
                  val resultDroppingTheEndOfTheBase =
                    of(baseIndex, onePastLeftIndex = 0, onePastRightIndex).map(
                      _.focus(_.base)
                        .modify(_ :+ Contribution.Difference(baseElement))
                    )

                  val resultDroppingTheEndOfTheRight =
                    of(onePastBaseIndex, onePastLeftIndex = 0, rightIndex).map(
                      _.focus(_.right)
                        .modify(_ :+ Contribution.Difference(rightElement))
                    )

                  (
                    resultDroppingTheEndOfTheBase,
                    resultDroppingTheEndOfTheRight
                  ).mapN(Seq(_, _).maxBy(_.size))
                end if
              }
            )
          else if rightIsExhausted then
            val baseIndex = onePastBaseIndex - 1
            val leftIndex = onePastLeftIndex - 1

            val baseElement = base(baseIndex)
            val leftElement = left(leftIndex)

            getOrElseUpdate(
              (baseIndex, leftIndex, sentinelIndexForExhaustion), {
                val baseEqualsLeft = equality.eqv(baseElement, leftElement)

                if baseEqualsLeft then
                  of(baseIndex, leftIndex, onePastRightIndex = 0).map(
                    _.focus(_.base)
                      .modify(
                        _ :+ Contribution.CommonToBaseAndLeftOnly(baseElement)
                      )
                      .focus(_.left)
                      .modify(
                        _ :+ Contribution.CommonToBaseAndLeftOnly(leftElement)
                      )
                      .focus(_.commonToBaseAndLeftOnlySize)
                      .modify(1 + _)
                  )
                else
                  val resultDroppingTheEndOfTheBase =
                    of(baseIndex, onePastLeftIndex, onePastRightIndex = 0).map(
                      _.focus(_.base)
                        .modify(_ :+ Contribution.Difference(baseElement))
                    )

                  val resultDroppingTheEndOfTheLeft =
                    of(onePastBaseIndex, leftIndex, onePastRightIndex = 0).map(
                      _.focus(_.left)
                        .modify(_ :+ Contribution.Difference(leftElement))
                    )

                  (resultDroppingTheEndOfTheBase, resultDroppingTheEndOfTheLeft)
                    .mapN(Seq(_, _).maxBy(_.size))
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

            getOrElseUpdate(
              (baseIndex, leftIndex, rightIndex), {
                val baseEqualsLeft  = equality.eqv(baseElement, leftElement)
                val baseEqualsRight = equality.eqv(baseElement, rightElement)

                if baseEqualsLeft && baseEqualsRight
                then
                  of(baseIndex, leftIndex, rightIndex).map(
                    _.focus(_.base)
                      .modify(_ :+ Contribution.Common(baseElement))
                      .focus(_.left)
                      .modify(_ :+ Contribution.Common(leftElement))
                      .focus(_.right)
                      .modify(_ :+ Contribution.Common(rightElement))
                      .focus(_.commonSubsequenceSize)
                      .modify(1 + _)
                  )
                else
                  val resultDroppingTheBaseAndLeft = Option.when(
                    baseEqualsLeft
                  ) {
                    of(baseIndex, leftIndex, onePastRightIndex).map(
                      _.focus(_.base)
                        .modify(
                          _ :+ Contribution.CommonToBaseAndLeftOnly(baseElement)
                        )
                        .focus(_.left)
                        .modify(
                          _ :+ Contribution.CommonToBaseAndLeftOnly(leftElement)
                        )
                        .focus(_.commonToBaseAndLeftOnlySize)
                        .modify(1 + _)
                    )
                  }

                  val resultDroppingTheBaseAndRight = Option.when(
                    baseEqualsRight
                  ) {
                    of(baseIndex, onePastLeftIndex, rightIndex).map(
                      _.focus(_.base)
                        .modify(
                          _ :+ Contribution.CommonToBaseAndRightOnly(
                            baseElement
                          )
                        )
                        .focus(_.right)
                        .modify(
                          _ :+ Contribution.CommonToBaseAndRightOnly(
                            rightElement
                          )
                        )
                        .focus(_.commonToBaseAndRightOnlySize)
                        .modify(1 + _)
                    )
                  }

                  val leftEqualsRight = equality.eqv(leftElement, rightElement)

                  val resultDroppingTheLeftAndRight = Option.when(
                    leftEqualsRight
                  ) {
                    of(onePastBaseIndex, leftIndex, rightIndex).map(
                      _.focus(_.left)
                        .modify(
                          _ :+ Contribution.CommonToLeftAndRightOnly(
                            leftElement
                          )
                        )
                        .focus(_.right)
                        .modify(
                          _ :+ Contribution.CommonToLeftAndRightOnly(
                            rightElement
                          )
                        )
                        .focus(_.commonToLeftAndRightOnlySize)
                        .modify(1 + _)
                    )
                  }

                  val resultDroppingTheEndOfTheBase =
                    of(baseIndex, onePastLeftIndex, onePastRightIndex).map(
                      _.focus(_.base)
                        .modify(_ :+ Contribution.Difference(baseElement))
                    )
                  val resultDroppingTheEndOfTheLeft =
                    of(onePastBaseIndex, leftIndex, onePastRightIndex).map(
                      _.focus(_.left)
                        .modify(_ :+ Contribution.Difference(leftElement))
                    )
                  val resultDroppingTheEndOfTheRight =
                    of(onePastBaseIndex, onePastLeftIndex, rightIndex).map(
                      _.focus(_.right)
                        .modify(_ :+ Contribution.Difference(rightElement))
                    )

                  Seq(
                    resultDroppingTheBaseAndLeft,
                    resultDroppingTheBaseAndRight,
                    resultDroppingTheLeftAndRight,
                    Some(resultDroppingTheEndOfTheBase),
                    Some(resultDroppingTheEndOfTheLeft),
                    Some(resultDroppingTheEndOfTheRight)
                  ).flatten.sequence.map(_.maxBy(_.size))
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
    ).value

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
