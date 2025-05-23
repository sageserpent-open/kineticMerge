package com.sageserpent.kineticmerge.core

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.{catsSyntaxApplyOps, catsSyntaxFlatMapOps}
import cats.{Eq, Monad}
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.{
  CommonSubsequenceSize,
  Contribution
}
import monocle.syntax.all.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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

    /** The [[LongestCommonSubsequence]] solutions are organised into swathes,
      * where each swathe provides random-access to a solution via a notional
      * key of three indices corresponding to the base, left and right. Any
      * given swathe is populated by keys that all share at least one index
      * taking the value of the swathe's labelling index; all other indices in a
      * swathe's keys are lower than the swathe index.<p> For example, the
      * swathe of index 0 contains an entry using indices (0, 0, 0).<p> The
      * swathe of index 1 contains entries using indices (1, 1, 1), (1, 1, 0),
      * (1, 0, 1), (1, 0, 0).<p>This breakdown of keys means that a dynamic
      * programming approach can work up through the swathes, calculating
      * sub-problem solutions that depend only on solutions from within the
      * leading swathe and its predecessor.
      */
    trait Swathes:
      def consultRelevantSwatheForSolution(
          onePastBaseIndex: Int,
          onePastLeftIndex: Int,
          onePastRightIndex: Int
      ): LongestCommonSubsequence[Element]

      def storeSolutionInLeadingSwathe(
          onePastBaseIndex: Int,
          onePastLeftIndex: Int,
          onePastRightIndex: Int,
          longestCommonSubsequence: LongestCommonSubsequence[Element]
      ): Unit
    end Swathes

    object Swathes:
      private val maximumSwatheIndex =
        base.size max left.size max right.size

      def evaluateSolutionsInDependencyOrder(
          action: (Swathes, Int, Int, Int) => Unit
      ): LongestCommonSubsequence[Element] =
        object swathes extends Swathes:
          case class Storage(
              baseEqualToSwatheIndex: Array[LongestCommonSubsequence[Element]],
              leftEqualToSwatheIndex: Array[LongestCommonSubsequence[Element]],
              rightEqualToSwatheIndex: Array[LongestCommonSubsequence[Element]]
          ):
            def apply(
                swatheIndex: Int,
                onePastBaseIndex: Int,
                onePastLeftIndex: Int,
                onePastRightIndex: Int
            ): LongestCommonSubsequence[Element] =
              if swatheIndex == onePastLeftIndex then
                leftEqualToSwatheIndex(
                  onePastBaseIndex * (1 + right.size) + onePastRightIndex
                )
              else if swatheIndex == onePastRightIndex then
                rightEqualToSwatheIndex(
                  onePastBaseIndex * (1 + left.size) + onePastLeftIndex
                )
              else
                assume(swatheIndex == onePastBaseIndex)
                baseEqualToSwatheIndex(
                  onePastLeftIndex * (1 + right.size) + onePastRightIndex
                )
              end if
            end apply

            def update(
                swatheIndex: Int,
                onePastBaseIndex: Int,
                onePastLeftIndex: Int,
                onePastRightIndex: Int,
                entryToStore: LongestCommonSubsequence[Element]
            ): Unit =
              if swatheIndex == onePastLeftIndex then
                leftEqualToSwatheIndex(
                  onePastBaseIndex * (1 + right.size) + onePastRightIndex
                ) = entryToStore
              else if swatheIndex == onePastRightIndex then
                rightEqualToSwatheIndex(
                  onePastBaseIndex * (1 + left.size) + onePastLeftIndex
                ) = entryToStore
              else
                assume(swatheIndex == onePastBaseIndex)
                baseEqualToSwatheIndex(
                  onePastLeftIndex * (1 + right.size) + onePastRightIndex
                ) = entryToStore
              end if
            end update
          end Storage

          private val twoLotsOfStorage = Array(newStorage, newStorage)

          private val notYetAdvanced = -1

          private var _indexOfLeadingSwathe: Int = notYetAdvanced

          def advanceToNextLeadingSwathe(): Boolean =
            val resultSnapshotPriorToMutation = notYetReachedFinalSwathe

            if resultSnapshotPriorToMutation then _indexOfLeadingSwathe += 1

            resultSnapshotPriorToMutation
          end advanceToNextLeadingSwathe

          private def notYetReachedFinalSwathe =
            maximumSwatheIndex > _indexOfLeadingSwathe

          def topLevelSolution: LongestCommonSubsequence[Element] =
            require(!notYetReachedFinalSwathe)

            twoLotsOfStorage(storageLotForLeadingSwathe)(
              _indexOfLeadingSwathe,
              base.size,
              left.size,
              right.size
            )
          end topLevelSolution

          def consultRelevantSwatheForSolution(
              onePastBaseIndex: Int,
              onePastLeftIndex: Int,
              onePastRightIndex: Int
          ): LongestCommonSubsequence[Element] =
            require(_indexOfLeadingSwathe != notYetAdvanced)

            if indexOfLeadingSwathe == onePastBaseIndex
              || indexOfLeadingSwathe == onePastLeftIndex
              || indexOfLeadingSwathe == onePastRightIndex
            then
              twoLotsOfStorage(storageLotForLeadingSwathe)(
                _indexOfLeadingSwathe,
                onePastBaseIndex,
                onePastLeftIndex,
                onePastRightIndex
              )
            else
              twoLotsOfStorage(storageLotForPrecedingSwathe)(
                _indexOfLeadingSwathe - 1,
                onePastBaseIndex,
                onePastLeftIndex,
                onePastRightIndex
              )
            end if
          end consultRelevantSwatheForSolution

          inline private def storageLotForLeadingSwathe =
            _indexOfLeadingSwathe % 2
          end storageLotForLeadingSwathe

          inline private def storageLotForPrecedingSwathe =
            (1 + _indexOfLeadingSwathe) % 2
          end storageLotForPrecedingSwathe

          def indexOfLeadingSwathe: Int = _indexOfLeadingSwathe

          def storeSolutionInLeadingSwathe(
              onePastBaseIndex: Int,
              onePastLeftIndex: Int,
              onePastRightIndex: Int,
              longestCommonSubsequence: LongestCommonSubsequence[Element]
          ): Unit =
            require(_indexOfLeadingSwathe != notYetAdvanced)

            twoLotsOfStorage(storageLotForLeadingSwathe)(
              _indexOfLeadingSwathe,
              onePastBaseIndex,
              onePastLeftIndex,
              onePastRightIndex
            ) = longestCommonSubsequence
          end storeSolutionInLeadingSwathe

          inline private def newStorage = Storage(
            baseEqualToSwatheIndex =
              Array.ofDim((1 + left.size) * (1 + right.size)),
            leftEqualToSwatheIndex =
              Array.ofDim((1 + base.size) * (1 + right.size)),
            rightEqualToSwatheIndex =
              Array.ofDim((1 + base.size) * (1 + left.size))
          )
        end swathes

        val haveAdvancedToNextLeadingSwathe = IO {
          swathes.advanceToNextLeadingSwathe()
        }

        val indexOfLeadingSwathe: IO[Int] = IO { swathes.indexOfLeadingSwathe }

        // TODO: either cut over *completely* to using `IO` (but without
        // sacrificing performance obtained by using `Future` to do the heavy
        // lifting), or learn how to write a catamorphism for `Future` that
        // unfolds through the swathes. The problem is that without a
        // catamorphism, the only obvious way of doing this is to used
        // `Monad.whileM_`, and that really doesn't play well with `Future` as
        // it evaluates its condition eagerly and once.
        val allSolutionsOverAllSwathes = Monad[IO].whileM_(
          haveAdvancedToNextLeadingSwathe
        )(indexOfLeadingSwathe.flatMap { indexOfLeadingSwathe =>
          val maximumLesserBaseIndex =
            base.size min (indexOfLeadingSwathe - 1)
          val maximumLesserLeftIndex =
            left.size min (indexOfLeadingSwathe - 1)
          val maximumLesserRightIndex =
            right.size min (indexOfLeadingSwathe - 1)

          enum IndexPermutation:
            inline def evaluateAt(shortIndex: Int, longIndex: Int): Unit =
              this match
                case BaseHeldLeftIsShort =>
                  action(
                    swathes,
                    indexOfLeadingSwathe,
                    shortIndex,
                    longIndex
                  )
                case BaseHeldRightIsShort =>
                  action(
                    swathes,
                    indexOfLeadingSwathe,
                    longIndex,
                    shortIndex
                  )
                case LeftHeldBaseIsShort =>
                  action(
                    swathes,
                    shortIndex,
                    indexOfLeadingSwathe,
                    longIndex
                  )
                case LeftHeldRightIsShort =>
                  action(
                    swathes,
                    longIndex,
                    indexOfLeadingSwathe,
                    shortIndex
                  )
                case RightHeldBaseIsShort =>
                  action(
                    swathes,
                    shortIndex,
                    longIndex,
                    indexOfLeadingSwathe
                  )
                case RightHeldLeftIsShort =>
                  action(
                    swathes,
                    longIndex,
                    shortIndex,
                    indexOfLeadingSwathe
                  )

            case BaseHeldLeftIsShort
            case BaseHeldRightIsShort
            case LeftHeldBaseIsShort
            case LeftHeldRightIsShort
            case RightHeldBaseIsShort
            case RightHeldLeftIsShort
          end IndexPermutation

          def traverseInDiagonalStripes(
              maximumShortIndex: Int,
              maximumLongIndex: Int,
              indexPermutation: IndexPermutation
          ): Unit =
            // Evaluate along initial short diagonals increasing in length...
            for
              ceiling    <- 0 until maximumShortIndex
              shortIndex <- 0 to ceiling
              longIndex = ceiling - shortIndex
            do indexPermutation.evaluateAt(shortIndex, longIndex)
            end for
            // Evaluate along full-length diagonals...
            for
              ceiling    <- maximumShortIndex to maximumLongIndex
              shortIndex <- 0 to maximumShortIndex
              longIndex = ceiling - shortIndex
            do indexPermutation.evaluateAt(shortIndex, longIndex)
            end for
            // Evaluate along final short diagonals decreasing in length...
            for
              ceiling <-
                (1 + maximumLongIndex) to (maximumShortIndex + maximumLongIndex)
              shortIndex <- (ceiling - maximumLongIndex) to maximumShortIndex
              longIndex = ceiling - shortIndex
            do indexPermutation.evaluateAt(shortIndex, longIndex)
            end for
          end traverseInDiagonalStripes

          val solutionsHoldingTheBase = Future {
            if base.size >= indexOfLeadingSwathe then
              // Hold the base index at the maximum for this swathe and evaluate
              // all solutions with lesser left and right indices in dependency
              // order within this swathe...
              if maximumLesserLeftIndex < maximumLesserRightIndex then
                traverseInDiagonalStripes(
                  maximumShortIndex = maximumLesserLeftIndex,
                  maximumLongIndex = maximumLesserRightIndex,
                  indexPermutation = IndexPermutation.BaseHeldLeftIsShort
                )
              else
                traverseInDiagonalStripes(
                  maximumShortIndex = maximumLesserRightIndex,
                  maximumLongIndex = maximumLesserLeftIndex,
                  indexPermutation = IndexPermutation.BaseHeldRightIsShort
                )
              end if
            end if
          }

          val solutionsHoldingTheLeft = Future {
            if left.size >= indexOfLeadingSwathe then
              // Hold the left index at the maximum for this swathe and evaluate
              // all solutions with lesser base and right indices in dependency
              // order within this swathe...
              if maximumLesserBaseIndex < maximumLesserRightIndex then
                traverseInDiagonalStripes(
                  maximumShortIndex = maximumLesserBaseIndex,
                  maximumLongIndex = maximumLesserRightIndex,
                  indexPermutation = IndexPermutation.LeftHeldBaseIsShort
                )
              else
                traverseInDiagonalStripes(
                  maximumShortIndex = maximumLesserRightIndex,
                  maximumLongIndex = maximumLesserBaseIndex,
                  indexPermutation = IndexPermutation.LeftHeldRightIsShort
                )
              end if
            end if
          }

          val solutionsHoldingTheRight = Future {
            if right.size >= indexOfLeadingSwathe then
              // Hold the right index at the maximum for this swathe and
              // evaluate all solutions with lesser base and left indices in
              // dependency order within this swathe...
              if maximumLesserBaseIndex < maximumLesserLeftIndex then
                traverseInDiagonalStripes(
                  maximumShortIndex = maximumLesserBaseIndex,
                  maximumLongIndex = maximumLesserLeftIndex,
                  indexPermutation = IndexPermutation.RightHeldBaseIsShort
                )
              else
                traverseInDiagonalStripes(
                  maximumShortIndex = maximumLesserLeftIndex,
                  maximumLongIndex = maximumLesserBaseIndex,
                  indexPermutation = IndexPermutation.RightHeldLeftIsShort
                )
              end if
            end if
          }

          val solutionsHoldingEachOfTheThreeSides =
            solutionsHoldingTheBase *> solutionsHoldingTheLeft *> solutionsHoldingTheRight

          val solutionsHoldingTheBaseAndLeft =
            solutionsHoldingEachOfTheThreeSides >> Future {
              if base.size >= indexOfLeadingSwathe && left.size >= indexOfLeadingSwathe
              then
                for rightIndex <- 0 to maximumLesserRightIndex do
                  action(
                    swathes,
                    indexOfLeadingSwathe,
                    indexOfLeadingSwathe,
                    rightIndex
                  )
                end for
              end if
            }

          val solutionsHoldingTheBaseAndRight =
            solutionsHoldingEachOfTheThreeSides >> Future {
              if base.size >= indexOfLeadingSwathe && right.size >= indexOfLeadingSwathe
              then
                for leftIndex <- 0 to maximumLesserLeftIndex do
                  action(
                    swathes,
                    indexOfLeadingSwathe,
                    leftIndex,
                    indexOfLeadingSwathe
                  )
                end for
              end if
            }

          val solutionsHoldingTheLeftAndRight =
            solutionsHoldingEachOfTheThreeSides >> Future {
              if left.size >= indexOfLeadingSwathe && right.size >= indexOfLeadingSwathe
              then
                for baseIndex <- 0 to maximumLesserBaseIndex do
                  action(
                    swathes,
                    baseIndex,
                    indexOfLeadingSwathe,
                    indexOfLeadingSwathe
                  )
                end for
              end if
            }

          val allExceptTopLevelSolution =
            solutionsHoldingTheBaseAndLeft *> solutionsHoldingTheBaseAndRight *> solutionsHoldingTheLeftAndRight

          val topLevelSolution =
            allExceptTopLevelSolution >> Future {
              if base.size >= indexOfLeadingSwathe && left.size >= indexOfLeadingSwathe && right.size >= indexOfLeadingSwathe
              then
                // Top-level solution for the leading swathe...
                action(
                  swathes,
                  indexOfLeadingSwathe,
                  indexOfLeadingSwathe,
                  indexOfLeadingSwathe
                )
              end if
            }

          IO.fromFuture(IO(topLevelSolution))
        })

        allSolutionsOverAllSwathes.unsafeRunSync()

        swathes.topLevelSolution
      end evaluateSolutionsInDependencyOrder
    end Swathes

    def ofConsultingSwathesForSubProblems(
        swathes: Swathes,
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

      (baseIsExhausted, leftIsExhausted, rightIsExhausted) match
        case (true, true, true) | (true, true, false) | (true, false, true) |
            (false, true, true) =>
          // There is nothing left to compare from one side to any other...
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

        case (true, false, false) =>
          // Base is exhausted...
          val leftIndex  = onePastLeftIndex - 1
          val rightIndex = onePastRightIndex - 1

          val leftElement  = left(leftIndex)
          val rightElement = right(rightIndex)

          val leftEqualsRight = equality.eqv(leftElement, rightElement)

          if leftEqualsRight then
            swathes
              .consultRelevantSwatheForSolution(0, leftIndex, rightIndex)
              .addCommonLeftAndRight(leftElement, rightElement)(
                sized.sizeOf
              )
          else
            val resultDroppingTheEndOfTheLeft =
              swathes
                .consultRelevantSwatheForSolution(
                  0,
                  leftIndex,
                  onePastRightIndex
                )
                .addLeftDifference(leftElement)

            val resultDroppingTheEndOfTheRight =
              swathes
                .consultRelevantSwatheForSolution(
                  0,
                  onePastLeftIndex,
                  rightIndex
                )
                .addRightDifference(rightElement)

            orderBySize.max(
              resultDroppingTheEndOfTheLeft,
              resultDroppingTheEndOfTheRight
            )
          end if

        case (false, true, false) =>
          // Left is exhausted...
          val baseIndex  = onePastBaseIndex - 1
          val rightIndex = onePastRightIndex - 1

          val baseElement  = base(baseIndex)
          val rightElement = right(rightIndex)

          val baseEqualsRight = equality.eqv(baseElement, rightElement)

          if baseEqualsRight then
            swathes
              .consultRelevantSwatheForSolution(baseIndex, 0, rightIndex)
              .addCommonBaseAndRight(baseElement, rightElement)(
                sized.sizeOf
              )
          else
            val resultDroppingTheEndOfTheBase =
              swathes
                .consultRelevantSwatheForSolution(
                  baseIndex,
                  0,
                  onePastRightIndex
                )
                .addBaseDifference(baseElement)

            val resultDroppingTheEndOfTheRight =
              swathes
                .consultRelevantSwatheForSolution(
                  onePastBaseIndex,
                  0,
                  rightIndex
                )
                .addRightDifference(rightElement)

            orderBySize.max(
              resultDroppingTheEndOfTheBase,
              resultDroppingTheEndOfTheRight
            )
          end if

        case (false, false, true) =>
          // Right is exhausted...
          val baseIndex = onePastBaseIndex - 1
          val leftIndex = onePastLeftIndex - 1

          val baseElement = base(baseIndex)
          val leftElement = left(leftIndex)

          val baseEqualsLeft = equality.eqv(baseElement, leftElement)

          if baseEqualsLeft then
            swathes
              .consultRelevantSwatheForSolution(baseIndex, leftIndex, 0)
              .addCommonBaseAndLeft(baseElement, leftElement)(
                sized.sizeOf
              )
          else
            val resultDroppingTheEndOfTheBase =
              swathes
                .consultRelevantSwatheForSolution(
                  baseIndex,
                  onePastLeftIndex,
                  0
                )
                .addBaseDifference(baseElement)

            val resultDroppingTheEndOfTheLeft =
              swathes
                .consultRelevantSwatheForSolution(
                  onePastBaseIndex,
                  leftIndex,
                  0
                )
                .addLeftDifference(leftElement)

            orderBySize.max(
              resultDroppingTheEndOfTheBase,
              resultDroppingTheEndOfTheLeft
            )
          end if

        case (false, false, false) =>
          // Nothing is exhausted...
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
            swathes
              .consultRelevantSwatheForSolution(
                baseIndex,
                leftIndex,
                rightIndex
              )
              .addCommon(baseElement, leftElement, rightElement)(
                sized.sizeOf
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
              swathes
                .consultRelevantSwatheForSolution(
                  baseIndex,
                  onePastLeftIndex,
                  onePastRightIndex
                )
                .addBaseDifference(baseElement)

            val resultDroppingTheEndOfTheLeft =
              swathes
                .consultRelevantSwatheForSolution(
                  onePastBaseIndex,
                  leftIndex,
                  onePastRightIndex
                )
                .addLeftDifference(leftElement)

            val resultDroppingTheEndOfTheRight =
              swathes
                .consultRelevantSwatheForSolution(
                  onePastBaseIndex,
                  onePastLeftIndex,
                  rightIndex
                )
                .addRightDifference(rightElement)

            val resultDroppingTheBaseAndLeft =
              if baseEqualsLeft then
                swathes
                  .consultRelevantSwatheForSolution(
                    baseIndex,
                    leftIndex,
                    onePastRightIndex
                  )
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
                swathes
                  .consultRelevantSwatheForSolution(
                    baseIndex,
                    onePastLeftIndex,
                    rightIndex
                  )
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

            val resultDroppingTheLeftAndRight =
              if leftEqualsRight then
                swathes
                  .consultRelevantSwatheForSolution(
                    onePastBaseIndex,
                    leftIndex,
                    rightIndex
                  )
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
      end match
    end ofConsultingSwathesForSubProblems

    // Brute-forced and ignorant dynamic programming. Completely unsafe reliance
    // on imperative updates priming each sub-problem solution in the leading
    // swathe prior to its use. Got to love it!
    Swathes.evaluateSolutionsInDependencyOrder {
      case (
            swathes,
            onePastBaseIndex,
            onePastLeftIndex,
            onePastRightIndex
          ) =>
        swathes.storeSolutionInLeadingSwathe(
          onePastBaseIndex,
          onePastLeftIndex,
          onePastRightIndex,
          ofConsultingSwathesForSubProblems(
            swathes,
            onePastBaseIndex,
            onePastLeftIndex,
            onePastRightIndex
          )
        )
    }
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
