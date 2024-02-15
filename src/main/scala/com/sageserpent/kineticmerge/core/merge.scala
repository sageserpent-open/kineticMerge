package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Contribution
import com.typesafe.scalalogging.StrictLogging

import scala.annotation.tailrec

object merge extends StrictLogging:
  /** Performs a three-way merge.
    * @param base
    *   {@code left} and {@code right} are considered modified versions of this
    *   sequence.
    * @param left
    *   A modified version of {@code base}. Where there is a preservation or a
    *   coincident insertion, then this provides the representative element for
    *   the merge.
    * @param right
    *   The other modified version of {@code base}.
    * @param equality
    *   This determines whether elements are considered equal. Because it is not
    *   necessarily `==` or `eq`, this requires a preference for which of the
    *   equivalent elements to pick in a preservation of coincident insertion.
    *   That preference is to take the element from {@code left}.
    * @tparam Element
    *   What the sequences contain.
    * @return
    *   A [[Result]] representing either a full merge without conflicts or a
    *   conflicted merge, wrapped in an [[Either]]. Currently the [[Either]] is
    *   always a [[Right]], as there is no support for code motion and thus no
    *   possibility of a divergence error.
    * @note
    *   The three-way merge has an implicit precondition that the {@code base},
    *   {@code left} and {@code right} are yielded from a
    *   [[LongestCommonSubsequence]], or at least conform to the implied
    *   postcondition of calling [[LongestCommonSubsequence.of]]. Failure to
    *   meet this may result in some unspecified exception being thrown.
    *
    * @note
    *   Rules of the game: If an element is [[Contribution.Common]] to all three
    *   sides, it is preserved in its left form in the merge; the merge uses the
    *   least common subsequence as a backbone to arrange other changes around.
    *   <p><p> If an element is [[Contribution.CommonToLeftAndRightOnly]], it is
    *   coincidentally inserted on both the left and right side, so goes in its
    *   left form into the merge, extending the backbone. <p><p> If an element
    *   is either [[Contribution.CommonToBaseAndLeftOnly]] or
    *   [[Contribution.CommonToBaseAndRightOnly]], this implies a deletion or an
    *   edit on the other side, extending the backbone. <p><p> If an element is
    *   a [[Contribution.Difference]] in {@code base}, then depending on the
    *   {@code left} and {@code right} context, it may mean a coincident
    *   deletion of that element, or conflicting edits of that element, or a
    *   conflict between an edit and an outright deletion of that element.
    *   <p><p> If an element is a [[Contribution.Difference]] in {@code left} or
    *   in {@code right}, then depending on context this may mean an insertion
    *   of the element on that side, or an edit, or may be one of two
    *   conflicting elements.<p><p> Edits are always looked for if they can
    *   avoid a conflict; so for example, if a [[Contribution.Difference]] in
    *   {@code left} is available to pair off with a
    *   [[Contribution.CommonToBaseAndRightOnly]] in both a {@code base} and
    *   {@code right}, it will be taken to be a left-edit. This will not
    *   conflict with a following [[Contribution.Difference]] in {@code right}
    *   as that will be taken to be a standalone right-insertion. <p><p>Edits
    *   are greedy in that they will eagerly take successive
    *   [[Contribution.Difference]] elements to make a long edit.<p><p>However,
    *   if there is for example a [[Contribution.Difference]] sandwiched between
    *   two [[Contribution.CommonToBaseAndRightOnly]] elements in {@code right},
    *   this breaks up the left-edits to preserve the sandwich in edited form in
    *   the merge.
    */
  def of[Element](
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  )(
      equality: Eq[Element]
  ): Either[Divergence.type, Result[Element]] =
    @tailrec
    def mergeBetweenRunsOfCommonElements(
        base: Seq[Contribution[Element]],
        left: Seq[Contribution[Element]],
        right: Seq[Contribution[Element]]
    )(partialResult: Result[Element]): Result[Element] =
      (base, left, right) match
        case (
              Seq(Contribution.Common(_), baseTail*),
              Seq(Contribution.Common(leftElement), leftTail*),
              Seq(Contribution.Common(_), rightTail*)
            ) => // Preservation.
          logger.debug(
            s"Preservation of $leftElement as it is common to all three sides."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            partialResult.addCommonOrEdit(leftElement)
          )

        case (
              Seq(Contribution.Difference(baseElement), baseTail*),
              Seq(
                Contribution.CommonToLeftAndRightOnly(leftElement),
                leftTail*
              ),
              Seq(
                Contribution.CommonToLeftAndRightOnly(_),
                rightTail*
              )
            ) => // Coincident edit.
          baseTail match
            case Seq(Contribution.Difference(followingBaseElement), _*) =>
              // If the following element in the base would also be
              // coincidentally deleted, coalesce into a single coincident
              // edit.
              logger.debug(
                s"Coalescing coincident edit of $baseElement into $leftElement with following coincident deletion of $followingBaseElement."
              )
              mergeBetweenRunsOfCommonElements(baseTail, left, right)(
                partialResult
              )

            case _ =>
              logger.debug(
                s"Coincident edit of $baseElement into $leftElement."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                partialResult.addCommonOrEdit(leftElement)
              )
          end match

        case (
              _,
              Seq(
                Contribution.CommonToLeftAndRightOnly(leftElement),
                leftTail*
              ),
              Seq(
                Contribution.CommonToLeftAndRightOnly(_),
                rightTail*
              )
            ) => // Coincident insertion.
          logger.debug(
            s"Coincident insertion on left and right side of $leftElement."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
            partialResult.addCommonOrEdit(leftElement)
          )

        case (
              Seq(
                editedBaseElement @ Contribution.CommonToBaseAndRightOnly(_),
                baseTail*
              ),
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(
                Contribution.CommonToBaseAndRightOnly(_),
                rightTail*
              )
            ) => // Left edit.
          baseTail -> leftTail match
            case (
                  Seq(
                    Contribution.Difference(followingBaseElement),
                    baseTailAfterCoincidentDeletion*
                  ),
                  Seq(Contribution.Difference(_), _*)
                ) =>
              // There is a pending coincident deletion; process it first in
              // case it hides a subsequent left edit.
              logger.debug(s"Coincident deletion of $followingBaseElement.")
              mergeBetweenRunsOfCommonElements(
                editedBaseElement +: baseTailAfterCoincidentDeletion,
                left,
                right
              )(partialResult)

            case (
                  Seq(Contribution.CommonToBaseAndRightOnly(_), _*),
                  Seq(Contribution.Difference(_), _*)
                )
                // Guard against a coincident insertion prior to the right side
                // of a pending left edit or deletion; that would maroon the
                // latter, so we *would* coalesce the following element on the
                // left.
                if rightTail
                  .takeWhile {
                    case Contribution.CommonToBaseAndRightOnly(_) => false
                    case _                                        => true
                  }
                  .forall {
                    case Contribution.CommonToLeftAndRightOnly(_) => false
                    case _                                        => true
                  } =>
              // There is another pending left edit, but *don't* coalesce the
              // following element on the left; that then respects any
              // insertions that may be lurking on the right prior to the
              // pending left edit claiming the following element on the left.
              logger.debug(
                s"Left edit of ${editedBaseElement.element} into $leftElement, not coalescing with following left edit due to prior coincident insertion."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                partialResult.addCommonOrEdit(leftElement)
              )

            case (_, Seq(Contribution.Difference(followingLeftElement), _*)) =>
              // If the following element on the left would also be inserted,
              // coalesce into a single edit.
              logger.debug(
                s"Coalescing left edit of ${editedBaseElement.element} into $leftElement with following insertion of $followingLeftElement on the left."
              )
              mergeBetweenRunsOfCommonElements(base, leftTail, right)(
                partialResult.addCommonOrEdit(leftElement)
              )

            case _ =>
              logger.debug(
                s"Left edit of ${editedBaseElement.element} into $leftElement."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                partialResult.addCommonOrEdit(leftElement)
              )
          end match

        case (
              Seq(
                Contribution.CommonToBaseAndRightOnly(deletedBaseElement),
                baseTail*
              ),
              _,
              Seq(
                Contribution.CommonToBaseAndRightOnly(_),
                rightTail*
              )
            ) => // Left deletion.
          logger.debug(s"Left deletion of $deletedBaseElement.")
          mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
            partialResult
          )

        case (
              Seq(
                editedBaseElement @ Contribution.CommonToBaseAndLeftOnly(_),
                baseTail*
              ),
              Seq(
                Contribution.CommonToBaseAndLeftOnly(_),
                leftTail*
              ),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Right edit.
          baseTail -> rightTail match
            case (
                  Seq(
                    Contribution.Difference(followingBaseElement),
                    baseTailAfterCoincidentDeletion*
                  ),
                  Seq(Contribution.Difference(_), _*)
                ) =>
              // There is a pending coincident deletion; process it first in
              // case it hides a subsequent right edit.
              logger.debug(s"Coincident deletion of $followingBaseElement.")
              mergeBetweenRunsOfCommonElements(
                editedBaseElement +: baseTailAfterCoincidentDeletion,
                left,
                right
              )(partialResult)

            case (
                  Seq(Contribution.CommonToBaseAndLeftOnly(_), _*),
                  Seq(Contribution.Difference(_), _*)
                )
                // Guard against a coincident insertion prior to the left side
                // of a pending right edit or deletion; that would maroon the
                // latter, so we *would* coalesce the following element on the
                // right.
                if leftTail
                  .takeWhile {
                    case Contribution.CommonToBaseAndLeftOnly(_) => false
                    case _                                       => true
                  }
                  .forall {
                    case Contribution.CommonToLeftAndRightOnly(_) => false
                    case _                                        => true
                  } =>
              // There is another pending right edit, but *don't* coalesce the
              // following element on the right; that then respects any
              // insertions that may be lurking on the left prior to the pending
              // right edit claiming the following element on the right.
              logger.debug(
                s"Right edit of ${editedBaseElement.element} into $rightElement, not coalescing with following right edit due to prior coincident insertion."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                partialResult.addCommonOrEdit(rightElement)
              )

            case (_, Seq(Contribution.Difference(followingRightElement), _*)) =>
              // If the following element on the right would also be inserted,
              // coalesce into a single edit.
              logger.debug(
                s"Coalescing right edit of ${editedBaseElement.element} into $rightElement with following insertion of $followingRightElement on the right."
              )
              mergeBetweenRunsOfCommonElements(base, left, rightTail)(
                partialResult.addCommonOrEdit(rightElement)
              )

            case _ =>
              logger.debug(
                s"Right edit of ${editedBaseElement.element} into $rightElement."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                partialResult.addCommonOrEdit(rightElement)
              )
          end match

        case (
              Seq(
                Contribution.CommonToBaseAndLeftOnly(deletedBaseElement),
                baseTail*
              ),
              Seq(
                Contribution.CommonToBaseAndLeftOnly(_),
                leftTail*
              ),
              _
            ) => // Right deletion.
          logger.debug(s"Right deletion of $deletedBaseElement.")
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
            partialResult
          )

        case (
              Seq(Contribution.Difference(editedBaseElement), baseTail*),
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Conflict, multiple possibilities.
          baseTail match
            case Seq(Contribution.Difference(followingBaseElement), _*) =>
              // If the following element in the base would also be edited,
              // coalesce into a single coincident edit conflict.
              logger.debug(
                s"Coalescing edit conflict of $editedBaseElement into $leftElement on the left and $rightElement on the right with following coincident deletion of $followingBaseElement."
              )
              mergeBetweenRunsOfCommonElements(baseTail, left, right)(
                partialResult
              )

            case Seq(
                  Contribution.CommonToBaseAndLeftOnly(_),
                  _*
                ) => // Left edit / right deletion conflict with pending right edit.
              logger.debug(
                s"Conflict between right deletion of $editedBaseElement and its edit on the left into $leftElement with following right edit."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
                partialResult.addLeftSideOfConflict(
                  leftElement
                )
              )

            case Seq(
                  Contribution.CommonToBaseAndRightOnly(_),
                  _*
                ) => // Right edit / left deletion conflict with pending left edit.
              logger.debug(
                s"Conflict between left deletion of $editedBaseElement and its edit on the right into $rightElement with following left edit."
              )
              mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
                partialResult.addRightSideOfConflict(
                  rightElement
                )
              )

            case _ =>
              // Edit conflict.
              logger.debug(
                s"Edit conflict of $editedBaseElement into $leftElement on the left and $rightElement on the right."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                partialResult.addBothSidesOfConflict(leftElement, rightElement)
              )
          end match

        case (
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*)
            ) => // Left insertion with pending coincident edit.
          logger.debug(
            s"Left insertion of $leftElement with following coincident edit."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            partialResult.addFromLeft(leftElement)
          )

        case (
              Seq(Contribution.Difference(deletedBaseElement), baseTail*),
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.CommonToBaseAndRightOnly(_), _*)
            ) => // Coincident deletion with pending left edit.
          logger.debug(
            s"Coincident deletion of $deletedBaseElement with following left edit."
          )
          mergeBetweenRunsOfCommonElements(baseTail, left, right)(partialResult)

        case (
              Seq(Contribution.Difference(editedBaseElement), baseTail*),
              Seq(Contribution.Difference(leftElement), leftTail*),
              _
            ) => // Left edit / right deletion conflict.
          logger.debug(
            s"Conflict between right deletion of $editedBaseElement and its edit on the left into $leftElement."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
            partialResult.addLeftSideOfConflict(leftElement)
          )

        case (
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Right insertion with pending coincident edit.
          logger.debug(
            s"Right insertion of $rightElement with following coincident edit."
          )
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            partialResult.addFromRight(rightElement)
          )

        case (
              Seq(Contribution.Difference(deletedBaseElement), baseTail*),
              Seq(Contribution.CommonToBaseAndLeftOnly(_), _*),
              Seq(Contribution.Difference(_), _*)
            ) => // Coincident deletion with pending right edit.
          logger.debug(
            s"Coincident deletion of $deletedBaseElement with following right edit."
          )
          mergeBetweenRunsOfCommonElements(baseTail, left, right)(partialResult)

        case (
              Seq(Contribution.Difference(editedBaseElement), baseTail*),
              _,
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Right edit / left deletion conflict.
          logger.debug(
            s"Conflict between left deletion of $editedBaseElement and its edit on the right into $rightElement."
          )
          mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
            partialResult.addRightSideOfConflict(rightElement)
          )

        case (
              Seq(Contribution.Difference(deletedBaseElement), baseTail*),
              _,
              _
            ) => // Coincident deletion.
          logger.debug(s"Coincident deletion of $deletedBaseElement.")
          mergeBetweenRunsOfCommonElements(baseTail, left, right)(partialResult)

        case (
              Seq(Contribution.CommonToBaseAndLeftOnly(_), _*),
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.Difference(_), _*)
            ) => // Left insertion with pending right edit.
          logger.debug(
            s"Left insertion of $leftElement with following right edit."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            partialResult.addFromLeft(leftElement)
          )

        case (
              Seq(Contribution.CommonToBaseAndRightOnly(_), _*),
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Right insertion with pending left edit.
          logger.debug(
            s"Right insertion of $rightElement with following left edit."
          )
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            partialResult.addFromRight(rightElement)
          )

        case (
              _,
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Insertion conflict.
          logger.debug(
            s"Conflict between left insertion of $leftElement and right insertion of $rightElement."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
            partialResult.addBothSidesOfConflict(leftElement, rightElement)
          )

        case (
              Seq(
                Contribution.Common(_) |
                Contribution.CommonToBaseAndLeftOnly(_) |
                Contribution.CommonToLeftAndRightOnly(_) |
                Contribution.CommonToBaseAndRightOnly(_),
                _*
              ) | Seq(),
              Seq(Contribution.Difference(leftElement), leftTail*),
              _
            ) => // Left insertion.
          logger.debug(s"Left insertion of $leftElement.")
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            partialResult.addFromLeft(leftElement)
          )

        case (
              Seq(
                Contribution.Common(_) |
                Contribution.CommonToBaseAndRightOnly(_) |
                Contribution.CommonToLeftAndRightOnly(_) |
                Contribution.CommonToBaseAndLeftOnly(_),
                _*
              ) | Seq(),
              _,
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Right insertion.
          logger.debug(s"Right insertion of $rightElement.")
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            partialResult.addFromRight(rightElement)
          )

        case (Seq(), Seq(), Seq()) => // Terminating case!
          logger.debug(s"Merge yielded:\n${pprint(partialResult)}")
          partialResult
      end match
    end mergeBetweenRunsOfCommonElements

    val longestCommonSubsequence =
      LongestCommonSubsequence.of(base, left, right)(equality)

    val emptyResult: Result[Element] = FullyMerged(IndexedSeq.empty)

    // TODO: for now, failure is not tolerated, but obviously that will have to
    // be accommodated - both merge conflicts and later divergences.
    Right(
      mergeBetweenRunsOfCommonElements(
        longestCommonSubsequence.base,
        longestCommonSubsequence.left,
        longestCommonSubsequence.right
      )(emptyResult)
    )
  end of

  sealed trait Result[Element]:
    def addCommonOrEdit(
        commonElement: Element
    ): Result[Element] =
      this match
        case FullyMerged(elements) =>
          FullyMerged(elements :+ commonElement)
        case MergedWithConflicts(leftElements, rightElements) =>
          MergedWithConflicts(
            leftElements :+ commonElement,
            rightElements :+ commonElement
          )

    def addFromLeft(
        leftElement: Element
    ): Result[Element] =
      this match
        case FullyMerged(elements) =>
          FullyMerged(elements :+ leftElement)
        case MergedWithConflicts(leftElements, rightElements) =>
          MergedWithConflicts(
            leftElements :+ leftElement,
            rightElements
          )

    def addFromRight(
        rightElement: Element
    ): Result[Element] =
      this match
        case FullyMerged(elements) =>
          FullyMerged(elements :+ rightElement)
        case MergedWithConflicts(leftElements, rightElements) =>
          MergedWithConflicts(
            leftElements,
            rightElements :+ rightElement
          )

    def addLeftSideOfConflict(
        leftElement: Element
    ): Result[Element] =
      this match
        case FullyMerged(elements) =>
          MergedWithConflicts(
            leftElements = elements :+ leftElement,
            rightElements = elements
          )
        case MergedWithConflicts(leftElements, rightElements) =>
          MergedWithConflicts(leftElements :+ leftElement, rightElements)

    def addRightSideOfConflict(
        rightElement: Element
    ): Result[Element] =
      this match
        case FullyMerged(elements) =>
          MergedWithConflicts(
            leftElements = elements,
            rightElements = elements :+ rightElement
          )
        case MergedWithConflicts(leftElements, rightElements) =>
          MergedWithConflicts(leftElements, rightElements :+ rightElement)

    def addBothSidesOfConflict(
        leftElement: Element,
        rightElement: Element
    ): Result[Element] =
      this match
        case FullyMerged(elements) =>
          MergedWithConflicts(
            elements :+ leftElement,
            elements :+ rightElement
          )
        case MergedWithConflicts(leftElements, rightElements) =>
          MergedWithConflicts(
            leftElements :+ leftElement,
            rightElements :+ rightElement
          )
  end Result

  case class FullyMerged[Element](elements: IndexedSeq[Element])
      extends Result[Element]

  /** @param leftElements
    *   The left hand form of the merge. Has all the clean merges, plus the left
    *   side of the conflicts.
    * @param rightElements
    *   The right hand form of the merge. Has all the clean merges, plus the
    *   right side of the conflicts.
    * @tparam Element
    */
  case class MergedWithConflicts[Element](
      leftElements: IndexedSeq[Element],
      rightElements: IndexedSeq[Element]
  ) extends Result[Element]

  // TODO: "Something went wrong!" - "What was it?"
  case object Divergence

end merge
