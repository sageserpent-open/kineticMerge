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
    *   the merge. <p><p>Similarly, successive edits on the same side will be
    *   treated as isolated edits rather than allowing the first to greedily
    *   capture all the [[Contribution.Difference]] elements.
    */
  def of[Result[_], Element](mergeAlgebra: MergeAlgebra[Result, Element])(
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  )(
      equality: Eq[Element]
  ): Either[Divergence.type, Result[Element]] =
    def rightEditNotMarooned(leftTail: Seq[Contribution[Element]]) =
      // Guard against a coincident insertion prior to the left side
      // of a pending right edit or deletion; that would maroon the
      // latter, so we *would* coalesce the following element on the
      // right.
      leftTail
        .takeWhile {
          case Contribution.CommonToBaseAndLeftOnly(_) => false
          case _                                       => true
        }
        .forall {
          case Contribution.CommonToLeftAndRightOnly(_) => false
          case _                                        => true
        }

    def leftEditNotMarooned(rightTail: Seq[Contribution[Element]]) =
      // Guard against a coincident insertion prior to the right side
      // of a pending left edit or deletion; that would maroon the
      // latter, so we *would* coalesce the following element on the
      // left.
      rightTail
        .takeWhile {
          case Contribution.CommonToBaseAndRightOnly(_) => false
          case _                                        => true
        }
        .forall {
          case Contribution.CommonToLeftAndRightOnly(_) => false
          case _                                        => true
        }

    @tailrec
    def mergeBetweenRunsOfCommonElements(
        base: Seq[Contribution[Element]],
        left: Seq[Contribution[Element]],
        right: Seq[Contribution[Element]]
    )(
        partialResult: Result[Element],
        deferredEdited: IndexedSeq[Element],
        deferredLeftEdits: IndexedSeq[Element],
        deferredRightEdits: IndexedSeq[Element]
    ): Result[Element] =
      (base, left, right) match
        case (
              Seq(Contribution.Common(_), baseTail*),
              Seq(Contribution.Common(leftElement), leftTail*),
              Seq(Contribution.Common(_), rightTail*)
            ) => // Preservation.
          require(
            deferredEdited.isEmpty && deferredLeftEdits.isEmpty && deferredRightEdits.isEmpty
          )
          logger.debug(
            s"Preservation of $leftElement as it is common to all three sides."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            mergeAlgebra.preservation(
              partialResult,
              preservedElement = leftElement
            ),
            deferredEdited,
            deferredLeftEdits,
            deferredRightEdits
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
          require(
            deferredEdited.isEmpty && deferredLeftEdits.isEmpty && deferredRightEdits.isEmpty
          )
          baseTail match
            case Seq(Contribution.Difference(_), _*) =>
              // If the following element in the base would also be
              // coincidentally deleted, defer the coincident edit and treat
              // this as a coincident deletion.
              logger.debug(
                s"Coincident deletion of $baseElement with following coincident edit."
              )
              mergeBetweenRunsOfCommonElements(baseTail, left, right)(
                mergeAlgebra.coincidentDeletion(
                  partialResult,
                  deletedElement = baseElement
                ),
                deferredEdited,
                deferredLeftEdits,
                deferredRightEdits
              )

            case _ =>
              logger.debug(
                s"Coincident edit of $baseElement into $leftElement."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                mergeAlgebra.coincidentEdit(
                  partialResult,
                  editedElement = baseElement,
                  // TODO: accumulate the edited elements...
                  editElements = IndexedSeq(leftElement)
                ),
                deferredEdited,
                deferredLeftEdits,
                deferredRightEdits
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
          require(
            deferredEdited.isEmpty && deferredLeftEdits.isEmpty && deferredRightEdits.isEmpty
          )
          logger.debug(
            s"Coincident insertion on left and right side of $leftElement."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
            mergeAlgebra.coincidentInsertion(
              partialResult,
              insertedElement = leftElement
            ),
            deferredEdited,
            deferredLeftEdits,
            deferredRightEdits
          )

        case (
              Seq(
                Contribution.CommonToBaseAndRightOnly(editedBaseElement),
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
                    _*
                  ),
                  _
                ) if leftEditNotMarooned(rightTail) =>
              // There is a pending coincident deletion; handle this left edit
              // in isolation without coalescing any following left insertion.
              logger.debug(
                s"Left edit of $editedBaseElement into $leftElement with following coincident deletion of $followingBaseElement."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                mergeAlgebra.leftEdit(
                  partialResult,
                  editedElement = editedBaseElement,
                  editElements = deferredLeftEdits :+ leftElement
                ),
                deferredEdited = deferredEdited,
                deferredLeftEdits = Vector.empty,
                deferredRightEdits = deferredRightEdits
              )

            case (
                  Seq(Contribution.CommonToBaseAndRightOnly(_), _*),
                  Seq(Contribution.Difference(_), _*)
                ) if leftEditNotMarooned(rightTail) =>
              // There is another pending left edit, but *don't* coalesce the
              // following element on the left; that then respects any
              // insertions that may be lurking on the right prior to the
              // pending left edit claiming the following element on the left.
              logger.debug(
                s"Left edit of $editedBaseElement into $leftElement, not coalescing with following left edit."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                mergeAlgebra.leftEdit(
                  partialResult,
                  editedElement = editedBaseElement,
                  editElements = deferredLeftEdits :+ leftElement
                ),
                deferredEdited = deferredEdited,
                deferredLeftEdits = Vector.empty,
                deferredRightEdits = deferredRightEdits
              )

            case (_, Seq(Contribution.Difference(followingLeftElement), _*)) =>
              // If the following element on the left would also be inserted,
              // coalesce into a single edit.
              logger.debug(
                s"Coalescing left edit of $editedBaseElement into $leftElement with following insertion of $followingLeftElement on the left."
              )
              mergeBetweenRunsOfCommonElements(base, leftTail, right)(
                partialResult,
                deferredEdited = deferredEdited,
                deferredLeftEdits = deferredLeftEdits :+ leftElement,
                deferredRightEdits = deferredRightEdits
              )

            case _ =>
              logger.debug(
                s"Left edit of $editedBaseElement into $leftElement."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                mergeAlgebra.leftEdit(
                  partialResult,
                  editedElement = editedBaseElement,
                  editElements = deferredLeftEdits :+ leftElement
                ),
                deferredEdited = deferredEdited,
                deferredLeftEdits = Vector.empty,
                deferredRightEdits = deferredRightEdits
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
          require(
            deferredEdited.isEmpty && deferredLeftEdits.isEmpty && deferredRightEdits.isEmpty
          )
          logger.debug(s"Left deletion of $deletedBaseElement.")
          mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
            mergeAlgebra.leftDeletion(
              partialResult,
              deletedElement = deletedBaseElement
            ),
            deferredEdited,
            deferredLeftEdits,
            deferredRightEdits
          )

        case (
              Seq(
                Contribution.CommonToBaseAndLeftOnly(editedBaseElement),
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
                    _*
                  ),
                  _
                ) if rightEditNotMarooned(leftTail) =>
              // There is a pending coincident deletion; handle this right edit
              // in isolation without coalescing any following right insertion.
              logger.debug(
                s"Right edit of $editedBaseElement into $rightElement with following coincident deletion of $followingBaseElement."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                mergeAlgebra.rightEdit(
                  partialResult,
                  editedElement = editedBaseElement,
                  editElements = deferredRightEdits :+ rightElement
                ),
                deferredEdited = deferredEdited,
                deferredLeftEdits = deferredLeftEdits,
                deferredRightEdits = Vector.empty
              )

            case (
                  Seq(Contribution.CommonToBaseAndLeftOnly(_), _*),
                  Seq(Contribution.Difference(_), _*)
                ) if rightEditNotMarooned(leftTail) =>
              // There is another pending right edit, but *don't* coalesce the
              // following element on the right; that then respects any
              // insertions that may be lurking on the left prior to the pending
              // right edit claiming the following element on the right.
              logger.debug(
                s"Right edit of $editedBaseElement into $rightElement, not coalescing with following right edit."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                mergeAlgebra.rightEdit(
                  partialResult,
                  editedElement = editedBaseElement,
                  editElements = deferredRightEdits :+ rightElement
                ),
                deferredEdited = deferredEdited,
                deferredLeftEdits = deferredLeftEdits,
                deferredRightEdits = Vector.empty
              )

            case (_, Seq(Contribution.Difference(followingRightElement), _*)) =>
              // If the following element on the right would also be inserted,
              // coalesce into a single edit.
              logger.debug(
                s"Coalescing right edit of $editedBaseElement into $rightElement with following insertion of $followingRightElement on the right."
              )
              mergeBetweenRunsOfCommonElements(base, left, rightTail)(
                partialResult,
                deferredEdited = deferredEdited,
                deferredLeftEdits = deferredLeftEdits,
                deferredRightEdits = deferredRightEdits :+ rightElement
              )

            case _ =>
              logger.debug(
                s"Right edit of $editedBaseElement into $rightElement."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                mergeAlgebra.rightEdit(
                  partialResult,
                  editedElement = editedBaseElement,
                  editElements = deferredRightEdits :+ rightElement
                ),
                deferredEdited = deferredEdited,
                deferredLeftEdits = deferredLeftEdits,
                deferredRightEdits = Vector.empty
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
          require(
            deferredEdited.isEmpty && deferredLeftEdits.isEmpty && deferredRightEdits.isEmpty
          )
          logger.debug(s"Right deletion of $deletedBaseElement.")
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
            mergeAlgebra.rightDeletion(
              partialResult,
              deletedElement = deletedBaseElement
            ),
            deferredEdited,
            deferredLeftEdits,
            deferredRightEdits
          )

        case (
              Seq(Contribution.Difference(editedBaseElement), baseTail*),
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Conflict, multiple possibilities.
          (baseTail, leftTail, rightTail) match
            case (
                  Seq(Contribution.Difference(_), _*),
                  Seq(Contribution.Difference(_), _*),
                  Seq(Contribution.Difference(_), _*)
                ) =>
              // Edit conflict with another pending edit conflict.
              logger.debug(
                s"Coalescing edit conflict of $editedBaseElement into $leftElement on the left and $rightElement on the right with following edit conflict."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                partialResult,
                deferredEdited = deferredEdited :+ editedBaseElement,
                deferredLeftEdits = deferredLeftEdits :+ leftElement,
                deferredRightEdits = deferredRightEdits :+ rightElement
              )

            case (
                  Seq(Contribution.Difference(_), _*),
                  Seq(Contribution.Difference(_), _*),
                  _
                ) =>
              // Edit conflict with a pending left edit versus right deletion
              // conflict.
              logger.debug(
                s"Coalescing edit conflict of $editedBaseElement into $leftElement on the left and $rightElement on the right with following left edit versus right deletion conflict."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
                partialResult,
                deferredEdited = deferredEdited :+ editedBaseElement,
                deferredLeftEdits = deferredLeftEdits :+ leftElement,
                deferredRightEdits = deferredRightEdits
              )

            case (
                  Seq(Contribution.Difference(_), _*),
                  _,
                  Seq(Contribution.Difference(_), _*)
                ) =>
              // Edit conflict with a pending right edit versus left deletion
              // conflict.
              logger.debug(
                s"Coalescing edit conflict of $editedBaseElement into $leftElement on the left and $rightElement on the right with following right edit versus left deletion conflict."
              )
              mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
                partialResult,
                deferredEdited = deferredEdited :+ editedBaseElement,
                deferredLeftEdits = deferredLeftEdits,
                deferredRightEdits = deferredRightEdits :+ rightElement
              )

            case (
                  _,
                  Seq(Contribution.Difference(_), _*),
                  Seq(Contribution.Difference(_), _*)
                ) =>
              // Edit conflict with a pending left insertion versus right
              // insertion conflict.
              logger.debug(
                s"Coalescing edit conflict of $editedBaseElement into $leftElement on the left and $rightElement on the right with following left insertion versus right insertion conflict."
              )
              mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
                partialResult,
                deferredEdited = deferredEdited,
                deferredLeftEdits = deferredLeftEdits :+ leftElement,
                deferredRightEdits = deferredRightEdits :+ rightElement
              )

            case (
                  Seq(Contribution.Difference(followingBaseElement), _*),
                  _,
                  _
                ) =>
              // If the following element in the base would also be deleted on
              // both sides, coalesce into a single coincident edit conflict.
              logger.debug(
                s"Coalescing edit conflict of $editedBaseElement into $leftElement on the left and $rightElement on the right with following coincident deletion of $followingBaseElement."
              )
              mergeBetweenRunsOfCommonElements(baseTail, left, right)(
                partialResult,
                deferredEdited = deferredEdited :+ editedBaseElement,
                deferredLeftEdits,
                deferredRightEdits
              )

            case (
                  Seq(
                    Contribution.CommonToBaseAndLeftOnly(_),
                    _*
                  ),
                  Seq(
                    Contribution.Difference(followingLeftElement),
                    _*
                  ),
                  _
                ) if rightEditNotMarooned(leftTail) =>
              // Left edit / right deletion conflict with pending left insertion
              // and pending right edit.
              logger.debug(
                s"Coalescing conflict between right deletion of $editedBaseElement and its edit on the left into $leftElement, with following insertion of $followingLeftElement on the left."
              )
              mergeBetweenRunsOfCommonElements(base, leftTail, right)(
                partialResult,
                deferredEdited = deferredEdited,
                deferredLeftEdits = deferredLeftEdits :+ leftElement,
                deferredRightEdits = deferredRightEdits
              )

            case (
                  Seq(
                    Contribution.CommonToBaseAndLeftOnly(_),
                    _*
                  ),
                  _,
                  _
                ) if rightEditNotMarooned(leftTail) =>
              // Left edit / right deletion conflict with pending right edit.
              logger.debug(
                s"Conflict between right deletion of $editedBaseElement and its edit on the left into $leftElement with following right edit."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
                mergeAlgebra.leftEditConflictingWithRightEdit(
                  partialResult,
                  editedElements = deferredEdited :+ editedBaseElement,
                  leftEditElements = deferredLeftEdits :+ leftElement,
                  rightEditElements = deferredRightEdits
                ),
                deferredEdited = Vector.empty,
                deferredLeftEdits = Vector.empty,
                deferredRightEdits = Vector.empty
              )

            case (
                  _,
                  _,
                  Seq(Contribution.Difference(followingRightElement), _*)
                ) =>
              // Edit conflict with a pending right insertion.
              logger.debug(
                s"Coalescing edit conflict of $editedBaseElement into $leftElement on the left and $rightElement on the right with following right insertion of $followingRightElement."
              )
              mergeBetweenRunsOfCommonElements(base, left, rightTail)(
                partialResult,
                deferredEdited = deferredEdited,
                deferredLeftEdits,
                deferredRightEdits :+ rightElement
              )

            case (
                  Seq(
                    Contribution.CommonToBaseAndRightOnly(_),
                    _*
                  ),
                  _,
                  Seq(Contribution.Difference(followingRightElement), _*)
                ) if leftEditNotMarooned(rightTail) =>
              // Right edit / left deletion conflict with pending right
              // insertion and pending left edit.
              logger.debug(
                s"Coalescing conflict between left deletion of $editedBaseElement and its edit on the right into $rightElement, with following insertion of $followingRightElement on the right."
              )
              mergeBetweenRunsOfCommonElements(base, left, rightTail)(
                partialResult,
                deferredEdited = deferredEdited,
                deferredLeftEdits = deferredLeftEdits,
                deferredRightEdits = deferredRightEdits :+ rightElement
              )

            case (
                  Seq(
                    Contribution.CommonToBaseAndRightOnly(_),
                    _*
                  ),
                  _,
                  _
                ) if leftEditNotMarooned(rightTail) =>
              // Right edit / left deletion conflict with pending left edit.
              logger.debug(
                s"Conflict between left deletion of $editedBaseElement and its edit on the right into $rightElement with following left edit."
              )
              mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
                mergeAlgebra.leftEditConflictingWithRightEdit(
                  partialResult,
                  editedElements = deferredEdited :+ editedBaseElement,
                  leftEditElements = deferredLeftEdits,
                  rightEditElements = deferredRightEdits :+ rightElement
                ),
                deferredEdited = Vector.empty,
                deferredLeftEdits = Vector.empty,
                deferredRightEdits = Vector.empty
              )

            case (
                  _,
                  Seq(Contribution.Difference(followingLeftElement), _*),
                  _
                ) =>
              // Edit conflict with a pending left insertion.
              logger.debug(
                s"Coalescing edit conflict of $editedBaseElement into $leftElement on the left and $rightElement on the right with following left insertion of $followingLeftElement."
              )
              mergeBetweenRunsOfCommonElements(base, leftTail, right)(
                partialResult,
                deferredEdited = deferredEdited,
                deferredLeftEdits :+ leftElement,
                deferredRightEdits
              )

            case _ =>
              // Edit conflict.
              logger.debug(
                s"Edit conflict of $editedBaseElement into $leftElement on the left and $rightElement on the right."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                mergeAlgebra.leftEditConflictingWithRightEdit(
                  partialResult,
                  editedElements = deferredEdited :+ editedBaseElement,
                  leftEditElements = deferredLeftEdits :+ leftElement,
                  rightEditElements = deferredRightEdits :+ rightElement
                ),
                deferredEdited = Vector.empty,
                deferredLeftEdits = Vector.empty,
                deferredRightEdits = Vector.empty
              )
          end match

        case (
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*)
            ) => // Left insertion with pending coincident edit.
          require(
            deferredEdited.isEmpty && deferredLeftEdits.isEmpty && deferredRightEdits.isEmpty
          )
          logger.debug(
            s"Left insertion of $leftElement with following coincident edit."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            mergeAlgebra.leftInsertion(
              partialResult,
              insertedElement = leftElement
            ),
            deferredEdited,
            deferredLeftEdits,
            deferredRightEdits
          )

        case (
              Seq(Contribution.Difference(deletedBaseElement), baseTail*),
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.CommonToBaseAndRightOnly(_), _*)
            ) => // Coincident deletion with pending left edit.
          require(
            deferredEdited.isEmpty && deferredLeftEdits.isEmpty && deferredRightEdits.isEmpty
          )
          logger.debug(
            s"Coincident deletion of $deletedBaseElement with following left edit."
          )
          mergeBetweenRunsOfCommonElements(baseTail, left, right)(
            mergeAlgebra.coincidentDeletion(
              partialResult,
              deletedElement = deletedBaseElement
            ),
            deferredEdited,
            deferredLeftEdits,
            deferredRightEdits
          )

        case (
              Seq(Contribution.Difference(editedBaseElement), baseTail*),
              Seq(Contribution.Difference(leftElement), leftTail*),
              _
            ) => // Left edit / right deletion conflict.
          baseTail -> leftTail match
            case (
                  Seq(Contribution.Difference(_), _*),
                  Seq(Contribution.Difference(_), _*)
                ) =>
              logger.debug(
                s"Conflict between right deletion of $editedBaseElement and its edit on the left into $leftElement, coalescing with following right deletion and left edit conflict."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
                partialResult,
                deferredEdited = deferredEdited :+ editedBaseElement,
                deferredLeftEdits = deferredLeftEdits :+ leftElement,
                deferredRightEdits = deferredRightEdits
              )

            case (_, Seq(Contribution.Difference(followingLeftElement), _*)) =>
              logger.debug(
                s"Coalescing conflict between right deletion of $editedBaseElement and its edit on the left into $leftElement, with following insertion of $followingLeftElement on the left."
              )
              mergeBetweenRunsOfCommonElements(base, leftTail, right)(
                partialResult,
                deferredEdited = deferredEdited,
                deferredLeftEdits = deferredLeftEdits :+ leftElement,
                deferredRightEdits = deferredRightEdits
              )

            case _ =>
              logger.debug(
                s"Conflict between right deletion of $editedBaseElement and its edit on the left into $leftElement."
              )
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
                mergeAlgebra.leftEditConflictingWithRightEdit(
                  partialResult,
                  editedElements = deferredEdited :+ editedBaseElement,
                  leftEditElements = deferredLeftEdits :+ leftElement,
                  rightEditElements = deferredRightEdits
                ),
                deferredEdited = Vector.empty,
                deferredLeftEdits = Vector.empty,
                deferredRightEdits = Vector.empty
              )

        case (
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Right insertion with pending coincident edit.
          require(
            deferredEdited.isEmpty && deferredLeftEdits.isEmpty && deferredRightEdits.isEmpty
          )
          logger.debug(
            s"Right insertion of $rightElement with following coincident edit."
          )
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            mergeAlgebra.rightInsertion(
              partialResult,
              insertedElement = rightElement
            ),
            deferredEdited,
            deferredLeftEdits,
            deferredRightEdits
          )

        case (
              Seq(Contribution.Difference(deletedBaseElement), baseTail*),
              Seq(Contribution.CommonToBaseAndLeftOnly(_), _*),
              Seq(Contribution.Difference(_), _*)
            ) => // Coincident deletion with pending right edit.
          require(
            deferredEdited.isEmpty && deferredLeftEdits.isEmpty && deferredRightEdits.isEmpty
          )
          logger.debug(
            s"Coincident deletion of $deletedBaseElement with following right edit."
          )
          mergeBetweenRunsOfCommonElements(baseTail, left, right)(
            mergeAlgebra.coincidentDeletion(
              partialResult,
              deletedElement = deletedBaseElement
            ),
            deferredEdited,
            deferredLeftEdits,
            deferredRightEdits
          )

        case (
              Seq(Contribution.Difference(editedBaseElement), baseTail*),
              _,
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Right edit / left deletion conflict.
          baseTail -> rightTail match
            case (
                  Seq(Contribution.Difference(_), _*),
                  Seq(Contribution.Difference(_), _*)
                ) =>
              logger.debug(
                s"Conflict between left deletion of $editedBaseElement and its edit on the right into $rightElement, coalescing with following left deletion and right edit conflict."
              )
              mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
                partialResult,
                deferredEdited = deferredEdited :+ editedBaseElement,
                deferredLeftEdits = deferredLeftEdits,
                deferredRightEdits = deferredRightEdits :+ rightElement
              )

            case (_, Seq(Contribution.Difference(followingRightElement), _*)) =>
              logger.debug(
                s"Coalescing conflict between left deletion of $editedBaseElement and its edit on the right into $rightElement, with following insertion of $followingRightElement on the right."
              )
              mergeBetweenRunsOfCommonElements(base, left, rightTail)(
                partialResult,
                deferredEdited = deferredEdited,
                deferredLeftEdits = deferredLeftEdits,
                deferredRightEdits = deferredRightEdits :+ rightElement
              )

            case _ =>
              logger.debug(
                s"Conflict between left deletion of $editedBaseElement and its edit on the right into $rightElement."
              )
              mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
                mergeAlgebra.leftEditConflictingWithRightEdit(
                  partialResult,
                  editedElements = deferredEdited :+ editedBaseElement,
                  leftEditElements = deferredLeftEdits,
                  rightEditElements = deferredRightEdits :+ rightElement
                ),
                deferredEdited = Vector.empty,
                deferredLeftEdits = Vector.empty,
                deferredRightEdits = Vector.empty
              )

        case (
              Seq(Contribution.Difference(deletedBaseElement), baseTail*),
              _,
              _
            ) => // Coincident deletion.
          require(
            deferredEdited.isEmpty && deferredLeftEdits.isEmpty && deferredRightEdits.isEmpty
          )
          logger.debug(s"Coincident deletion of $deletedBaseElement.")
          mergeBetweenRunsOfCommonElements(baseTail, left, right)(
            mergeAlgebra.coincidentDeletion(
              partialResult,
              deletedElement = deletedBaseElement
            ),
            deferredEdited,
            deferredLeftEdits,
            deferredRightEdits
          )

        case (
              Seq(Contribution.CommonToBaseAndLeftOnly(_), _*),
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.Difference(_), _*)
            ) => // Left insertion with pending right edit.
          require(
            deferredEdited.isEmpty && deferredLeftEdits.isEmpty && deferredRightEdits.isEmpty
          )
          logger.debug(
            s"Left insertion of $leftElement with following right edit."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            mergeAlgebra.leftInsertion(
              partialResult,
              insertedElement = leftElement
            ),
            deferredEdited,
            deferredLeftEdits,
            deferredRightEdits
          )

        case (
              Seq(Contribution.CommonToBaseAndRightOnly(_), _*),
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Right insertion with pending left edit.
          require(
            deferredEdited.isEmpty && deferredLeftEdits.isEmpty && deferredRightEdits.isEmpty
          )
          logger.debug(
            s"Right insertion of $rightElement with following left edit."
          )
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            mergeAlgebra.rightInsertion(
              partialResult,
              insertedElement = rightElement
            ),
            deferredEdited,
            deferredLeftEdits,
            deferredRightEdits
          )

        case (
              _,
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Insertion conflict.
          leftTail -> rightTail match
            case (
                  Seq(Contribution.Difference(_), _*),
                  Seq(Contribution.Difference(_), _*)
                ) =>
              logger.debug(
                s"Coalescing conflict between left insertion of $leftElement and right insertion of $rightElement with following left insertion and right insertion conflict."
              )
              mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
                partialResult,
                deferredEdited,
                deferredLeftEdits = deferredLeftEdits :+ leftElement,
                deferredRightEdits = deferredRightEdits :+ rightElement
              )

            case (Seq(Contribution.Difference(_), _*), _) =>
              logger.debug(
                s"Coalescing conflict between left insertion of $leftElement and right insertion of $rightElement with following left insertion."
              )
              mergeBetweenRunsOfCommonElements(base, leftTail, right)(
                partialResult,
                deferredEdited,
                deferredLeftEdits = deferredLeftEdits :+ leftElement,
                deferredRightEdits = deferredRightEdits
              )

            case (_, Seq(Contribution.Difference(_), _*)) =>
              logger.debug(
                s"Coalescing conflict between left insertion of $leftElement and right insertion of $rightElement with following right insertion."
              )
              mergeBetweenRunsOfCommonElements(base, left, rightTail)(
                partialResult,
                deferredEdited,
                deferredLeftEdits = deferredLeftEdits,
                deferredRightEdits = deferredRightEdits :+ rightElement
              )

            case _ =>
              logger.debug(
                s"Conflict between left insertion of $leftElement and right insertion of $rightElement."
              )
              mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
                mergeAlgebra.leftEditConflictingWithRightEdit(
                  partialResult,
                  editedElements = deferredEdited,
                  leftEditElements = deferredLeftEdits :+ leftElement,
                  rightEditElements = deferredRightEdits :+ rightElement
                ),
                deferredEdited = Vector.empty,
                deferredLeftEdits = Vector.empty,
                deferredRightEdits = Vector.empty
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
          require(
            deferredEdited.isEmpty && deferredLeftEdits.isEmpty && deferredRightEdits.isEmpty
          )
          logger.debug(s"Left insertion of $leftElement.")
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            mergeAlgebra.leftInsertion(
              partialResult,
              insertedElement = leftElement
            ),
            deferredEdited,
            deferredLeftEdits,
            deferredRightEdits
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
          require(
            deferredEdited.isEmpty && deferredLeftEdits.isEmpty && deferredRightEdits.isEmpty
          )
          logger.debug(s"Right insertion of $rightElement.")
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            mergeAlgebra.rightInsertion(
              partialResult,
              insertedElement = rightElement
            ),
            deferredEdited,
            deferredLeftEdits,
            deferredRightEdits
          )

        case (Seq(), Seq(), Seq()) => // Terminating case!
          require(
            deferredEdited.isEmpty && deferredLeftEdits.isEmpty && deferredRightEdits.isEmpty
          )
          logger.debug(s"Merge yielded:\n${pprint(partialResult)}")
          partialResult
      end match
    end mergeBetweenRunsOfCommonElements

    val longestCommonSubsequence =
      LongestCommonSubsequence.of(base, left, right)(equality)

    // TODO: for now, failure is not tolerated, but obviously that will have to
    // be accommodated - both merge conflicts and later divergences.
    Right(
      mergeBetweenRunsOfCommonElements(
        longestCommonSubsequence.base,
        longestCommonSubsequence.left,
        longestCommonSubsequence.right
      )(
        partialResult = mergeAlgebra.empty,
        deferredEdited = IndexedSeq.empty,
        deferredLeftEdits = IndexedSeq.empty,
        deferredRightEdits = IndexedSeq.empty
      )
    )
  end of

  trait MergeAlgebra[Result[_], Element]:
    def empty: Result[Element]
    def preservation(
        result: Result[Element],
        preservedElement: Element
    ): Result[Element]

    def leftInsertion(
        result: Result[Element],
        insertedElement: Element
    ): Result[Element]

    def rightInsertion(
        result: Result[Element],
        insertedElement: Element
    ): Result[Element]

    def coincidentInsertion(
        result: Result[Element],
        insertedElement: Element
    ): Result[Element]

    def leftDeletion(
        result: Result[Element],
        deletedElement: Element
    ): Result[Element]

    def rightDeletion(
        result: Result[Element],
        deletedElement: Element
    ): Result[Element]

    def coincidentDeletion(
        result: Result[Element],
        deletedElement: Element
    ): Result[Element]

    def leftEdit(
        result: Result[Element],
        editedElement: Element,
        editElements: IndexedSeq[Element]
    ): Result[Element]

    def rightEdit(
        result: Result[Element],
        editedElement: Element,
        editElements: IndexedSeq[Element]
    ): Result[Element]

    def coincidentEdit(
        result: Result[Element],
        editedElement: Element,
        editElements: IndexedSeq[Element]
    ): Result[Element]

    // TODO: this could in theory model all of the other operations in the merge
    // algebra, but for now we'll keep it distinct to make our intent clear.
    // Even coincident deletions are treated separately as they aren't
    // conflicts. It does mean we should probably have some contract checking to
    // make sure a call really does represent a conflict.
    def leftEditConflictingWithRightEdit(
        result: Result[Element],
        editedElements: IndexedSeq[Element],
        // If this is empty, it represents a left-deletion rather than a
        // left-edit.
        leftEditElements: IndexedSeq[Element],
        // If this is empty, it represents a right-deletion rather than a
        // right-edit.
        rightEditElements: IndexedSeq[Element]
    ): Result[Element]
  end MergeAlgebra

  // TODO: "Something went wrong!" - "What was it?"
  case object Divergence

end merge
