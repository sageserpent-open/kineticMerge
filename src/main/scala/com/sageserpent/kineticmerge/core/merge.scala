package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.{Contribution, Sized}
import com.typesafe.scalalogging.StrictLogging
import monocle.syntax.all.*

import scala.annotation.tailrec

object merge extends StrictLogging:
  /** Performs a three-way merge.
    *
    * @param mergeAlgebra
    *   Carries out the merge operations determined by the merge algorithm. This
    *   allows a pluggable backend for deciding which element takes precedence
    *   when say, elements from either side compare equal via {@code equality},
    *   but may not actually be the same via their natural equality. It can also
    *   support propagation of edits and insertions, as well as contract
    *   checking when testing.
    * @param base
    *   {@code left} and {@code right} are considered modified versions of this
    *   sequence.
    * @param left
    *   A modified version of {@code base}. Where there is a preservation or a
    *   coincident insertion, then this provides the representative element for
    *   the merge.
    * @param right
    *   The other modified version of {@code base}.
    * @param elementSize
    *   Some measure of an element's size, used to determine the longest common
    *   subsequence that provides the backbone for the merge.
    * @tparam Element
    *   What the sequences contain.
    * @return
    *   A [[Result]] representing either a full merge without conflicts or a
    *   conflicted merge.
    * @note
    *   Rules of the game: If an element is [[Contribution.Common]] to all three
    *   sides, it is preserved according to the merge algebra in the merge; the
    *   merge uses the longest common subsequence as a backbone to arrange other
    *   changes around. <p><p> If an element is
    *   [[Contribution.CommonToLeftAndRightOnly]], it is coincidentally inserted
    *   on both the left and right side, so goes in according to the merge
    *   algebra into the merge, extending the backbone. <p><p> If an element is
    *   either [[Contribution.CommonToBaseAndLeftOnly]] or
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
    *   capture all the [[Contribution.Difference]] elements.<p></p>Conflicts
    *   are also greedy, taking successive left- and right-insertions to make a
    *   longer conflict. This occurs even if following insertions could be made
    *   in isolation without causing a conflict. However, as mentioned above,
    *   edits are preferred over conflicts, so an edit will win out over a
    *   conflict in taking insertions.<p></p>Coincident edits, where an element
    *   is a [[Contribution.Difference]] in {@code base} that is edited into a
    *   [[Contribution.CommonToLeftAndRightOnly]] in both the {@code left} and
    *   {@code right} are also greedy and will take successive
    *   [[Contribution.CommonToLeftAndRightOnly]] elements to make a long
    *   coincident edit.
    */
  def of[Result[_], Element: Eq: Sized](
      mergeAlgebra: MergeAlgebra[Result, Element]
  )(
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  ): Result[Element] =
    val equality = summon[Eq[Element]]

    def rightEditNotMaroonedByPriorCoincidentInsertion(
        leftTail: Seq[Contribution[Element]]
    ) =
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

    def leftEditNotMaroonedByPriorCoincidentInsertion(
        rightTail: Seq[Contribution[Element]]
    ) =
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

    def rightEditNotMaroonedByPriorLeftDeletion(
        baseTail: Seq[Contribution[Element]]
    ) =
      baseTail
        .takeWhile {
          case Contribution.CommonToBaseAndLeftOnly(_) => false
          case _                                       => true
        }
        .forall {
          case Contribution.CommonToBaseAndRightOnly(_) => false
          case _                                        => true
        }

    def leftEditNotMaroonedByPriorRightDeletion(
        baseTail: Seq[Contribution[Element]]
    ) =
      baseTail
        .takeWhile {
          case Contribution.CommonToBaseAndRightOnly(_) => false
          case _                                        => true
        }
        .forall {
          case Contribution.CommonToBaseAndLeftOnly(_) => false
          case _                                       => true
        }

    trait LeftEditOperations:
      def coalesceLeftInsertion(insertedElement: Element): LeftEdit
      def finalLeftEdit(
          result: Result[Element],
          editedElement: Element,
          insertedElement: Element
      ): Result[Element]
    end LeftEditOperations

    trait RightEditOperations:
      def coalesceRightInsertion(insertedElement: Element): RightEdit
      def finalRightEdit(
          result: Result[Element],
          editedElement: Element,
          insertedElement: Element
      ): Result[Element]
    end RightEditOperations

    trait CoincidentEditOperations:
      def coalesceCoincidentInsertion(
          insertedElementOnLeft: Element,
          insertedElementOnRight: Element
      ): CoincidentEdit
      def finalCoincidentEdit(
          result: Result[Element],
          editedElement: Element,
          insertedElementOnLeft: Element,
          insertedElementOnRight: Element
      ): Result[Element]
    end CoincidentEditOperations

    trait ConflictOperations:
      def coalesceConflict(
          editedBaseElement: Option[Element],
          leftElement: Option[Element],
          rightElement: Option[Element]
      ): Conflict
      def finalConflict(
          result: Result[Element],
          editedElement: Option[Element],
          leftElement: Option[Element],
          rightElement: Option[Element]
      ): Result[Element]
    end ConflictOperations

    trait Coalescence:
    end Coalescence
    object NoCoalescence
        extends Coalescence
        with LeftEditOperations
        with RightEditOperations
        with CoincidentEditOperations
        with ConflictOperations:
      private val leftEdit = LeftEdit(
        deferredLeftEdits = IndexedSeq.empty
      )
      private val rightEdit = RightEdit(
        deferredRightEdits = IndexedSeq.empty
      )
      private val coincidentEdit = CoincidentEdit(
        deferredCoincidentEdits = IndexedSeq.empty
      )
      private val conflict = Conflict(
        deferredEdited = IndexedSeq.empty,
        deferredLeftEdits = IndexedSeq.empty,
        deferredRightEdits = IndexedSeq.empty
      )

      // Export the union of the various APIs...
      export leftEdit.{coalesceLeftInsertion, finalLeftEdit}
      export rightEdit.{coalesceRightInsertion, finalRightEdit}
      export coincidentEdit.{coalesceCoincidentInsertion, finalCoincidentEdit}
      export conflict.{coalesceConflict, finalConflict}
    end NoCoalescence

    case class LeftEdit(
        deferredLeftEdits: IndexedSeq[Element]
    ) extends Coalescence
        with LeftEditOperations:
      override def coalesceLeftInsertion(insertedElement: Element): LeftEdit =
        this.focus(_.deferredLeftEdits).modify(_ :+ insertedElement)
      override def finalLeftEdit(
          result: Result[Element],
          editedElement: Element,
          insertedElement: Element
      ): Result[Element] = mergeAlgebra.leftEdit(
        result,
        editedElement,
        deferredLeftEdits :+ insertedElement
      )
    end LeftEdit

    case class RightEdit(
        deferredRightEdits: IndexedSeq[Element]
    ) extends Coalescence
        with RightEditOperations:
      override def coalesceRightInsertion(insertedElement: Element): RightEdit =
        this.focus(_.deferredRightEdits).modify(_ :+ insertedElement)

      override def finalRightEdit(
          result: Result[Element],
          editedElement: Element,
          insertedElement: Element
      ): Result[Element] = mergeAlgebra.rightEdit(
        result,
        editedElement,
        editElements = deferredRightEdits :+ insertedElement
      )
    end RightEdit

    case class CoincidentEdit(
        deferredCoincidentEdits: IndexedSeq[(Element, Element)]
    ) extends Coalescence
        with CoincidentEditOperations:
      override def coalesceCoincidentInsertion(
          insertedElementOnLeft: Element,
          insertedElementOnRight: Element
      ): CoincidentEdit =
        this
          .focus(_.deferredCoincidentEdits)
          .modify(_ :+ (insertedElementOnLeft -> insertedElementOnRight))
      override def finalCoincidentEdit(
          result: Result[Element],
          editedElement: Element,
          insertedElementOnLeft: Element,
          insertedElementOnRight: Element
      ): Result[Element] =
        mergeAlgebra.coincidentEdit(
          result,
          editedElement,
          editElements =
            deferredCoincidentEdits :+ (insertedElementOnLeft -> insertedElementOnRight)
        )
    end CoincidentEdit

    case class Conflict(
        deferredEdited: IndexedSeq[Element],
        deferredLeftEdits: IndexedSeq[Element],
        deferredRightEdits: IndexedSeq[Element]
    ) extends Coalescence
        with ConflictOperations:
      override def coalesceConflict(
          editedBaseElement: Option[Element],
          leftElement: Option[Element],
          rightElement: Option[Element]
      ): Conflict = this
        .focus(_.deferredEdited)
        .modify(_ ++ editedBaseElement)
        .focus(_.deferredLeftEdits)
        .modify(_ ++ leftElement)
        .focus(_.deferredRightEdits)
        .modify(_ ++ rightElement)

      override def finalConflict(
          result: Result[Element],
          editedElement: Option[Element],
          leftElement: Option[Element],
          rightElement: Option[Element]
      ): Result[Element] = mergeAlgebra.conflict(
        result,
        editedElements = deferredEdited ++ editedElement,
        leftEditElements = deferredLeftEdits ++ leftElement,
        rightEditElements = deferredRightEdits ++ rightElement
      )
    end Conflict

    inline def insertionConflict(
        base: Seq[Contribution[Element]],
        left: Seq[Contribution[Element]],
        right: Seq[Contribution[Element]],
        partialResult: Result[Element],
        conflictOperations: ConflictOperations,
        leftElement: Element,
        leftTail: Seq[Contribution[Element]],
        rightElement: Element,
        rightTail: Seq[Contribution[Element]]
    ) =
      leftTail -> rightTail match
        case (
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(_), _*)
            ) =>
          logger.debug(
            s"Conflict between left insertion of ${pprintCustomised(leftElement)} and right insertion of ${pprintCustomised(rightElement)}, coalescing with following left insertion and right insertion conflict."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              None,
              Some(leftElement),
              Some(rightElement)
            )
          )

        case (Seq(Contribution.Difference(_), _*), _) =>
          logger.debug(
            s"Conflict between left insertion of ${pprintCustomised(leftElement)} and right insertion of ${pprintCustomised(rightElement)}, coalescing with following left insertion."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              None,
              Some(leftElement),
              None
            )
          )

        case (_, Seq(Contribution.Difference(_), _*)) =>
          logger.debug(
            s"Conflict between left insertion of ${pprintCustomised(leftElement)} and right insertion of ${pprintCustomised(rightElement)}, coalescing with following right insertion."
          )
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              None,
              None,
              Some(rightElement)
            )
          )

        case _ =>
          logger.debug(
            s"Conflict between left insertion of ${pprintCustomised(leftElement)} and right insertion of ${pprintCustomised(rightElement)}."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
            partialResult = conflictOperations.finalConflict(
              partialResult,
              None,
              Some(leftElement),
              Some(rightElement)
            ),
            coalescence = NoCoalescence
          )

    inline def rightEditLeftDeletionConflict(
        base: Seq[Contribution[Element]],
        partialResult: Result[Element],
        editedBaseElement: Element,
        baseTail: Seq[Contribution[Element]]
    )(
        conflictOperations: ConflictOperations,
        left: Seq[Contribution[Element]],
        rightElement: Element,
        rightTail: Seq[Contribution[Element]]
    ) =
      baseTail -> rightTail match
        case (
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(_), _*)
            ) =>
          logger.debug(
            s"Conflict between left deletion of ${pprintCustomised(editedBaseElement)} and its edit on the right into ${pprintCustomised(rightElement)}, coalescing with following left deletion and right edit conflict."
          )
          mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              Some(editedBaseElement),
              None,
              Some(rightElement)
            )
          )

        case (_, Seq(Contribution.Difference(followingRightElement), _*)) =>
          logger.debug(
            s"Conflict between left deletion of ${pprintCustomised(editedBaseElement)} and its edit on the right into ${pprintCustomised(rightElement)}, coalescing with following insertion of ${pprintCustomised(followingRightElement)} on the right."
          )
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              None,
              None,
              Some(rightElement)
            )
          )

        case _ =>
          logger.debug(
            s"Conflict between left deletion of ${pprintCustomised(editedBaseElement)} and its edit on the right into ${pprintCustomised(rightElement)}."
          )
          mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
            partialResult = conflictOperations.finalConflict(
              partialResult,
              Some(editedBaseElement),
              None,
              Some(rightElement)
            ),
            coalescence = NoCoalescence
          )

    inline def leftEditRightDeletionConflict(
        base: Seq[Contribution[Element]],
        partialResult: Result[Element],
        editedBaseElement: Element,
        baseTail: Seq[Contribution[Element]]
    )(
        conflictOperations: ConflictOperations,
        right: Seq[Contribution[Element]],
        leftElement: Element,
        leftTail: Seq[Contribution[Element]]
    ) =
      baseTail -> leftTail match
        case (
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(_), _*)
            ) =>
          logger.debug(
            s"Conflict between right deletion of ${pprintCustomised(editedBaseElement)} and its edit on the left into ${pprintCustomised(leftElement)}, coalescing with following right deletion and left edit conflict."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              Some(editedBaseElement),
              Some(leftElement),
              None
            )
          )

        case (_, Seq(Contribution.Difference(followingLeftElement), _*)) =>
          logger.debug(
            s"Conflict between right deletion of ${pprintCustomised(editedBaseElement)} and its edit on the left into ${pprintCustomised(leftElement)}, coalescing with following insertion of ${pprintCustomised(followingLeftElement)} on the left."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              None,
              Some(leftElement),
              None
            )
          )

        case _ =>
          logger.debug(
            s"Conflict between right deletion of ${pprintCustomised(editedBaseElement)} and its edit on the left into ${pprintCustomised(leftElement)}."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
            partialResult = conflictOperations.finalConflict(
              partialResult,
              Some(editedBaseElement),
              Some(leftElement),
              None
            ),
            coalescence = NoCoalescence
          )

    inline def conflict(
        base: Seq[Contribution[Element]],
        left: Seq[Contribution[Element]],
        right: Seq[Contribution[Element]],
        partialResult: Result[Element],
        conflictOperations: ConflictOperations,
        editedBaseElement: Element,
        baseTail: Seq[Contribution[Element]],
        leftElement: Element,
        leftTail: Seq[Contribution[Element]],
        rightElement: Element,
        rightTail: Seq[Contribution[Element]]
    ) =
      (baseTail, leftTail, rightTail) match
        case (
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(_), _*)
            ) =>
          // Edit conflict with another pending edit conflict.
          logger.debug(
            s"Edit conflict of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(leftElement)} on the left and ${pprintCustomised(rightElement)} on the right, coalescing with following edit conflict."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              Some(editedBaseElement),
              Some(leftElement),
              Some(rightElement)
            )
          )

        case (
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(_), _*),
              _
            ) =>
          // Edit conflict with a pending left edit versus right deletion
          // conflict.
          logger.debug(
            s"Edit conflict of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(leftElement)} on the left and ${pprintCustomised(rightElement)} on the right, coalescing with following left edit versus right deletion conflict."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              Some(editedBaseElement),
              Some(leftElement),
              None
            )
          )

        case (
              Seq(Contribution.Difference(_), _*),
              _,
              Seq(Contribution.Difference(_), _*)
            ) =>
          // Edit conflict with a pending right edit versus left deletion
          // conflict.
          logger.debug(
            s"Edit conflict of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(leftElement)} on the left and ${pprintCustomised(rightElement)} on the right, coalescing with following right edit versus left deletion conflict."
          )
          mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              Some(editedBaseElement),
              None,
              Some(rightElement)
            )
          )

        case (
              _,
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(_), _*)
            ) =>
          // Edit conflict with a pending left insertion versus right
          // insertion conflict.
          logger.debug(
            s"Edit conflict of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(leftElement)} on the left and ${pprintCustomised(rightElement)} on the right, coalescing with following left insertion versus right insertion conflict."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              None,
              Some(leftElement),
              Some(rightElement)
            )
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
            ) if rightEditNotMaroonedByPriorCoincidentInsertion(leftTail) =>
          // Left edit / right deletion conflict with pending left insertion
          // and pending right edit.
          logger.debug(
            s"Conflict between right deletion of ${pprintCustomised(editedBaseElement)} and its edit on the left into ${pprintCustomised(leftElement)}, coalescing with following insertion of ${pprintCustomised(followingLeftElement)} on the left."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              None,
              Some(leftElement),
              None
            )
          )

        case (
              Seq(
                Contribution.CommonToBaseAndLeftOnly(_),
                _*
              ),
              _,
              _
            ) if rightEditNotMaroonedByPriorCoincidentInsertion(leftTail) =>
          // Left edit / right deletion conflict with pending right edit.
          logger.debug(
            s"Conflict between right deletion of ${pprintCustomised(editedBaseElement)} and its edit on the left into ${pprintCustomised(leftElement)} with following right edit."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
            partialResult = conflictOperations.finalConflict(
              partialResult,
              Some(editedBaseElement),
              Some(leftElement),
              None
            ),
            coalescence = NoCoalescence
          )

        case (
              Seq(
                Contribution.CommonToBaseAndRightOnly(_),
                _*
              ),
              _,
              Seq(Contribution.Difference(followingRightElement), _*)
            ) if leftEditNotMaroonedByPriorCoincidentInsertion(rightTail) =>
          // Right edit / left deletion conflict with pending right
          // insertion and pending left edit.
          logger.debug(
            s"Conflict between left deletion of ${pprintCustomised(editedBaseElement)} and its edit on the right into ${pprintCustomised(rightElement)}, coalescing with following insertion of ${pprintCustomised(followingRightElement)} on the right."
          )
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              None,
              None,
              Some(rightElement)
            )
          )

        case (
              Seq(
                Contribution.CommonToBaseAndRightOnly(_),
                _*
              ),
              _,
              _
            ) if leftEditNotMaroonedByPriorCoincidentInsertion(rightTail) =>
          // Right edit / left deletion conflict with pending left edit.
          logger.debug(
            s"Conflict between left deletion of ${pprintCustomised(editedBaseElement)} and its edit on the right into ${pprintCustomised(rightElement)} with following left edit."
          )
          mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
            partialResult = conflictOperations.finalConflict(
              partialResult,
              Some(editedBaseElement),
              None,
              Some(rightElement)
            ),
            coalescence = NoCoalescence
          )

        case (
              Seq(Contribution.Difference(followingBaseElement), _*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*)
            ) =>
          // Edit conflict with a pending coincident edit.
          logger.debug(
            s"Edit conflict of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(leftElement)} on the left and ${pprintCustomised(rightElement)} on the right with following coincident edit of ${pprintCustomised(followingBaseElement)}."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            partialResult = conflictOperations.finalConflict(
              partialResult,
              Some(editedBaseElement),
              Some(leftElement),
              Some(rightElement)
            ),
            coalescence = NoCoalescence
          )

        case (
              Seq(Contribution.Difference(followingBaseElement), _*),
              _,
              _
            ) =>
          // If the following element in the base would also be deleted on
          // both sides, coalesce into a single edit conflict.
          logger.debug(
            s"Edit conflict of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(leftElement)} on the left and ${pprintCustomised(rightElement)} on the right, coalescing with following coincident deletion of ${pprintCustomised(followingBaseElement)}."
          )
          mergeBetweenRunsOfCommonElements(baseTail, left, right)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              Some(editedBaseElement),
              None,
              None
            )
          )

        case (
              _,
              Seq(Contribution.Difference(followingLeftElement), _*),
              _
            ) =>
          // Edit conflict with a pending left insertion.
          logger.debug(
            s"Edit conflict of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(leftElement)} on the left and ${pprintCustomised(rightElement)} on the right, coalescing with following left insertion of ${pprintCustomised(followingLeftElement)}."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              None,
              Some(leftElement),
              None
            )
          )

        case (
              _,
              _,
              Seq(Contribution.Difference(followingRightElement), _*)
            ) =>
          // Edit conflict with a pending right insertion.
          logger.debug(
            s"Edit conflict of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(leftElement)} on the left and ${pprintCustomised(rightElement)} on the right, coalescing with following right insertion of ${pprintCustomised(followingRightElement)}."
          )
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            partialResult,
            coalescence = conflictOperations.coalesceConflict(
              None,
              None,
              Some(rightElement)
            )
          )

        case _ =>
          // Edit conflict.
          logger.debug(
            s"Edit conflict of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(leftElement)} on the left and ${pprintCustomised(rightElement)} on the right."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            partialResult = conflictOperations.finalConflict(
              partialResult,
              Some(editedBaseElement),
              Some(leftElement),
              Some(rightElement)
            ),
            coalescence = NoCoalescence
          )
      end match
    end conflict

    inline def leftEdit(
        base: Seq[Contribution[Element]],
        partialResult: Result[Element],
        editedBaseElement: Element,
        baseTail: Seq[Contribution[Element]],
        leftTail: Seq[Contribution[Element]],
        rightTail: Seq[Contribution[Element]]
    )(
        leftEditOperations: LeftEditOperations,
        leftElement: Element,
        right: Seq[Contribution[Element]]
    ) =
      baseTail -> leftTail match
        case (
              Seq(
                Contribution.Difference(followingBaseElement),
                _*
              ),
              _
            ) if leftEditNotMaroonedByPriorCoincidentInsertion(rightTail) =>
          // There is a pending coincident deletion; handle this left edit
          // in isolation without coalescing any following left insertion.
          logger.debug(
            s"Left edit of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(leftElement)} with following coincident deletion of ${pprintCustomised(followingBaseElement)}."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            partialResult = leftEditOperations
              .finalLeftEdit(partialResult, editedBaseElement, leftElement),
            coalescence = NoCoalescence
          )

        case (
              Seq(Contribution.CommonToBaseAndRightOnly(_), _*),
              Seq(Contribution.Difference(_), _*)
            ) if leftEditNotMaroonedByPriorCoincidentInsertion(rightTail) =>
          // There is another pending left edit, but *don't* coalesce the
          // following element on the left; that then respects any
          // insertions that may be lurking on the right prior to the
          // pending left edit claiming the following element on the left.
          logger.debug(
            s"Left edit of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(leftElement)}, not coalescing with following left edit."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            partialResult = leftEditOperations
              .finalLeftEdit(partialResult, editedBaseElement, leftElement),
            coalescence = NoCoalescence
          )

        case (_, Seq(Contribution.Difference(followingLeftElement), _*)) =>
          // If the following element on the left would also be inserted,
          // coalesce into a single edit.
          logger.debug(
            s"Left edit of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(leftElement)}, coalescing with following insertion of ${pprintCustomised(followingLeftElement)} on the left."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            partialResult,
            coalescence = leftEditOperations.coalesceLeftInsertion(leftElement)
          )

        case _ =>
          logger.debug(
            s"Left edit of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(leftElement)}."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            partialResult = leftEditOperations
              .finalLeftEdit(partialResult, editedBaseElement, leftElement),
            coalescence = NoCoalescence
          )
      end match
    end leftEdit

    inline def rightEdit(
        base: Seq[Contribution[Element]],
        partialResult: Result[Element],
        editedBaseElement: Element,
        baseTail: Seq[Contribution[Element]],
        leftTail: Seq[Contribution[Element]],
        rightTail: Seq[Contribution[Element]]
    )(
        rightEditOperations: RightEditOperations,
        rightElement: Element,
        left: Seq[Contribution[Element]]
    ) =
      baseTail -> rightTail match
        case (
              Seq(
                Contribution.Difference(followingBaseElement),
                _*
              ),
              _
            ) if rightEditNotMaroonedByPriorCoincidentInsertion(leftTail) =>
          // There is a pending coincident deletion; handle this right edit
          // in isolation without coalescing any following right insertion.
          logger.debug(
            s"Right edit of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(rightElement)} with following coincident deletion of ${pprintCustomised(followingBaseElement)}."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            partialResult = rightEditOperations.finalRightEdit(
              partialResult,
              editedBaseElement,
              rightElement
            ),
            coalescence = NoCoalescence
          )

        case (
              Seq(Contribution.CommonToBaseAndLeftOnly(_), _*),
              Seq(Contribution.Difference(_), _*)
            ) if rightEditNotMaroonedByPriorCoincidentInsertion(leftTail) =>
          // There is another pending right edit, but *don't* coalesce the
          // following element on the right; that then respects any
          // insertions that may be lurking on the left prior to the pending
          // right edit claiming the following element on the right.
          logger.debug(
            s"Right edit of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(rightElement)}, not coalescing with following right edit."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            partialResult = rightEditOperations.finalRightEdit(
              partialResult,
              editedBaseElement,
              rightElement
            ),
            coalescence = NoCoalescence
          )

        case (_, Seq(Contribution.Difference(followingRightElement), _*)) =>
          // If the following element on the right would also be inserted,
          // coalesce into a single edit.
          logger.debug(
            s"Right edit of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(rightElement)}, coalescing with following insertion of ${pprintCustomised(followingRightElement)} on the right."
          )
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            partialResult,
            coalescence =
              rightEditOperations.coalesceRightInsertion(rightElement)
          )

        case _ =>
          logger.debug(
            s"Right edit of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(rightElement)}."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            rightEditOperations.finalRightEdit(
              partialResult,
              editedBaseElement,
              rightElement
            ),
            coalescence = NoCoalescence
          )
      end match
    end rightEdit

    inline def coincidentEdit(
        base: Seq[Contribution[Element]],
        partialResult: Result[Element],
        editedBaseElement: Element,
        baseTail: Seq[Contribution[Element]],
        leftTail: Seq[Contribution[Element]],
        rightTail: Seq[Contribution[Element]]
    )(
        coincidentEditOperations: CoincidentEditOperations,
        coincidentElementOnLeft: Element,
        coincidentElementOnRight: Element
    ) =
      (baseTail, leftTail, rightTail) match
        case (
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*)
            ) =>
          logger.debug(
            s"Coincident edit of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(coincidentElementOnLeft)} on the left and ${pprintCustomised(coincidentElementOnRight)} on the right, not coalescing with following coincident edit."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            partialResult = coincidentEditOperations.finalCoincidentEdit(
              partialResult,
              editedBaseElement,
              coincidentElementOnLeft,
              coincidentElementOnRight
            ),
            coalescence = NoCoalescence
          )

        case (
              _,
              Seq(
                Contribution.CommonToLeftAndRightOnly(followingLeftElement),
                _*
              ),
              Seq(
                Contribution.CommonToLeftAndRightOnly(followingRightElement),
                _*
              )
            ) =>
          logger.debug(
            s"Coincident edit of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(coincidentElementOnLeft)} on the left and ${pprintCustomised(coincidentElementOnRight)} on the right, coalescing with following coincident insertion of ${pprintCustomised(followingLeftElement)} on the left and ${pprintCustomised(followingRightElement)} on the right."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
            partialResult,
            coalescence = coincidentEditOperations.coalesceCoincidentInsertion(
              coincidentElementOnLeft,
              coincidentElementOnRight
            )
          )

        case _ =>
          logger.debug(
            s"Coincident edit of ${pprintCustomised(editedBaseElement)} into ${pprintCustomised(coincidentElementOnLeft)} on the left and ${pprintCustomised(coincidentElementOnRight)} on the right."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            coincidentEditOperations.finalCoincidentEdit(
              partialResult,
              editedBaseElement,
              coincidentElementOnLeft,
              coincidentElementOnRight
            ),
            coalescence = NoCoalescence
          )
      end match
    end coincidentEdit

    @tailrec
    def mergeBetweenRunsOfCommonElements(
        base: Seq[Contribution[Element]],
        left: Seq[Contribution[Element]],
        right: Seq[Contribution[Element]]
    )(
        partialResult: Result[Element],
        coalescence: Coalescence
    ): Result[Element] =
      (coalescence, base, left, right) match
        // SYMMETRIC...
        case (
              NoCoalescence,
              Seq(Contribution.Common(baseElement), baseTail*),
              Seq(Contribution.Common(leftElement), leftTail*),
              Seq(Contribution.Common(rightElement), rightTail*)
            ) => // Preservation.
          logger.debug(
            s"Preservation of ${pprintCustomised(leftElement)} as it is common to all three sides."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            partialResult = mergeAlgebra.preservation(
              partialResult,
              preservedBaseElement = baseElement,
              preservedElementOnLeft = leftElement,
              preservedElementOnRight = rightElement
            ),
            coalescence = NoCoalescence
          )

        // SYMMETRIC...
        case (
              coincidentEditOperations: CoincidentEditOperations,
              Seq(Contribution.Difference(editedBaseElement), baseTail*),
              Seq(
                Contribution.CommonToLeftAndRightOnly(leftElement),
                leftTail*
              ),
              Seq(
                Contribution.CommonToLeftAndRightOnly(rightElement),
                rightTail*
              )
            ) => // Coincident edit.
          coincidentEdit(
            base,
            partialResult,
            editedBaseElement,
            baseTail,
            leftTail,
            rightTail
          )(
            coincidentEditOperations,
            coincidentElementOnLeft = leftElement,
            coincidentElementOnRight = rightElement
          )

        // SYMMETRIC...
        case (
              NoCoalescence,
              _,
              Seq(
                Contribution.CommonToLeftAndRightOnly(leftElement),
                leftTail*
              ),
              Seq(
                Contribution.CommonToLeftAndRightOnly(rightElement),
                rightTail*
              )
            ) => // Coincident insertion.
          logger.debug(
            s"Coincident insertion on left and right side of ${pprintCustomised(leftElement)}."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
            partialResult = mergeAlgebra.coincidentInsertion(
              partialResult,
              insertedElementOnLeft = leftElement,
              insertedElementOnRight = rightElement
            ),
            coalescence = NoCoalescence
          )

        // LEFT...
        case (
              leftEditOperations: LeftEditOperations,
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
          leftEdit(
            base,
            partialResult,
            editedBaseElement,
            baseTail,
            leftTail,
            rightTail
          )(leftEditOperations, leftElement, right)

        // RIGHT...
        case (
              rightEditOperations: RightEditOperations,
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
          rightEdit(
            base,
            partialResult,
            editedBaseElement,
            baseTail,
            leftTail,
            rightTail
          )(rightEditOperations, rightElement, left)

        // LEFT...
        case (
              NoCoalescence,
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
          logger.debug(
            s"Left deletion of ${pprintCustomised(deletedBaseElement)}."
          )
          mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
            mergeAlgebra.leftDeletion(
              partialResult,
              deletedElement = deletedBaseElement
            ),
            coalescence = NoCoalescence
          )

        // RIGHT...
        case (
              NoCoalescence,
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
          logger.debug(
            s"Right deletion of ${pprintCustomised(deletedBaseElement)}."
          )
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
            mergeAlgebra.rightDeletion(
              partialResult,
              deletedElement = deletedBaseElement
            ),
            coalescence = NoCoalescence
          )

        // SYMMETRIC...
        case (
              conflictOperations: ConflictOperations,
              Seq(Contribution.Difference(editedBaseElement), baseTail*),
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Conflict, multiple possibilities.
          conflict(
            base,
            left,
            right,
            partialResult,
            conflictOperations,
            editedBaseElement,
            baseTail,
            leftElement,
            leftTail,
            rightElement,
            rightTail
          )

        // LEFT...
        case (
              NoCoalescence,
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*)
            ) => // Left insertion with pending coincident edit.
          logger.debug(
            s"Left insertion of ${pprintCustomised(leftElement)} with following coincident edit."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            mergeAlgebra.leftInsertion(
              partialResult,
              insertedElement = leftElement
            ),
            coalescence = NoCoalescence
          )

        // RIGHT...
        case (
              NoCoalescence,
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Right insertion with pending coincident edit.
          logger.debug(
            s"Right insertion of ${pprintCustomised(rightElement)} with following coincident edit."
          )
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            mergeAlgebra.rightInsertion(
              partialResult,
              insertedElement = rightElement
            ),
            coalescence = NoCoalescence
          )

        // LEFT...
        case (
              NoCoalescence,
              Seq(Contribution.Difference(deletedBaseElement), baseTail*),
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.CommonToBaseAndRightOnly(_), _*)
            )
            if leftEditNotMaroonedByPriorRightDeletion(
              baseTail
            ) => // Coincident deletion with pending left edit.
          logger.debug(
            s"Coincident deletion of ${pprintCustomised(deletedBaseElement)} with following left edit."
          )
          mergeBetweenRunsOfCommonElements(baseTail, left, right)(
            mergeAlgebra.coincidentDeletion(
              partialResult,
              deletedElement = deletedBaseElement
            ),
            coalescence = NoCoalescence
          )

        // RIGHT...
        case (
              NoCoalescence,
              Seq(Contribution.Difference(deletedBaseElement), baseTail*),
              Seq(Contribution.CommonToBaseAndLeftOnly(_), _*),
              Seq(Contribution.Difference(_), _*)
            )
            if rightEditNotMaroonedByPriorLeftDeletion(
              baseTail
            ) => // Coincident deletion with pending right edit.
          logger.debug(
            s"Coincident deletion of ${pprintCustomised(deletedBaseElement)} with following right edit."
          )
          mergeBetweenRunsOfCommonElements(baseTail, left, right)(
            mergeAlgebra.coincidentDeletion(
              partialResult,
              deletedElement = deletedBaseElement
            ),
            coalescence = NoCoalescence
          )

        // LEFT...
        case (
              conflictOperations: ConflictOperations,
              Seq(Contribution.Difference(editedBaseElement), baseTail*),
              Seq(Contribution.Difference(leftElement), leftTail*),
              _
            ) => // Left edit / right deletion conflict.
          leftEditRightDeletionConflict(
            base,
            partialResult,
            editedBaseElement,
            baseTail
          )(conflictOperations, right, leftElement, leftTail)

        // RIGHT...
        case (
              conflictOperations: ConflictOperations,
              Seq(Contribution.Difference(editedBaseElement), baseTail*),
              _,
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Right edit / left deletion conflict.
          rightEditLeftDeletionConflict(
            base,
            partialResult,
            editedBaseElement,
            baseTail
          )(conflictOperations, left, rightElement, rightTail)

        // SYMMETRIC...
        case (
              NoCoalescence,
              Seq(Contribution.Difference(deletedBaseElement), baseTail*),
              _,
              _
            ) => // Coincident deletion.
          logger.debug(
            s"Coincident deletion of ${pprintCustomised(deletedBaseElement)}."
          )
          mergeBetweenRunsOfCommonElements(baseTail, left, right)(
            mergeAlgebra.coincidentDeletion(
              partialResult,
              deletedElement = deletedBaseElement
            ),
            coalescence = NoCoalescence
          )

        // LEFT...
        case (
              NoCoalescence,
              Seq(Contribution.CommonToBaseAndLeftOnly(_), _*),
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.Difference(_), _*)
            ) => // Left insertion with pending right edit.
          logger.debug(
            s"Left insertion of ${pprintCustomised(leftElement)} with following right edit."
          )
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            mergeAlgebra.leftInsertion(
              partialResult,
              insertedElement = leftElement
            ),
            coalescence = NoCoalescence
          )

        // RIGHT...
        case (
              NoCoalescence,
              Seq(Contribution.CommonToBaseAndRightOnly(_), _*),
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Right insertion with pending left edit.
          logger.debug(
            s"Right insertion of ${pprintCustomised(rightElement)} with following left edit."
          )
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            mergeAlgebra.rightInsertion(
              partialResult,
              insertedElement = rightElement
            ),
            coalescence = NoCoalescence
          )

        // SYMMETRIC...
        case (
              conflictOperations: ConflictOperations,
              _,
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Insertion conflict.
          insertionConflict(
            base,
            left,
            right,
            partialResult,
            conflictOperations,
            leftElement,
            leftTail,
            rightElement,
            rightTail
          )

        // LEFT...
        case (
              NoCoalescence,
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
          logger.debug(s"Left insertion of ${pprintCustomised(leftElement)}.")
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            mergeAlgebra.leftInsertion(
              partialResult,
              insertedElement = leftElement
            ),
            coalescence = NoCoalescence
          )

        // RIGHT...
        case (
              NoCoalescence,
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
          logger.debug(s"Right insertion of ${pprintCustomised(rightElement)}.")
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            mergeAlgebra.rightInsertion(
              partialResult,
              insertedElement = rightElement
            ),
            coalescence = NoCoalescence
          )

        // SYMMETRIC...
        case (NoCoalescence, Seq(), Seq(), Seq()) => // Terminating case!
          logger.debug(s"Merge yielded:\n${pprintCustomised(partialResult)}")
          partialResult
      end match
    end mergeBetweenRunsOfCommonElements

    val longestCommonSubsequence =
      LongestCommonSubsequence.of(base, left, right)

    mergeBetweenRunsOfCommonElements(
      longestCommonSubsequence.base,
      longestCommonSubsequence.left,
      longestCommonSubsequence.right
    )(
      partialResult = mergeAlgebra.empty,
      coalescence = NoCoalescence
    )
  end of

  trait MergeAlgebra[Result[_], Element]:
    def empty: Result[Element]
    def preservation(
        result: Result[Element],
        preservedBaseElement: Element,
        preservedElementOnLeft: Element,
        preservedElementOnRight: Element
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
        insertedElementOnLeft: Element,
        insertedElementOnRight: Element
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

    /** @note
      *   Edits will coalesce with following *insertions*, but each edited
      *   element from the base side retains its own edit, in contrast with a
      *   conflict.
      */
    def leftEdit(
        result: Result[Element],
        editedElement: Element,
        editElements: IndexedSeq[Element]
    ): Result[Element]

    /** @note
      *   Edits will coalesce with following *insertions*, but each edited
      *   element from the base side retains its own edit, in contrast with a
      *   conflict.
      */
    def rightEdit(
        result: Result[Element],
        editedElement: Element,
        editElements: IndexedSeq[Element]
    ): Result[Element]

    /** @note
      *   Edits will coalesce with following *insertions*, but each edited
      *   element from the base side retains its own edit, in contrast with a
      *   conflict.
      */
    def coincidentEdit(
        result: Result[Element],
        editedElement: Element,
        editElements: IndexedSeq[(Element, Element)]
    ): Result[Element]

    /** @note
      *   In contrast with edits, conflicts will coalesce on all three sides.
      *   This is intended for client code to assign edits on one side of a
      *   conflict to moved sections on the other side.<p></p>Edits have a
      *   simpler treatment of moved sections, thus they isolate their edited
      *   elements.
      */
    def conflict(
        result: Result[Element],
        // If this is empty, it represents an insertion conflict.
        editedElements: IndexedSeq[Element],
        // If this is empty, it represents a left-deletion rather than a
        // left-edit.
        leftEditElements: IndexedSeq[Element],
        // If this is empty, it represents a right-deletion rather than a
        // right-edit.
        rightEditElements: IndexedSeq[Element]
    ): Result[Element]
  end MergeAlgebra
end merge
