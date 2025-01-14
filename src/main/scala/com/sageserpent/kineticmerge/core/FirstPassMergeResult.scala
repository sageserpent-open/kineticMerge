package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.FirstPassMergeResult.Recording
import com.sageserpent.kineticmerge.core.merge.MergeAlgebra
import monocle.syntax.all.*

/** @param recording
  *   The sequence of operations originally applied to the merge algebra from
  *   [[FirstPassMergeResult.mergeAlgebra]] used to create the merge result.
  *   Used to play back the same operations on a second pass merge result.
  * @param speculativeMigrationsBySource
  *   Migrations keyed by source element - the source element is taken from the
  *   <b>base</b> side.
  * @param speculativeMoveDestinations
  * @param insertions
  *   Insertions that may need to be migrated, these have to be collected
  *   speculatively upfront and then associated with anchors once the global
  *   picture of code motion is available.
  * @param oneSidedDeletionsFromBase
  *   Deleted elements from the base, where the deletion is on just one of the
  *   left or right side.
  * @param oneSidedDeletionsFromOppositeSide
  *   Deleted elements from the side opposite to the deletion.
  * @tparam CoreResult
  * @tparam Element
  * @note
  *   A move source refers to only one migration, although it is quite possible
  *   for a single source to be part of multiple movements on either or both the
  *   left and right side. It is also possible for multiple move sources (and
  *   thus migrations) to converge on the same move destination.
  */
case class FirstPassMergeResult[Element](
    recording: Recording[Element],
    speculativeMigrationsBySource: Map[Element, SpeculativeContentMigration[
      Element
    ]],
    speculativeMoveDestinations: Set[SpeculativeMoveDestination[Element]],
    basePreservations: Set[Element],
    leftPreservations: Set[Element],
    rightPreservations: Set[Element]
)

enum Side:
  case Left
  case Right
end Side

object FirstPassMergeResult:
  opaque type Recording[Element] =
    Vector[[Result[_]] => MergeAlgebra[Result, Element] => Result[
      Element
    ] => Result[Element]]

  extension [Element](recording: Recording[Element])
    def playback[Result[_]](
        mergeAlgebra: MergeAlgebra[Result, Element]
    ): Result[Element] =
      recording.foldLeft(mergeAlgebra.empty)((partialResult, step) =>
        step(mergeAlgebra)(partialResult)
      )
    end playback
  end extension

  def mergeAlgebra[Element](
      inContextOfFileDeletion: Boolean
  ): MergeAlgebra[FirstPassMergeResult, Element] =
    new MergeAlgebra[FirstPassMergeResult, Element]:
      override def empty: FirstPassMergeResult[Element] =
        FirstPassMergeResult(
          recording = Vector.empty,
          speculativeMigrationsBySource = Map.empty,
          speculativeMoveDestinations = Set.empty,
          basePreservations = Set.empty,
          rightPreservations = Set.empty,
          leftPreservations = Set.empty
        )

      override def preservation(
          result: FirstPassMergeResult[Element],
          preservedBaseElement: Element,
          preservedElementOnLeft: Element,
          preservedElementOnRight: Element
      ): FirstPassMergeResult[Element] = result
        .focus(_.recording)
        .modify(
          _.appended(
            [Result[_]] =>
              (mergeAlgebra: MergeAlgebra[Result, Element]) =>
                mergeAlgebra.preservation(
                  _,
                  preservedBaseElement,
                  preservedElementOnLeft,
                  preservedElementOnRight
              )
          )
        )
        .focus(_.basePreservations)
        .modify(_ + preservedBaseElement)
        .focus(_.leftPreservations)
        .modify(_ + preservedElementOnLeft)
        .focus(_.rightPreservations)
        .modify(_ + preservedElementOnRight)
      end preservation

      override def leftInsertion(
          result: FirstPassMergeResult[Element],
          insertedElement: Element
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.recording)
          .modify(
            _.appended(
              [Result[_]] =>
                (mergeAlgebra: MergeAlgebra[Result, Element]) =>
                  mergeAlgebra.leftInsertion(
                    _,
                    insertedElement
                )
            )
          )
          .focus(_.speculativeMoveDestinations)
          .modify(_ + SpeculativeMoveDestination.Left(insertedElement))
      end leftInsertion

      override def rightInsertion(
          result: FirstPassMergeResult[Element],
          insertedElement: Element
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.recording)
          .modify(
            _.appended(
              [Result[_]] =>
                (mergeAlgebra: MergeAlgebra[Result, Element]) =>
                  mergeAlgebra.rightInsertion(
                    _,
                    insertedElement
                )
            )
          )
          .focus(_.speculativeMoveDestinations)
          .modify(_ + SpeculativeMoveDestination.Right(insertedElement))
      end rightInsertion

      override def coincidentInsertion(
          result: FirstPassMergeResult[Element],
          insertedElementOnLeft: Element,
          insertedElementOnRight: Element
      ): FirstPassMergeResult[Element] = result
        .focus(_.recording)
        .modify(
          _.appended(
            [Result[_]] =>
              (mergeAlgebra: MergeAlgebra[Result, Element]) =>
                mergeAlgebra.coincidentInsertion(
                  _,
                  insertedElementOnLeft,
                  insertedElementOnRight
              )
          )
        )
        .focus(_.speculativeMoveDestinations)
        .modify(
          _ + SpeculativeMoveDestination.Coincident(
            insertedElementOnLeft -> insertedElementOnRight
          )
        )
      end coincidentInsertion

      override def leftDeletion(
          result: FirstPassMergeResult[Element],
          deletedBaseElement: Element,
          deletedRightElement: Element
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.recording)
          .modify(
            _.appended(
              [Result[_]] =>
                (mergeAlgebra: MergeAlgebra[Result, Element]) =>
                  mergeAlgebra.leftDeletion(
                    _,
                    deletedBaseElement,
                    deletedRightElement
                )
            )
          )
          .focus(_.speculativeMigrationsBySource)
          .modify(
            _ + (deletedBaseElement -> SpeculativeContentMigration
              .LeftEditOrDeletion(
                opposingRightElement = deletedRightElement,
                inContextOfFileDeletion
              ))
          )
      end leftDeletion

      override def rightDeletion(
          result: FirstPassMergeResult[Element],
          deletedBaseElement: Element,
          deletedLeftElement: Element
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.recording)
          .modify(
            _.appended(
              [Result[_]] =>
                (mergeAlgebra: MergeAlgebra[Result, Element]) =>
                  mergeAlgebra.rightDeletion(
                    _,
                    deletedBaseElement,
                    deletedLeftElement
                )
            )
          )
          .focus(_.speculativeMigrationsBySource)
          .modify(
            _ + (deletedBaseElement -> SpeculativeContentMigration
              .RightEditOrDeletion(
                opposingLeftElement = deletedLeftElement,
                inContextOfFileDeletion
              ))
          )
      end rightDeletion

      override def coincidentDeletion(
          result: FirstPassMergeResult[Element],
          deletedElement: Element
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.recording)
          .modify(
            _.appended(
              [Result[_]] =>
                (mergeAlgebra: MergeAlgebra[Result, Element]) =>
                  mergeAlgebra.coincidentDeletion(
                    _,
                    deletedElement
                )
            )
          )
          .focus(_.speculativeMigrationsBySource)
          .modify(
            _ + (deletedElement -> SpeculativeContentMigration
              .CoincidentEditOrDeletion())
          )
      end coincidentDeletion

      override def leftEdit(
          result: FirstPassMergeResult[Element],
          editedBaseElement: Element,
          editedRightElement: Element,
          editElements: IndexedSeq[Element]
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.recording)
          .modify(
            _.appended(
              [Result[_]] =>
                (mergeAlgebra: MergeAlgebra[Result, Element]) =>
                  mergeAlgebra.leftEdit(
                    _,
                    editedBaseElement,
                    editedRightElement,
                    editElements
                )
            )
          )
          .focus(_.speculativeMigrationsBySource)
          .modify(
            _ + (editedBaseElement -> SpeculativeContentMigration
              .LeftEditOrDeletion(
                opposingRightElement = editedRightElement,
                inContextOfFileDeletion = false
              ))
          )
          .focus(_.speculativeMoveDestinations)
          .modify(
            editElements.foldLeft(_)(_ + SpeculativeMoveDestination.Left(_))
          )
      end leftEdit

      override def rightEdit(
          result: FirstPassMergeResult[Element],
          editedBaseElement: Element,
          editedLeftElement: Element,
          editElements: IndexedSeq[Element]
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.recording)
          .modify(
            _.appended(
              [Result[_]] =>
                (mergeAlgebra: MergeAlgebra[Result, Element]) =>
                  mergeAlgebra.rightEdit(
                    _,
                    editedBaseElement,
                    editedLeftElement,
                    editElements
                )
            )
          )
          .focus(_.speculativeMigrationsBySource)
          .modify(
            _ + (editedBaseElement -> SpeculativeContentMigration
              .RightEditOrDeletion(
                opposingLeftElement = editedLeftElement,
                inContextOfFileDeletion = false
              ))
          )
          .focus(_.speculativeMoveDestinations)
          .modify(
            editElements.foldLeft(_)(_ + SpeculativeMoveDestination.Right(_))
          )
      end rightEdit

      override def coincidentEdit(
          result: FirstPassMergeResult[Element],
          editedElement: Element,
          editElements: IndexedSeq[(Element, Element)]
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.recording)
          .modify(
            _.appended(
              [Result[_]] =>
                (mergeAlgebra: MergeAlgebra[Result, Element]) =>
                  mergeAlgebra.coincidentEdit(
                    _,
                    editedElement,
                    editElements
                )
            )
          )
          .focus(_.speculativeMigrationsBySource)
          // NOTE: the edit elements are *never* taken as content to be
          // migrated, because they have arrived at the same place on both
          // sides; we don't break up the paired content (and indeed regard it
          // as a potential coincident move destination).
          .modify(
            _ + (editedElement -> SpeculativeContentMigration
              .CoincidentEditOrDeletion())
          )
          .focus(_.speculativeMoveDestinations)
          .modify(
            editElements.foldLeft(_)(
              _ + SpeculativeMoveDestination.Coincident(_)
            )
          )
      end coincidentEdit

      override def conflict(
          result: FirstPassMergeResult[Element],
          editedElements: IndexedSeq[Element],
          leftEditElements: IndexedSeq[Element],
          rightEditElements: IndexedSeq[Element]
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.recording)
          .modify(
            _.appended(
              [Result[_]] =>
                (mergeAlgebra: MergeAlgebra[Result, Element]) =>
                  mergeAlgebra.conflict(
                    _,
                    editedElements,
                    leftEditElements,
                    rightEditElements
                )
            )
          )
          .focus(_.speculativeMigrationsBySource)
          .modify(
            editedElements.foldLeft(_)((partialResult, editedElement) =>
              partialResult + (editedElement -> SpeculativeContentMigration
                .Conflict(
                  leftEditElements,
                  rightEditElements,
                  inContextOfFileDeletion
                ))
            )
          )
          .focus(_.speculativeMoveDestinations)
          .modify(
            leftEditElements.foldLeft(_)(
              _ + SpeculativeMoveDestination.Left(_)
            )
          )
          .focus(_.speculativeMoveDestinations)
          .modify(
            rightEditElements.foldLeft(_)(
              _ + SpeculativeMoveDestination.Right(_)
            )
          )
      end conflict
    end new
  end mergeAlgebra
end FirstPassMergeResult
