package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.FirstPassMergeResult.Recording
import com.sageserpent.kineticmerge.core.merge.MergeAlgebra
import com.typesafe.scalalogging.StrictLogging
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
  * @param oneSidedDeletions
  *   Deletions on just one side that may influence the discovery of anchors for
  *   insertion migration.
  * @tparam CoreResult
  * @tparam Element
  * @note
  *   A move source refers to only one migration, although it is quite possible
  *   for a single source to be part of multiple movements on either or both the
  *   left and right side. It is also possible for multiple move sources (and
  *   thus migrations) to converge on the same move destination.
  */
case class FirstPassMergeResult[Element](
    speculativeMigrationsBySource: Map[Element, ContentMigration[
      Element
    ]],
    speculativeMoveDestinations: Set[SpeculativeMoveDestination[Element]],
    insertions: Seq[Insertion[Element]],
    oneSidedDeletions: Set[Element],
    recording: Recording[Element]
)

enum Side:
  case Left
  case Right
end Side

case class Insertion[Element](side: Side, inserted: Element)

object FirstPassMergeResult extends StrictLogging:
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

  def mergeAlgebra[Element](): MergeAlgebra[FirstPassMergeResult, Element] =
    new MergeAlgebra[FirstPassMergeResult, Element]:
      override def empty: FirstPassMergeResult[Element] =
        FirstPassMergeResult(
          speculativeMigrationsBySource = Map.empty,
          speculativeMoveDestinations = Set.empty,
          insertions = Vector.empty,
          oneSidedDeletions = Set.empty,
          recording = Vector.empty
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

      override def leftInsertion(
          result: FirstPassMergeResult[Element],
          insertedElement: Element
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.insertions)
          .modify(_ :+ Insertion(Side.Left, insertedElement))
          .focus(_.speculativeMoveDestinations)
          .modify(_ + SpeculativeMoveDestination.Left(insertedElement))
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
      end leftInsertion

      override def rightInsertion(
          result: FirstPassMergeResult[Element],
          insertedElement: Element
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.insertions)
          .modify(_ :+ Insertion(Side.Right, insertedElement))
          .focus(_.speculativeMoveDestinations)
          .modify(_ + SpeculativeMoveDestination.Right(insertedElement))
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

      override def coincidentInsertion(
          result: FirstPassMergeResult[Element],
          insertedElementOnLeft: Element,
          insertedElementOnRight: Element
      ): FirstPassMergeResult[Element] = result
        .focus(_.speculativeMoveDestinations)
        .modify(
          _ + SpeculativeMoveDestination.Coincident(
            insertedElementOnLeft -> insertedElementOnRight
          )
        )
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

      override def leftDeletion(
          result: FirstPassMergeResult[Element],
          deletedBaseElement: Element,
          deletedRightElement: Element
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.oneSidedDeletions)
          .modify(_ + deletedRightElement)
          .focus(_.speculativeMigrationsBySource)
          .modify(
            _ + (deletedBaseElement -> ContentMigration
              .PlainMove(deletedRightElement))
          )
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
      end leftDeletion

      override def rightDeletion(
          result: FirstPassMergeResult[Element],
          deletedBaseElement: Element,
          deletedLeftElement: Element
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.oneSidedDeletions)
          .modify(_ + deletedLeftElement)
          .focus(_.speculativeMigrationsBySource)
          .modify(
            _ + (deletedBaseElement -> ContentMigration
              .PlainMove(deletedLeftElement))
          )
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
      end rightDeletion

      override def coincidentDeletion(
          result: FirstPassMergeResult[Element],
          deletedElement: Element
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.speculativeMigrationsBySource)
          .modify(
            _ + (deletedElement -> ContentMigration.Deletion())
          )
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
      end coincidentDeletion

      override def leftEdit(
          result: FirstPassMergeResult[Element],
          editedBaseElement: Element,
          editedRightElement: Element,
          editElements: IndexedSeq[Element]
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.speculativeMigrationsBySource)
          .modify(
            _ + (editedBaseElement -> ContentMigration.PlainMove(
              editedRightElement
            ))
          )
          .focus(_.speculativeMoveDestinations)
          .modify(
            editElements.foldLeft(_)(_ + SpeculativeMoveDestination.Left(_))
          )
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
      end leftEdit

      override def rightEdit(
          result: FirstPassMergeResult[Element],
          editedBaseElement: Element,
          editedLeftElement: Element,
          editElements: IndexedSeq[Element]
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.speculativeMigrationsBySource)
          .modify(
            _ + (editedBaseElement -> ContentMigration.PlainMove(
              editedLeftElement
            ))
          )
          .focus(_.speculativeMoveDestinations)
          .modify(
            editElements.foldLeft(_)(_ + SpeculativeMoveDestination.Right(_))
          )
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
      end rightEdit

      override def coincidentEdit(
          result: FirstPassMergeResult[Element],
          editedElement: Element,
          editElements: IndexedSeq[(Element, Element)]
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.speculativeMigrationsBySource)
          // NOTE: the edit elements are *never* taken as content to be
          // migrated, because they have arrived at the same place on both
          // sides; we don't break up the paired content (and indeed regard it
          // as a potential coincident move destination).
          .modify(
            _ + (editedElement -> ContentMigration.Deletion())
          )
          .focus(_.speculativeMoveDestinations)
          .modify(
            editElements.foldLeft(_)(
              _ + SpeculativeMoveDestination.Coincident(_)
            )
          )
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
      end coincidentEdit

      override def conflict(
          result: FirstPassMergeResult[Element],
          editedElements: IndexedSeq[Element],
          leftEditElements: IndexedSeq[Element],
          rightEditElements: IndexedSeq[Element]
      ): FirstPassMergeResult[Element] =
        result
          .focus(_.insertions)
          .modify(leftEditElements.foldLeft(_)(_ :+ Insertion(Side.Left, _)))
          .focus(_.insertions)
          .modify(rightEditElements.foldLeft(_)(_ :+ Insertion(Side.Right, _)))
          .focus(_.speculativeMigrationsBySource)
          .modify(
            editedElements.foldLeft(_)((partialResult, editedElement) =>
              partialResult + (editedElement -> ContentMigration
                .Edit(leftEditElements, rightEditElements))
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
      end conflict
    end new
  end mergeAlgebra
end FirstPassMergeResult
