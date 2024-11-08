package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.merge.MergeAlgebra
import com.typesafe.scalalogging.StrictLogging
import monocle.syntax.all.*

/** @param coreMergeResult
  *   What is says on the tin: a simpler merge result that is delegated to by
  *   the operations implemented in
  *   [[MatchesContext.MergeResultDetectingMotion.mergeAlgebra]].
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
case class MergeResultDetectingMotion[CoreResult[_], Element](
    coreMergeResult: CoreResult[Element],
    speculativeMigrationsBySource: Map[Element, ContentMigration[
      Element
    ]],
    speculativeMoveDestinations: Set[SpeculativeMoveDestination[Element]],
    insertions: Seq[Insertion[Element]],
    oneSidedDeletions: Set[Element]
)

enum Side:
  case Left
  case Right
end Side

case class Insertion[Element](side: Side, inserted: Element)

object MergeResultDetectingMotion extends StrictLogging:
  private type MergeResultDetectingMotionType[CoreResult[_]] =
    [Element] =>> MergeResultDetectingMotion[CoreResult, Element]

  def mergeAlgebra[CoreResult[_], Element](
      coreMergeAlgebra: merge.MergeAlgebra[CoreResult, Element]
  ): MergeAlgebra[MergeResultDetectingMotionType[CoreResult], Element] =
    type ConfiguredMergeResultDetectingMotion[Element] =
      MergeResultDetectingMotionType[CoreResult][Element]

    new MergeAlgebra[MergeResultDetectingMotionType[CoreResult], Element]:
      override def empty: ConfiguredMergeResultDetectingMotion[Element] =
        MergeResultDetectingMotion(
          coreMergeResult = coreMergeAlgebra.empty,
          speculativeMigrationsBySource = Map.empty,
          speculativeMoveDestinations = Set.empty,
          insertions = Vector.empty,
          oneSidedDeletions = Set.empty
        )

      override def preservation(
          result: MergeResultDetectingMotionType[CoreResult][Element],
          preservedBaseElement: Element,
          preservedElementOnLeft: Element,
          preservedElementOnRight: Element
      ): ConfiguredMergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify((result: CoreResult[Element]) =>
          coreMergeAlgebra.preservation(
            result,
            preservedBaseElement,
            preservedElementOnLeft,
            preservedElementOnRight
          )
        )

      override def leftInsertion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          insertedElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.insertions)
          .modify(_ :+ Insertion(Side.Left, insertedElement))
          .focus(_.coreMergeResult)
          .modify(coreMergeAlgebra.leftInsertion(_, insertedElement))
          .focus(_.speculativeMoveDestinations)
          .modify(_ + SpeculativeMoveDestination.Left(insertedElement))
      end leftInsertion

      override def rightInsertion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          insertedElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.insertions)
          .modify(_ :+ Insertion(Side.Right, insertedElement))
          .focus(_.coreMergeResult)
          .modify(coreMergeAlgebra.rightInsertion(_, insertedElement))
          .focus(_.speculativeMoveDestinations)
          .modify(_ + SpeculativeMoveDestination.Right(insertedElement))

      override def coincidentInsertion(
          result: MergeResultDetectingMotionType[CoreResult][Element],
          insertedElementOnLeft: Element,
          insertedElementOnRight: Element
      ): ConfiguredMergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify((result: CoreResult[Element]) =>
          coreMergeAlgebra.coincidentInsertion(
            result,
            insertedElementOnLeft,
            insertedElementOnRight
          )
        )
        .focus(_.speculativeMoveDestinations)
        .modify(
          _ + SpeculativeMoveDestination.Coincident(
            insertedElementOnLeft -> insertedElementOnRight
          )
        )

      override def leftDeletion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          deletedBaseElement: Element,
          deletedRightElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.coreMergeResult)
          .modify(
            coreMergeAlgebra
              .leftDeletion(_, deletedBaseElement, deletedRightElement)
          )
          .focus(_.oneSidedDeletions)
          .modify(_ + deletedRightElement)
          .focus(_.speculativeMigrationsBySource)
          .modify(
            _ + (deletedBaseElement -> ContentMigration
              .PlainMove(deletedRightElement))
          )
      end leftDeletion

      override def rightDeletion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          deletedBaseElement: Element,
          deletedLeftElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.coreMergeResult)
          .modify(
            coreMergeAlgebra
              .rightDeletion(_, deletedBaseElement, deletedLeftElement)
          )
          .focus(_.oneSidedDeletions)
          .modify(_ + deletedLeftElement)
          .focus(_.speculativeMigrationsBySource)
          .modify(
            _ + (deletedBaseElement -> ContentMigration
              .PlainMove(deletedLeftElement))
          )
      end rightDeletion

      override def coincidentDeletion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          deletedElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.coreMergeResult)
          .modify(coreMergeAlgebra.coincidentDeletion(_, deletedElement))
          .focus(_.speculativeMigrationsBySource)
          .modify(
            _ + (deletedElement -> ContentMigration.Deletion())
          )
      end coincidentDeletion

      override def leftEdit(
          result: ConfiguredMergeResultDetectingMotion[Element],
          editedBaseElement: Element,
          editedRightElement: Element,
          editElements: IndexedSeq[Element]
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.coreMergeResult)
          .modify(
            coreMergeAlgebra
              .leftEdit(
                _,
                editedBaseElement,
                editedRightElement,
                editElements
              )
          )
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
      end leftEdit

      override def rightEdit(
          result: ConfiguredMergeResultDetectingMotion[Element],
          editedBaseElement: Element,
          editedLeftElement: Element,
          editElements: IndexedSeq[Element]
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.coreMergeResult)
          .modify(
            coreMergeAlgebra
              .rightEdit(
                _,
                editedBaseElement,
                editedLeftElement,
                editElements
              )
          )
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
      end rightEdit

      override def coincidentEdit(
          result: ConfiguredMergeResultDetectingMotion[Element],
          editedElement: Element,
          editElements: IndexedSeq[(Element, Element)]
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.coreMergeResult)
          .modify(
            coreMergeAlgebra.coincidentEdit(_, editedElement, editElements)
          )
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
      end coincidentEdit

      override def conflict(
          result: ConfiguredMergeResultDetectingMotion[Element],
          editedElements: IndexedSeq[Element],
          leftEditElements: IndexedSeq[Element],
          rightEditElements: IndexedSeq[Element]
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.insertions)
          .modify(leftEditElements.foldLeft(_)(_ :+ Insertion(Side.Left, _)))
          .focus(_.insertions)
          .modify(rightEditElements.foldLeft(_)(_ :+ Insertion(Side.Right, _)))
          .focus(_.coreMergeResult)
          .modify(
            coreMergeAlgebra.conflict(
              _,
              editedElements,
              leftEditElements,
              rightEditElements
            )
          )
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

      end conflict
    end new
  end mergeAlgebra
end MergeResultDetectingMotion
