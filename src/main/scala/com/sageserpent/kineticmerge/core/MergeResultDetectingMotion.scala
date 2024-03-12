package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.merge.MergeAlgebra
import monocle.syntax.all.*

object MergeResultDetectingMotion:
  def mergeAlgebra[Element](
      matchesFor: Element => collection.Set[Match[Element]]
  ): MergeAlgebra[MergeResultDetectingMotion, Element] =
    new MergeAlgebra[MergeResultDetectingMotion, Element]:
      private val coreMergeAlgebra = MergeResult.mergeAlgebra[Element]

      override def empty: MergeResultDetectingMotion[Element] =
        MergeResultDetectingMotion(
          coreMergeResult = coreMergeAlgebra.empty,
          changesPropagatedThroughMotion = Map.empty
        )

      override def preservation(
          result: MergeResultDetectingMotion[Element],
          preservedElement: Element
      ): MergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.preservation(_, preservedElement))

      override def leftInsertion(
          result: MergeResultDetectingMotion[Element],
          insertedElement: Element
      ): MergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.leftInsertion(_, insertedElement))

      override def rightInsertion(
          result: MergeResultDetectingMotion[Element],
          insertedElement: Element
      ): MergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.rightInsertion(_, insertedElement))

      override def coincidentInsertion(
          result: MergeResultDetectingMotion[Element],
          insertedElement: Element
      ): MergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.coincidentInsertion(_, insertedElement))

      override def leftDeletion(
          result: MergeResultDetectingMotion[Element],
          deletedElement: Element
      ): MergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.leftDeletion(_, deletedElement))

      override def rightDeletion(
          result: MergeResultDetectingMotion[Element],
          deletedElement: Element
      ): MergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.rightDeletion(_, deletedElement))

      override def coincidentDeletion(
          result: MergeResultDetectingMotion[Element],
          deletedElement: Element
      ): MergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.coincidentDeletion(_, deletedElement))

      override def leftEdit(
          result: MergeResultDetectingMotion[Element],
          editedElement: Element,
          editElements: IndexedSeq[Element]
      ): MergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.leftEdit(_, editedElement, editElements))

      override def rightEdit(
          result: MergeResultDetectingMotion[Element],
          editedElement: Element,
          editElements: IndexedSeq[Element]
      ): MergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.rightEdit(_, editedElement, editElements))

      override def coincidentEdit(
          result: MergeResultDetectingMotion[Element],
          editedElement: Element,
          editElements: IndexedSeq[Element]
      ): MergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.coincidentEdit(_, editedElement, editElements))

      override def conflict(
          result: MergeResultDetectingMotion[Element],
          editedElements: IndexedSeq[Element],
          leftEditElements: IndexedSeq[Element],
          rightEditElements: IndexedSeq[Element]
      ): MergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(
          coreMergeAlgebra
            .conflict(_, editedElements, leftEditElements, rightEditElements)
        )
end MergeResultDetectingMotion

case class MergeResultDetectingMotion[Element](
    coreMergeResult: MergeResult[Element],
    // Use `Option[Element]` to model the difference between an edit and an
    // outright deletion.
    changesPropagatedThroughMotion: Map[Element, Option[Element]]
):
end MergeResultDetectingMotion
