package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.Match.*
import com.sageserpent.kineticmerge.core.merge.MergeAlgebra
import monocle.syntax.all.*

object MergeResultDetectingMotion:
  type MergeResultDetectingMotionType[CoreResult[_]] =
    [Element] =>> MergeResultDetectingMotion[CoreResult, Element]

  def mergeAlgebra[CoreResult[_], Element](
      matchesFor: Element => collection.Set[Match[Element]],
      coreMergeAlgebra: merge.MergeAlgebra[CoreResult, Element]
  ): MergeAlgebra[MergeResultDetectingMotionType[CoreResult], Element] =
    type ConfiguredMergeResultDetectingMotion =
      MergeResultDetectingMotionType[CoreResult][Element]

    new MergeAlgebra[MergeResultDetectingMotionType[CoreResult], Element]:
      override def empty: ConfiguredMergeResultDetectingMotion =
        MergeResultDetectingMotion(
          coreMergeResult = coreMergeAlgebra.empty,
          changesPropagatedThroughMotion = Map.empty
        )

      override def preservation(
          result: ConfiguredMergeResultDetectingMotion,
          preservedElement: Element
      ): ConfiguredMergeResultDetectingMotion = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.preservation(_, preservedElement))

      override def leftInsertion(
          result: ConfiguredMergeResultDetectingMotion,
          insertedElement: Element
      ): ConfiguredMergeResultDetectingMotion = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.leftInsertion(_, insertedElement))

      override def rightInsertion(
          result: ConfiguredMergeResultDetectingMotion,
          insertedElement: Element
      ): ConfiguredMergeResultDetectingMotion = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.rightInsertion(_, insertedElement))

      override def coincidentInsertion(
          result: ConfiguredMergeResultDetectingMotion,
          insertedElement: Element
      ): ConfiguredMergeResultDetectingMotion = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.coincidentInsertion(_, insertedElement))

      override def leftDeletion(
          result: ConfiguredMergeResultDetectingMotion,
          deletedElement: Element
      ): ConfiguredMergeResultDetectingMotion = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.leftDeletion(_, deletedElement))

      override def rightDeletion(
          result: ConfiguredMergeResultDetectingMotion,
          deletedElement: Element
      ): ConfiguredMergeResultDetectingMotion = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.rightDeletion(_, deletedElement))

      override def coincidentDeletion(
          result: ConfiguredMergeResultDetectingMotion,
          deletedElement: Element
      ): ConfiguredMergeResultDetectingMotion =
        matchesFor(deletedElement).toSeq match
          case Seq(BaseAndLeft(_, leftElement)) =>
            result
              .focus(_.coreMergeResult)
              .modify(coreMergeAlgebra.coincidentDeletion(_, deletedElement))
              .focus(_.changesPropagatedThroughMotion)
              .modify(_ + (leftElement -> None))
          case Seq(BaseAndRight(_, rightElement)) =>
            result
              .focus(_.coreMergeResult)
              .modify(coreMergeAlgebra.coincidentDeletion(_, deletedElement))
              .focus(_.changesPropagatedThroughMotion)
              .modify(_ + (rightElement -> None))
          case Seq() =>
            result
              .focus(_.coreMergeResult)
              .modify(coreMergeAlgebra.coincidentDeletion(_, deletedElement))
          case _ =>
            // TODO: is this divergence? Perhaps we could do hit the core merge
            // algebra and propagate changes for each match?
            ???

      override def leftEdit(
          result: ConfiguredMergeResultDetectingMotion,
          editedElement: Element,
          editElements: IndexedSeq[Element]
      ): ConfiguredMergeResultDetectingMotion = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.leftEdit(_, editedElement, editElements))

      override def rightEdit(
          result: ConfiguredMergeResultDetectingMotion,
          editedElement: Element,
          editElements: IndexedSeq[Element]
      ): ConfiguredMergeResultDetectingMotion = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.rightEdit(_, editedElement, editElements))

      override def coincidentEdit(
          result: ConfiguredMergeResultDetectingMotion,
          editedElement: Element,
          editElements: IndexedSeq[Element]
      ): ConfiguredMergeResultDetectingMotion = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.coincidentEdit(_, editedElement, editElements))

      override def conflict(
          result: ConfiguredMergeResultDetectingMotion,
          editedElements: IndexedSeq[Element],
          leftEditElements: IndexedSeq[Element],
          rightEditElements: IndexedSeq[Element]
      ): ConfiguredMergeResultDetectingMotion = result
        .focus(_.coreMergeResult)
        .modify(
          coreMergeAlgebra
            .conflict(_, editedElements, leftEditElements, rightEditElements)
        )
    end new
  end mergeAlgebra
end MergeResultDetectingMotion

case class MergeResultDetectingMotion[CoreResult[_], Element](
    coreMergeResult: CoreResult[Element],
    // Use `Option[Element]` to model the difference between an edit and an
    // outright deletion.
    changesPropagatedThroughMotion: Map[Element, Option[Element]]
):
end MergeResultDetectingMotion
