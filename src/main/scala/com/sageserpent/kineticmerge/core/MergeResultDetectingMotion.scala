package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.Match.*
import com.sageserpent.kineticmerge.core.merge.MergeAlgebra
import monocle.syntax.all.*

object MergeResultDetectingMotion:
  private type MergeResultDetectingMotionType[CoreResult[_]] =
    [Element] =>> MergeResultDetectingMotion[CoreResult, Element]

  def mergeAlgebra[CoreResult[_], Element](
      matchesFor: Element => collection.Set[Match[Element]],
      coreMergeAlgebra: merge.MergeAlgebra[CoreResult, Element]
  ): MergeAlgebra[MergeResultDetectingMotionType[CoreResult], Element] =
    type ConfiguredMergeResultDetectingMotion[Element] =
      MergeResultDetectingMotionType[CoreResult][Element]

    new MergeAlgebra[MergeResultDetectingMotionType[CoreResult], Element]:
      override def empty: ConfiguredMergeResultDetectingMotion[Element] =
        MergeResultDetectingMotion(
          coreMergeResult = coreMergeAlgebra.empty,
          changesPropagatedThroughMotion = Map.empty
        )

      override def preservation(
          result: ConfiguredMergeResultDetectingMotion[Element],
          preservedElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.preservation(_, preservedElement))

      override def leftInsertion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          insertedElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.leftInsertion(_, insertedElement))

      override def rightInsertion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          insertedElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.rightInsertion(_, insertedElement))

      override def coincidentInsertion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          insertedElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.coincidentInsertion(_, insertedElement))

      override def leftDeletion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          deletedElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.leftDeletion(_, deletedElement))

      override def rightDeletion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          deletedElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.rightDeletion(_, deletedElement))

      override def coincidentDeletion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          deletedElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] =
        matchesFor(deletedElement).toSeq match
          case Seq(BaseAndLeft(_, leftElementAtMoveDestination)) =>
            result
              .focus(_.coreMergeResult)
              .modify(coreMergeAlgebra.coincidentDeletion(_, deletedElement))
              .focus(_.changesPropagatedThroughMotion)
              .modify(_ + (leftElementAtMoveDestination -> None))
          case Seq(BaseAndRight(_, rightElementAtMoveDestination)) =>
            result
              .focus(_.coreMergeResult)
              .modify(coreMergeAlgebra.coincidentDeletion(_, deletedElement))
              .focus(_.changesPropagatedThroughMotion)
              .modify(_ + (rightElementAtMoveDestination -> None))
          case Seq() =>
            result
              .focus(_.coreMergeResult)
              .modify(coreMergeAlgebra.coincidentDeletion(_, deletedElement))
          case _ =>
            // TODO: is this divergence? Perhaps we could do hit the core merge
            // algebra and propagate changes for each match?
            ???

      override def leftEdit(
          result: ConfiguredMergeResultDetectingMotion[Element],
          editedElement: Element,
          editElements: IndexedSeq[Element]
      ): ConfiguredMergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.leftEdit(_, editedElement, editElements))

      override def rightEdit(
          result: ConfiguredMergeResultDetectingMotion[Element],
          editedElement: Element,
          editElements: IndexedSeq[Element]
      ): ConfiguredMergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.rightEdit(_, editedElement, editElements))

      override def coincidentEdit(
          result: ConfiguredMergeResultDetectingMotion[Element],
          editedElement: Element,
          editElements: IndexedSeq[Element]
      ): ConfiguredMergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(coreMergeAlgebra.coincidentEdit(_, editedElement, editElements))

      override def conflict(
          result: ConfiguredMergeResultDetectingMotion[Element],
          editedElements: IndexedSeq[Element],
          leftEditElements: IndexedSeq[Element],
          rightEditElements: IndexedSeq[Element]
      ): ConfiguredMergeResultDetectingMotion[Element] =
        def default =
          result
            .focus(_.coreMergeResult)
            .modify(
              coreMergeAlgebra.conflict(
                _,
                editedElements,
                leftEditElements,
                rightEditElements
              )
            )

        (editedElements, leftEditElements, rightEditElements) match
          case (Seq(baseElement), Seq(leftElement), Seq()) =>
            matchesFor(baseElement).toSeq match
              case Seq(BaseAndRight(_, rightElementAtMoveDestination)) =>
                result
                  .focus(_.coreMergeResult)
                  .modify(coreMergeAlgebra.coincidentDeletion(_, baseElement))
                  .focus(_.changesPropagatedThroughMotion)
                  .modify(
                    _ + (rightElementAtMoveDestination -> Some(leftElement))
                  )

              case Seq(BaseAndLeft(_, leftElementAtMoveDestination)) =>
                result
                  .focus(_.coreMergeResult)
                  .modify(
                    coreMergeAlgebra
                      .coincidentDeletion(_, baseElement)
                  )
                  .focus(_.coreMergeResult)
                  .modify(
                    coreMergeAlgebra
                      .leftInsertion(_, leftElement)
                  )
                  .focus(_.changesPropagatedThroughMotion)
                  .modify(
                    _ + (leftElementAtMoveDestination -> None)
                  )

              case _ => default

          case (Seq(baseElement), Seq(), Seq(rightElement)) =>
            matchesFor(baseElement).toSeq match
              case Seq(BaseAndLeft(_, leftElementAtMoveDestination)) =>
                result
                  .focus(_.coreMergeResult)
                  .modify(coreMergeAlgebra.coincidentDeletion(_, baseElement))
                  .focus(_.changesPropagatedThroughMotion)
                  .modify(
                    _ + (leftElementAtMoveDestination -> Some(rightElement))
                  )

              case Seq(BaseAndRight(_, rightElementAtMoveDestination)) =>
                result
                  .focus(_.coreMergeResult)
                  .modify(
                    coreMergeAlgebra
                      .coincidentDeletion(_, baseElement)
                  )
                  .focus(_.coreMergeResult)
                  .modify(
                    coreMergeAlgebra
                      .rightInsertion(_, rightElement)
                  )
                  .focus(_.changesPropagatedThroughMotion)
                  .modify(
                    _ + (rightElementAtMoveDestination -> None)
                  )

              case _ => default
          case _ =>
            default
        end match
      end conflict
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
