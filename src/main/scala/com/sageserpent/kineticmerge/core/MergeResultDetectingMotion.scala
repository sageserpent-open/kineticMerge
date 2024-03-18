package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.Match.*
import com.sageserpent.kineticmerge.core.merge.MergeAlgebra
import com.typesafe.scalalogging.StrictLogging
import monocle.syntax.all.*

object MergeResultDetectingMotion extends StrictLogging:
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
          case Seq(
                AllSides(
                  _,
                  leftElementAtMoveDestination,
                  rightElementAtMoveDestination
                )
              ) =>
            logger.debug(
              s"Coincident deletion at origin of move: propagating deletion to left move destination $leftElementAtMoveDestination and right move destination $rightElementAtMoveDestination."
            )
            result
              .focus(_.coreMergeResult)
              .modify(coreMergeAlgebra.coincidentDeletion(_, deletedElement))
              .focus(_.changesPropagatedThroughMotion)
              .modify(
                _ + (leftElementAtMoveDestination -> None) + (rightElementAtMoveDestination -> None)
              )
          case Seq(BaseAndLeft(_, leftElementAtMoveDestination)) =>
            logger.debug(
              s"Coincident deletion at origin of move: propagating deletion to left move destination $leftElementAtMoveDestination."
            )
            result
              .focus(_.coreMergeResult)
              .modify(coreMergeAlgebra.coincidentDeletion(_, deletedElement))
              .focus(_.changesPropagatedThroughMotion)
              .modify(_ + (leftElementAtMoveDestination -> None))
          case Seq(BaseAndRight(_, rightElementAtMoveDestination)) =>
            logger.debug(
              s"Coincident deletion at origin of move: propagating deletion to right move destination $rightElementAtMoveDestination."
            )
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
      ): ConfiguredMergeResultDetectingMotion[Element] =
        def default = result
          .focus(_.coreMergeResult)
          .modify(
            coreMergeAlgebra.coincidentEdit(_, editedElement, editElements)
          )

        matchesFor(editedElement).toSeq match
          // NOTE: we're not missing the all-sides case below - the default
          // handles it perfectly well, as the left and right contributions to
          // the match are *incoming* moves, so there is nothing to propagate.
          case Seq(BaseAndLeft(_, leftElementAtMoveDestination)) =>
            logger.debug(
              s"Coincident edit at origin of move: propagating deletion to left move destination $leftElementAtMoveDestination."
            )
            default
              .focus(_.changesPropagatedThroughMotion)
              .modify(
                _ + (leftElementAtMoveDestination -> None)
              )
          case Seq(BaseAndRight(_, rightElementAtMoveDestination)) =>
            logger.debug(
              s"Coincident edit at origin of move: propagating deletion to right move destination $rightElementAtMoveDestination."
            )
            default
              .focus(_.changesPropagatedThroughMotion)
              .modify(
                _ + (rightElementAtMoveDestination -> None)
              )
          case _ =>
            default
        end match
      end coincidentEdit

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
              case Seq(
                    AllSides(
                      _,
                      _,
                      rightElementAtMoveDestination
                    )
                  ) =>
                logger.debug(
                  s"Conflict at origin of move: resolved as a coincident deletion of $baseElement and a left insertion of $leftElement; propagating left edit $leftElement to right move destination $rightElementAtMoveDestination."
                )
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
                    _ + (rightElementAtMoveDestination -> Some(leftElement))
                  )
              case Seq(BaseAndRight(_, rightElementAtMoveDestination)) =>
                logger.debug(
                  s"Conflict at origin of move: resolved as a coincident deletion of $baseElement; propagating left edit $leftElement to right move destination $rightElementAtMoveDestination."
                )
                result
                  .focus(_.coreMergeResult)
                  .modify(coreMergeAlgebra.coincidentDeletion(_, baseElement))
                  .focus(_.changesPropagatedThroughMotion)
                  .modify(
                    _ + (rightElementAtMoveDestination -> Some(leftElement))
                  )

              case Seq(BaseAndLeft(_, leftElementAtMoveDestination)) =>
                logger.debug(
                  s"Conflict at origin of move: resolved as a coincident deletion of $baseElement and a left insertion of $leftElement; propagating deletion to left move destination $leftElementAtMoveDestination."
                )
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
              case Seq(
                    AllSides(
                      _,
                      leftElementAtMoveDestination,
                      _
                    )
                  ) =>
                logger.debug(
                  s"Conflict at origin of move: resolved as a coincident deletion of $baseElement and a right insertion of $rightElement; propagating right edit $rightElement to left move destination $leftElementAtMoveDestination."
                )
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
                    _ + (leftElementAtMoveDestination -> Some(rightElement))
                  )
              case Seq(BaseAndLeft(_, leftElementAtMoveDestination)) =>
                logger.debug(
                  s"Conflict at origin of move: resolved as a coincident deletion of $baseElement; propagating right edit $rightElement to left move destination $leftElementAtMoveDestination."
                )
                result
                  .focus(_.coreMergeResult)
                  .modify(coreMergeAlgebra.coincidentDeletion(_, baseElement))
                  .focus(_.changesPropagatedThroughMotion)
                  .modify(
                    _ + (leftElementAtMoveDestination -> Some(rightElement))
                  )

              case Seq(BaseAndRight(_, rightElementAtMoveDestination)) =>
                logger.debug(
                  s"Conflict at origin of move: resolved as a coincident deletion of $baseElement and a right insertion of $rightElement; propagating deletion to right move destination $rightElementAtMoveDestination."
                )
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

          case (Seq(baseElement), Seq(leftElement), Seq(rightElement)) =>
            matchesFor(baseElement).toSeq match
              case Seq(
                    AllSides(
                      _,
                      leftElementAtMoveDestination,
                      rightElementAtMoveDestination
                    )
                  ) =>
                logger.debug(
                  s"Conflict at origin of move: propagating right edit $rightElement to left move destination $leftElementAtMoveDestination and left edit $leftElement to right move destination $rightElementAtMoveDestination."
                )
                default
                  .focus(_.changesPropagatedThroughMotion)
                  .modify(
                    _ + (leftElementAtMoveDestination -> Some(rightElement))
                  )
                  .focus(_.changesPropagatedThroughMotion)
                  .modify(
                    _ + (rightElementAtMoveDestination -> Some(leftElement))
                  )
              case Seq(BaseAndLeft(_, leftElementAtMoveDestination)) =>
                logger.debug(
                  s"Conflict at origin of move: resolved as a coincident deletion of $baseElement and a left insertion of $leftElement; propagating right edit $rightElement to left move destination $leftElementAtMoveDestination."
                )
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
                    _ + (leftElementAtMoveDestination -> Some(rightElement))
                  )

              case Seq(BaseAndRight(_, rightElementAtMoveDestination)) =>
                logger.debug(
                  s"Conflict at origin of move: resolved as a coincident deletion of $baseElement and a right insertion of $rightElement; propagating left edit $leftElement to right move destination $rightElementAtMoveDestination."
                )
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
                    _ + (rightElementAtMoveDestination -> Some(leftElement))
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
