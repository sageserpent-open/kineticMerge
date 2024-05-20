package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.Match.*
import com.sageserpent.kineticmerge.core.merge.MergeAlgebra
import com.sageserpent.kineticmerge.mergeByKeyWith
import com.typesafe.scalalogging.StrictLogging
import monocle.syntax.all.*

import scala.collection.immutable.MultiDict

class MatchesContext[Element](
    matchesFor: Element => collection.Set[Match[Element]]
):
  val emptyReport: MoveDestinationsReport = MoveDestinationsReport(Map.empty)

  protected def adjustmentOfEditByFollowingCoincidentDeletion(
      editElement: Element,
      deletedElement: Element
  ): Element = editElement

  private def dominantsOf(element: Element): collection.Set[Element] =
    matchesFor(element).map(_.dominantElement)

  private def sourcesOf(element: Element): collection.Set[Element] =
    matchesFor(element).flatMap {
      case BaseAndLeft(baseElement, _)  => Some(baseElement)
      case BaseAndRight(baseElement, _) => Some(baseElement)
      case LeftAndRight(_, _)           => None
      case AllSides(baseElement, _, _)  => Some(baseElement)
    }

  case class MergeResultDetectingMotion[CoreResult[_], Element](
      coreMergeResult: CoreResult[Element],
      // Use `Option[Element]` to model the difference between an edit and an
      // outright deletion.
      changesPropagatedThroughMotion: MultiDict[Element, Option[Element]],
      editsPropagatedThroughMotionSubjectToAdjustment: Seq[(Element, Element)],
      moveDestinationsReport: MoveDestinationsReport,
      insertions: Set[Insertion]
  )

  enum Side:
    case Left
    case Right
  end Side

  case class Insertion(inserted: Element, side: Side)

  case class MoveDestinationsReport(
      moveDestinationsByDominantSet: Map[collection.Set[
        Element
      ], MoveDestinations[Element]]
  ):
    def leftMoveOf(
        element: Element
    ): MoveDestinationsReport =
      val dominants = dominantsOf(element)

      if dominants.nonEmpty then
        MoveDestinationsReport(
          moveDestinationsByDominantSet.updatedWith(dominants) {
            case None =>
              Some(
                MoveDestinations(
                  sources = sourcesOf(element),
                  left = Set(element),
                  right = Set.empty,
                  coincident = Set.empty
                )
              )
            case Some(moveDestinations) =>
              Some(moveDestinations.focus(_.left).modify(_ + element))
          }
        )
      else this
      end if
    end leftMoveOf

    def rightMoveOf(
        element: Element
    ): MoveDestinationsReport =
      val dominants = dominantsOf(element)

      if dominants.nonEmpty then
        MoveDestinationsReport(
          moveDestinationsByDominantSet.updatedWith(dominants) {
            case None =>
              Some(
                MoveDestinations(
                  sources = sourcesOf(element),
                  left = Set.empty,
                  right = Set(element),
                  coincident = Set.empty
                )
              )
            case Some(moveDestinations) =>
              Some(moveDestinations.focus(_.right).modify(_ + element))
          }
        )
      else this
      end if
    end rightMoveOf

    def coincidentMoveOf(
        elementPairAcrossLeftAndRight: (Element, Element)
    ): MoveDestinationsReport =
      val dominants = dominantsOf(elementPairAcrossLeftAndRight._1)

      if dominants.nonEmpty then
        MoveDestinationsReport(
          moveDestinationsByDominantSet.updatedWith(dominants) {
            case None =>
              Some(
                MoveDestinations(
                  sources = sourcesOf(elementPairAcrossLeftAndRight._1),
                  left = Set.empty,
                  right = Set.empty,
                  coincident = Set(elementPairAcrossLeftAndRight)
                )
              )
            case Some(moveDestinations) =>
              Some(
                moveDestinations
                  .focus(_.coincident)
                  .modify(_ + elementPairAcrossLeftAndRight)
              )
          }
        )
      else this
      end if
    end coincidentMoveOf

    def mergeWith(another: MoveDestinationsReport): MoveDestinationsReport =
      MoveDestinationsReport(
        this.moveDestinationsByDominantSet.mergeByKeyWith(
          another.moveDestinationsByDominantSet
        ) {
          case (Some(lhs), Some(rhs)) => lhs mergeWith rhs
          case (Some(lhs), None)      => lhs
          case (None, Some(rhs))      => rhs
        }
      )

    def summarizeInText: Iterable[String] =
      moveDestinationsByDominantSet.values.map(_.description)
  end MoveDestinationsReport

  object MergeResultDetectingMotion extends StrictLogging:
    private type MergeResultDetectingMotionType[CoreResult[_]] =
      [Element] =>> MergeResultDetectingMotion[CoreResult, Element]

    def mergeAlgebra[CoreResult[_]](
        coreMergeAlgebra: merge.MergeAlgebra[CoreResult, Element]
    ): MergeAlgebra[MergeResultDetectingMotionType[CoreResult], Element] =
      type ConfiguredMergeResultDetectingMotion[Element] =
        MergeResultDetectingMotionType[CoreResult][Element]

      new MergeAlgebra[MergeResultDetectingMotionType[CoreResult], Element]:
        override def empty: ConfiguredMergeResultDetectingMotion[Element] =
          MergeResultDetectingMotion(
            coreMergeResult = coreMergeAlgebra.empty,
            changesPropagatedThroughMotion = MultiDict.empty,
            editsPropagatedThroughMotionSubjectToAdjustment = Seq.empty,
            moveDestinationsReport = emptyReport,
            insertions = Set.empty
          )

        override def preservation(
            result: MergeResultDetectingMotionType[CoreResult][Element],
            preservedElementOnLeft: Element,
            preservedElementOnRight: Element
        ): ConfiguredMergeResultDetectingMotion[Element] = result
          .focus(_.coreMergeResult)
          .modify((result: CoreResult[Element]) =>
            coreMergeAlgebra.preservation(
              result,
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
            .modify(_ + Insertion(insertedElement, Side.Left))
            .focus(_.coreMergeResult)
            .modify(coreMergeAlgebra.leftInsertion(_, insertedElement))
            .focus(_.moveDestinationsReport)
            .modify(_.leftMoveOf(insertedElement))
        end leftInsertion

        override def rightInsertion(
            result: ConfiguredMergeResultDetectingMotion[Element],
            insertedElement: Element
        ): ConfiguredMergeResultDetectingMotion[Element] =
          result
            .focus(_.insertions)
            .modify(_ + Insertion(insertedElement, Side.Right))
            .focus(_.coreMergeResult)
            .modify(coreMergeAlgebra.rightInsertion(_, insertedElement))
            .focus(_.moveDestinationsReport)
            .modify(_.rightMoveOf(insertedElement))

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
          .focus(_.moveDestinationsReport)
          .modify(
            _.coincidentMoveOf(insertedElementOnLeft -> insertedElementOnRight)
          )

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
          def default = result
            .focus(_.coreMergeResult)
            .modify(coreMergeAlgebra.coincidentDeletion(_, deletedElement))

          val matches = matchesFor(deletedElement).toSeq

          matches match
            case Seq(_: AllSides[Section[Element]], _*) =>
              default

            case Seq(_: BaseAndLeft[Section[Element]], _*) =>
              matches.foldLeft(default) {
                case (result, BaseAndLeft(_, leftElementAtMoveDestination)) =>
                  logger.debug(
                    s"Coincident deletion at origin of move: propagating deletion to left move destination ${pprintCustomised(leftElementAtMoveDestination)}."
                  )

                  result
                    .focus(_.changesPropagatedThroughMotion)
                    .modify(_ + (leftElementAtMoveDestination -> None))
              }

            case Seq(_: BaseAndRight[Section[Element]], _*) =>
              matches.foldLeft(default) {
                case (result, BaseAndRight(_, rightElementAtMoveDestination)) =>
                  logger.debug(
                    s"Coincident deletion at origin of move: propagating deletion to right move destination ${pprintCustomised(rightElementAtMoveDestination)}."
                  )

                  result
                    .focus(_.changesPropagatedThroughMotion)
                    .modify(_ + (rightElementAtMoveDestination -> None))
              }

            case Seq() =>
              default
                .focus(_.changesPropagatedThroughMotion)
                .modify(changesPropagatedThroughMotion =>
                  default.editsPropagatedThroughMotionSubjectToAdjustment
                    .foldLeft(changesPropagatedThroughMotion)(
                      {
                        case (
                              changesPropagatedThroughMotion,
                              (destination, edit)
                            ) =>
                          changesPropagatedThroughMotion - (destination -> Some(
                            edit
                          )) + (destination -> Some(
                            adjustmentOfEditByFollowingCoincidentDeletion(
                              editElement = edit,
                              deletedElement = deletedElement
                            )
                          ))
                      }
                    )
                )
                .focus(_.editsPropagatedThroughMotionSubjectToAdjustment)
                .modify(_ => Seq.empty)
          end match
        end coincidentDeletion

        override def leftEdit(
            result: ConfiguredMergeResultDetectingMotion[Element],
            editedElement: Element,
            editElements: IndexedSeq[Element]
        ): ConfiguredMergeResultDetectingMotion[Element] = result
          .focus(_.coreMergeResult)
          .modify(coreMergeAlgebra.leftEdit(_, editedElement, editElements))
          .focus(_.moveDestinationsReport)
          .modify(editElements.foldLeft(_)(_ leftMoveOf _))

        override def rightEdit(
            result: ConfiguredMergeResultDetectingMotion[Element],
            editedElement: Element,
            editElements: IndexedSeq[Element]
        ): ConfiguredMergeResultDetectingMotion[Element] = result
          .focus(_.coreMergeResult)
          .modify(coreMergeAlgebra.rightEdit(_, editedElement, editElements))
          .focus(_.moveDestinationsReport)
          .modify(editElements.foldLeft(_)(_ rightMoveOf _))

        override def coincidentEdit(
            result: ConfiguredMergeResultDetectingMotion[Element],
            editedElement: Element,
            editElements: IndexedSeq[(Element, Element)]
        ): ConfiguredMergeResultDetectingMotion[Element] =
          def default = result
            .focus(_.coreMergeResult)
            .modify(
              coreMergeAlgebra.coincidentEdit(_, editedElement, editElements)
            )
            .focus(_.moveDestinationsReport)
            .modify(editElements.foldLeft(_)(_ coincidentMoveOf _))

          val matches = matchesFor(editedElement).toSeq

          matches match
            // NOTE: we're not missing the all-sides case below - the default
            // handles it perfectly well, as the left and right contributions to
            // the match are *incoming* moves, so there is nothing to propagate.
            case Seq(_: BaseAndLeft[Section[Element]], _*) =>
              matches.foldLeft(default) {
                case (result, BaseAndLeft(_, leftElementAtMoveDestination)) =>
                  logger.debug(
                    s"Coincident edit at origin of move: propagating deletion to left move destination ${pprintCustomised(leftElementAtMoveDestination)}."
                  )

                  result
                    .focus(_.changesPropagatedThroughMotion)
                    .modify(
                      _ + (leftElementAtMoveDestination -> None)
                    )
              }

            case Seq(_: BaseAndRight[Section[Element]], _*) =>
              matches.foldLeft(default) {
                case (result, BaseAndRight(_, rightElementAtMoveDestination)) =>
                  logger.debug(
                    s"Coincident edit at origin of move: propagating deletion to right move destination ${pprintCustomised(rightElementAtMoveDestination)}."
                  )

                  result
                    .focus(_.changesPropagatedThroughMotion)
                    .modify(
                      _ + (rightElementAtMoveDestination -> None)
                    )
              }

            case Seq() =>
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
              val matches = matchesFor(baseElement).toSeq

              matches match
                case Seq(_: AllSides[Section[Element]], _*) =>
                  default

                case Seq(_: BaseAndRight[Section[Element]], _*) =>
                  matches.foldLeft(
                    result
                      .focus(_.coreMergeResult)
                      .modify(
                        coreMergeAlgebra.coincidentDeletion(_, baseElement)
                      )
                  ) {
                    case (
                          result,
                          BaseAndRight(_, rightElementAtMoveDestination)
                        ) =>
                      logger.debug(
                        s"Conflict at origin of move: resolved as a coincident deletion of ${pprintCustomised(baseElement)}; propagating left edit ${pprintCustomised(leftElement)} to right move destination ${pprintCustomised(rightElementAtMoveDestination)}."
                      )

                      result
                        .focus(_.changesPropagatedThroughMotion)
                        .modify(
                          _ + (rightElementAtMoveDestination -> Some(
                            leftElement
                          ))
                        )
                        .focus(
                          _.editsPropagatedThroughMotionSubjectToAdjustment
                        )
                        .modify(
                          _ :+ (rightElementAtMoveDestination -> leftElement)
                        )
                  }

                case Seq(_: BaseAndLeft[Section[Element]], _*) =>
                  matches.foldLeft(
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
                  ) {
                    case (
                          result,
                          BaseAndLeft(_, leftElementAtMoveDestination)
                        ) =>
                      logger.debug(
                        s"Conflict at origin of move: resolved as a coincident deletion of ${pprintCustomised(baseElement)} and a left insertion of ${pprintCustomised(leftElement)}; propagating deletion to left move destination ${pprintCustomised(leftElementAtMoveDestination)}."
                      )

                      result
                        .focus(_.changesPropagatedThroughMotion)
                        .modify(
                          _ + (leftElementAtMoveDestination -> None)
                        )
                  }

                case Seq() => default
              end match

            case (Seq(baseElement), Seq(), Seq(rightElement)) =>
              val matches = matchesFor(baseElement).toSeq

              matches match
                case Seq(_: AllSides[Section[Element]], _*) =>
                  default

                case Seq(_: BaseAndLeft[Section[Element]], _*) =>
                  matches.foldLeft(
                    result
                      .focus(_.coreMergeResult)
                      .modify(
                        coreMergeAlgebra.coincidentDeletion(_, baseElement)
                      )
                  ) {
                    case (
                          result,
                          BaseAndLeft(_, leftElementAtMoveDestination)
                        ) =>
                      logger.debug(
                        s"Conflict at origin of move: resolved as a coincident deletion of ${pprintCustomised(baseElement)}; propagating right edit ${pprintCustomised(rightElement)} to left move destination ${pprintCustomised(leftElementAtMoveDestination)}."
                      )

                      result
                        .focus(_.changesPropagatedThroughMotion)
                        .modify(
                          _ + (leftElementAtMoveDestination -> Some(
                            rightElement
                          ))
                        )
                        .focus(
                          _.editsPropagatedThroughMotionSubjectToAdjustment
                        )
                        .modify(
                          _ :+ (leftElementAtMoveDestination -> rightElement)
                        )
                  }

                case Seq(_: BaseAndRight[Section[Element]], _*) =>
                  matches.foldLeft(
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
                  ) {
                    case (
                          result,
                          BaseAndRight(_, rightElementAtMoveDestination)
                        ) =>
                      logger.debug(
                        s"Conflict at origin of move: resolved as a coincident deletion of ${pprintCustomised(baseElement)} and a right insertion of ${pprintCustomised(rightElement)}; propagating deletion to right move destination ${pprintCustomised(rightElementAtMoveDestination)}."
                      )

                      result
                        .focus(_.changesPropagatedThroughMotion)
                        .modify(
                          _ + (rightElementAtMoveDestination -> None)
                        )
                  }

                case Seq() => default
              end match

            case (Seq(baseElement), Seq(leftElement), Seq(rightElement)) =>
              val matches = matchesFor(baseElement).toSeq

              matches match
                case Seq(_: AllSides[Section[Element]], _*) =>
                  default

                case Seq(_: BaseAndLeft[Section[Element]], _*) =>
                  matches.foldLeft(
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
                  ) {
                    case (
                          result,
                          BaseAndLeft(_, leftElementAtMoveDestination)
                        ) =>
                      logger.debug(
                        s"Conflict at origin of move: resolved as a coincident deletion of ${pprintCustomised(baseElement)} and a left insertion of ${pprintCustomised(leftElement)}; propagating right edit ${pprintCustomised(rightElement)} to left move destination ${pprintCustomised(leftElementAtMoveDestination)}."
                      )

                      result
                        .focus(_.changesPropagatedThroughMotion)
                        .modify(
                          _ + (leftElementAtMoveDestination -> Some(
                            rightElement
                          ))
                        )
                        .focus(
                          _.editsPropagatedThroughMotionSubjectToAdjustment
                        )
                        .modify(
                          _ :+ (leftElementAtMoveDestination -> leftElement)
                        )
                  }

                case Seq(_: BaseAndRight[Section[Element]], _*) =>
                  matches.foldLeft(
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
                  ) {
                    case (
                          result,
                          BaseAndRight(_, rightElementAtMoveDestination)
                        ) =>
                      logger.debug(
                        s"Conflict at origin of move: resolved as a coincident deletion of ${pprintCustomised(baseElement)} and a right insertion of ${pprintCustomised(rightElement)}; propagating left edit ${pprintCustomised(leftElement)} to right move destination ${pprintCustomised(rightElementAtMoveDestination)}."
                      )

                      result
                        .focus(_.changesPropagatedThroughMotion)
                        .modify(
                          _ + (rightElementAtMoveDestination -> Some(
                            leftElement
                          ))
                        )
                        .focus(
                          _.editsPropagatedThroughMotionSubjectToAdjustment
                        )
                        .modify(
                          _ :+ (rightElementAtMoveDestination -> leftElement)
                        )
                  }

                case Seq() => default
              end match
            case _ =>
              default
          end match
        end conflict
      end new
    end mergeAlgebra
  end MergeResultDetectingMotion
end MatchesContext
