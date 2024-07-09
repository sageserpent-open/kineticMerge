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

  def dominantsOf(element: Element): collection.Set[Element] =
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
      changesMigratedThroughMotion: MultiDict[Element, IndexedSeq[Element]],
      moveDestinationsReport: MoveDestinationsReport,
      insertions: Seq[Insertion]
  )

  // NOTE: this could be moved into a companion object for `MatchesContext`, but
  // we still need to define `MoveDestinationsReport` in scope of the class,
  // thus requiring an import from the companion *and* an instance of the class
  // itself. It is possible to define `MoveDestinationsReport` as a trait in the
  // companion and thus to move `MergeResultDetectingMotion` into the companion
  // too, but I'm not sure it makes things more readable elsewhere. We also want
  // to merge instances of `MoveDestinationsReport` that share the same
  // `MatchesContext`. All in all, let's stick with the current design for
  // now...
  enum Side:
    case Left
    case Right
  end Side

  case class Insertion(side: Side, inserted: Element)

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
        coreMergeAlgebra: merge.MergeAlgebra[CoreResult, Element],
        resolution: Resolution[Element]
    ): MergeAlgebra[MergeResultDetectingMotionType[CoreResult], Element] =
      type ConfiguredMergeResultDetectingMotion[Element] =
        MergeResultDetectingMotionType[CoreResult][Element]

      new MergeAlgebra[MergeResultDetectingMotionType[CoreResult], Element]:
        override def empty: ConfiguredMergeResultDetectingMotion[Element] =
          MergeResultDetectingMotion(
            coreMergeResult = coreMergeAlgebra.empty,
            changesMigratedThroughMotion = MultiDict.empty,
            moveDestinationsReport = emptyReport,
            insertions = Vector.empty
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
            .focus(_.moveDestinationsReport)
            .modify(_.leftMoveOf(insertedElement))
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
            deletedBaseElement: Element,
            deletedRightElement: Element
        ): ConfiguredMergeResultDetectingMotion[Element] =
          def default = result
            .focus(_.coreMergeResult)
            .modify(
              coreMergeAlgebra
                .leftDeletion(_, deletedBaseElement, deletedRightElement)
            )

          val matches = matchesFor(deletedBaseElement).toSeq

          matches match
            case Seq(_: AllSides[Section[Element]], _*) =>
              matches.foldLeft(default) {
                case (
                      partialResult,
                      AllSides(_, leftElementAtMoveDestination, rightElement)
                    ) if deletedRightElement == rightElement =>
                  val resolved = resolution(
                    Some(deletedBaseElement),
                    leftElementAtMoveDestination,
                    deletedRightElement
                  )

                  if resolved != leftElementAtMoveDestination then
                    logger.debug(
                      // TODO: this message is confusing!
                      s"Left deletion at origin of move: migrating resolved minor edit ${pprintCustomised(resolved)} to left move destination ${pprintCustomised(leftElementAtMoveDestination)}."
                    )

                    partialResult
                      .focus(_.changesMigratedThroughMotion)
                      .modify(
                        _ + (leftElementAtMoveDestination -> IndexedSeq(
                          resolved
                        ))
                      )
                  else partialResult
                  end if
                case (partialResult, _) => partialResult
              }
            case _ => default
          end match
        end leftDeletion

        override def rightDeletion(
            result: ConfiguredMergeResultDetectingMotion[Element],
            deletedBaseElement: Element,
            deletedLeftElement: Element
        ): ConfiguredMergeResultDetectingMotion[Element] =
          def default = result
            .focus(_.coreMergeResult)
            .modify(
              coreMergeAlgebra
                .rightDeletion(_, deletedBaseElement, deletedLeftElement)
            )

          val matches = matchesFor(deletedBaseElement).toSeq

          matches match
            case Seq(_: AllSides[Section[Element]], _*) =>
              matches.foldLeft(default) {
                case (
                      partialResult,
                      AllSides(_, leftElement, rightElementAtMoveDestination)
                    ) if deletedLeftElement == leftElement =>
                  val resolved = resolution(
                    Some(deletedBaseElement),
                    deletedLeftElement,
                    rightElementAtMoveDestination
                  )

                  if resolved != rightElementAtMoveDestination then
                    logger.debug(
                      // TODO: this message is confusing!
                      s"Right deletion at origin of move: migrating resolved minor edit ${pprintCustomised(resolved)} to right move destination ${pprintCustomised(rightElementAtMoveDestination)}."
                    )

                    partialResult
                      .focus(_.changesMigratedThroughMotion)
                      .modify(
                        _ + (rightElementAtMoveDestination -> IndexedSeq(
                          resolved
                        ))
                      )
                  else partialResult
                  end if
                case (partialResult, _) => partialResult
              }
            case _ => default
          end match
        end rightDeletion

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
                    s"Coincident deletion at origin of move: migrating deletion to left move destination ${pprintCustomised(leftElementAtMoveDestination)}."
                  )

                  result
                    .focus(_.changesMigratedThroughMotion)
                    .modify(
                      _ + (leftElementAtMoveDestination -> IndexedSeq.empty)
                    )
              }

            case Seq(_: BaseAndRight[Section[Element]], _*) =>
              matches.foldLeft(default) {
                case (result, BaseAndRight(_, rightElementAtMoveDestination)) =>
                  logger.debug(
                    s"Coincident deletion at origin of move: migrating deletion to right move destination ${pprintCustomised(rightElementAtMoveDestination)}."
                  )

                  result
                    .focus(_.changesMigratedThroughMotion)
                    .modify(
                      _ + (rightElementAtMoveDestination -> IndexedSeq.empty)
                    )
              }

            case Seq() => default
          end match
        end coincidentDeletion

        override def leftEdit(
            result: ConfiguredMergeResultDetectingMotion[Element],
            editedBaseElement: Element,
            editedRightElement: Element,
            editElements: IndexedSeq[Element]
        ): ConfiguredMergeResultDetectingMotion[Element] =
          def default = result
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
            .focus(_.moveDestinationsReport)
            .modify(editElements.foldLeft(_)(_ leftMoveOf _))

          val matches = matchesFor(editedBaseElement).toSeq

          matches match
            case Seq(_: AllSides[Section[Element]], _*) =>
              matches.foldLeft(default) {
                case (
                      partialResult,
                      AllSides(_, leftElementAtMoveDestination, rightElement)
                    ) if editedRightElement == rightElement =>
                  val resolved = resolution(
                    Some(editedBaseElement),
                    leftElementAtMoveDestination,
                    editedRightElement
                  )

                  if resolved != leftElementAtMoveDestination then
                    logger.debug(
                      // TODO: this message is confusing!
                      s"Left edit at origin of move: migrating resolved minor edit ${pprintCustomised(resolved)} to left move destination ${pprintCustomised(leftElementAtMoveDestination)}."
                    )

                    partialResult
                      .focus(_.changesMigratedThroughMotion)
                      .modify(
                        _ + (leftElementAtMoveDestination -> IndexedSeq(
                          resolved
                        ))
                      )
                  else partialResult
                  end if
                case (partialResult, _) => partialResult
              }
            case _ => default
          end match
        end leftEdit

        override def rightEdit(
            result: ConfiguredMergeResultDetectingMotion[Element],
            editedBaseElement: Element,
            editedLeftElement: Element,
            editElements: IndexedSeq[Element]
        ): ConfiguredMergeResultDetectingMotion[Element] =
          def default = result
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
            .focus(_.moveDestinationsReport)
            .modify(editElements.foldLeft(_)(_ rightMoveOf _))

          val matches = matchesFor(editedBaseElement).toSeq

          matches match
            case Seq(_: AllSides[Section[Element]], _*) =>
              matches.foldLeft(default) {
                case (
                      partialResult,
                      AllSides(_, leftElement, rightElementAtMoveDestination)
                    ) if editedLeftElement == leftElement =>
                  val resolved = resolution(
                    Some(editedBaseElement),
                    editedLeftElement,
                    rightElementAtMoveDestination
                  )

                  if resolved != rightElementAtMoveDestination then
                    logger.debug(
                      // TODO: this message is confusing!
                      s"Right edit at origin of move: migrating resolved minor edit ${pprintCustomised(resolved)} to right move destination ${pprintCustomised(rightElementAtMoveDestination)}."
                    )

                    partialResult
                      .focus(_.changesMigratedThroughMotion)
                      .modify(
                        _ + (rightElementAtMoveDestination -> IndexedSeq(
                          resolved
                        ))
                      )
                  else partialResult
                  end if
                case (partialResult, _) => partialResult
              }
            case _ => default
          end match
        end rightEdit

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
            // the match are *incoming* moves, so there is nothing to migrate.
            case Seq(_: BaseAndLeft[Section[Element]], _*) =>
              matches.foldLeft(default) {
                case (result, BaseAndLeft(_, leftElementAtMoveDestination)) =>
                  logger.debug(
                    s"Coincident edit at origin of move: migrating deletion to left move destination ${pprintCustomised(leftElementAtMoveDestination)}."
                  )

                  result
                    .focus(_.changesMigratedThroughMotion)
                    .modify(
                      _ + (leftElementAtMoveDestination -> IndexedSeq.empty)
                    )
              }

            case Seq(_: BaseAndRight[Section[Element]], _*) =>
              matches.foldLeft(default) {
                case (result, BaseAndRight(_, rightElementAtMoveDestination)) =>
                  logger.debug(
                    s"Coincident edit at origin of move: migrating deletion to right move destination ${pprintCustomised(rightElementAtMoveDestination)}."
                  )

                  result
                    .focus(_.changesMigratedThroughMotion)
                    .modify(
                      _ + (rightElementAtMoveDestination -> IndexedSeq.empty)
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
            case (Seq(baseElement), leftElements @ Seq(_, _*), Seq()) =>
              val matches = matchesFor(baseElement).toSeq

              matches match
                case Seq(_: AllSides[Section[Element]], _*) =>
                  default

                case Seq(_: BaseAndRight[Section[Element]], _*) =>
                  val withCoincidentDeletion = result
                    .focus(_.coreMergeResult)
                    .modify(
                      coreMergeAlgebra.coincidentDeletion(_, baseElement)
                    )

                  matches.foldLeft(withCoincidentDeletion) {
                    case (
                          result,
                          BaseAndRight(_, rightElementAtMoveDestination)
                        ) =>
                      logger.debug(
                        s"Conflict at origin of move: resolved as a coincident deletion of ${pprintCustomised(baseElement)}; migrating left edit ${pprintCustomised(leftElements)} to right move destination ${pprintCustomised(rightElementAtMoveDestination)}."
                      )

                      result
                        .focus(_.changesMigratedThroughMotion)
                        .modify(
                          _ + (rightElementAtMoveDestination -> leftElements)
                        )
                  }

                case Seq(_: BaseAndLeft[Section[Element]], _*) =>
                  val withCoincidentDeletion = result
                    .focus(_.coreMergeResult)
                    .modify(
                      coreMergeAlgebra
                        .coincidentDeletion(_, baseElement)
                    )

                  val withLeftInsertions = leftElements.foldLeft(
                    withCoincidentDeletion
                  )(leftInsertion)

                  matches.foldLeft(withLeftInsertions) {
                    case (
                          partialResult,
                          BaseAndLeft(_, leftElementAtMoveDestination)
                        ) =>
                      logger.debug(
                        s"Conflict at origin of move: resolved as a coincident deletion of ${pprintCustomised(baseElement)} and a left insertion of ${pprintCustomised(leftElements)}; migrating deletion to left move destination ${pprintCustomised(leftElementAtMoveDestination)}."
                      )

                      partialResult
                        .focus(_.changesMigratedThroughMotion)
                        .modify(
                          _ + (leftElementAtMoveDestination -> IndexedSeq.empty)
                        )
                  }

                case Seq() => default
              end match

            case (Seq(baseElement), Seq(), rightElements @ Seq(_, _*)) =>
              val matches = matchesFor(baseElement).toSeq

              matches match
                case Seq(_: AllSides[Section[Element]], _*) =>
                  default

                case Seq(_: BaseAndLeft[Section[Element]], _*) =>
                  val withCoincidentDeletion = result
                    .focus(_.coreMergeResult)
                    .modify(
                      coreMergeAlgebra.coincidentDeletion(_, baseElement)
                    )

                  matches.foldLeft(
                    withCoincidentDeletion
                  ) {
                    case (
                          result,
                          BaseAndLeft(_, leftElementAtMoveDestination)
                        ) =>
                      logger.debug(
                        s"Conflict at origin of move: resolved as a coincident deletion of ${pprintCustomised(baseElement)}; migrating right edit ${pprintCustomised(rightElements)} to left move destination ${pprintCustomised(leftElementAtMoveDestination)}."
                      )

                      result
                        .focus(_.changesMigratedThroughMotion)
                        .modify(
                          _ + (leftElementAtMoveDestination -> rightElements)
                        )
                  }

                case Seq(_: BaseAndRight[Section[Element]], _*) =>
                  val withCoincidentDeletion = result
                    .focus(_.coreMergeResult)
                    .modify(
                      coreMergeAlgebra
                        .coincidentDeletion(_, baseElement)
                    )

                  val withRightInsertions = rightElements.foldLeft(
                    withCoincidentDeletion
                  )(rightInsertion)

                  matches.foldLeft(withRightInsertions) {
                    case (
                          partialResult,
                          BaseAndRight(_, rightElementAtMoveDestination)
                        ) =>
                      logger.debug(
                        s"Conflict at origin of move: resolved as a coincident deletion of ${pprintCustomised(baseElement)} and a right insertion of ${pprintCustomised(rightElements)}; migrating deletion to right move destination ${pprintCustomised(rightElementAtMoveDestination)}."
                      )

                      partialResult
                        .focus(_.changesMigratedThroughMotion)
                        .modify(
                          _ + (rightElementAtMoveDestination -> IndexedSeq.empty)
                        )
                  }

                case Seq() => default
              end match

            case (
                  Seq(baseElement),
                  leftElements @ Seq(_, _*),
                  rightElements @ Seq(_, _*)
                ) =>
              val matches = matchesFor(baseElement).toSeq

              matches match
                case Seq(_: AllSides[Section[Element]], _*) =>
                  default

                case Seq(_: BaseAndLeft[Section[Element]], _*) =>
                  val withCoincidentDeletion = result
                    .focus(_.coreMergeResult)
                    .modify(
                      coreMergeAlgebra
                        .coincidentDeletion(_, baseElement)
                    )

                  val withLeftInsertions = leftElements.foldLeft(
                    withCoincidentDeletion
                  )(leftInsertion)

                  matches.foldLeft(withLeftInsertions) {
                    case (
                          partialResult,
                          BaseAndLeft(_, leftElementAtMoveDestination)
                        ) =>
                      logger.debug(
                        s"Conflict at origin of move: resolved as a coincident deletion of ${pprintCustomised(baseElement)} and a left insertion of ${pprintCustomised(leftElements)}; migrating right edit ${pprintCustomised(rightElements)} to left move destination ${pprintCustomised(leftElementAtMoveDestination)}."
                      )

                      partialResult
                        .focus(_.changesMigratedThroughMotion)
                        .modify(
                          _ + (leftElementAtMoveDestination -> rightElements)
                        )
                  }

                case Seq(_: BaseAndRight[Section[Element]], _*) =>
                  val withCoincidentDeletion = result
                    .focus(_.coreMergeResult)
                    .modify(
                      coreMergeAlgebra
                        .coincidentDeletion(_, baseElement)
                    )

                  val withRightInsertions = rightElements.foldLeft(
                    withCoincidentDeletion
                  )(rightInsertion)

                  matches.foldLeft(withRightInsertions) {
                    case (
                          partialResult,
                          BaseAndRight(_, rightElementAtMoveDestination)
                        ) =>
                      logger.debug(
                        s"Conflict at origin of move: resolved as a coincident deletion of ${pprintCustomised(baseElement)} and a right insertion of ${pprintCustomised(rightElements)}; migrating left edit ${pprintCustomised(leftElements)} to right move destination ${pprintCustomised(rightElementAtMoveDestination)}."
                      )

                      partialResult
                        .focus(_.changesMigratedThroughMotion)
                        .modify(
                          _ + (rightElementAtMoveDestination -> leftElements)
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
