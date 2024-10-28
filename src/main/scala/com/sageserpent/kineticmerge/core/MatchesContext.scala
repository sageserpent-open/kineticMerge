package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.Match.*
import com.sageserpent.kineticmerge.core.merge.MergeAlgebra
import com.typesafe.scalalogging.StrictLogging
import monocle.syntax.all.*

import scala.collection.decorators.mapDecorator
import scala.collection.immutable.MultiDict

class MatchesContext[Element](
    matchesFor: Element => collection.Set[Match[Element]]
):
  val emptyReport: MoveDestinationsReport = MoveDestinationsReport(Map.empty)

  extension (aMatch: Match[Element])
    def base: Option[Element] = aMatch match
      case BaseAndLeft(baseElement, _)  => Some(baseElement)
      case BaseAndRight(baseElement, _) => Some(baseElement)
      case LeftAndRight(_, _)           => None
      case AllSides(baseElement, _, _)  => Some(baseElement)
    end base
  end extension

  /** @param coreMergeResult
    *   What is says on the tin: a simpler merge result that is delegated to by
    *   the operations implemented in
    *   [[MatchesContext.MergeResultDetectingMotion.mergeAlgebra]].
    * @param changesMigratedThroughMotion
    *   Edits and deletions to be migrated, referenced by move destination.
    * @param moveDestinationsReport
    * @param insertions
    *   Insertions that may need to be migrated - in contrast to
    *   {@code changesMigratedThroughMotion} , these have to be collected
    *   speculatively upfront and then associated with anchors once the global
    *   picture of code motion is available.
    * @param oneSidedDeletions
    *   Deletions on just one side that may influence the discovery of anchors
    *   for insertion migration.
    * @tparam CoreResult
    * @tparam Element
    */
  case class MergeResultDetectingMotion[CoreResult[_], Element](
      coreMergeResult: CoreResult[Element],
      changesMigratedThroughMotion: MultiDict[Element, IndexedSeq[Element]],
      moveDestinationsReport: MoveDestinationsReport,
      insertions: Seq[Insertion],
      oneSidedDeletions: Set[Element]
  )

  // NOTE: this could be moved into a companion object for `MatchesContext`, but
  // we still need to define `MoveDestinationsReport` in scope of the class,
  // thus requiring an import from the companion *and* an instance of the class
  // itself. It is possible to define `MoveDestinationsReport` as a trait in the
  // companion and thus to move `MergeResultDetectingMotion` into the companion
  // too, but I'm not sure that it makes things more readable elsewhere. We also
  // want to merge instances of `MoveDestinationsReport` that share the same
  // `MatchesContext`. All in all, let's stick with the current design for
  // now...
  enum Side:
    case Left
    case Right
  end Side

  case class Insertion(side: Side, inserted: Element)

  case class MoveDestinationsReport(
      moveDestinationsByMatches: Map[collection.Set[
        Match[Element]
      ], MoveDestinations[Element]]
  ):
    def leftMoveOf(
        element: Element
    ): MoveDestinationsReport =
      val matches = matchesFor(element)
      val sources = matches.flatMap(_.base)

      if matches.nonEmpty && sources.nonEmpty then
        MoveDestinationsReport(
          moveDestinationsByMatches.updatedWith(matches) {
            case None =>
              Some(
                MoveDestinations(
                  sources = sources,
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
      val matches = matchesFor(element)
      val sources = matches.flatMap(_.base)

      if matches.nonEmpty && sources.nonEmpty then
        MoveDestinationsReport(
          moveDestinationsByMatches.updatedWith(matches) {
            case None =>
              Some(
                MoveDestinations(
                  sources = sources,
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
      val matches = matchesFor(elementPairAcrossLeftAndRight._1)
      val sources = matches.flatMap(_.base)

      if matches.nonEmpty && sources.nonEmpty then
        MoveDestinationsReport(
          moveDestinationsByMatches.updatedWith(matches) {
            case None =>
              Some(
                MoveDestinations(
                  sources = sources,
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
        this.moveDestinationsByMatches.mergeByKeyWith(
          another.moveDestinationsByMatches
        ) {
          case (Some(lhs), Some(rhs)) => lhs mergeWith rhs
          case (Some(lhs), None)      => lhs
          case (None, Some(rhs))      => rhs
        }
      )

    def summarizeInText: Iterable[String] =
      moveDestinationsByMatches.values.map(_.description)
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
            .focus(_.oneSidedDeletions)
            .modify(_ + deletedRightElement)

          val matches = matchesFor(deletedBaseElement).toSeq

          matches match
            case Seq(_: AllSides[Element], _*) =>
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
                      s"Left deletion at origin of move: also migrating additional minor edit resolution ${pprintCustomised(resolved)} to left move destination ${pprintCustomised(leftElementAtMoveDestination)}."
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
            .focus(_.oneSidedDeletions)
            .modify(_ + deletedLeftElement)

          val matches = matchesFor(deletedBaseElement).toSeq

          matches match
            case Seq(_: AllSides[Element], _*) =>
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
                      s"Right deletion at origin of move: also migrating additional minor edit resolution ${pprintCustomised(resolved)} to right move destination ${pprintCustomised(rightElementAtMoveDestination)}."
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
            case Seq(_: AllSides[Element], _*) =>
              logger.debug(
                s"Coincident deletion at origin of moves on both sides."
              )

              default

            case Seq(_: BaseAndLeft[Element], _*) =>
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

            case Seq(_: BaseAndRight[Element], _*) =>
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
            case Seq(_: AllSides[Element], _*) =>
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
                      s"Left edit at origin of move: also migrating additional minor edit resolution ${pprintCustomised(resolved)} to left move destination ${pprintCustomised(leftElementAtMoveDestination)}."
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
            case Seq(_: AllSides[Element], _*) =>
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
                      s"Right edit at origin of move: also migrating additional minor edit resolution ${pprintCustomised(resolved)} to right move destination ${pprintCustomised(rightElementAtMoveDestination)}."
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
            case Seq(_: BaseAndLeft[Element], _*) =>
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

            case Seq(_: BaseAndRight[Element], _*) =>
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
          val insertionConflict =
            editedElements.isEmpty && leftEditElements.nonEmpty && rightEditElements.nonEmpty
          val conflictInvolvingAnEdit =
            editedElements.nonEmpty && (leftEditElements.nonEmpty || rightEditElements.nonEmpty)

          require(
            insertionConflict || conflictInvolvingAnEdit
          )

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
              .focus(_.moveDestinationsReport)
              .modify(leftEditElements.foldLeft(_)(_ leftMoveOf _))
              .focus(_.moveDestinationsReport)
              .modify(rightEditElements.foldLeft(_)(_ rightMoveOf _))

          (editedElements, leftEditElements, rightEditElements) match
            case (Seq(baseElement), leftElements @ Seq(_, _*), Seq()) =>
              // Left edit versus right deletion conflict...

              val matches = matchesFor(baseElement).toSeq

              matches match
                case Seq(_: AllSides[Element], _*) =>
                  val withCoincidentDeletion = result
                    .focus(_.coreMergeResult)
                    .modify(
                      coreMergeAlgebra
                        .coincidentDeletion(_, baseElement)
                    )

                  val withLeftInsertions = leftElements.foldLeft(
                    withCoincidentDeletion
                  )(leftInsertion)

                  logger.debug(
                    s"Conflict at origin of moves on both sides: resolved as a coincident deletion of ${pprintCustomised(baseElement)} and a left insertion of ${pprintCustomised(leftElements)}."
                  )

                  withLeftInsertions

                case Seq(_: BaseAndRight[Element], _*) =>
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

                case Seq(_: BaseAndLeft[Element], _*) =>
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
              // Left deletion versus right edit conflict...

              val matches = matchesFor(baseElement).toSeq

              matches match
                case Seq(_: AllSides[Element], _*) =>
                  val withCoincidentDeletion = result
                    .focus(_.coreMergeResult)
                    .modify(
                      coreMergeAlgebra
                        .coincidentDeletion(_, baseElement)
                    )

                  val withRightInsertions = rightElements.foldLeft(
                    withCoincidentDeletion
                  )(rightInsertion)

                  logger.debug(
                    s"Conflict at origin of moves on both sides: resolved as a coincident deletion of ${pprintCustomised(baseElement)} and a right insertion of ${pprintCustomised(rightElements)}."
                  )

                  withRightInsertions

                case Seq(_: BaseAndLeft[Element], _*) =>
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

                case Seq(_: BaseAndRight[Element], _*) =>
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
              // Left edit versus right edit conflict...

              val matches = matchesFor(baseElement).toSeq

              matches match
                case Seq(_: AllSides[Element], _*) =>
                  default

                case Seq(_: BaseAndLeft[Element], _*) =>
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

                case Seq(_: BaseAndRight[Element], _*) =>
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
            case (
                  Seq(),
                  Seq(_, _*),
                  Seq(_, _*)
                ) =>
              // Left insertion versus right insertion conflict...

              default
          end match
        end conflict
      end new
    end mergeAlgebra
  end MergeResultDetectingMotion
end MatchesContext
