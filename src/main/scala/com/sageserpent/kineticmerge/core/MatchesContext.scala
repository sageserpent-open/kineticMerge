package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.Match.*
import com.sageserpent.kineticmerge.core.merge.MergeAlgebra
import com.typesafe.scalalogging.StrictLogging
import monocle.syntax.all.*

import scala.collection.decorators.mapDecorator

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

  /** Represents the content migrated through a move.
    */
  enum Migration:
    /** This represents a simple move with no associated changes. We model this
      * explicitly so that trivial variations of the moved element can be
      * resolved at the move destination.
      *
      * @note
      *   We only need the side opposite the move destination because a
      *   migration is keyed by a source element taken from the base. The move
      *   destination itself completes the three sides' contributions.
      */
    case PlainMove(elementOnTheOppositeSideToTheMoveDestination: Element)

    /** If {@code change} is empty, this represents a deletion, otherwise it
      * represents an edit.
      */
    case Change(change: IndexedSeq[Element])
  end Migration

  /** @param coreMergeResult
    *   What is says on the tin: a simpler merge result that is delegated to by
    *   the operations implemented in
    *   [[MatchesContext.MergeResultDetectingMotion.mergeAlgebra]].
    * @param migrationsBySource
    *   Migrations keyed by source element - the source element is taken from
    *   the <b>base</b> side.
    * @param moveDestinationsReport
    * @param insertions
    *   Insertions that may need to be migrated - in contrast to
    *   {@code migrationsBySource} , these have to be collected speculatively
    *   upfront and then associated with anchors once the global picture of code
    *   motion is available.
    * @param oneSidedDeletions
    *   Deletions on just one side that may influence the discovery of anchors
    *   for insertion migration.
    * @tparam CoreResult
    * @tparam Element
    * @note
    *   A move source refers to only one migration, although it is quite
    *   possible for a single source to be part of multiple movements on either
    *   or both the left and right side. It is also possible for multiple move
    *   sources (and thus migrations) to converge on the same move destination.
    */
  case class MergeResultDetectingMotion[CoreResult[_], Element](
      coreMergeResult: CoreResult[Element],
      migrationsBySource: Map[Element, Migration],
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
        coreMergeAlgebra: merge.MergeAlgebra[CoreResult, Element]
    ): MergeAlgebra[MergeResultDetectingMotionType[CoreResult], Element] =
      type ConfiguredMergeResultDetectingMotion[Element] =
        MergeResultDetectingMotionType[CoreResult][Element]

      new MergeAlgebra[MergeResultDetectingMotionType[CoreResult], Element]:
        override def empty: ConfiguredMergeResultDetectingMotion[Element] =
          MergeResultDetectingMotion(
            coreMergeResult = coreMergeAlgebra.empty,
            migrationsBySource = Map.empty,
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

          logger.debug(
            s"Left deletion of ${pprintCustomised(deletedBaseElement)} taken as source of left move."
          )

          default
            .focus(_.migrationsBySource)
            .modify(
              _ + (deletedBaseElement -> Migration
                .PlainMove(deletedRightElement))
            )
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

          logger.debug(
            s"Right deletion of ${pprintCustomised(deletedBaseElement)} taken as source of right move."
          )

          default
            .focus(_.migrationsBySource)
            .modify(
              _ + (deletedBaseElement -> Migration
                .PlainMove(deletedLeftElement))
            )
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
                s"Coincident deletion of ${pprintCustomised(deletedElement)} taken as source of moves on both sides."
              )

              default

            case Seq(_: BaseAndLeft[Element], _*) =>
              logger.debug(
                s"Coincident deletion of ${pprintCustomised(deletedElement)} taken as source of left move; migrating deletion."
              )

              default
                .focus(_.migrationsBySource)
                .modify(
                  _ + (deletedElement -> Migration.Change(IndexedSeq.empty))
                )

            case Seq(_: BaseAndRight[Element], _*) =>
              logger.debug(
                s"Coincident deletion of ${pprintCustomised(deletedElement)} taken as source of right move; migrating deletion."
              )

              default
                .focus(_.migrationsBySource)
                .modify(
                  _ + (deletedElement -> Migration.Change(IndexedSeq.empty))
                )

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

          logger.debug(
            s"Left edit of ${pprintCustomised(editedBaseElement)} taken as source of left move."
          )

          default
            .focus(_.migrationsBySource)
            .modify(
              _ + (editedBaseElement -> Migration.PlainMove(editedRightElement))
            )
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

          logger.debug(
            s"Right edit of ${pprintCustomised(editedBaseElement)} taken as source of right move."
          )

          default
            .focus(_.migrationsBySource)
            .modify(
              _ + (editedBaseElement -> Migration.PlainMove(editedLeftElement))
            )
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
              logger.debug(
                s"Coincident edit of ${pprintCustomised(editedElement)} taken as source of left move; migrating deletion."
              )

              default
                .focus(_.migrationsBySource)
                .modify(
                  _ + (editedElement -> Migration.Change(IndexedSeq.empty))
                )

            case Seq(_: BaseAndRight[Element], _*) =>
              logger.debug(
                s"Coincident edit of ${pprintCustomised(editedElement)} taken as source of right move; migrating deletion."
              )

              default
                .focus(_.migrationsBySource)
                .modify(
                  _ + (editedElement -> Migration.Change(IndexedSeq.empty))
                )

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
                    s"Conflict resolved - coincident deletion of ${pprintCustomised(baseElement)} taken as source of moves on both sides and a left insertion of ${pprintCustomised(leftElements)}."
                  )

                  withLeftInsertions

                case Seq(_: BaseAndRight[Element], _*) =>
                  val withCoincidentDeletion = result
                    .focus(_.coreMergeResult)
                    .modify(
                      coreMergeAlgebra.coincidentDeletion(_, baseElement)
                    )

                  logger.debug(
                    s"Conflict resolved - coincident deletion of ${pprintCustomised(baseElement)} taken as source of right move; migrating left edit ${pprintCustomised(leftElements)}."
                  )

                  withCoincidentDeletion
                    .focus(_.migrationsBySource)
                    .modify(
                      _ + (baseElement -> Migration.Change(leftElements))
                    )

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

                  logger.debug(
                    s"Conflict resolved - coincident deletion of ${pprintCustomised(baseElement)} taken as source of left move and a left insertion of ${pprintCustomised(leftElements)}; migrating deletion."
                  )

                  withLeftInsertions
                    .focus(_.migrationsBySource)
                    .modify(
                      _ + (baseElement -> Migration.Change(IndexedSeq.empty))
                    )

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
                    s"Conflict resolved - coincident deletion of ${pprintCustomised(baseElement)} taken as source of moves on both sides and a right insertion of ${pprintCustomised(rightElements)}."
                  )

                  withRightInsertions

                case Seq(_: BaseAndLeft[Element], _*) =>
                  val withCoincidentDeletion = result
                    .focus(_.coreMergeResult)
                    .modify(
                      coreMergeAlgebra.coincidentDeletion(_, baseElement)
                    )

                  logger.debug(
                    s"Conflict resolved - coincident deletion of ${pprintCustomised(baseElement)} taken as source of left move; migrating right edit ${pprintCustomised(rightElements)}."
                  )

                  withCoincidentDeletion
                    .focus(_.migrationsBySource)
                    .modify(
                      _ + (baseElement -> Migration.Change(rightElements))
                    )

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

                  logger.debug(
                    s"Conflict resolved - coincident deletion of ${pprintCustomised(baseElement)} taken as source of right move and a right insertion of ${pprintCustomised(rightElements)}; migrating deletion."
                  )

                  withRightInsertions
                    .focus(_.migrationsBySource)
                    .modify(
                      _ + (baseElement -> Migration.Change(IndexedSeq.empty))
                    )

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

                  logger.debug(
                    s"Conflict resolved - coincident deletion of ${pprintCustomised(baseElement)} taken as source of left move and a left insertion of ${pprintCustomised(leftElements)}; migrating right edit ${pprintCustomised(rightElements)}."
                  )

                  withLeftInsertions
                    .focus(_.migrationsBySource)
                    .modify(
                      _ + (baseElement -> Migration.Change(rightElements))
                    )

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

                  logger.debug(
                    s"Conflict resolved - coincident deletion of ${pprintCustomised(baseElement)} taken as source of right move and a right insertion of ${pprintCustomised(rightElements)}; migrating left edit ${pprintCustomised(leftElements)}."
                  )

                  withRightInsertions
                    .focus(_.migrationsBySource)
                    .modify(
                      _ + (baseElement -> Migration.Change(leftElements))
                    )

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
