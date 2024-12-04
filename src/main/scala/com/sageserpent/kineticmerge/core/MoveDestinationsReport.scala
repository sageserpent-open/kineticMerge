package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.ContentMigration.PlainMove

import scala.collection.immutable.MultiDict

/** Represents the content migrated through a move.
  */
enum ContentMigration[Element]:
  this match
    case Edit(leftContent, rightContent) =>
      require(leftContent.nonEmpty || rightContent.nonEmpty)
    case _ =>
  end match
  /** This represents a simple move with no associated changes. We model this
    * explicitly so that trivial variations of the moved element across the move
    * can be resolved at the move destination.
    *
    * @note
    *   We only need the side opposite the move destination because a migration
    *   is keyed by a source element taken from the base. The move destination
    *   itself completes the three sides' contributions.
    */
  case PlainMove(elementOnTheOppositeSideToTheMoveDestination: Element)
  case Edit(leftContent: IndexedSeq[Element], rightContent: IndexedSeq[Element])
  case Deletion()
end ContentMigration

enum SpeculativeMoveDestination[Element]:
  def element: Element = this match
    case Left(leftElement)          => leftElement
    case Right(rightElement)        => rightElement
    case Coincident(leftElement, _) =>
      // TODO: throwing away the right element seems sloppy. What are we really
      // trying to achieve?
      leftElement

  case Left(leftElement: Element)
  case Right(rightElement: Element)
  case Coincident(elementPairAcrossLeftAndRight: (Element, Element))
end SpeculativeMoveDestination

case class MoveDestinationsReport[Element](
    moveDestinationsBySources: Map[Element, MoveDestinations[Element]]
):
  def sources: Set[Element] = moveDestinationsBySources.keySet

  def summarizeInText: Iterable[String] =
    moveDestinationsBySources.map((source, moveDestinations) =>
      moveDestinations.description(source)
    )

  def all: Set[Element] = moveDestinationsBySources.values.flatMap(_.all).toSet
end MoveDestinationsReport

object MoveDestinationsReport:
  def empty[Element]: MoveDestinationsReport[Element] = MoveDestinationsReport(
    Map.empty
  )

  def evaluateSpeculativeSourcesAndDestinations[Element](
      speculativeMigrationsBySource: Map[Element, ContentMigration[
        Element
      ]],
      speculativeMoveDestinations: Set[SpeculativeMoveDestination[Element]]
  )(
      matchesFor: Element => collection.Set[Match[Element]],
      resolution: Resolution[Element]
  ): EvaluatedMoves[Element] =
    val destinationsBySource =
      // TODO: flip the logic around so that the matches are taken from the
      // *sources*, and then whittled down by the speculative *destinations*.
      // That sidesteps the ugliness of having to choose a side for a
      // speculative coincident destination.
      MultiDict.from(speculativeMoveDestinations.iterator.flatMap {
        speculativeMoveDestination =>
          val sources = matchesFor(speculativeMoveDestination.element)
            .flatMap(_.base)
            .intersect(speculativeMigrationsBySource.keySet)

          sources.map(_ -> speculativeMoveDestination)
      })

    val moveDestinationsBySource =
      destinationsBySource.sets.map((source, destinations) =>
        source -> new MoveDestinations(destinations)
      )

    val migratedEditSuppressions: Set[Element] =
      moveDestinationsBySource.toSeq
        .flatMap((source, moveDestinations) =>
          if !moveDestinations.isDivergent
          then
            val contentMigration = speculativeMigrationsBySource(source)

            if moveDestinations.left.nonEmpty then
              contentMigration match
                case ContentMigration.Edit(_, rightContent) =>
                  rightContent
                case _ =>
                  Seq.empty
            else if moveDestinations.right.nonEmpty then
              contentMigration match
                case ContentMigration.Edit(leftContent, _) =>
                  leftContent
                case _ =>
                  Seq.empty
            else
              assume(moveDestinations.coincident.nonEmpty)
              Seq.empty
            end if
          else Seq.empty
          end if
        )
        .toSet

    val substitutionsByDestination: MultiDict[Element, IndexedSeq[Element]] =
      MultiDict.from(
        moveDestinationsBySource.toSeq.flatMap((source, moveDestinations) =>
          if !moveDestinations.isDivergent
          then
            val contentMigration = speculativeMigrationsBySource(source)

            if moveDestinations.left.nonEmpty then
              contentMigration match
                case PlainMove(elementOnTheOppositeSideToTheMoveDestination) =>
                  moveDestinations.left
                    .map(destinationElement =>
                      val resolved = resolution(
                        Some(source),
                        destinationElement,
                        elementOnTheOppositeSideToTheMoveDestination
                      )
                      destinationElement -> resolved
                    )
                    .collect {
                      case (destinationElement, resolved)
                          if destinationElement != resolved =>
                        destinationElement -> IndexedSeq(resolved)
                    }
                case ContentMigration.Edit(_, rightContent) =>
                  moveDestinations.left.map(_ -> rightContent)
                case ContentMigration.Deletion() =>
                  moveDestinations.left.map(_ -> IndexedSeq.empty)
            else if moveDestinations.right.nonEmpty then
              contentMigration match
                case PlainMove(elementOnTheOppositeSideToTheMoveDestination) =>
                  moveDestinations.right
                    .map(destinationElement =>
                      val resolved = resolution(
                        Some(source),
                        elementOnTheOppositeSideToTheMoveDestination,
                        destinationElement
                      )
                      destinationElement -> resolved
                    )
                    .collect {
                      case (destinationElement, resolved)
                          if destinationElement != resolved =>
                        destinationElement -> IndexedSeq(resolved)
                    }
                case ContentMigration.Edit(leftContent, _) =>
                  moveDestinations.right.map(_ -> leftContent)
                case ContentMigration.Deletion() =>
                  moveDestinations.right.map(_ -> IndexedSeq.empty)
            else
              assume(moveDestinations.coincident.nonEmpty)
              contentMigration match
                case ContentMigration.Deletion() =>
                  moveDestinations.coincident.flatMap {
                    case (leftDestinationElement, rightDestinationElement) =>
                      val resolved = resolution(
                        Some(source),
                        leftDestinationElement,
                        rightDestinationElement
                      )

                      Seq(
                        leftDestinationElement  -> resolved,
                        rightDestinationElement -> resolved
                      ).collect {
                        case (destinationElement, resolved)
                            if destinationElement != resolved =>
                          destinationElement -> IndexedSeq(resolved)
                      }
                  }
                case _ => Seq.empty
              end match
            end if
          else Seq.empty
          end if
        )
      )

    val anchoredMoves: Set[AnchoredMove[Element]] =
      moveDestinationsBySource.toSeq
        .flatMap((source, moveDestinations) =>
          if !moveDestinations.isDivergent
          then
            val contentMigration = speculativeMigrationsBySource(source)

            if moveDestinations.left.nonEmpty then
              contentMigration match
                case PlainMove(elementOnTheOppositeSideToTheMoveDestination) =>
                  moveDestinations.left.map(moveDestination =>
                    AnchoredMove(
                      moveDestinationSide = Side.Left,
                      moveDestinationAnchor = moveDestination,
                      oppositeSideAnchor = OppositeSideAnchor.Plain(
                        elementOnTheOppositeSideToTheMoveDestination
                      ),
                      sourceAnchor = source
                    )
                  )
                case ContentMigration.Edit(_, rightContent) =>
                  moveDestinations.left.flatMap(moveDestination =>
                    Seq(
                      AnchoredMove(
                        moveDestinationSide = Side.Left,
                        moveDestinationAnchor = moveDestination,
                        oppositeSideAnchor =
                          OppositeSideAnchor.FirstInMigratedEdit(
                            rightContent.head
                          ),
                        sourceAnchor = source
                      ),
                      AnchoredMove(
                        moveDestinationSide = Side.Left,
                        moveDestinationAnchor = moveDestination,
                        oppositeSideAnchor =
                          OppositeSideAnchor.LastInMigratedEdit(
                            rightContent.last
                          ),
                        sourceAnchor = source
                      )
                    )
                  )
                case _ =>
                  Seq.empty
            else if moveDestinations.right.nonEmpty then
              contentMigration match
                case PlainMove(elementOnTheOppositeSideToTheMoveDestination) =>
                  moveDestinations.right.map(moveDestination =>
                    AnchoredMove(
                      moveDestinationSide = Side.Right,
                      moveDestinationAnchor = moveDestination,
                      oppositeSideAnchor = OppositeSideAnchor.Plain(
                        elementOnTheOppositeSideToTheMoveDestination
                      ),
                      sourceAnchor = source
                    )
                  )
                case ContentMigration.Edit(leftContent, _) =>
                  moveDestinations.right.flatMap(moveDestination =>
                    Seq(
                      AnchoredMove(
                        moveDestinationSide = Side.Right,
                        moveDestinationAnchor = moveDestination,
                        oppositeSideAnchor =
                          OppositeSideAnchor.FirstInMigratedEdit(
                            leftContent.head
                          ),
                        sourceAnchor = source
                      ),
                      AnchoredMove(
                        moveDestinationSide = Side.Right,
                        moveDestinationAnchor = moveDestination,
                        oppositeSideAnchor =
                          OppositeSideAnchor.LastInMigratedEdit(
                            leftContent.last
                          ),
                        sourceAnchor = source
                      )
                    )
                  )
                case _ =>
                  Seq.empty
            else
              assume(moveDestinations.coincident.nonEmpty)
              /* Coincident moves are not anchors: they can't pick up insertions
               * from just one side and don't need to, because any such
               * insertions would be dealt with by the initial merge anyway. */
              Seq.empty
            end if
          else Seq.empty
          end if
        )
        .toSet

    EvaluatedMoves(
      moveDestinationsReport = MoveDestinationsReport(moveDestinationsBySource),
      migratedEditSuppressions = migratedEditSuppressions,
      substitutionsByDestination = substitutionsByDestination,
      anchoredMoves = anchoredMoves
    )
  end evaluateSpeculativeSourcesAndDestinations

  enum OppositeSideAnchor[Element]:
    def element: Element
    case Plain(element: Element)
    case FirstInMigratedEdit(element: Element)
    case LastInMigratedEdit(element: Element)
  end OppositeSideAnchor

  case class AnchoredMove[Element](
      moveDestinationSide: Side,
      moveDestinationAnchor: Element,
      oppositeSideAnchor: OppositeSideAnchor[Element],
      sourceAnchor: Element
  )

  case class EvaluatedMoves[Element](
      moveDestinationsReport: MoveDestinationsReport[Element],
      migratedEditSuppressions: Set[Element],
      substitutionsByDestination: MultiDict[Element, IndexedSeq[Element]],
      anchoredMoves: Set[AnchoredMove[Element]]
  ):
    {
      val anchorSources       = anchoredMoves.map(_.sourceAnchor)
      val anchorOppositeSides = anchoredMoves.map(_.oppositeSideAnchor)
      val anchorDestinations  = anchoredMoves.map(_.moveDestinationAnchor)
      val substitutionDestinations: collection.Set[Element] =
        substitutionsByDestination.keySet
      val substitutions       = substitutionsByDestination.values.flatten.toSet
      val allMoveDestinations = moveDestinationsReport.all
      val allMoveSources      = moveDestinationsReport.sources
      val oppositeSideAnchorsForPlainMoves = anchorOppositeSides.collect {
        case OppositeSideAnchor.Plain(element) => element
      }
      val oppositeSideAnchorsForMigratedEdits = anchorOppositeSides.collect {
        case OppositeSideAnchor.FirstInMigratedEdit(element) => element
        case OppositeSideAnchor.LastInMigratedEdit(element)  => element
      }

      require(
        anchorSources subsetOf allMoveSources,
        message =
          s"Anchor sources: ${pprintCustomised(anchorSources)}, all move sources: ${pprintCustomised(allMoveSources)}."
      )

      require(
        anchorDestinations subsetOf allMoveDestinations,
        message =
          s"Anchor destinations: ${pprintCustomised(anchorDestinations)}, all move destinations: ${pprintCustomised(allMoveDestinations)}."
      )

      require(
        substitutionDestinations subsetOf allMoveDestinations,
        message =
          s"Substitution destinations: ${pprintCustomised(substitutionDestinations)}, all move destinations: ${pprintCustomised(allMoveDestinations)}."
      )

      require(
        (migratedEditSuppressions intersect oppositeSideAnchorsForPlainMoves).isEmpty,
        message =
          s"Migrated edit suppressions: ${pprintCustomised(migratedEditSuppressions)}, opposite side elements for plain moves: ${pprintCustomised(oppositeSideAnchorsForPlainMoves)}."
      )

      require(
        oppositeSideAnchorsForMigratedEdits subsetOf migratedEditSuppressions,
        message =
          s"Migrated edit suppressions: ${pprintCustomised(migratedEditSuppressions)}, opposite side elements for migrated edits: ${pprintCustomised(oppositeSideAnchorsForMigratedEdits)}."
      )

      require(
        (substitutionDestinations intersect substitutions).isEmpty,
        message =
          s"Substitution destinations: ${pprintCustomised(substitutionDestinations)}, substitutions: ${pprintCustomised(substitutions)}."
      )
    }
  end EvaluatedMoves
end MoveDestinationsReport
