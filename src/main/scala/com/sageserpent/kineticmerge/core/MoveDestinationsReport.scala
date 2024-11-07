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
  def summarizeInText: Iterable[String] =
    moveDestinationsBySources.map((source, moveDestinations) =>
      moveDestinations.description(source)
    )
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

    val destinationPatchesBySource
        : Map[Element, Element => IndexedSeq[Element]] =
      moveDestinationsBySource.flatMap((source, moveDestinations) =>
        (if !moveDestinations.isDivergent && moveDestinations.coincident.isEmpty
         then
           val contentMigration = speculativeMigrationsBySource(source)

           if moveDestinations.left.nonEmpty then
             contentMigration match
               case PlainMove(elementOnTheOppositeSideToTheMoveDestination) =>
                 Some((destinationElement: Element) =>
                   IndexedSeq(
                     resolution(
                       Some(source),
                       destinationElement,
                       elementOnTheOppositeSideToTheMoveDestination
                     )
                   )
                 )
               case ContentMigration.Edit(_, rightContent) =>
                 Some((_: Element) => rightContent)
               case ContentMigration.Deletion() =>
                 Some((_: Element) => IndexedSeq.empty)
           else if moveDestinations.right.nonEmpty then
             contentMigration match
               case PlainMove(elementOnTheOppositeSideToTheMoveDestination) =>
                 Some((destinationElement: Element) =>
                   IndexedSeq(
                     resolution(
                       Some(source),
                       elementOnTheOppositeSideToTheMoveDestination,
                       destinationElement
                     )
                   )
                 )
               case ContentMigration.Edit(leftContent, _) =>
                 Some((_: Element) => leftContent)
               case ContentMigration.Deletion() =>
                 Some((_: Element) => IndexedSeq.empty)
           else
             // TODO: this case should be unreachable at runtime, given we've
             // already excluded having coincident destinations.
             None
           end if
         else None)
        .map(source -> _)
      )

    EvaluatedMoves(
      moveDestinationsReport = MoveDestinationsReport(moveDestinationsBySource),
      destinationPatchesBySource = destinationPatchesBySource
    )
  end evaluateSpeculativeSourcesAndDestinations

  case class EvaluatedMoves[Element](
      moveDestinationsReport: MoveDestinationsReport[Element],
      destinationPatchesBySource: Map[Element, Element => IndexedSeq[Element]]
  )
end MoveDestinationsReport
