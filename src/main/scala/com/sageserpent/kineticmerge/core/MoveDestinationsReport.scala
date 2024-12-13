package com.sageserpent.kineticmerge.core

import scala.collection.immutable.MultiDict

/** Represents the content migrated through a move.
  */
enum SpeculativeContentMigration[Element]:
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
end SpeculativeContentMigration

enum SpeculativeMoveDestination[Element]:
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
      speculativeMigrationsBySource: Map[Element, SpeculativeContentMigration[
        Element
      ]],
      speculativeMoveDestinations: Set[SpeculativeMoveDestination[Element]]
  )(
      matchesFor: Element => collection.Set[Match[Element]],
      resolution: Resolution[Element]
  ): MoveEvaluation[Element] =
    val destinationsBySource =
      MultiDict.from(speculativeMigrationsBySource.keys.flatMap {
        speculativeSource =>
          val destinations = matchesFor(speculativeSource)
            .flatMap {
              case Match.AllSides(_, leftElement, rightElement) =>
                Seq(
                  SpeculativeMoveDestination.Left(leftElement),
                  SpeculativeMoveDestination.Right(rightElement),
                  SpeculativeMoveDestination
                    .Coincident(leftElement, rightElement)
                )
              case Match.BaseAndLeft(_, leftElement) =>
                Seq(SpeculativeMoveDestination.Left(leftElement))
              case Match.BaseAndRight(_, rightElement) =>
                Seq(SpeculativeMoveDestination.Right(rightElement))
              case _: Match.LeftAndRight[Element] => Seq.empty
            }
            .intersect(speculativeMoveDestinations)

          destinations.map(speculativeSource -> _)
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
                case SpeculativeContentMigration.Edit(_, rightContent) =>
                  rightContent
                case _ =>
                  Seq.empty
            else if moveDestinations.right.nonEmpty then
              contentMigration match
                case SpeculativeContentMigration.Edit(leftContent, _) =>
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
                case SpeculativeContentMigration.PlainMove(
                      elementOnTheOppositeSideToTheMoveDestination
                    ) =>
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
                case SpeculativeContentMigration.Edit(_, rightContent) =>
                  moveDestinations.left.map(_ -> rightContent)
                case SpeculativeContentMigration.Deletion() =>
                  moveDestinations.left.map(_ -> IndexedSeq.empty)
            else if moveDestinations.right.nonEmpty then
              contentMigration match
                case SpeculativeContentMigration.PlainMove(
                      elementOnTheOppositeSideToTheMoveDestination
                    ) =>
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
                case SpeculativeContentMigration.Edit(leftContent, _) =>
                  moveDestinations.right.map(_ -> leftContent)
                case SpeculativeContentMigration.Deletion() =>
                  moveDestinations.right.map(_ -> IndexedSeq.empty)
            else
              assume(moveDestinations.coincident.nonEmpty)
              contentMigration match
                case SpeculativeContentMigration.Deletion() =>
                  moveDestinations.coincident
                    .map {
                      case (leftDestinationElement, rightDestinationElement) =>
                        val resolved = resolution(
                          Some(source),
                          leftDestinationElement,
                          rightDestinationElement
                        )

                        // NASTY HACK: this relies on external context, namely
                        // that `CoreMergeAlgebra` has made a two-way resolution
                        // that has turned out to be wrong in the light of
                        // discovering a coincident move.
                        val inaccuratelyResolvedBecauseOfCoreMerge = resolution(
                          None,
                          leftDestinationElement,
                          rightDestinationElement
                        )

                        inaccuratelyResolvedBecauseOfCoreMerge -> resolved
                    }
                    .collect {
                      case (destinationElement, resolved)
                          if destinationElement != resolved =>
                        destinationElement -> IndexedSeq(resolved)
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
                case SpeculativeContentMigration.PlainMove(
                      elementOnTheOppositeSideToTheMoveDestination
                    ) =>
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
                case SpeculativeContentMigration
                      .Edit(_, IndexedSeq(loneRightElement)) =>
                  moveDestinations.left.map(moveDestination =>
                    AnchoredMove(
                      moveDestinationSide = Side.Left,
                      moveDestinationAnchor = moveDestination,
                      oppositeSideAnchor =
                        OppositeSideAnchor.OnlyOneInMigratedEdit(
                          loneRightElement
                        ),
                      sourceAnchor = source
                    )
                  )
                case SpeculativeContentMigration.Edit(_, rightContent)
                    // NOTE: have to be careful here, as both a migrated
                    // deletion with an edit on the same side as the move and a
                    // migrated edit look like an edit versus deletion conflict.
                    // If the content on the *opposite* side of the move is
                    // empty, it is the former situation; we handle that with
                    // the default.
                    if rightContent.nonEmpty =>
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
                case SpeculativeContentMigration.PlainMove(
                      elementOnTheOppositeSideToTheMoveDestination
                    ) =>
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
                case SpeculativeContentMigration
                      .Edit(IndexedSeq(loneLeftElement), _) =>
                  moveDestinations.right.map(moveDestination =>
                    AnchoredMove(
                      moveDestinationSide = Side.Right,
                      moveDestinationAnchor = moveDestination,
                      oppositeSideAnchor =
                        OppositeSideAnchor.OnlyOneInMigratedEdit(
                          loneLeftElement
                        ),
                      sourceAnchor = source
                    )
                  )
                case SpeculativeContentMigration.Edit(leftContent, _)
                    // NOTE: have to be careful here, as both a migrated
                    // deletion with an edit on the same side as the move and a
                    // migrated edit look like an edit versus deletion conflict.
                    // If the content on the *opposite* side of the move is
                    // empty, it is the former situation; we handle that with
                    // the default.
                    if leftContent.nonEmpty =>
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

    MoveEvaluation(
      moveDestinationsReport = MoveDestinationsReport(moveDestinationsBySource),
      migratedEditSuppressions = migratedEditSuppressions,
      substitutionsByDestination = substitutionsByDestination,
      anchoredMoves = anchoredMoves
    )
  end evaluateSpeculativeSourcesAndDestinations

  enum OppositeSideAnchor[Element]:
    def element: Element
    case Plain(element: Element)
    case OnlyOneInMigratedEdit(element: Element)
    case FirstInMigratedEdit(element: Element)
    case LastInMigratedEdit(element: Element)
  end OppositeSideAnchor

  /** Represents a move that is anchored; this means there is an anchor element
    * on all three sides of the overall merge that can be used to merge
    * surrounding content on each side to make a migration splice. This is *not*
    * the same as an all-sides match, although there is some overlap between the
    * two concepts.<p>For one thing, not all all-sides matches turn out to be
    * moves - they are usually just straight preservations in the per-file
    * initial merges. The other thing is that migrated edits are also moves with
    * the edit standing in as an anchor; this is particularly useful as it keeps
    * migrated edits out of the anchored runs.
    * @param moveDestinationSide
    * @param moveDestinationAnchor
    * @param oppositeSideAnchor
    * @param sourceAnchor
    * @tparam Element
    */
  case class AnchoredMove[Element](
      moveDestinationSide: Side,
      moveDestinationAnchor: Element,
      oppositeSideAnchor: OppositeSideAnchor[Element],
      sourceAnchor: Element
  )

  case class MoveEvaluation[Element](
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
      val allMoveDestinations = moveDestinationsReport.all
      val allMoveSources      = moveDestinationsReport.sources
      val oppositeSideAnchorsForPlainMoves = anchorOppositeSides.collect {
        case OppositeSideAnchor.Plain(element) => element
      }
      val oppositeSideAnchorsForMigratedEdits = anchorOppositeSides.collect {
        case OppositeSideAnchor.OnlyOneInMigratedEdit(element) => element
        case OppositeSideAnchor.FirstInMigratedEdit(element)   => element
        case OppositeSideAnchor.LastInMigratedEdit(element)    => element
      }

      // NOTE: don't be tempted to assert that substitution destinations and
      // their substitutions form disjoint sets - that assumption does not hold
      // when edits are forwarded.

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

      substitutionsByDestination.foreach {
        case (substitutionDestination, substitution) =>
          require(
            !substitution.contains(substitutionDestination),
            s"Substitution destination: ${pprintCustomised(substitutionDestination)}, substitution: ${pprintCustomised(substitution)}."
          )
      }

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
    }
  end MoveEvaluation
end MoveDestinationsReport
