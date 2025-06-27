package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.FirstPassMergeResult.FileDeletionContext

import scala.collection.immutable.MultiDict

/** Represents the content migrated through a move.
  */
enum SpeculativeContentMigration[Element]:
  this match
    case Conflict(leftContent, rightContent, FileDeletionContext.None) =>
      require(leftContent.nonEmpty || rightContent.nonEmpty)
    case Conflict(leftContent, rightContent, FileDeletionContext.Left) =>
      require(leftContent.isEmpty && rightContent.nonEmpty)
    case Conflict(leftContent, rightContent, FileDeletionContext.Right) =>
      require(leftContent.nonEmpty && rightContent.isEmpty)
    case _ =>
  end match

  case LeftEditOrDeletion(
      opposingRightElement: Element,
      inContextOfFileDeletion: Boolean
  )
  case RightEditOrDeletion(
      opposingLeftElement: Element,
      inContextOfFileDeletion: Boolean
  )
  case Conflict(
      leftContent: IndexedSeq[Element],
      rightContent: IndexedSeq[Element],
      fileDeletionContext: FileDeletionContext
  )
  case CoincidentEditOrDeletion(fileDeletionContext: FileDeletionContext)
  case FileDeletion()
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
  def evaluateSpeculativeSourcesAndDestinations[Element: Eq](
      speculativeMigrationsBySource: Map[Element, SpeculativeContentMigration[
        Element
      ]],
      speculativeMoveDestinations: Set[SpeculativeMoveDestination[Element]]
  )(
      matchesFor: Element => collection.Set[Match[Element]]
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
                case SpeculativeContentMigration.LeftEditOrDeletion(
                      elementOnTheOppositeSideToTheMoveDestination,
                      true
                    ) =>
                  // NOTE: this complements how
                  // `FirstPassMergeResult.mergeAlgebra` deals the deletion of
                  // an entire file - it doesn't actually delete the content. So
                  // if content has actually moved elsewhere, then we really do
                  // want to suppress it in its original location.
                  IndexedSeq(elementOnTheOppositeSideToTheMoveDestination)
                case SpeculativeContentMigration.Conflict(_, rightContent, _) =>
                  rightContent
                case _ =>
                  Seq.empty
            else if moveDestinations.right.nonEmpty then
              contentMigration match
                case SpeculativeContentMigration.RightEditOrDeletion(
                      elementOnTheOppositeSideToTheMoveDestination,
                      true
                    ) =>
                  // NOTE: this complements how
                  // `FirstPassMergeResult.mergeAlgebra` deals the deletion of
                  // an entire file - it doesn't actually delete the content. So
                  // if content has actually moved elsewhere, then we really do
                  // want to suppress it in its original location.
                  IndexedSeq(elementOnTheOppositeSideToTheMoveDestination)
                case SpeculativeContentMigration.Conflict(leftContent, _, _) =>
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

    val substitutionsByDestination
        : MultiDict[MultiSided[Element], IndexedSeq[MultiSided[Element]]] =
      MultiDict.from(
        moveDestinationsBySource.toSeq.flatMap((source, moveDestinations) =>
          if !moveDestinations.isDivergent
          then
            val contentMigration = speculativeMigrationsBySource(source)

            if moveDestinations.left.nonEmpty then
              contentMigration match
                case SpeculativeContentMigration.LeftEditOrDeletion(
                      elementOnTheOppositeSideToTheMoveDestination,
                      _
                    ) =>
                  moveDestinations.left
                    .map(destinationElement =>
                      MultiSided.Unique(destinationElement) -> IndexedSeq(
                        MultiSided.Preserved(
                          baseElement = source,
                          leftElement = destinationElement,
                          rightElement =
                            elementOnTheOppositeSideToTheMoveDestination
                        )
                      )
                    )
                case SpeculativeContentMigration
                      .Conflict(_, rightContent, fileDeletionContext)
                    if fileDeletionContext != FileDeletionContext.Right || rightContent.nonEmpty =>
                  moveDestinations.left.map(destinationElement =>
                    MultiSided.Unique(destinationElement) ->
                      // The move destination is on the left, so take whatever
                      // is on the right of the conflict as being migrated - if
                      // empty, it's a deletion, otherwise it's an edit.
                      rightContent.map(MultiSided.Unique.apply)
                  )
                case SpeculativeContentMigration.CoincidentEditOrDeletion(
                      FileDeletionContext.None | FileDeletionContext.Left
                    ) =>
                  moveDestinations.left.map(destinationElement =>
                    MultiSided.Unique(destinationElement) ->
                      // The move destination is on the left, so migrate the
                      // implied deletion on the right.
                      IndexedSeq.empty
                  )
                case _ => Seq.empty
            else if moveDestinations.right.nonEmpty then
              contentMigration match
                case SpeculativeContentMigration.RightEditOrDeletion(
                      elementOnTheOppositeSideToTheMoveDestination,
                      _
                    ) =>
                  moveDestinations.right
                    .map(destinationElement =>
                      MultiSided.Unique(destinationElement) -> IndexedSeq(
                        MultiSided.Preserved(
                          baseElement = source,
                          leftElement =
                            elementOnTheOppositeSideToTheMoveDestination,
                          rightElement = destinationElement
                        )
                      )
                    )
                case SpeculativeContentMigration
                      .Conflict(leftContent, _, fileDeletionContext)
                    if fileDeletionContext != FileDeletionContext.Left || leftContent.nonEmpty =>
                  moveDestinations.right.map(destinationElement =>
                    MultiSided.Unique(destinationElement) ->
                      // The move destination is on the right, so take whatever
                      // is on the left of the conflict as being migrated - if
                      // empty, it's a deletion, otherwise it's an edit.
                      leftContent.map(MultiSided.Unique.apply)
                  )
                case SpeculativeContentMigration.CoincidentEditOrDeletion(
                      FileDeletionContext.None | FileDeletionContext.Right
                    ) =>
                  moveDestinations.right.map(destinationElement =>
                    MultiSided.Unique(destinationElement) ->
                      // The move destination is on the right, so migrate the
                      // implied deletion on the left.
                      IndexedSeq.empty
                  )
                case _ => Seq.empty
            else
              assume(moveDestinations.coincident.nonEmpty)
              contentMigration match
                case SpeculativeContentMigration.CoincidentEditOrDeletion(_) |
                    SpeculativeContentMigration.FileDeletion() =>
                  // NOTE: we don't distinguish between a deletion due to a
                  // merge or due to the removal of a path; either way the
                  // content has made a coincident move.
                  moveDestinations.coincident
                    .map {
                      case (leftDestinationElement, rightDestinationElement) =>
                        MultiSided.Coincident(
                          leftDestinationElement,
                          rightDestinationElement
                        ) -> IndexedSeq(
                          MultiSided.Preserved(
                            baseElement = source,
                            leftElement = leftDestinationElement,
                            rightElement = rightDestinationElement
                          )
                        )
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
                case SpeculativeContentMigration.LeftEditOrDeletion(
                      elementOnTheOppositeSideToTheMoveDestination,
                      _
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
                      .Conflict(_, IndexedSeq(loneRightElement), _) =>
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
                case SpeculativeContentMigration.Conflict(_, rightContent, _)
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
                case SpeculativeContentMigration.RightEditOrDeletion(
                      elementOnTheOppositeSideToTheMoveDestination,
                      _
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
                      .Conflict(IndexedSeq(loneLeftElement), _, _) =>
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
                case SpeculativeContentMigration.Conflict(leftContent, _, _)
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
      substitutionsByDestination: MultiDict[MultiSided[Element], IndexedSeq[
        MultiSided[Element]
      ]],
      anchoredMoves: Set[AnchoredMove[Element]]
  ):
    {
      val anchorSources       = anchoredMoves.map(_.sourceAnchor)
      val anchorOppositeSides = anchoredMoves.map(_.oppositeSideAnchor)
      val anchorDestinations  = anchoredMoves.map(_.moveDestinationAnchor)
      val substitutionDestinations: collection.Set[Element] =
        substitutionsByDestination.keySet.flatMap {
          case MultiSided.Unique(uniqueDestination) => Seq(uniqueDestination)
          case MultiSided.Coincident(leftDestination, rightDestination) =>
            Seq(leftDestination, rightDestination)
          case MultiSided.Preserved(_, leftDestination, rightDestination) =>
            Seq(leftDestination, rightDestination)
        }
      val allMoveDestinations                 = moveDestinationsReport.all
      val allMoveSources                      = moveDestinationsReport.sources
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
        oppositeSideAnchorsForMigratedEdits subsetOf migratedEditSuppressions,
        message =
          s"Migrated edit suppressions: ${pprintCustomised(migratedEditSuppressions)}, opposite side elements for migrated edits: ${pprintCustomised(oppositeSideAnchorsForMigratedEdits)}."
      )
    }
  end MoveEvaluation
end MoveDestinationsReport
