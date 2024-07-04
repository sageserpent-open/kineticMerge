package com.sageserpent.kineticmerge.core

import cats.{Eq, Order}
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Sized
import com.sageserpent.kineticmerge.core.merge.of as mergeOf
import com.typesafe.scalalogging.StrictLogging
import monocle.syntax.all.*

import scala.collection.Searching
import scala.collection.immutable.MultiDict

object CodeMotionAnalysisExtension extends StrictLogging:

  /** Add merging capability to a [[CodeMotionAnalysis]].
    *
    * Not sure exactly where this capability should be implemented - is it
    * really a core part of the API for [[CodeMotionAnalysis]]? Hence the
    * extension as a temporary measure.
    */

  extension [Path, Element: Eq: Order](
      codeMotionAnalysis: CodeMotionAnalysis[Path, Element]
  )
    def merge: (
        Map[Path, MergeResult[Element]],
        MatchesContext[Section[Element]]#MoveDestinationsReport
    ) =
      def dominantsOf(
          section: Section[Element]
      ): collection.Set[Section[Element]] =
        codeMotionAnalysis
          .matchesFor(section)
          .map(_.dominantElement)

      given Eq[Section[Element]] with
        /** This is most definitely *not* [[Section.equals]] - we want to
          * compare the underlying content of the dominant sections, as the
          * sections are expected to come from *different* sides.
          * [[Section.equals]] is expected to consider sections from different
          * sides as unequal. <p>If neither section is involved in a match, fall
          * back to comparing the contents; this is vital for comparing sections
          * that would have been part of a larger match if not for that match
          * not achieving the threshold size.
          */
        override def eqv(
            lhs: Section[Element],
            rhs: Section[Element]
        ): Boolean =
          val bothBelongToTheSameMatches =
            dominantsOf(lhs).intersect(dominantsOf(rhs)).nonEmpty

          bothBelongToTheSameMatches || Eq[Seq[Element]]
            .eqv(lhs.content, rhs.content)
        end eqv
      end given

      val matchesContext = MatchesContext(
        codeMotionAnalysis.matchesFor
      )

      import matchesContext.*

      val paths =
        codeMotionAnalysis.base.keySet ++ codeMotionAnalysis.left.keySet ++ codeMotionAnalysis.right.keySet

      case class InsertionsAtPath(path: Path, insertions: Seq[Insertion])

      val (
        mergeResultsByPath,
        changesMigratedThroughMotion,
        moveDestinationsReport,
        insertionsAtPath
      ) =
        paths.foldLeft(
          Map.empty[Path, MergeResult[Section[Element]]],
          Iterable.empty[
            (
                Section[Element],
                IndexedSeq[Section[Element]]
            )
          ],
          emptyReport,
          Vector.empty[InsertionsAtPath]
        ) {
          case (
                (
                  mergeResultsByPath,
                  changesMigratedThroughMotion,
                  moveDestinationsReport,
                  insertionsAtPath
                ),
                path
              ) =>
            val base  = codeMotionAnalysis.base.get(path).map(_.sections)
            val left  = codeMotionAnalysis.left.get(path).map(_.sections)
            val right = codeMotionAnalysis.right.get(path).map(_.sections)

            (base, left, right) match
              case (None, Some(leftSections), None) =>
                // File added only on the left; pass through as there is neither
                // anything to merge nor any sources of edits or deletions...
                (
                  mergeResultsByPath + (path -> FullyMerged(
                    leftSections
                  )),
                  changesMigratedThroughMotion,
                  leftSections.foldLeft(moveDestinationsReport)(
                    _.leftMoveOf(_)
                  ),
                  insertionsAtPath
                )
              case (None, None, Some(rightSections)) =>
                // File added only on the right; pass through as there is
                // neither anything to merge nor any sources of edits or
                // deletions...
                (
                  mergeResultsByPath + (path -> FullyMerged(
                    rightSections
                  )),
                  changesMigratedThroughMotion,
                  rightSections.foldLeft(moveDestinationsReport)(
                    _.rightMoveOf(_)
                  ),
                  insertionsAtPath
                )
              case (
                    optionalBaseSections,
                    optionalLeftSections,
                    optionalRightSections
                  ) =>
                // Mix of possibilities - the file may have been added on both
                // sides, or modified on either or both sides, or deleted on one
                // side and modified on the other, or deleted on one or both
                // sides. There is also an extraneous case where there is no
                // file on any of the sides, and another extraneous case where
                // the file is on all three sides but hasn't changed.

                // Whichever is the case, merge...

                given Sized[Section[Element]] = _.size

                val mergedSectionsResult
                    : MergeResultDetectingMotion[MergeResult, Section[
                      Element
                    ]] =
                  mergeOf(mergeAlgebra =
                    MergeResultDetectingMotion.mergeAlgebra(coreMergeAlgebra =
                      MergeResult.mergeAlgebra(???)
                    )
                  )(
                    base = optionalBaseSections.getOrElse(IndexedSeq.empty),
                    left = optionalLeftSections.getOrElse(IndexedSeq.empty),
                    right = optionalRightSections.getOrElse(IndexedSeq.empty)
                  )

                (
                  mergeResultsByPath + (path -> mergedSectionsResult.coreMergeResult),
                  changesMigratedThroughMotion ++ mergedSectionsResult.changesMigratedThroughMotion,
                  moveDestinationsReport.mergeWith(
                    mergedSectionsResult.moveDestinationsReport
                  ),
                  insertionsAtPath :+ InsertionsAtPath(
                    path,
                    mergedSectionsResult.insertions
                  )
                )
            end match
        }

      def isMoveDestinationOnGivenSide(
          section: Section[Element],
          side: matchesContext.Side,
          moveDestinations: MoveDestinations[Section[Element]]
      ) =
        side match
          case Side.Left =>
            moveDestinations.left.contains(
              section
            ) || moveDestinations.coincident.exists { case (leftPart, _) =>
              section == leftPart
            }
          case Side.Right =>
            moveDestinations.right
              .contains(section) || moveDestinations.coincident.exists {
              case (_, rightPart) => section == rightPart
            }

      val migrationOrdering: Ordering[Seq[Section[Element]]] =
        Ordering.Implicits.seqOrdering(
          Ordering.by[Section[Element], IndexedSeq[Element]](_.content)(
            Ordering.Implicits.seqOrdering(summon[Eq[Element]].toOrdering)
          )
        )

      def uniqueMigrations[Sequence[Item] <: Seq[Item]](
          migratedChanges: collection.Set[Sequence[Section[Element]]]
      ) =
        require(migratedChanges.nonEmpty)

        val migratedChangesSortedByContent =
          migratedChanges.toSeq.sorted(migrationOrdering)

        val uniqueMigratedChanges =
          migratedChangesSortedByContent.tail.foldLeft(
            List(migratedChangesSortedByContent.head)
          ) { case (partialResult @ head :: _, change) =>
            if 0 == migrationOrdering.compare(head, change) then partialResult
            else change :: partialResult
          }

        assume(uniqueMigratedChanges.nonEmpty)

        uniqueMigratedChanges
      end uniqueMigrations

      enum Anchoring:
        case Predecessor
        case Successor
      end Anchoring

      val migratedInsertionsByAnchorDestinations
          : MultiDict[(Section[Element], Anchoring), Seq[
            Section[Element]
          ]] =
        MultiDict.from(insertionsAtPath.flatMap {
          case InsertionsAtPath(path, insertions) =>
            val insertionsThatAreNotMoveDestinations = insertions.filterNot {
              case Insertion(side, inserted) =>
                val dominants = dominantsOf(inserted)
                moveDestinationsReport.moveDestinationsByDominantSet
                  .get(dominants)
                  .fold(ifEmpty = false)(
                    isMoveDestinationOnGivenSide(inserted, side, _)
                  )
            }

            case class InsertionRun(
                side: Side,
                contiguousInsertions: Seq[Section[Element]]
            ):
              require(contiguousInsertions.nonEmpty)
            end InsertionRun

            val (partialResult, insertionRun) =
              insertionsThatAreNotMoveDestinations
                .foldLeft(
                  Vector.empty[InsertionRun],
                  None: Option[InsertionRun]
                ) {
                  case (
                        (
                          partialResult,
                          insertionRun
                        ),
                        insertion @ Insertion(side, inserted)
                      ) =>
                    insertionRun match
                      case Some(InsertionRun(previousSide, previouslyInserted))
                          if previousSide == side && previouslyInserted.last.onePastEndOffset == inserted.startOffset =>
                        partialResult -> insertionRun
                          .focus(_.some.contiguousInsertions)
                          .modify(_ :+ inserted)
                      case _ =>
                        (partialResult ++ insertionRun) -> Some(
                          InsertionRun(
                            side = side,
                            contiguousInsertions = Vector(inserted)
                          )
                        )
                }

            val insertionRuns = partialResult ++ insertionRun

            // NOTE: the same insertion may not only be associated with multiple
            // anchor destinations due to ambiguous matches; it may also be
            // flanked on either side by anchor destinations. Hence the use of
            // `Anchoring` to track whether the anchor precedes or succeeds the
            // insertion.

            insertionRuns.flatMap {
              case InsertionRun(side, contiguousInsertions) =>
                val file = side match
                  case Side.Left =>
                    codeMotionAnalysis.left(path)
                  case Side.Right =>
                    codeMotionAnalysis.right(path)

                {
                  def destinationsForValidAnchor(
                      anchor: Section[Element]
                  ): collection.Set[Section[Element]] =
                    val dominants = dominantsOf(anchor)
                    moveDestinationsReport.moveDestinationsByDominantSet
                      .get(dominants)
                      .fold(ifEmpty = Set.empty)(moveDestinations =>
                        if !isMoveDestinationOnGivenSide(
                            anchor,
                            side,
                            moveDestinations
                          )
                        then
                          side match
                            case Side.Left  => moveDestinations.right
                            case Side.Right => moveDestinations.left
                        else Set.empty
                      )
                  end destinationsForValidAnchor

                  val Searching.Found(indexOfLeadingInsertedSection) =
                    file.searchByStartOffset(
                      contiguousInsertions.head.startOffset
                    ): @unchecked

                  val predecessorAnchorDestinations = Option
                    .when(0 < indexOfLeadingInsertedSection)(
                      file.sections(indexOfLeadingInsertedSection - 1)
                    )
                    .flatMap(destinationsForValidAnchor)

                  val onePastIndex =
                    contiguousInsertions.size + indexOfLeadingInsertedSection
                  val successorAnchorDestinations = Option
                    .when(file.sections.length > onePastIndex)(
                      file.sections(onePastIndex)
                    )
                    .flatMap(destinationsForValidAnchor)

                  predecessorAnchorDestinations.map(
                    _ -> Anchoring.Predecessor
                  ) ++ successorAnchorDestinations.map(_ -> Anchoring.Successor)
                }.map(_ -> contiguousInsertions)
            }
        })

      val migratedInsertions =
        migratedInsertionsByAnchorDestinations.values.flatten.toSet

      def migrateAnchoredInsertions(
          path: Path,
          mergeResult: MergeResult[Section[Element]]
      ): (Path, MergeResult[Section[Element]]) =
        // Remove migrated insertions from their original location, and patch
        // them in relative to their anchor destinations. Need to watch out for
        // when the same insertion is patched in after a predecessor anchor and
        // then patched in before a successor anchor - the second patch should
        // be suppressed.

        def removeMigratedInsertions(
            sections: IndexedSeq[Section[Element]]
        ): IndexedSeq[Section[Element]] =
          sections.filterNot(migratedInsertions.contains)

        def insertOrDeferAnyMigratedInsertions(
            sections: IndexedSeq[Section[Element]]
        ): IndexedSeq[Section[Element]] =
          sections
            .foldLeft(
              IndexedSeq
                .empty[Section[Element]] -> (None: Option[
                Seq[Section[Element]]
              ])
            ) {
              case (
                    (partialResult, deferredMigratedInsertion),
                    candidateAnchorDestination
                  ) =>
                val precedingMigratedInsertions =
                  migratedInsertionsByAnchorDestinations.get(
                    candidateAnchorDestination -> Anchoring.Successor
                  )

                val precedingMigratedInsertion =
                  Option.when(precedingMigratedInsertions.nonEmpty) {
                    val uniqueMigratedInsertions =
                      uniqueMigrations(precedingMigratedInsertions)

                    uniqueMigratedInsertions match
                      case head :: Nil => head
                      case _ =>
                        throw new RuntimeException(
                          s"""
                           |Multiple potential insertions migrated before destination: $candidateAnchorDestination,
                           |these are:
                           |${uniqueMigratedInsertions
                              .map(insertion => s"PRE-INSERTION: $insertion")
                              .zipWithIndex
                              .map((insertion, index) =>
                                s"${1 + index}. $insertion"
                              )
                              .mkString("\n")}
                           |These are from ambiguous matches of anchor text with the destination.
                           |Consider setting the command line parameter `--minimum-ambiguous-match-size` to something larger than ${candidateAnchorDestination.size}.
                              """.stripMargin
                        )
                    end match
                  }

                val succeedingMigratedInsertions =
                  migratedInsertionsByAnchorDestinations.get(
                    candidateAnchorDestination -> Anchoring.Predecessor
                  )

                val succeedingMigratedInsertion =
                  Option.when(succeedingMigratedInsertions.nonEmpty) {
                    val uniqueMigratedInsertions =
                      uniqueMigrations(succeedingMigratedInsertions)

                    uniqueMigratedInsertions match
                      case head :: Nil => head
                      case _ =>
                        throw new RuntimeException(
                          s"""
                             |Multiple potential insertions migrated after destination: $candidateAnchorDestination,
                             |these are:
                             |${uniqueMigratedInsertions
                              .map(insertion => s"POST-INSERTION: $insertion")
                              .zipWithIndex
                              .map((insertion, index) =>
                                s"${1 + index}. $insertion"
                              )
                              .mkString("\n")}
                             |These are from ambiguous matches of anchor text with the destination.
                             |Consider setting the command line parameter `--minimum-ambiguous-match-size` to something larger than ${candidateAnchorDestination.size}.
                              """.stripMargin
                        )
                    end match
                  }

                val result =
                  (deferredMigratedInsertion, precedingMigratedInsertion) match
                    case (None, None) =>
                      partialResult :+ candidateAnchorDestination
                    case (Some(deferred), None) =>
                      partialResult ++ deferred :+ candidateAnchorDestination
                    case (None, Some(preceding)) =>
                      partialResult ++ preceding :+ candidateAnchorDestination
                    case (Some(deferred), Some(preceding)) =>
                      if deferred == preceding then
                        partialResult ++ deferred :+ candidateAnchorDestination
                      else
                        partialResult ++ deferred ++ preceding :+ candidateAnchorDestination

                result -> succeedingMigratedInsertion
            } match
            case (partialResult, deferredMigratedInsertion) =>
              deferredMigratedInsertion.fold(ifEmpty = partialResult)(
                partialResult ++ _
              )

        path -> (mergeResult match
          case FullyMerged(sections) =>
            FullyMerged(elements =
              insertOrDeferAnyMigratedInsertions(
                removeMigratedInsertions(sections)
              )
            )
          case MergedWithConflicts(leftSections, rightSections) =>
            MergedWithConflicts(
              insertOrDeferAnyMigratedInsertions(
                removeMigratedInsertions(leftSections)
              ),
              insertOrDeferAnyMigratedInsertions(
                removeMigratedInsertions(rightSections)
              )
            )
        )
      end migrateAnchoredInsertions

      val potentialValidDestinationsForMigratingChangesTo =
        moveDestinationsReport.moveDestinationsByDominantSet.values
          .filterNot(moveDestinations =>
            moveDestinations.isDegenerate || moveDestinations.isDivergent
          )
          .flatMap(moveDestinations =>
            // NOTE: coincident move destinations can't pick up edits as there
            // would be no side to contribute the edit; instead, both of them
            // would contribute a move.
            moveDestinations.left ++ moveDestinations.right
          )
          .toSet

      val vettedChangesMigratedThroughMotion =
        MultiDict.from(changesMigratedThroughMotion.filter {
          case (potentialDestination, _) =>
            potentialValidDestinationsForMigratingChangesTo.contains(
              potentialDestination
            )
        })

      def substituteMigratedChangesOrDominants(
          path: Path,
          mergeResult: MergeResult[Section[Element]]
      ): (Path, MergeResult[Section[Element]]) =
        def substituteFor(
            section: Section[Element]
        ): IndexedSeq[Section[Element]] =
          val migratedChanges = vettedChangesMigratedThroughMotion
            .get(section)

          if migratedChanges.nonEmpty then
            val uniqueMigratedChanges = uniqueMigrations(migratedChanges)

            val migratedChange: IndexedSeq[Section[Element]] =
              uniqueMigratedChanges match
                case head :: Nil => head
                case _ =>
                  throw new RuntimeException(
                    s"""
                       |Multiple potential changes migrated to destination: $section,
                       |these are:
                       |${uniqueMigratedChanges
                        .map(change =>
                          if change.isEmpty then "DELETION"
                          else s"EDIT: $change"
                        )
                        .zipWithIndex
                        .map((change, index) => s"${1 + index}. $change")
                        .mkString("\n")}
                       |These are from ambiguous matches of text with the destination.
                       |Consider setting the command line parameter `--minimum-ambiguous-match-size` to something larger than ${section.size}.
                        """.stripMargin
                  )

            // There is no need to look for the dominant - either the section
            // was deleted or edited; matched sections are not considered as
            // edit candidates.

            if migratedChange.isEmpty then
              logger.debug(
                s"Applying migrated deletion to move destination: ${pprintCustomised(section)}."
              )
            else
              logger.debug(
                s"Applying migrated edit into ${pprintCustomised(migratedChange)} to move destination: ${pprintCustomised(section)}."
              )
            end if

            migratedChange
          else
            val dominants = dominantsOf(section)

            IndexedSeq(
              if dominants.isEmpty then section
              else
                // NASTY HACK: this is hokey, but essentially correct - if we
                // have ambiguous matches leading to multiple dominants, then
                // they're all just as good in terms of their content. So just
                // choose any one.
                dominants.head
            )
          end if
        end substituteFor

        path -> (mergeResult match
          case FullyMerged(sections) =>
            FullyMerged(elements = sections.flatMap(substituteFor))
          case MergedWithConflicts(leftSections, rightSections) =>
            MergedWithConflicts(
              leftSections.flatMap(substituteFor),
              rightSections.flatMap(substituteFor)
            )
        )
      end substituteMigratedChangesOrDominants

      def explodeSections(
          path: Path,
          mergeResult: MergeResult[Section[Element]]
      ): (Path, MergeResult[Element]) =
        path -> (mergeResult match
          case FullyMerged(sections) =>
            FullyMerged(elements = sections.flatMap(_.content))
          case MergedWithConflicts(leftSections, rightSections) =>
            val leftElements  = leftSections.flatMap(_.content)
            val rightElements = rightSections.flatMap(_.content)

            // Just in case the conflict was resolved by the migrated
            // changes...
            if leftElements.corresponds(
                rightElements
              )(summon[Eq[Element]].eqv)
            then FullyMerged(leftElements)
            else
              MergedWithConflicts(
                leftElements,
                rightElements
              )
            end if
        )
      end explodeSections

      mergeResultsByPath
        .map(migrateAnchoredInsertions)
        .map(substituteMigratedChangesOrDominants)
        .map(explodeSections) -> moveDestinationsReport
    end merge
  end extension
end CodeMotionAnalysisExtension
