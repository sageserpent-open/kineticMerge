package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.merge.of as mergeOf
import com.typesafe.scalalogging.StrictLogging

import scala.collection.Searching
import scala.collection.immutable.MultiDict

object CodeMotionAnalysisExtension extends StrictLogging:

  /** Add merging capability to a [[CodeMotionAnalysis]].
    *
    * Not sure exactly where this capability should be implemented - is it
    * really a core part of the API for [[CodeMotionAnalysis]]? Hence the
    * extension as a temporary measure.
    */

  extension [Path, Element](
      codeMotionAnalysis: CodeMotionAnalysis[Path, Element]
  )
    def merge(
        equality: Eq[Element]
    ): (
        Map[Path, MergeResult[Element]],
        MatchesContext[Section[Element]]#MoveDestinationsReport
    ) =
      def dominantsOf(
          section: Section[Element]
      ): collection.Set[Section[Element]] =
        codeMotionAnalysis
          .matchesFor(section)
          .map(_.dominantElement)

      /** This is most definitely *not* [[Section.equals]] - we want to compare
        * the underlying content of the dominant sections, as the sections are
        * expected to come from *different* sides. [[Section.equals]] is
        * expected to consider sections from different sides as unequal. <p>If
        * neither section is involved in a match, fall back to comparing the
        * contents; this is vital for comparing sections that would have been
        * part of a larger match if not for that match not achieving the
        * threshold size.
        */
      def sectionEqualityViaDominantsFallingBackToContentComparison(
          lhs: Section[Element],
          rhs: Section[Element]
      ): Boolean =
        val bothBelongToTheSameMatches =
          dominantsOf(lhs).intersect(dominantsOf(rhs)).nonEmpty

        bothBelongToTheSameMatches || {
          given Eq[Element] = equality

          Eq[Seq[Element]].eqv(lhs.content, rhs.content)
        }
      end sectionEqualityViaDominantsFallingBackToContentComparison

      val matchesContext = MatchesContext(
        codeMotionAnalysis.matchesFor
      )

      import matchesContext.*

      val paths =
        codeMotionAnalysis.base.keySet ++ codeMotionAnalysis.left.keySet ++ codeMotionAnalysis.right.keySet

      val (
        mergeResultsByPath,
        changesPropagatedThroughMotion,
        moveDestinationsReport,
        insertionsAtPath
      ) =
        paths.foldLeft(
          Map.empty[Path, MergeResult[Section[Element]]],
          Iterable.empty[
            (
                Section[Element],
                Option[
                  Section[Element]
                ]
            )
          ],
          emptyReport,
          MultiDict.empty[Path, Insertion]
        ) {
          case (
                (
                  mergeResultsByPath,
                  changesPropagatedThroughMotion,
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
                  changesPropagatedThroughMotion,
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
                  changesPropagatedThroughMotion,
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
                val mergedSectionsResult
                    : MergeResultDetectingMotion[MergeResult, Section[
                      Element
                    ]] =
                  mergeOf(mergeAlgebra =
                    MergeResultDetectingMotion.mergeAlgebra(coreMergeAlgebra =
                      MergeResult.mergeAlgebra
                    )
                  )(
                    base = optionalBaseSections.getOrElse(IndexedSeq.empty),
                    left = optionalLeftSections.getOrElse(IndexedSeq.empty),
                    right = optionalRightSections.getOrElse(IndexedSeq.empty)
                  )(
                    equality =
                      sectionEqualityViaDominantsFallingBackToContentComparison,
                    elementSize = _.size
                  )

                (
                  mergeResultsByPath + (path -> mergedSectionsResult.coreMergeResult),
                  changesPropagatedThroughMotion ++ mergedSectionsResult.changesPropagatedThroughMotion,
                  moveDestinationsReport.mergeWith(
                    mergedSectionsResult.moveDestinationsReport
                  ),
                  mergedSectionsResult.insertions.foldLeft(insertionsAtPath)(
                    (insertionsAtPath, insertion) =>
                      insertionsAtPath + (path -> insertion)
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

      val insertionsThatAreNotMoveDestinations = insertionsAtPath.filterNot {
        case (_, Insertion(inserted, side)) =>
          val dominants = dominantsOf(inserted)
          moveDestinationsReport.moveDestinationsByDominantSet
            .get(dominants)
            .fold(ifEmpty = false)(
              isMoveDestinationOnGivenSide(inserted, side, _)
            )
      }

      enum Anchoring:
        case Predecessor
        case Successor
      end Anchoring

      // NOTE: the same insertion may not only be associated with multiple
      // anchor destinations due to ambiguous matches; it may also be flanked on
      // either side by anchor destinations. Hence the use of `Anchoring` to
      // track whether the anchor precedes or succeeds the insertion.
      // TODO: should we allow distinct sections with the same content to be
      // considered as identical?
      val migratedInsertionsByAnchorDestinations =
        insertionsThatAreNotMoveDestinations.flatMap {
          case (path, Insertion(inserted, side)) =>
            val file = side match
              case Side.Left =>
                codeMotionAnalysis.left(path)
              case Side.Right =>
                codeMotionAnalysis.right(path)

            {
              val Searching.Found(indexOfInsertedSection) =
                file.searchByStartOffset(inserted.startOffset): @unchecked

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

              val predecessorAnchorDestinations = Option
                .when(0 < indexOfInsertedSection)(
                  file.sections(indexOfInsertedSection - 1)
                )
                .flatMap(destinationsForValidAnchor)

              val onePastIndex = 1 + indexOfInsertedSection
              val successorAnchorDestinations = Option
                .when(file.sections.length > onePastIndex)(
                  file.sections(onePastIndex)
                )
                .flatMap(destinationsForValidAnchor)

              predecessorAnchorDestinations.map(
                _ -> Anchoring.Predecessor
              ) ++ successorAnchorDestinations.map(_ -> Anchoring.Successor)
            }.map(_ -> inserted)
        }

      val migratedInsertions =
        migratedInsertionsByAnchorDestinations.values.toSet

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
                .empty[Section[Element]] -> (None: Option[Section[Element]])
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
                  if 1 >= precedingMigratedInsertions.size then
                    precedingMigratedInsertions.headOption
                  else throw RuntimeException("THINK OF A AN ERROR MESSAGE.")

                val succeedingMigratedInsertions =
                  migratedInsertionsByAnchorDestinations.get(
                    candidateAnchorDestination -> Anchoring.Predecessor
                  )

                val succeedingMigratedInsertion =
                  if 1 >= succeedingMigratedInsertions.size then
                    succeedingMigratedInsertions.headOption
                  else throw RuntimeException("THINK OF A AN ERROR MESSAGE.")

                val result =
                  (deferredMigratedInsertion, precedingMigratedInsertion) match
                    case (None, None) =>
                      partialResult :+ candidateAnchorDestination
                    case (Some(deferred), None) =>
                      partialResult :+ deferred :+ candidateAnchorDestination
                    case (None, Some(preceding)) =>
                      partialResult :+ preceding :+ candidateAnchorDestination
                    case (Some(deferred), Some(preceding)) =>
                      if deferred == preceding then
                        partialResult :+ deferred :+ candidateAnchorDestination
                      else
                        partialResult :+ deferred :+ preceding :+ candidateAnchorDestination

                result -> succeedingMigratedInsertion
            } match
            case (partialResult, deferredMigratedInsertion) =>
              partialResult ++ deferredMigratedInsertion

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

      val potentialValidDestinationsForPropagatingChangesTo =
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

      val vettedChangesPropagatedThroughMotion =
        MultiDict.from(changesPropagatedThroughMotion.filter {
          case (potentialDestination, _) =>
            potentialValidDestinationsForPropagatingChangesTo.contains(
              potentialDestination
            )
        })

      def substitutePropagatedChangesOrDominants(
          path: Path,
          mergeResult: MergeResult[Section[Element]]
      ): (Path, MergeResult[Section[Element]]) =
        def substituteFor(section: Section[Element]): Option[Section[Element]] =
          val propagatedChanges: collection.Set[Option[Section[Element]]] =
            vettedChangesPropagatedThroughMotion.get(section)
          val propagatedChange: Option[Option[Section[Element]]] =
            if 1 >= propagatedChanges.size then propagatedChanges.headOption
            else
              throw new RuntimeException(
                s"""
                  |Multiple potential changes propagated to destination: $section,
                  |these are:
                  |${propagatedChanges
                    .map(
                      _.fold(ifEmpty = "DELETION")(edit => s"EDIT: $edit")
                    )
                    .zipWithIndex
                    .map((change, index) => s"${1 + index}. $change")
                    .mkString("\n")}
                  |These are from ambiguous matches of text with the destination.
                  |Consider setting the command line parameter `--minimum-ambiguous-match-size` to something larger than ${section.size}.
                      """.stripMargin
              )

          // If we do have a propagated change, then there is no need to look
          // for the dominant - either the section was deleted or edited;
          // matched sections are not considered as edit candidates.
          propagatedChange.fold {
            val dominants = dominantsOf(section)

            Some(
              if dominants.isEmpty then section
              else
                // NASTY HACK: this is hokey, but essentially correct - if we
                // have ambiguous matches leading to multiple dominants, then
                // they're all just as good in terms of their content. So just
                // choose any one.
                dominants.head
            )
          }(
            _.fold(
              // Moved section was deleted...
              ifEmpty =
                logger.debug(
                  s"Applying propagated deletion to move destination: ${pprintCustomised(section)}."
                )
                None
            )(
              // Moved section was edited...
              { edit =>
                logger.debug(
                  s"Applying propagated edit into ${pprintCustomised(edit)} to move destination: ${pprintCustomised(section)}."
                )
                Some(edit)
              }
            )
          )

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
      end substitutePropagatedChangesOrDominants

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

            // Just in case the conflict was resolved by the propagated
            // changes...
            if leftElements.corresponds(
                rightElements
              )(equality.eqv)
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
        .map(substitutePropagatedChangesOrDominants)
        .map(explodeSections) -> moveDestinationsReport
    end merge
  end extension
end CodeMotionAnalysisExtension
