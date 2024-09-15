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
      import codeMotionAnalysis.matchesFor

      val matchesContext = MatchesContext(
        codeMotionAnalysis.matchesFor
      )

      import matchesContext.*

      given Eq[Section[Element]] with
        /** This is most definitely *not* [[Section.equals]] - we want to use
          * matching of content, as the sections are expected to come from
          * *different* sides. [[Section.equals]] is expected to consider
          * sections from different sides as unequal. <p>If neither section is
          * involved in a match, fall back to comparing the contents; this is
          * vital for comparing sections that would have been part of a larger
          * match if not for that match not achieving the threshold size.
          */
        override def eqv(
            lhs: Section[Element],
            rhs: Section[Element]
        ): Boolean =
          val bothBelongToTheSameMatches =
            matchesFor(lhs).intersect(matchesFor(rhs)).nonEmpty

          bothBelongToTheSameMatches || Eq[Seq[Element]]
            .eqv(lhs.content, rhs.content)
        end eqv
      end given

      val paths =
        codeMotionAnalysis.base.keySet ++ codeMotionAnalysis.left.keySet ++ codeMotionAnalysis.right.keySet

      case class InsertionsAtPath(path: Path, insertions: Seq[Insertion])

      def resolution(
          baseSection: Option[Section[Element]],
          leftSection: Section[Element],
          rightSection: Section[Element]
      ): Section[Element] = baseSection.fold(ifEmpty =
        // Break the symmetry - choose the left.
        leftSection
      ) { payload =>
        // Look at the content and use *exact* comparison.

        val lhsIsCompletelyUnchanged = payload.content == leftSection.content
        val rhsIsCompletelyUnchanged = payload.content == rightSection.content

        (lhsIsCompletelyUnchanged, rhsIsCompletelyUnchanged) match
          case (false, true) => leftSection
          case (true, false) => rightSection
          case _             =>
            // Break the symmetry - choose the left.
            leftSection
        end match
      }

      val (
        mergeResultsByPath,
        changesMigratedThroughMotion,
        moveDestinationsReport,
        insertionsAtPath,
        oneSidedDeletions
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
          Vector.empty[InsertionsAtPath],
          Set.empty[Section[Element]]
        ) {
          case (
                (
                  mergeResultsByPath,
                  changesMigratedThroughMotion,
                  moveDestinationsReport,
                  insertionsAtPath,
                  oneSidedDeletions
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
                  insertionsAtPath,
                  oneSidedDeletions
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
                  insertionsAtPath,
                  oneSidedDeletions
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
                    MergeResultDetectingMotion.mergeAlgebra(
                      coreMergeAlgebra = MergeResult.mergeAlgebra(resolution),
                      resolution
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
                  ),
                  oneSidedDeletions union mergedSectionsResult.oneSidedDeletions
                )
            end match
        }

      // NOTE: have to delay this assumption check until after the complete move
      // destination report has been finalized, and *not* make it an invariant
      // of `MoveDestination`. This is because divergent moves are entered as
      // separate left- and right-moves, so any such invariant could (and does)
      // break.
      moveDestinationsReport.moveDestinationsByMatches.values.foreach {
        moveDestination =>
          if moveDestination.isDegenerate then
            // We don't consider left- and right-insertions to be degenerate
            // moves, as there is no match involved.
            assume(
              moveDestination.isDivergent || moveDestination.coincident.nonEmpty
            )
          end if
      }

      def isMoveDestinationOnGivenSide(
          section: Section[Element],
          side: Side,
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

      given sectionRunOrdering[Sequence[Item] <: Seq[Item]]
          : Ordering[Sequence[Section[Element]]] =
        Ordering.Implicits.seqOrdering(
          Ordering.by[Section[Element], IndexedSeq[Element]](_.content)(
            Ordering.Implicits.seqOrdering(summon[Eq[Element]].toOrdering)
          )
        )

      given insertionSpliceOrdering: Ordering[InsertionSplice] =
        Ordering
          .by[InsertionSplice, Seq[Section[Element]]](_.insertions)
          .orElseBy(_.numberOfSkipsToTheAnchor)

      def uniqueItemsFrom[Item](
          items: collection.Set[Item]
      )(using itemOrdering: Ordering[Item]): List[Item] =
        require(items.nonEmpty)

        val migratedChangesSortedByContent =
          items.toSeq.sorted(itemOrdering)

        val result =
          migratedChangesSortedByContent.tail.foldLeft(
            List(migratedChangesSortedByContent.head)
          ) { case (partialResult @ head :: _, change) =>
            if 0 == itemOrdering.compare(head, change) then partialResult
            else change :: partialResult
          }

        assume(result.nonEmpty)

        result
      end uniqueItemsFrom

      enum Anchoring:
        case Predecessor
        case Successor
      end Anchoring

      case class InsertionSplice(
          insertions: Seq[Section[Element]],
          numberOfSkipsToTheAnchor: Int
      ):
        require(0 <= numberOfSkipsToTheAnchor)
      end InsertionSplice

      val migratedInsertionSplicesByAnchorDestinations
          : MultiDict[(Section[Element], Anchoring), InsertionSplice] =
        MultiDict.from(insertionsAtPath.flatMap {
          case InsertionsAtPath(path, insertions) =>
            case class InsertionRun(
                side: Side,
                contiguousInsertions: Seq[Section[Element]]
            ):
              require(contiguousInsertions.nonEmpty)
            end InsertionRun

            val (partialResult, insertionRun) =
              insertions
                .foldLeft(
                  Vector.empty[InsertionRun],
                  None: Option[InsertionRun]
                ) {
                  case (
                        (
                          partialResult,
                          insertionRun
                        ),
                        Insertion(side, inserted)
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
                  enum AnchorTestResult:
                    case Found(destinations: collection.Set[Section[Element]])
                    case StopLooking
                    case SkipOverAndKeepLooking
                  end AnchorTestResult

                  def testForAnchor(
                      potentialAnchor: Section[Element]
                  ): AnchorTestResult =
                    val matches = matchesFor(potentialAnchor)
                    moveDestinationsReport.moveDestinationsByMatches
                      .get(matches)
                      .fold(ifEmpty =
                        if oneSidedDeletions.contains(potentialAnchor) then
                          // If the potential anchor is a one-sided deletion,
                          // then it isn't an anchor; however the lack of an
                          // edit in the same file means we can think of it as
                          // noise that doesn't affect the validity of a
                          // subsequent anchor.
                          AnchorTestResult.SkipOverAndKeepLooking
                        else
                          // There is an edit on the other side of the potential
                          // anchor in the same file, this definitely isn't an
                          // anchor in itself and will hem in any insertions
                          // from the possibility of a subsequent anchor.
                          AnchorTestResult.StopLooking
                      )(moveDestinations =>
                        if !isMoveDestinationOnGivenSide(
                            potentialAnchor,
                            side,
                            moveDestinations
                          )
                        then
                          AnchorTestResult.Found(side match
                            case Side.Left  => moveDestinations.right
                            case Side.Right => moveDestinations.left
                          )
                        else AnchorTestResult.StopLooking
                      )
                  end testForAnchor

                  val Searching.Found(indexOfLeadingInsertedSection) =
                    file.searchByStartOffset(
                      contiguousInsertions.head.startOffset
                    ): @unchecked

                  val (
                    predecessorAnchorDestinations,
                    numberOfSkipsToPredecessor
                  ) =
                    val (skipped, remainder) = file.sections
                      .take(indexOfLeadingInsertedSection)
                      .view
                      .reverse
                      .map(testForAnchor)
                      .span(AnchorTestResult.SkipOverAndKeepLooking == _)

                    remainder.headOption
                      .collect { case AnchorTestResult.Found(destinations) =>
                        destinations
                      }
                      .getOrElse(Set.empty) -> skipped.size
                  end val

                  val onePastIndex =
                    contiguousInsertions.size + indexOfLeadingInsertedSection

                  val (successorAnchorDestinations, numberOfSkipsToSuccessor) =
                    val (skipped, remainder) = file.sections
                      .drop(onePastIndex)
                      .view
                      .map(testForAnchor)
                      .span(AnchorTestResult.SkipOverAndKeepLooking == _)

                    remainder.headOption
                      .collect { case AnchorTestResult.Found(destinations) =>
                        destinations
                      }
                      .getOrElse(Set.empty) -> skipped.size
                  end val

                  predecessorAnchorDestinations
                    .map(
                      _ -> Anchoring.Predecessor
                    )
                    .map(
                      _ -> InsertionSplice(
                        contiguousInsertions,
                        numberOfSkipsToPredecessor
                      )
                    ) ++ successorAnchorDestinations
                    .map(_ -> Anchoring.Successor)
                    .map(
                      _ -> InsertionSplice(
                        contiguousInsertions,
                        numberOfSkipsToSuccessor
                      )
                    )
                }
            }
        })

      val migratedInsertions =
        migratedInsertionSplicesByAnchorDestinations.values
          .flatMap(_.insertions)
          .toSet

      val suppressedMoveDestinationsDueToMigratedInsertions =
        migratedInsertions.flatMap { insertion =>
          val matches = matchesFor(insertion)

          moveDestinationsReport.moveDestinationsByMatches
            .get(matches)
            .fold(ifEmpty = Set.empty[Section[Element]])(_.all)
        }

      def applyMigrations(
          path: Path,
          mergeResult: MergeResult[Section[Element]]
      ): (Path, MergeResult[Section[Element]]) =
        val potentialValidDestinationsForMigratingChangesTo =
          moveDestinationsReport.moveDestinationsByMatches.values
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

        def substituteFor(
            section: Section[Element]
        ): IndexedSeq[Section[Element]] =
          val migratedChanges = vettedChangesMigratedThroughMotion
            .get(section)

          if migratedChanges.nonEmpty then
            val uniqueMigratedChanges = uniqueItemsFrom(migratedChanges)

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
          else IndexedSeq(section)
          end if
        end substituteFor

        def removeMigratedInsertions(
            sections: IndexedSeq[Section[Element]]
        ): IndexedSeq[Section[Element]] =
          sections
            .filterNot(migratedInsertions.contains)
            .filterNot(
              suppressedMoveDestinationsDueToMigratedInsertions.contains
            )

        extension (sections: IndexedSeq[Section[Element]])
          private def appendMigratedInsertions(
              migratedInsertions: Seq[Section[Element]]
          ): IndexedSeq[Section[Element]] =
            if migratedInsertions.nonEmpty then
              logger.debug(
                s"Applying migrated insertion of ${pprintCustomised(migratedInsertions)} after destination: ${pprintCustomised(sections.last)}."
              )
            end if
            sections ++ migratedInsertions

        def migrateInsertionsAndApplySubstitutions(
            sections: IndexedSeq[Section[Element]]
        ): IndexedSeq[Section[Element]] =
          case class Deferrals(
              deferredInsertions: Seq[Section[Element]],
              numberOfSkipsToTheAnchorOrDeferredContent: Either[Int, Seq[
                Section[Element]
              ]]
          ):
            numberOfSkipsToTheAnchorOrDeferredContent.left.foreach(
              numberOfSkipsToTheAnchor => require(0 <= numberOfSkipsToTheAnchor)
            )
          end Deferrals

          object Deferrals:
            // TODO: use Chimney instead!
            def apply(insertionSplice: InsertionSplice): Deferrals =
              Deferrals(
                deferredInsertions = insertionSplice.insertions,
                numberOfSkipsToTheAnchorOrDeferredContent =
                  Left(insertionSplice.numberOfSkipsToTheAnchor)
              )
          end Deferrals

          val emptyContext: Deferrals = Deferrals(
            deferredInsertions = Seq.empty,
            numberOfSkipsToTheAnchorOrDeferredContent = Right(Seq.empty)
          )

          sections
            .foldLeft(
              IndexedSeq
                .empty[Section[Element]] -> emptyContext
            ) {
              case (
                    (partialResult, anchorContext),
                    candidateAnchorDestination
                  ) =>
                val precedingMigratedInsertionSplices =
                  migratedInsertionSplicesByAnchorDestinations.get(
                    candidateAnchorDestination -> Anchoring.Successor
                  )

                val precedingInsertionSplice =
                  Option.when(precedingMigratedInsertionSplices.nonEmpty) {
                    val uniqueInsertionSplices =
                      uniqueItemsFrom(precedingMigratedInsertionSplices)

                    uniqueInsertionSplices match
                      case head :: Nil => head
                      case _ =>
                        throw new RuntimeException(
                          s"""
                             |Multiple potential insertions migrated before destination: $candidateAnchorDestination,
                             |these are:
                             |${uniqueInsertionSplices
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

                val succeedingMigratedInsertionSplices =
                  migratedInsertionSplicesByAnchorDestinations.get(
                    candidateAnchorDestination -> Anchoring.Predecessor
                  )

                val succeedingInsertionSplice =
                  Option.when(succeedingMigratedInsertionSplices.nonEmpty) {
                    val uniqueInsertionSplices =
                      uniqueItemsFrom(succeedingMigratedInsertionSplices)

                    uniqueInsertionSplices match
                      case head :: Nil => head
                      case _ =>
                        throw new RuntimeException(
                          s"""
                             |Multiple potential insertions migrated after destination: $candidateAnchorDestination,
                             |these are:
                             |${uniqueInsertionSplices
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

                precedingInsertionSplice.foreach(splice =>
                  logger.debug(
                    s"Encountered succeeding anchor destination: ${pprintCustomised(candidateAnchorDestination)} with associated preceding insertion splice: ${pprintCustomised(splice)}."
                  )
                )
                succeedingInsertionSplice.foreach(splice =>
                  logger.debug(
                    s"Encountered preceding anchor destination: ${pprintCustomised(candidateAnchorDestination)} with associated following insertion splice: ${pprintCustomised(splice)}."
                  )
                )

                val substituted = substituteFor(candidateAnchorDestination)

                (
                  anchorContext,
                  precedingInsertionSplice,
                  succeedingInsertionSplice
                ) match
                  // NOTE: avoid use of lenses in the cases below when we
                  // already have to pattern match deeply anyway...

                  case (
                        Deferrals(deferredInsertions, Left(0)),
                        None,
                        None
                      ) =>
                    // We have arrived at the insertion point after a preceding
                    // anchor, but have to defer both the insertions and
                    // the substitutions for `candidateAnchorDestination` (which
                    // is not an anchor after all) in case of a following
                    // succeeding anchor...
                    partialResult -> Deferrals(
                      deferredInsertions = deferredInsertions,
                      numberOfSkipsToTheAnchorOrDeferredContent =
                        Right(substituted)
                    )
                  case (
                        Deferrals(
                          deferredInsertions,
                          Left(numberOfSkipsToTheAnchor)
                        ),
                        None,
                        None
                      ) =>
                    // Consider the substitutions
                    // for`candidateAnchorDestination` (which is not an anchor
                    // after all) to be an edit of a skipped section coming
                    // after a preceding anchor.
                    (partialResult ++ substituted) -> Deferrals(
                      deferredInsertions,
                      Left(
                        numberOfSkipsToTheAnchor - substituted.length
                      )
                    )
                  case (
                        Deferrals(
                          deferredInsertions,
                          Right(deferredContent)
                        ),
                        None,
                        None
                      ) =>
                    // We have to defer the substitutions
                    // for`candidateAnchorDestination` (which is not an anchor
                    // after all) in case of a following succeeding anchor...
                    partialResult -> Deferrals(
                      deferredInsertions,
                      Right(deferredContent ++ substituted)
                    )
                  case (
                        Deferrals(
                          deferredInsertions,
                          Left(_)
                        ),
                        Some(InsertionSplice(insertionsForSucceedingAnchor, _)),
                        _
                      ) =>
                    // We have encountered a succeeding anchor; will this refer
                    // to the same insertions as the context?

                    if sectionRunOrdering.equiv(
                        deferredInsertions,
                        insertionsForSucceedingAnchor
                      )
                    then
                      // The implied preceding anchor and the succeeding anchor
                      // just encountered bracket the same insertions.
                      (partialResult.appendMigratedInsertions(
                        deferredInsertions
                      ) ++ substituted) -> succeedingInsertionSplice
                        .fold(ifEmpty = emptyContext)(Deferrals.apply)
                    else
                      // The implied preceding anchor and the succeeding anchor
                      // just encountered refer to distinct insertions that have
                      // migrated adjacent to each other. As insertions try to
                      // stick as close as possible to their anchor, we put
                      // `deferredInsertionsForImpliedPrecedingAnchor` first.
                      (partialResult.appendMigratedInsertions(
                        deferredInsertions ++ insertionsForSucceedingAnchor
                      ) ++ substituted) -> succeedingInsertionSplice
                        .fold(ifEmpty = emptyContext)(Deferrals.apply)
                  case (
                        Deferrals(
                          deferredInsertions,
                          Right(deferredContent)
                        ),
                        Some(spliceIntoDeferredContext),
                        _
                      ) =>
                    // We have encountered a succeeding anchor, so we use the
                    // deferred context to decide where to place the insertions.
                    val (prefix, suffix) = deferredContent.splitAt(
                      deferredContent.length - spliceIntoDeferredContext.numberOfSkipsToTheAnchor
                    )

                    if prefix.isEmpty && sectionRunOrdering.equiv(
                        deferredInsertions,
                        spliceIntoDeferredContext.insertions
                      )
                    then
                      // The implied preceding anchor and the succeeding anchor
                      // just encountered bracket the same insertions.
                      (partialResult.appendMigratedInsertions(
                        deferredInsertions
                      ) ++ suffix ++ substituted) -> succeedingInsertionSplice
                        .fold(ifEmpty = emptyContext)(Deferrals.apply)
                    else
                      ((partialResult.appendMigratedInsertions(
                        deferredInsertions
                      ) ++ prefix).appendMigratedInsertions(
                        spliceIntoDeferredContext.insertions
                      ) ++ suffix ++ substituted) -> succeedingInsertionSplice
                        .fold(ifEmpty = emptyContext)(Deferrals.apply)
                    end if
                  case (
                        Deferrals(deferredInsertions, Left(_)),
                        None,
                        Some(insertionSplice)
                      ) =>
                    // We have encountered a preceding anchor, so the deferred
                    // insertions from the previous preceding anchor can finally
                    // be added to the partial result.
                    (partialResult.appendMigratedInsertions(
                      deferredInsertions
                    ) ++ substituted) -> Deferrals(
                      insertionSplice
                    )
                  case (
                        Deferrals(
                          deferredInsertions,
                          Right(deferredContent)
                        ),
                        None,
                        Some(insertionSplice)
                      ) =>
                    // We have encountered a preceding anchor, so the deferred
                    // insertions from the previous preceding anchor and the
                    // deferred content can finally be added to the partial
                    // result.
                    (partialResult.appendMigratedInsertions(
                      deferredInsertions
                    ) ++ deferredContent ++ substituted) -> Deferrals(
                      insertionSplice
                    )
                end match
            } match
            case (partialResult, anchorContext) =>
              anchorContext match
                case Deferrals(deferredInsertions, Left(_)) =>
                  partialResult.appendMigratedInsertions(deferredInsertions)
                case Deferrals(
                      deferredInsertions,
                      Right(deferredContent)
                    ) =>
                  partialResult.appendMigratedInsertions(
                    deferredInsertions
                  ) ++ deferredContent
          end match
        end migrateInsertionsAndApplySubstitutions

        path -> (mergeResult match
          case FullyMerged(sections) =>
            FullyMerged(elements =
              migrateInsertionsAndApplySubstitutions(
                removeMigratedInsertions(sections)
              )
            )
          case MergedWithConflicts(leftSections, rightSections) =>
            MergedWithConflicts(
              migrateInsertionsAndApplySubstitutions(
                removeMigratedInsertions(leftSections)
              ),
              migrateInsertionsAndApplySubstitutions(
                removeMigratedInsertions(rightSections)
              )
            )
        )
      end applyMigrations

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
        .map(applyMigrations)
        .map(explodeSections) -> moveDestinationsReport
    end merge
  end extension
end CodeMotionAnalysisExtension
