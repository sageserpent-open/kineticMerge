package com.sageserpent.kineticmerge.core

import cats.{Eq, Order}
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import com.sageserpent.kineticmerge.core
import com.sageserpent.kineticmerge.core.CodeMotionAnalysis.AdmissibleFailure
import com.sageserpent.kineticmerge.core.CoreMergeAlgebra.MultiSidedMergeResult
import com.sageserpent.kineticmerge.core.FirstPassMergeResult.{
  FileDeletionContext,
  Recording
}
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Sized
import com.sageserpent.kineticmerge.core.MergeResult.given
import com.sageserpent.kineticmerge.core.MoveDestinationsReport.{
  AnchoredMove,
  MoveEvaluation,
  OppositeSideAnchor
}
import com.sageserpent.kineticmerge.core.merge.of as mergeOf
import com.typesafe.scalalogging.StrictLogging
import monocle.syntax.all.*

import scala.annotation.tailrec
import scala.collection.immutable.MultiDict
import scala.collection.{IndexedSeqView, Searching}
import scala.math.Ordering.Implicits.seqOrdering

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
        MoveDestinationsReport[Section[Element]]
    ) =
      import codeMotionAnalysis.matchesFor

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

          bothBelongToTheSameMatches || lhs.size == rhs.size && Eq[Seq[Element]]
            .eqv(lhs.content, rhs.content)
        end eqv
      end given

      given Sized[Section[Element]] = _.size

      extension [Item: Sized](multiSided: MultiSided[Item])
        private def size: Int =
          val sized = summon[Sized[Item]]
          multiSided match
            case MultiSided.Unique(unique)          => sized.sizeOf(unique)
            case MultiSided.Coincident(left, right) =>
              sized.sizeOf(left)
            case MultiSided.Preserved(base, left, right) =>
              sized.sizeOf(base)
          end match
      end extension

      val paths =
        codeMotionAnalysis.base.keySet ++ codeMotionAnalysis.left.keySet ++ codeMotionAnalysis.right.keySet

      def resolution(
          multiSided: MultiSided[Section[Element]]
      ): IndexedSeq[Element] =
        multiSided match
          case MultiSided.Unique(element) => element.content
          case MultiSided.Coincident(leftElement, rightElement) =>
            // Break the symmetry - choose the left.
            leftElement.content
          case MultiSided.Preserved(
                baseElement,
                leftElement,
                rightElement
              ) =>
            // Look at the content and use *exact* comparison.

            val lhsIsCompletelyUnchanged =
              baseElement.content == leftElement.content
            val rhsIsCompletelyUnchanged =
              baseElement.content == rightElement.content

            (lhsIsCompletelyUnchanged, rhsIsCompletelyUnchanged) match
              case (false, true) => leftElement.content
              case (true, false) => rightElement.content
              case _             =>
                // Break the symmetry - choose the left.
                leftElement.content
            end match

      type SecondPassInput =
        Either[MergeResult[Section[Element]], Recording[Section[Element]]]

      case class AggregatedInitialMergeResult(
          secondPassInputsByPath: Map[Path, SecondPassInput],
          speculativeMigrationsBySource: Map[Section[
            Element
          ], SpeculativeContentMigration[
            Section[Element]
          ]],
          speculativeMoveDestinations: Set[
            SpeculativeMoveDestination[Section[Element]]
          ],
          basePreservations: Set[Section[Element]],
          leftPreservations: Set[Section[Element]],
          rightPreservations: Set[Section[Element]]
      ):
        def recordContentOfFileAddedOnLeft(
            path: Path,
            leftSections: IndexedSeq[Section[Element]]
        ): AggregatedInitialMergeResult =
          this
            .focus(_.secondPassInputsByPath)
            .modify(
              _ + (path -> Left(
                MergeResult.of(leftSections*)
              ))
            )
            .focus(_.speculativeMoveDestinations)
            .modify(
              leftSections.foldLeft(_)(
                _ + SpeculativeMoveDestination.Left(_)
              )
            )

        def recordContentOfFileAddedOnRight(
            path: Path,
            rightSections: IndexedSeq[Section[Element]]
        ): AggregatedInitialMergeResult =
          this
            .focus(_.secondPassInputsByPath)
            .modify(
              _ + (path -> Left(
                MergeResult.of(rightSections*)
              ))
            )
            .focus(_.speculativeMoveDestinations)
            .modify(
              rightSections.foldLeft(_)(
                _ + SpeculativeMoveDestination.Right(_)
              )
            )

        def recordContentOfFileDeletedOnLeftAndRight(
            baseSections: IndexedSeq[Section[Element]]
        ): AggregatedInitialMergeResult =
          this
            .focus(_.speculativeMigrationsBySource)
            .modify(
              _ ++ baseSections.map(
                _ -> SpeculativeContentMigration.FileDeletion()
              )
            )

        def aggregate(
            path: Path,
            firstPassMergeResult: FirstPassMergeResult[Section[Element]]
        ): AggregatedInitialMergeResult =
          this
            .focus(_.secondPassInputsByPath)
            .modify(
              _ + (path -> Right(
                firstPassMergeResult.recording
              ))
            )
            .focus(_.speculativeMigrationsBySource)
            .modify(_ concat firstPassMergeResult.speculativeMigrationsBySource)
            .focus(_.speculativeMoveDestinations)
            .modify(_ union firstPassMergeResult.speculativeMoveDestinations)
            .focus(_.basePreservations)
            .modify(_ union firstPassMergeResult.basePreservations)
            .focus(_.leftPreservations)
            .modify(_ union firstPassMergeResult.leftPreservations)
            .focus(_.rightPreservations)
            .modify(_ union firstPassMergeResult.rightPreservations)
      end AggregatedInitialMergeResult

      object AggregatedInitialMergeResult:
        def empty: AggregatedInitialMergeResult = AggregatedInitialMergeResult(
          secondPassInputsByPath = Map.empty,
          speculativeMigrationsBySource = Map.empty,
          speculativeMoveDestinations = Set.empty,
          basePreservations = Set.empty,
          leftPreservations = Set.empty,
          rightPreservations = Set.empty
        )
      end AggregatedInitialMergeResult

      val AggregatedInitialMergeResult(
        secondPassInputsByPath,
        speculativeMigrationsBySource,
        speculativeMoveDestinations,
        basePreservations,
        leftPreservations,
        rightPreservations
      ) =
        paths.foldLeft(AggregatedInitialMergeResult.empty) {
          case (partialMergeResult, path) =>
            val base  = codeMotionAnalysis.base.get(path).map(_.sections)
            val left  = codeMotionAnalysis.left.get(path).map(_.sections)
            val right = codeMotionAnalysis.right.get(path).map(_.sections)

            (base, left, right) match
              case (None, Some(leftSections), None) =>
                partialMergeResult.recordContentOfFileAddedOnLeft(
                  path,
                  leftSections
                )
              case (None, None, Some(rightSections)) =>
                partialMergeResult.recordContentOfFileAddedOnRight(
                  path,
                  rightSections
                )
              case (Some(baseSections), None, None) =>
                // The file has disappeared on both sides. That may indicate a
                // coincident deletion of the entire file, or may be a
                // coincident renaming of the file on both sides, or a divergent
                // rename to different paths on either side, or a conflict
                // between a deletion of the entire file on one side and its
                // renaming on the other. Unlike the situation where a file
                // disappears on just *one* side, there is no need to actually
                // perform a merge, so there is no merge result made under
                // `path`.
                partialMergeResult.recordContentOfFileDeletedOnLeftAndRight(
                  baseSections
                )
              case (Some(baseSections), None, Some(rightSections)) =>
                // The file has disappeared on the left side. That may indicate
                // a simple deletion of the file, or may be a renaming on the
                // left.

                // Merge with fake empty content on the left...

                val firstPassMergeResult
                    : FirstPassMergeResult[Section[Element]] =
                  mergeOf(mergeAlgebra =
                    FirstPassMergeResult.mergeAlgebra(fileDeletionContext =
                      FileDeletionContext.Left
                    )
                  )(
                    base = baseSections,
                    left = IndexedSeq.empty,
                    right = rightSections
                  )

                partialMergeResult.aggregate(path, firstPassMergeResult)
              case (Some(baseSections), Some(leftSections), None) =>
                // The file has disappeared on the right side. That may indicate
                // a simple deletion of the file, or may be a renaming on the
                // right.

                // Merge with fake empty content on the right...

                val firstPassMergeResult
                    : FirstPassMergeResult[Section[Element]] =
                  mergeOf(mergeAlgebra =
                    FirstPassMergeResult.mergeAlgebra(fileDeletionContext =
                      FileDeletionContext.Right
                    )
                  )(
                    base = baseSections,
                    left = leftSections,
                    right = IndexedSeq.empty
                  )

                partialMergeResult.aggregate(path, firstPassMergeResult)
              case (
                    optionalBaseSections,
                    Some(leftSections),
                    Some(rightSections)
                  ) =>
                // Mix of possibilities - the file may have been added on both
                // sides, or modified on either or both sides. There is also an
                // extraneous case where there is no file on any of the sides,
                // and another extraneous case where the file is on all three
                // sides but hasn't changed.

                // Whichever is the case, merge...

                val firstPassMergeResult
                    : FirstPassMergeResult[Section[Element]] =
                  mergeOf(mergeAlgebra =
                    FirstPassMergeResult.mergeAlgebra(fileDeletionContext =
                      FileDeletionContext.None
                    )
                  )(
                    base = optionalBaseSections.getOrElse(IndexedSeq.empty),
                    left = leftSections,
                    right = rightSections
                  )

                partialMergeResult.aggregate(path, firstPassMergeResult)
            end match
        }

      val (
        coincidentInsertionsOrEditsOnLeft,
        coincidentInsertionsOrEditsOnRight
      ) = speculativeMoveDestinations.collect {
        case SpeculativeMoveDestination.Coincident(leftSection, rightSection) =>
          leftSection -> rightSection
      }.unzip

      logger.debug(
        s"Base preservations: ${pprintCustomised(basePreservations)}."
      )
      logger.debug(
        s"Left preservations: ${pprintCustomised(leftPreservations)}."
      )
      logger.debug(
        s"Right preservations: ${pprintCustomised(rightPreservations)}."
      )
      logger.debug(
        s"Coincident insertions or edits on left: ${pprintCustomised(coincidentInsertionsOrEditsOnLeft)}."
      )
      logger.debug(
        s"Coincident insertions or edits on right: ${pprintCustomised(coincidentInsertionsOrEditsOnRight)}."
      )

      val moveEvaluation @ MoveEvaluation(
        moveDestinationsReport,
        migratedEditSuppressions,
        substitutionsByDestination,
        anchoredMoves
      ) =
        MoveDestinationsReport.evaluateSpeculativeSourcesAndDestinations(
          speculativeMigrationsBySource,
          speculativeMoveDestinations
        )(matchesFor)

      logger.debug(s"Move evaluation: ${pprintCustomised(moveEvaluation)}.")

      val sourceAnchors       = anchoredMoves.map(_.sourceAnchor)
      val oppositeSideAnchors =
        anchoredMoves.map(_.oppositeSideAnchor.element)
      val moveDestinationAnchors = anchoredMoves.map(_.moveDestinationAnchor)

      given sectionOrdering: Ordering[Section[Element]] =
        Ordering.by[Section[Element], IndexedSeq[Element]](_.content)(
          seqOrdering(summon[Order[Element]].toOrdering)
        )

      val specialCaseEquivalenceBasedOnOrdering
          : Eq[MultiSided[Section[Element]]] =
        Order.fromOrdering[MultiSided[Section[Element]]]

      def uniqueSortedItemsFrom[Item](
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
      end uniqueSortedItemsFrom

      enum AnchoringSense:
        case Predecessor
        case Successor
      end AnchoringSense

      def precedingAnchoredContentUsingSelection(
          file: File[Element],
          anchor: Section[Element]
      )(
          selection: IndexedSeqView[Section[Element]] => IndexedSeq[
            Section[Element]
          ]
      ) =
        val Searching.Found(indexOfSection) =
          file.searchByStartOffset(anchor.startOffset): @unchecked

        selection(
          file.sections.view.take(indexOfSection).reverse
        ).reverse
      end precedingAnchoredContentUsingSelection

      def succeedingAnchoredContentUsingSelection(
          file: File[Element],
          anchor: Section[Element]
      )(
          selection: IndexedSeqView[Section[Element]] => IndexedSeq[
            Section[Element]
          ]
      ) =
        val Searching.Found(indexOfSection) =
          file.searchByStartOffset(anchor.startOffset): @unchecked

        selection(file.sections.view.drop(1 + indexOfSection))
      end succeedingAnchoredContentUsingSelection

      def anchoredContentFromSource(
          sourceAnchor: Section[Element]
      ): (IndexedSeq[Section[Element]], IndexedSeq[Section[Element]]) =
        val file =
          codeMotionAnalysis.base(codeMotionAnalysis.basePathFor(sourceAnchor))

        def selection(
            candidates: IndexedSeqView[Section[Element]]
        ): IndexedSeq[Section[Element]] =
          candidates
            .takeWhile(candidate =>
              !basePreservations.contains(candidate) && !sourceAnchors
                .contains(
                  candidate
                )
            )
            // At this point, we only have a plain view rather than an indexed
            // one...
            .toIndexedSeq

        precedingAnchoredContentUsingSelection(file, sourceAnchor)(
          selection
        ) -> succeedingAnchoredContentUsingSelection(file, sourceAnchor)(
          selection
        )
      end anchoredContentFromSource

      def anchoredContentFromOppositeSide(
          moveDestinationSide: Side,
          oppositeSideAnchor: OppositeSideAnchor[Section[Element]]
      ): (IndexedSeq[Section[Element]], IndexedSeq[Section[Element]]) =
        val (file, preservations, coincidentInsertionsOrEdits) =
          moveDestinationSide match
            case Side.Left =>
              (
                codeMotionAnalysis.right(
                  codeMotionAnalysis.rightPathFor(oppositeSideAnchor.element)
                ),
                rightPreservations,
                coincidentInsertionsOrEditsOnRight
              )
            case Side.Right =>
              (
                codeMotionAnalysis.left(
                  codeMotionAnalysis.leftPathFor(oppositeSideAnchor.element)
                ),
                leftPreservations,
                coincidentInsertionsOrEditsOnLeft
              )

        def selection(
            candidates: IndexedSeqView[Section[Element]]
        ): IndexedSeq[Section[Element]] = candidates
          .takeWhile(candidate =>
            !preservations.contains(
              candidate
            ) && !oppositeSideAnchors.contains(
              candidate
            ) && !coincidentInsertionsOrEdits.contains(candidate)
          )
          // At this point, we only have a plain view rather than an indexed
          // one...
          .toIndexedSeq

        oppositeSideAnchor match
          case OppositeSideAnchor.Plain(element) =>
            precedingAnchoredContentUsingSelection(
              file,
              element
            )(
              selection
            ) -> succeedingAnchoredContentUsingSelection(
              file,
              element
            )(selection)
          case OppositeSideAnchor.OnlyOneInMigratedEdit(element) =>
            precedingAnchoredContentUsingSelection(
              file,
              element
            )(
              selection
            ) -> succeedingAnchoredContentUsingSelection(
              file,
              element
            )(selection)
          case OppositeSideAnchor.FirstInMigratedEdit(element) =>
            precedingAnchoredContentUsingSelection(
              file,
              element
            )(
              selection
            ) -> IndexedSeq.empty
          case OppositeSideAnchor.LastInMigratedEdit(element) =>
            IndexedSeq.empty -> succeedingAnchoredContentUsingSelection(
              file,
              element
            )(selection)
        end match
      end anchoredContentFromOppositeSide

      def anchoredContentFromModeDestinationSide(
          moveDestinationSide: Side,
          moveDestinationAnchor: Section[Element]
      ): (IndexedSeq[Section[Element]], IndexedSeq[Section[Element]]) =
        val (file, preservations, coincidentInsertionsOrEdits) =
          moveDestinationSide match
            case Side.Left =>
              (
                codeMotionAnalysis.left(
                  codeMotionAnalysis.leftPathFor(moveDestinationAnchor)
                ),
                leftPreservations,
                coincidentInsertionsOrEditsOnLeft
              )
            case Side.Right =>
              (
                codeMotionAnalysis.right(
                  codeMotionAnalysis.rightPathFor(moveDestinationAnchor)
                ),
                rightPreservations,
                coincidentInsertionsOrEditsOnRight
              )

        def selection(
            candidates: IndexedSeqView[Section[Element]]
        ): IndexedSeq[Section[Element]] = candidates
          .takeWhile(candidate =>
            !preservations.contains(
              candidate
            ) && !moveDestinationAnchors.contains(
              candidate
            ) && !coincidentInsertionsOrEdits.contains(candidate)
          )
          // At this point, we only have a plain view rather than an indexed
          // one...
          .toIndexedSeq

        precedingAnchoredContentUsingSelection(file, moveDestinationAnchor)(
          selection
        ) -> succeedingAnchoredContentUsingSelection(
          file,
          moveDestinationAnchor
        )(
          selection
        )
      end anchoredContentFromModeDestinationSide

      case class MigrationSplices(
          precedingSplice: MergeResult[MultiSided[Section[Element]]],
          succeedingSplice: MergeResult[MultiSided[Section[Element]]],
          spliceMigrationSuppressions: Set[Section[Element]]
      )

      val conflictResolvingMergeAlgebra =
        new ConflictResolvingMergeAlgebra(migratedEditSuppressions)

      object CachedAnchoredContentMerges:
        private case class MergeInput(
            moveDestinationSide: Side,
            anchoredContentFromSource: IndexedSeq[Section[Element]],
            anchoredContentFromOppositeSide: IndexedSeq[
              Section[Element]
            ],
            anchoredContentFromMoveDestinationSide: IndexedSeq[Section[Element]]
        )

        private val resultsCache: Cache[MergeInput, MultiSidedMergeResult[
          Section[Element]
        ]] = Caffeine.newBuilder().build()

        def of(
            moveDestinationSide: Side,
            anchoredContentFromSource: IndexedSeq[Section[Element]],
            anchoredContentFromOppositeSide: IndexedSeq[Section[Element]],
            anchoredContentFromMoveDestinationSide: IndexedSeq[Section[Element]]
        ): MultiSidedMergeResult[Section[Element]] =
          moveDestinationSide match
            case Side.Left =>
              logger.debug(
                s"""Requesting merge of anchored content,\nsource:\n${pprintCustomised(
                    anchoredContentFromSource
                  )}, 
                   |left is the anchored move destination side:\n${pprintCustomised(
                    anchoredContentFromMoveDestinationSide
                  )}, 
                   |right side:\n${pprintCustomised(
                    anchoredContentFromOppositeSide
                  )}""".stripMargin
              )
            case Side.Right =>
              logger.debug(
                s"""Requesting merge of anchored content,\nsource:\n${pprintCustomised(
                    anchoredContentFromSource
                  )}, 
                   |left side:\n${pprintCustomised(
                    anchoredContentFromOppositeSide
                  )}, 
                   |right is the anchored move destination side:\n${pprintCustomised(
                    anchoredContentFromMoveDestinationSide
                  )}""".stripMargin
              )

          end match
          // NASTY HACK: how would you do it better?
          var updatedCache = false

          val cached = resultsCache.get(
            MergeInput(
              moveDestinationSide,
              anchoredContentFromSource,
              anchoredContentFromOppositeSide,
              anchoredContentFromMoveDestinationSide
            ),
            _ =>
              updatedCache = true

              moveDestinationSide match
                case Side.Left =>
                  mergeOf(mergeAlgebra = conflictResolvingMergeAlgebra)(
                    base = anchoredContentFromSource,
                    left = anchoredContentFromMoveDestinationSide,
                    right = anchoredContentFromOppositeSide
                  )
                case Side.Right =>
                  mergeOf(mergeAlgebra = conflictResolvingMergeAlgebra)(
                    base = anchoredContentFromSource,
                    left = anchoredContentFromOppositeSide,
                    right = anchoredContentFromMoveDestinationSide
                  )
              end match
          )

          if !updatedCache then
            logger.debug(s"Retrieved cached merge: ${pprintCustomised(cached)}")
          end if

          cached
        end of
      end CachedAnchoredContentMerges

      def mergesFrom(
          anchoredMove: AnchoredMove[Section[Element]]
      ): MigrationSplices =
        logger.debug(
          s"Merging anchored content on behalf of anchored move: ${pprintCustomised(anchoredMove)}."
        )

        val (
          precedingAnchoredContentFromSource,
          succeedingAnchoredContentFromSource
        ) =
          anchoredContentFromSource(anchoredMove.sourceAnchor)

        val (
          precedingAnchoredContentFromOppositeSide,
          succeedingAnchoredContentFromOppositeSide
        ) = anchoredContentFromOppositeSide(
          anchoredMove.moveDestinationSide,
          anchoredMove.oppositeSideAnchor
        )

        val (
          precedingAnchoredContentFromMoveDestinationSide,
          succeedingAnchoredContentFromMoveDestinationSide
        ) = anchoredContentFromModeDestinationSide(
          anchoredMove.moveDestinationSide,
          anchoredMove.moveDestinationAnchor
        )

        val spliceMigrationSuppressions =
          (precedingAnchoredContentFromOppositeSide
            ++ precedingAnchoredContentFromMoveDestinationSide
            ++ succeedingAnchoredContentFromOppositeSide
            ++ succeedingAnchoredContentFromMoveDestinationSide).toSet

        val precedingMerge = CachedAnchoredContentMerges
          .of(
            anchoredMove.moveDestinationSide,
            precedingAnchoredContentFromSource,
            precedingAnchoredContentFromOppositeSide,
            precedingAnchoredContentFromMoveDestinationSide
          )

        val succeedingMerge = CachedAnchoredContentMerges
          .of(
            anchoredMove.moveDestinationSide,
            succeedingAnchoredContentFromSource,
            succeedingAnchoredContentFromOppositeSide,
            succeedingAnchoredContentFromMoveDestinationSide
          )

        MigrationSplices(
          precedingMerge,
          succeedingMerge,
          spliceMigrationSuppressions
        )
      end mergesFrom

      val (
        splicesByAnchoredMoveDestination: MultiDict[
          (Section[Element], AnchoringSense),
          MergeResult[MultiSided[Section[Element]]]
        ],
        spliceMigrationSuppressions: Set[Section[Element]]
      ) = anchoredMoves.foldLeft(
        MultiDict.empty[(Section[Element], AnchoringSense), MergeResult[
          MultiSided[Section[Element]]
        ]] -> Set.empty[Section[Element]]
      ) {
        case (
              (partialKeyedSplices, partialSpliceMigrationSuppressions),
              anchoredMove
            ) =>
          val MigrationSplices(
            precedingSplice,
            succeedingSplice,
            spliceMigrationSuppressions
          ) = mergesFrom(anchoredMove)

          // NOTE: yes, this looks horrible, but try writing it using
          // `Option.unless` with flattening, or with `Option.fold`, or with
          // filters and folds, or a pattern match. Does it look any better?
          (if precedingSplice.isEmpty && succeedingSplice.isEmpty then
             partialKeyedSplices
           else if precedingSplice.isEmpty then
             partialKeyedSplices.add(
               anchoredMove.moveDestinationAnchor -> AnchoringSense.Predecessor,
               succeedingSplice
             )
           else if succeedingSplice.isEmpty then
             partialKeyedSplices.add(
               anchoredMove.moveDestinationAnchor -> AnchoringSense.Successor,
               precedingSplice
             )
           else
             partialKeyedSplices
               .add(
                 anchoredMove.moveDestinationAnchor -> AnchoringSense.Predecessor,
                 succeedingSplice
               )
               .add(
                 anchoredMove.moveDestinationAnchor -> AnchoringSense.Successor,
                 precedingSplice
               )
          )
          -> (partialSpliceMigrationSuppressions union spliceMigrationSuppressions)
      }

      logger.debug(
        s"Splices by anchored move destination: ${pprintCustomised(splicesByAnchoredMoveDestination)}."
      )
      logger.debug(
        s"Splice migration suppressions: ${pprintCustomised(spliceMigrationSuppressions)}."
      )

      def applySplices(
          path: Path,
          mergeResult: MultiSidedMergeResult[Section[Element]]
      ): (Path, MultiSidedMergeResult[Section[Element]]) =
        // Apply the suppressions....

        val withSuppressions = mergeResult.filterNot {
          case MultiSided.Unique(section) =>
            spliceMigrationSuppressions.contains(section)
          case MultiSided.Preserved(_, leftSection, rightSection) =>
            spliceMigrationSuppressions.contains(
              leftSection
            ) || spliceMigrationSuppressions.contains(rightSection)
          case _ => false
        }

        // Insert the splices....

        // Plan:
        // 1. Any anchored splice should be placed right next to the anchor.
        // 2. There has to be just one unique splice.
        // 3. If two anchors are adjacent and the predecessor shares the same
        // splice with the successor, just splice in one copy.

        extension (precedingSection: Option[MultiSided[Section[Element]]])
          private def notingMigratedSplice(
              migratedSplice: MergeResult[MultiSided[Section[Element]]]
          ): MergeResult[MultiSided[Section[Element]]] =
            if migratedSplice.nonEmpty then
              precedingSection.fold(ifEmpty =
                logger.debug(
                  s"Applying migrated splice of ${pprintCustomised(migratedSplice)} at the start."
                )
              )(destination =>
                logger.debug(
                  s"Applying migrated splice of ${pprintCustomised(migratedSplice)} after destination: ${pprintCustomised(destination)}."
                )
              )
            end if

            migratedSplice
        end extension

        def insertAnchoredSplices(
            side: MergeResult.Side[MultiSided[Section[Element]]]
        ): MergeResult.Side[MergeResult[MultiSided[Section[Element]]]] =
          val (
            (precedingSectionForLoggingContext, deferredMigration, _),
            sideWithAccumulatedSplices
          ) = side.innerFlatMapAccumulate(
            (
              None: Option[MultiSided[Section[Element]]],
              MergeResult.empty[MultiSided[Section[Element]]],
              false
            )
          ) {
            case (
                  (
                    precedingSectionForLoggingContext,
                    deferredSplice,
                    anchorPrecedingDeferredSpliceIsAmbiguous
                  ),
                  section @ MultiSided.Unique(candidateAnchorDestination)
                ) =>
              // TODO: shouldn't the helper `deduplicateWhenPossible` from
              // further down be used here to thin out the splice
              // alternatives?

              val precedingSplice =
                val precedingSpliceAlternatives =
                  splicesByAnchoredMoveDestination
                    .get(
                      candidateAnchorDestination -> AnchoringSense.Successor
                    )

                Option.when(precedingSpliceAlternatives.nonEmpty) {
                  val uniqueSplices =
                    uniqueSortedItemsFrom(precedingSpliceAlternatives)

                  uniqueSplices match
                    case head :: Nil =>
                      logger.debug(
                        s"Encountered succeeding anchor destination: ${pprintCustomised(candidateAnchorDestination)} with associated preceding migration splice: ${pprintCustomised(head)}."
                      )

                      head
                    case _ =>
                      throw new AdmissibleFailure(
                        s"""
                               |Multiple potential splices before destination: $candidateAnchorDestination,
                               |these are:
                               |${uniqueSplices
                            .map(splice => s"PRE-INSERTED: $splice")
                            .zipWithIndex
                            .map((splice, index) => s"${1 + index}. $splice")
                            .mkString("\n")}
                               |These are from ambiguous matches of anchor text with the destination.
                               |Consider setting the command line parameter `--minimum-ambiguous-match-size` to something larger than ${candidateAnchorDestination.size}.
                                """.stripMargin
                      )
                  end match
                }
              end precedingSplice

              val anchorIsAmbiguous =
                moveDestinationsReport.ambiguous.contains(
                  candidateAnchorDestination
                )

              val succeedingSplice =
                val succeedingSpliceAlternatives =
                  splicesByAnchoredMoveDestination
                    .get(
                      candidateAnchorDestination -> AnchoringSense.Predecessor
                    )

                Option.when(succeedingSpliceAlternatives.nonEmpty) {
                  val uniqueSplices =
                    uniqueSortedItemsFrom(succeedingSpliceAlternatives)

                  uniqueSplices match
                    case head :: Nil =>
                      logger.debug(
                        s"Encountered preceding anchor destination: ${pprintCustomised(candidateAnchorDestination)} with associated following migration splice: ${pprintCustomised(head)}."
                      )

                      head
                    case _ =>
                      throw new AdmissibleFailure(
                        s"""
                               |Multiple potential splices after destination: $candidateAnchorDestination,
                               |these are:
                               |${uniqueSplices
                            .map(splice => s"POST-INSERTION: $splice")
                            .zipWithIndex
                            .map((splice, index) => s"${1 + index}. $splice")
                            .mkString("\n")}
                               |These are from ambiguous matches of anchor text with the destination.
                               |Consider setting the command line parameter `--minimum-ambiguous-match-size` to something larger than ${candidateAnchorDestination.size}.
                                """.stripMargin
                      )
                  end match
                }
              end succeedingSplice

              precedingSplice match
                // NOTE: avoid use of lenses in the cases below when we
                // already have to pattern match deeply anyway...
                case None =>
                  // `candidateAnchorDestination` is not a succeeding anchor, so
                  // we can splice in the deferred migration from the previous
                  // preceding anchor.
                  (
                    Some(section),
                    succeedingSplice.getOrElse(
                      MergeResult.empty[MultiSided[Section[Element]]]
                    ),
                    succeedingSplice.fold(ifEmpty = true)(_ =>
                      anchorIsAmbiguous
                    )
                  ) -> Seq(
                    precedingSectionForLoggingContext.notingMigratedSplice(
                      deferredSplice
                    ),
                    MergeResult.of[MultiSided[Section[Element]]](section)
                  )

                case Some(precedingMigrationSplice) =>
                  // We have encountered a succeeding anchor...

                  @tailrec
                  def deduplicateWhenPossible(
                      first: MultiSided[Section[Element]],
                      second: MultiSided[Section[Element]]
                  ): Option[MultiSided[Section[Element]]] =
                    (first, second) match
                      case (
                            MultiSided.Preserved(_, firstLeft, firstRight),
                            MultiSided.Coincident(_, _)
                          ) =>
                        deduplicateWhenPossible(
                          MultiSided.Coincident(firstLeft, firstRight),
                          second
                        )
                      case (
                            MultiSided.Coincident(_, _),
                            MultiSided.Preserved(_, secondLeft, secondRight)
                          ) =>
                        deduplicateWhenPossible(
                          first,
                          MultiSided.Coincident(secondLeft, secondRight)
                        )
                      case (
                            MultiSided.Preserved(firstBase, _, _),
                            MultiSided.Unique(_)
                          ) =>
                        deduplicateWhenPossible(
                          MultiSided.Unique(firstBase),
                          second
                        )
                      case (
                            MultiSided.Unique(_),
                            MultiSided.Preserved(secondBase, _, _)
                          ) =>
                        deduplicateWhenPossible(
                          first,
                          MultiSided.Unique(secondBase)
                        )
                      case (
                            MultiSided.Coincident(firstLeft, _),
                            MultiSided.Unique(_)
                          ) =>
                        deduplicateWhenPossible(
                          MultiSided.Unique(firstLeft),
                          second
                        )
                      case (
                            MultiSided.Unique(_),
                            MultiSided.Coincident(secondLeft, _)
                          ) =>
                        deduplicateWhenPossible(
                          first,
                          MultiSided.Unique(secondLeft)
                        )
                      case _ =>
                        Option.when(
                          Ordering[MultiSided[Section[Element]]]
                            .equiv(first, second)
                        )(first)

                  val potentiallyDeduplicated = deferredSplice.fuseWith(
                    precedingMigrationSplice
                  )(deduplicateWhenPossible)

                  def oneSpliceOnly(
                      deduplicated: MergeResult[MultiSided[Section[Element]]]
                  ) =
                    (
                      Some(section),
                      succeedingSplice.getOrElse(
                        MergeResult.empty[MultiSided[Section[Element]]]
                      ),
                      anchorIsAmbiguous
                    ) -> Seq(
                      precedingSectionForLoggingContext
                        .notingMigratedSplice(
                          deduplicated
                        ),
                      MergeResult.of[MultiSided[Section[Element]]](section)
                    )

                  def twoSplices =
                    (
                      Some(section),
                      succeedingSplice
                        .getOrElse(
                          MergeResult.empty[MultiSided[Section[Element]]]
                        ),
                      anchorIsAmbiguous
                    ) -> Seq(
                      precedingSectionForLoggingContext
                        .notingMigratedSplice(
                          deferredSplice
                        ),
                      precedingSectionForLoggingContext
                        .notingMigratedSplice(
                          precedingMigrationSplice
                        ),
                      MergeResult.of[MultiSided[Section[Element]]](section)
                    )

                  potentiallyDeduplicated.fold(ifEmpty =
                    // The deferred migration from the previous preceding
                    // anchor and the succeeding anchor each contribute their
                    // own migration.
                    (
                      anchorPrecedingDeferredSpliceIsAmbiguous,
                      anchorIsAmbiguous
                    ) match
                      case (true, true)   => twoSplices
                      case (false, false) => twoSplices
                      case (true, false)  =>
                        oneSpliceOnly(precedingMigrationSplice)
                      case (false, true) => oneSpliceOnly(deferredSplice)
                  )(deduplicated =>
                    // The deferred migration from the previous preceding
                    // anchor and the succeeding anchor just encountered
                    // bracket the same migration.
                    oneSpliceOnly(deduplicated)
                  )
              end match
            case (
                  (precedingSectionForLoggingContext, deferredSplice, _),
                  section
                ) =>
              // If this matches, then `section` is definitely not an anchor,
              // so we can splice in the deferred migration from the previous
              // preceding anchor.
              (
                Some(section),
                MergeResult.empty[MultiSided[Section[Element]]],
                true
              ) -> Seq(
                precedingSectionForLoggingContext.notingMigratedSplice(
                  deferredSplice
                ),
                MergeResult.of[MultiSided[Section[Element]]](section)
              )
          }

          sideWithAccumulatedSplices.append(
            Seq(
              precedingSectionForLoggingContext.notingMigratedSplice(
                deferredMigration
              )
            )
          )
        end insertAnchoredSplices

        path -> withSuppressions.onEachSide(insertAnchoredSplices).flatten
      end applySplices

      def applySubstitutions(
          path: Path,
          mergeResult: MultiSidedMergeResult[Section[Element]]
      ): (Path, MultiSidedMergeResult[Section[Element]]) =
        def substituteFor(
            section: MultiSided[Section[Element]]
        ): Seq[MultiSided[Section[Element]]] =
          val substitutions = substitutionsByDestination
            .get(section)

          if substitutions.nonEmpty then
            val uniqueSubstitutions = uniqueSortedItemsFrom(substitutions)

            val substitution: Seq[MultiSided[Section[Element]]] =
              uniqueSubstitutions match
                case head :: Nil => head
                case _           =>
                  throw new AdmissibleFailure(
                    s"""
                       |Multiple potential changes migrated to destination: $section,
                       |these are:
                       |${uniqueSubstitutions
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

            if substitution.isEmpty then
              logger.debug(
                s"Applying migrated deletion to move destination: ${pprintCustomised(section)}."
              )
            else if !Ordering[Seq[MultiSided[Section[Element]]]]
                .equiv(substitution, IndexedSeq(section))
            then
              logger.debug(
                s"Applying migrated edit into ${pprintCustomised(substitution)} to move destination: ${pprintCustomised(section)}."
              )
            end if

            substitution
          else IndexedSeq(section)
          end if
        end substituteFor

        @tailrec
        def substituteThroughSectionsEliminatingAdjacentDuplicateSubstitutions(
            side: MergeResult.Side[MultiSided[Section[Element]]],
            previouslyAppliedSubstitutions: Set[
              Seq[MultiSided[Section[Element]]]
            ]
        ): MergeResult.Side[MultiSided[Section[Element]]] =
          val (
            (_, previouslyAppliedSubstitutionsWithLatestAdded),
            withLatestRoundOfSubstitutions
          ) = side.innerFlatMapAccumulate(
            (None: Option[
              Seq[MultiSided[Section[Element]]]
            ]) -> previouslyAppliedSubstitutions
          ) {
            case (
                  state @ (
                    priorSubstitution,
                    previouslyAppliedSubstitutionsPartialUpdate
                  ),
                  section
                ) =>
              val substitution = substituteFor(section)

              // NOTE: the use of `previouslyAppliedSubstitutions` in the
              // if-condition is *not* a mistake - it's perfectly OK (and
              // expected) to perform the same substitution in different
              // places; this check is to stop doing this in successive
              // recursive passes.
              if !previouslyAppliedSubstitutions.contains(substitution) then
                priorSubstitution match
                  case Some(duplicatedSubstitution)
                      if substitution == duplicatedSubstitution =>
                    state -> IndexedSeq.empty
                  case _ =>
                    (
                      Some(substitution),
                      previouslyAppliedSubstitutionsPartialUpdate + substitution
                    ) -> substitution

                end match
              else state -> IndexedSeq(section)
              end if
          }

          if side != withLatestRoundOfSubstitutions then
            // Keep repeating passes of substitution in case we have forwarded
            // edits or deletions. For a detailed example of this in operation,
            // see: https://github.com/sageserpent-open/kineticMerge/issues/205.
            substituteThroughSectionsEliminatingAdjacentDuplicateSubstitutions(
              withLatestRoundOfSubstitutions,
              previouslyAppliedSubstitutionsWithLatestAdded
            )
          else withLatestRoundOfSubstitutions
          end if
        end substituteThroughSectionsEliminatingAdjacentDuplicateSubstitutions

        path -> mergeResult.onEachSide { side =>
          substituteThroughSectionsEliminatingAdjacentDuplicateSubstitutions(
            side,
            previouslyAppliedSubstitutions = Set.empty
          )
        }
      end applySubstitutions

      def resolveSections(
          path: Path,
          mergeResult: MultiSidedMergeResult[Section[Element]]
      ): (Path, MergeResult[Element]) =
        path -> mergeResult.innerFlatMap(resolution)
      end resolveSections

      val secondPassMergeResultsByPath
          : Map[Path, MultiSidedMergeResult[Section[Element]]] =
        secondPassInputsByPath.map {
          case (path, Right(recording)) =>
            path -> recording
              .playback(conflictResolvingMergeAlgebra)
          case (path, Left(fullyMerged)) =>
            path -> fullyMerged.map(
              MultiSided.Unique.apply: Section[Element] => MultiSided[
                Section[Element]
              ]
            )
        }

      secondPassMergeResultsByPath
        .map(applySplices)
        .map(applySubstitutions)
        .map(resolveSections) -> moveDestinationsReport
    end merge
  end extension
end CodeMotionAnalysisExtension
