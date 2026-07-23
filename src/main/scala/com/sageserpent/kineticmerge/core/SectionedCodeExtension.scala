package com.sageserpent.kineticmerge.core

import cats.collections.{AvlSet, DisjointSets}
import cats.data.State
import cats.syntax.flatMap.catsSyntaxFlatMapOps
import cats.{Eq, Foldable, Order}
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import com.sageserpent.kineticmerge.core.CoreMergeAlgebra.MultiSidedMergeResult
import com.sageserpent.kineticmerge.core.FirstPassMergeResult.{
  FileDeletionContext,
  Recording
}
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.{
  Contribution,
  Sized
}
import com.sageserpent.kineticmerge.core.MergeResult.given
import com.sageserpent.kineticmerge.core.MoveDestinationsReport.{
  AnchoredMove,
  MoveEvaluation,
  OppositeSideAnchor
}
import com.sageserpent.kineticmerge.core.SectionedCode.Block
import com.sageserpent.kineticmerge.core.merge.{
  MergeAlgebra,
  mergeUsing,
  of as mergeOf
}
import com.sageserpent.kineticmerge.{
  ProgressRecording,
  ProgressRecordingSession,
  SilentProgressRecording,
  core
}
import com.typesafe.scalalogging.StrictLogging
import monocle.syntax.all.*

import scala.annotation.tailrec
import scala.collection.immutable.MultiDict
import scala.collection.{IndexedSeqView, Searching}
import scala.math.Ordering.Implicits.seqOrdering
import scala.util.Using

object SectionedCodeExtension extends StrictLogging:
  /** Add merging capability to a [[SectionedCode]]. */
  extension [Path, Element: Eq: Order](
      sectionedCode: SectionedCode[Path, Element]
  )
    def longestCommonSubsequenceOf(
        path: Path
    )(using
        progressRecording: ProgressRecording,
        sectionEq: Eq[Section[Element]],
        sectionSized: Sized[Section[Element]]
    ): LongestCommonSubsequence[Section[Element]] =
      val groupsOfParallelMatches = sectionedCode.groupsOfParallelMatches

      val baseBlocks  = sectionedCode.baseBlocksFor(path)
      val leftBlocks  = sectionedCode.leftBlocksFor(path)
      val rightBlocks = sectionedCode.rightBlocksFor(path)

      given Eq[Block[Element]] =
        // NOTE: this is subtle - comparing by `Block.parallelMatchesGroupId` is
        // OK, but consider situations where two distinct blocks covering the
        // same content on the same side can fool the following block-level
        // merge into a less than optimal alignment of blocks whenever the group
        // ids of the relevant blocks swap around from one to another. Peering
        // inside the two blocks matched content and the groups of matches that
        // the blocks refer to allows the merge to ignore the scrambled group
        // ids.
        // NOTE: tip of the hat to Jules for suggesting the content-based
        // comparison approach; I cheerfully ignored it at the time but have
        // come to realise its virtue!
        given Order[Match[Section[Element]]] = Order.by(aMatch =>
          (
            aMatch.baseContribution.map(_.content: Seq[Element]),
            aMatch.leftContribution.map(_.content: Seq[Element]),
            aMatch.rightContribution.map(_.content: Seq[Element])
          )
        )

        Eq.or(
          (lhs, rhs) =>
            (
              lhs.parallelMatchesGroupId,
              rhs.parallelMatchesGroupId
            ) match
              case (Some(lhsGroupId), Some(rhsGroupId)) =>
                lhsGroupId == rhsGroupId || Eq.eqv(
                  groupsOfParallelMatches(lhsGroupId),
                  groupsOfParallelMatches(rhsGroupId)
                )
              case _ => false,
          (lhs, rhs) =>
            Eq.eqv(
              lhs.sectionsCoveredByGroup: Seq[Section[Element]],
              rhs.sectionsCoveredByGroup: Seq[Section[Element]]
            )
        )
      end given

      given Sized[Block[Element]] = _.size

      case class ThreeSidedClump[X](
          base: IndexedSeq[X],
          left: IndexedSeq[X],
          right: IndexedSeq[X]
      ):
        def concatenate(successor: ThreeSidedClump[X]): ThreeSidedClump[X] =
          ThreeSidedClump(
            base = base ++ successor.base,
            left = left ++ successor.left,
            right = right ++ successor.right
          )
      end ThreeSidedClump

      type ThreeSidedClumps[X] = Vector[ThreeSidedClump[X]]

      val blockLevelMergeAlgebra =
        new MergeAlgebra[ThreeSidedClumps, Block[Element]]:
          override def empty: ThreeSidedClumps[Block[Element]] = Vector.empty
          override def preservation(
              result: ThreeSidedClumps[Block[Element]],
              preservedBaseElement: Block[Element],
              preservedElementOnLeft: Block[Element],
              preservedElementOnRight: Block[Element]
          ): ThreeSidedClumps[Block[Element]] = result.appended(
            ThreeSidedClump(
              Vector(preservedBaseElement),
              Vector(preservedElementOnLeft),
              Vector(preservedElementOnRight)
            )
          )

          override def leftInsertion(
              result: ThreeSidedClumps[Block[Element]],
              insertedElement: Block[Element]
          ): ThreeSidedClumps[Block[Element]] =
            result.appended(
              ThreeSidedClump(
                Vector.empty,
                Vector(insertedElement),
                Vector.empty
              )
            )

          override def rightInsertion(
              result: ThreeSidedClumps[Block[Element]],
              insertedElement: Block[Element]
          ): ThreeSidedClumps[Block[Element]] =
            result.appended(
              ThreeSidedClump(
                Vector.empty,
                Vector.empty,
                Vector(insertedElement)
              )
            )

          override def coincidentInsertion(
              result: ThreeSidedClumps[Block[Element]],
              insertedElementOnLeft: Block[Element],
              insertedElementOnRight: Block[Element]
          ): ThreeSidedClumps[Block[Element]] = result.appended(
            ThreeSidedClump(
              Vector.empty,
              Vector(insertedElementOnLeft),
              Vector(insertedElementOnRight)
            )
          )

          override def leftDeletion(
              result: ThreeSidedClumps[Block[Element]],
              deletedBaseElement: Block[Element],
              deletedRightElement: Block[Element]
          ): ThreeSidedClumps[Block[Element]] = result.appended(
            ThreeSidedClump(
              Vector(deletedBaseElement),
              Vector.empty,
              Vector(deletedRightElement)
            )
          )

          override def rightDeletion(
              result: ThreeSidedClumps[Block[Element]],
              deletedBaseElement: Block[Element],
              deletedLeftElement: Block[Element]
          ): ThreeSidedClumps[Block[Element]] = result.appended(
            ThreeSidedClump(
              Vector(deletedBaseElement),
              Vector(deletedLeftElement),
              Vector.empty
            )
          )

          override def coincidentDeletion(
              result: ThreeSidedClumps[Block[Element]],
              deletedElement: Block[Element]
          ): ThreeSidedClumps[Block[Element]] =
            result.appended(
              ThreeSidedClump(
                Vector(deletedElement),
                Vector.empty,
                Vector.empty
              )
            )

          override def leftEdit(
              result: ThreeSidedClumps[Block[Element]],
              editedBaseElement: Block[Element],
              editedRightElement: Block[Element],
              editElements: IndexedSeq[Block[Element]]
          ): ThreeSidedClumps[Block[Element]] = result.appended(
            ThreeSidedClump(
              Vector(editedBaseElement),
              editElements,
              Vector(editedRightElement)
            )
          )

          override def rightEdit(
              result: ThreeSidedClumps[Block[Element]],
              editedBaseElement: Block[Element],
              editedLeftElement: Block[Element],
              editElements: IndexedSeq[Block[Element]]
          ): ThreeSidedClumps[Block[Element]] = result.appended(
            ThreeSidedClump(
              Vector(editedBaseElement),
              Vector(editedLeftElement),
              editElements
            )
          )

          override def coincidentEdit(
              result: ThreeSidedClumps[Block[Element]],
              editedElement: Block[Element],
              editElements: IndexedSeq[(Block[Element], Block[Element])]
          ): ThreeSidedClumps[Block[Element]] =
            val (leftEditElements, rightEditElements) = editElements.unzip
            result.appended(
              ThreeSidedClump(
                Vector(editedElement),
                leftEditElements,
                rightEditElements
              )
            )
          end coincidentEdit

          override def conflict(
              result: ThreeSidedClumps[Block[Element]],
              editedElements: IndexedSeq[Block[Element]],
              leftEditElements: IndexedSeq[Block[Element]],
              rightEditElements: IndexedSeq[Block[Element]]
          ): ThreeSidedClumps[Block[Element]] =
            result.appended(
              ThreeSidedClump(
                editedElements,
                leftEditElements,
                rightEditElements
              )
            )
        end new
      end blockLevelMergeAlgebra

      val threeSidedClumps: ThreeSidedClumps[Block[Element]] =
        mergeOf(blockLevelMergeAlgebra)(
          baseBlocks,
          leftBlocks,
          rightBlocks,
          label = "Blocks merged:"
        )

      val rawSectionClumps: Vector[ThreeSidedClump[Section[Element]]] =
        threeSidedClumps.map { clump =>
          ThreeSidedClump(
            base = clump.base.flatMap(_.sectionsCoveredByGroup).distinct,
            left = clump.left.flatMap(_.sectionsCoveredByGroup).distinct,
            right = clump.right.flatMap(_.sectionsCoveredByGroup).distinct
          )
        }

      val sectionClumps: Vector[ThreeSidedClump[Section[Element]]] =
        rawSectionClumps.foldLeft(Vector.empty[ThreeSidedClump[Section[Element]]]) { (coalescedClumps, successor) =>
          def overlaps(clump: ThreeSidedClump[Section[Element]]): Boolean =
            import cats.syntax.apply.catsSyntaxTuple2Semigroupal

            val baseOverlap = (
              clump.base.lastOption.map(_.onePastEndOffset),
              successor.base.headOption.map(_.startOffset)
            ).mapN(_ > _)

            val leftOverlap = (
              clump.left.lastOption.map(_.onePastEndOffset),
              successor.left.headOption.map(_.startOffset)
            ).mapN(_ > _)

            val rightOverlap = (
              clump.right.lastOption.map(_.onePastEndOffset),
              successor.right.headOption.map(_.startOffset)
            ).mapN(_ > _)

            baseOverlap
              .orElse(leftOverlap)
              .orElse(rightOverlap)
              .getOrElse(false)

          val earliestOverlappingIndexOpt = coalescedClumps.indexWhere(overlaps)

          if earliestOverlappingIndexOpt != -1 then
            val (prefix, toCoalesce) = coalescedClumps.splitAt(earliestOverlappingIndexOpt)
            val allToCoalesce = toCoalesce :+ successor
            val coalesced = ThreeSidedClump(
              base = allToCoalesce.flatMap(_.base).distinct,
              left = allToCoalesce.flatMap(_.left).distinct,
              right = allToCoalesce.flatMap(_.right).distinct
            )
            prefix :+ coalesced
          else
            coalescedClumps :+ successor
        }

      type MatchSequence[X] = Vector[Match[X]]

      val matchSequence: MatchSequence[Section[Element]] =
        def matchSequenceOf(
            threeSidedClump: ThreeSidedClump[Section[Element]]
        ): MatchSequence[Section[Element]] =
          val sectionLevelMergeAlgebraExtractingAlignedMatchesOnly =
            new MergeAlgebra[MatchSequence, Section[Element]]:
              override def empty: MatchSequence[Section[Element]] = Vector.empty

              override def preservation(
                  result: MatchSequence[Section[Element]],
                  preservedBaseElement: Section[Element],
                  preservedElementOnLeft: Section[Element],
                  preservedElementOnRight: Section[Element]
              ): MatchSequence[Section[Element]] = result.appended(
                Match.AllSides(
                  preservedBaseElement,
                  preservedElementOnLeft,
                  preservedElementOnRight
                )
              )

              override def leftInsertion(
                  result: MatchSequence[Section[Element]],
                  insertedElement: Section[Element]
              ): MatchSequence[Section[Element]] = result

              override def rightInsertion(
                  result: MatchSequence[Section[Element]],
                  insertedElement: Section[Element]
              ): MatchSequence[Section[Element]] = result

              override def coincidentInsertion(
                  result: MatchSequence[Section[Element]],
                  insertedElementOnLeft: Section[Element],
                  insertedElementOnRight: Section[Element]
              ): MatchSequence[Section[Element]] = result.appended(
                Match.LeftAndRight(
                  insertedElementOnLeft,
                  insertedElementOnRight
                )
              )

              override def leftDeletion(
                  result: MatchSequence[Section[Element]],
                  deletedBaseElement: Section[Element],
                  deletedRightElement: Section[Element]
              ): MatchSequence[Section[Element]] = result.appended(
                Match.BaseAndRight(deletedBaseElement, deletedRightElement)
              )

              override def rightDeletion(
                  result: MatchSequence[Section[Element]],
                  deletedBaseElement: Section[Element],
                  deletedLeftElement: Section[Element]
              ): MatchSequence[Section[Element]] = result.appended(
                Match.BaseAndLeft(deletedBaseElement, deletedLeftElement)
              )

              override def coincidentDeletion(
                  result: MatchSequence[Section[Element]],
                  deletedElement: Section[Element]
              ): MatchSequence[Section[Element]] = result

              override def leftEdit(
                  result: MatchSequence[Section[Element]],
                  editedBaseElement: Section[Element],
                  editedRightElement: Section[Element],
                  editElements: IndexedSeq[Section[Element]]
              ): MatchSequence[Section[Element]] = result.appended(
                Match.BaseAndRight(editedBaseElement, editedRightElement)
              )

              override def rightEdit(
                  result: MatchSequence[Section[Element]],
                  editedBaseElement: Section[Element],
                  editedLeftElement: Section[Element],
                  editElements: IndexedSeq[Section[Element]]
              ): MatchSequence[Section[Element]] = result.appended(
                Match.BaseAndLeft(editedBaseElement, editedLeftElement)
              )

              override def coincidentEdit(
                  result: MatchSequence[Section[Element]],
                  editedElement: Section[Element],
                  editElements: IndexedSeq[(Section[Element], Section[Element])]
              ): MatchSequence[Section[Element]] =
                result.concat(editElements.map(Match.LeftAndRight.apply))

              override def conflict(
                  result: MatchSequence[Section[Element]],
                  editedElements: IndexedSeq[Section[Element]],
                  leftEditElements: IndexedSeq[Section[Element]],
                  rightEditElements: IndexedSeq[Section[Element]]
              ): MatchSequence[Section[Element]] = result

          given ProgressRecording = SilentProgressRecording

          // A section may be involved in more than one block - that's fine in
          // itself and is important to model in the preceding block-level
          // merge, but here we are merging sections and thus need to avoid
          // making fake alignments due to the same section being repeated on
          // one side, say, but matching with distinct sections on the other.
          mergeOf(sectionLevelMergeAlgebraExtractingAlignedMatchesOnly)(
            threeSidedClump.base,
            threeSidedClump.left,
            threeSidedClump.right,
            label = "Sections merged in three-sided clump:"
          )
        end matchSequenceOf

        sectionClumps.flatMap(matchSequenceOf)
      end matchSequence

      // PLAN: put each match into its own disjoint set and use a mapping from
      // section to match to see if a match shares any of its sections with a
      // previously encountered match. If it does, unify the disjoint sets for
      // the two matches. Then go through each of the resulting disjoint sets
      // and select the best ranked match - so an all-sides if one is
      // available, otherwise a pairwise one (there should only be one kind of
      // pairwise match). If there is more than one best ranked match, choose
      // the one that comes first in the match sequence. Explode the chosen
      // matches into contributions.

      val setsOfMatchesThatShareSectionsOnAtLeastOneSide =
        import cats.instances.vector.catsStdInstancesForVector

        val workflow = Foldable[Vector].foldM(
          matchSequence,
          Map.empty[Section[Element], Match[Section[Element]]]
        ) { case (matchesBySection, matchJustEncountered) =>
          val sectionsFromEncounteredMatch = Seq(
            matchJustEncountered.baseContribution,
            matchJustEncountered.leftContribution,
            matchJustEncountered.rightContribution
          ).flatten

          val previouslySeenMatchesSharingAtLeastOneSection =
            sectionsFromEncounteredMatch
              .flatMap(matchesBySection.get)
              .distinct

          val resultStep = State.pure[DisjointSets[
            Match[Section[Element]]
          ], Map[Section[Element], Match[Section[Element]]]](
            sectionsFromEncounteredMatch.foldLeft(matchesBySection)(
              (partialResult, section) =>
                partialResult.updated(section, matchJustEncountered)
            )
          )

          // NOTE: have to unify *all* of the previously seen matches, because
          // sharing a section between matches is not a transitive relationship
          // in general.
          previouslySeenMatchesSharingAtLeastOneSection
            .foldRight(resultStep)(
              (previouslySeenMatchSharingAtLeastOneSection, partialResult) =>
                DisjointSets.union(
                  matchJustEncountered,
                  previouslySeenMatchSharingAtLeastOneSection
                ) >> partialResult
            )
        } >> DisjointSets.toSets

        // NOTE: this is a tiebreaking order that works across from base to left
        // to right. It is required by `DisjointSets`, but also turns up later
        // in `representativeMatchesFrom`, because that order is carried over to
        // each `AvlSet` that is passed in. The matches in that set all share at
        // least one section in common with another, and the unshared sections
        // should be correlated by the section-level merging that produced them,
        // so it is safe to assume that this order aligns with order of
        // appearance of the sections on each side.
        given Order[Match[Section[Element]]] = Order.by(aMatch =>
          (
            aMatch.baseContribution.map(_.startOffset),
            aMatch.leftContribution.map(_.startOffset),
            aMatch.rightContribution.map(_.startOffset)
          )
        )

        workflow
          .runA(DisjointSets(matchSequence*))
          .value
      end setsOfMatchesThatShareSectionsOnAtLeastOneSide

      def representativeMatchesFrom[X](
          matchesSharingAtLeastOneSection: AvlSet[Match[X]]
      ): Seq[Match[X]] =
        require(!matchesSharingAtLeastOneSection.isEmpty)

        val (allSidesMatches, pairwiseMatches) =
          matchesSharingAtLeastOneSection.toIterator.partition(
            _.isAnAllSidesMatch
          )

        if allSidesMatches.hasNext then
          var seenBase  = Set.empty[X]
          var seenLeft  = Set.empty[X]
          var seenRight = Set.empty[X]

          def markSeen(aMatch: Match[X]): Unit =
            aMatch.baseContribution.foreach(seenBase += _)
            aMatch.leftContribution.foreach(seenLeft += _)
            aMatch.rightContribution.foreach(seenRight += _)
          end markSeen

          allSidesMatches.flatMap { aMatch =>
            val base =
              aMatch.baseContribution.filterNot(seenBase.contains)
            val left =
              aMatch.leftContribution.filterNot(seenLeft.contains)
            val right =
              aMatch.rightContribution.filterNot(seenRight.contains)

            val demotedMatch = (base, left, right) match
              case (Some(b), Some(l), Some(r)) => Some(Match.AllSides(b, l, r))
              case (Some(b), Some(l), None)    => Some(Match.BaseAndLeft(b, l))
              case (Some(b), None, Some(r))    => Some(Match.BaseAndRight(b, r))
              case (None, Some(l), Some(r))    => Some(Match.LeftAndRight(l, r))
              case _                           => None

            demotedMatch.foreach(markSeen)

            demotedMatch
          }.toSeq
        else Seq(pairwiseMatches.next())
        end if
      end representativeMatchesFrom

      def matchesCross(
          m1: Match[Section[Element]],
          m2: Match[Section[Element]]
      ): Boolean =
        val baseLeftCross =
          for
            b1 <- m1.baseContribution
            b2 <- m2.baseContribution
            l1 <- m1.leftContribution
            l2 <- m2.leftContribution
          yield (b1.startOffset < b2.startOffset && l1.startOffset >= l2.startOffset) ||
            (b1.startOffset > b2.startOffset && l1.startOffset <= l2.startOffset)

        val baseRightCross =
          for
            b1 <- m1.baseContribution
            b2 <- m2.baseContribution
            r1 <- m1.rightContribution
            r2 <- m2.rightContribution
          yield (b1.startOffset < b2.startOffset && r1.startOffset >= r2.startOffset) ||
            (b1.startOffset > b2.startOffset && r1.startOffset <= r2.startOffset)

        val leftRightCross =
          for
            l1 <- m1.leftContribution
            l2 <- m2.leftContribution
            r1 <- m1.rightContribution
            r2 <- m2.rightContribution
          yield (l1.startOffset < l2.startOffset && r1.startOffset >= r2.startOffset) ||
            (l1.startOffset > l2.startOffset && r1.startOffset <= r2.startOffset)

        Seq(baseLeftCross, baseRightCross, leftRightCross).flatten.exists(
          identity
        )
      end matchesCross

      val bestMatches =
        val rawMatches = setsOfMatchesThatShareSectionsOnAtLeastOneSide.toList
          .flatMap((_, matchesSharingASectionOnAtLeastOneSide) =>
            representativeMatchesFrom(matchesSharingASectionOnAtLeastOneSide)
          )

        rawMatches.combinations(2).foreach {
          case Seq(m1, m2) =>
            assert(
              !matchesCross(m1, m2),
              s"Post-condition failed: matches cross!\n$m1\n$m2"
            )
          case _ =>
        }

        rawMatches
      end bestMatches

      type Contributions = Map[Section[Element], Contribution[Section[Element]]]

      def recordContributionsFromMatch(
          partialResult: Contributions,
          aMatch: Match[Section[Element]]
      ): Contributions =
        aMatch match
          case Match.AllSides(baseSection, leftSection, rightSection) =>
            partialResult
              .updated(baseSection, Contribution.Common(baseSection))
              .updated(leftSection, Contribution.Common(leftSection))
              .updated(rightSection, Contribution.Common(rightSection))
          case Match.BaseAndLeft(baseSection, leftSection) =>
            partialResult
              .updated(
                baseSection,
                Contribution.CommonToBaseAndLeftOnly(baseSection)
              )
              .updated(
                leftSection,
                Contribution.CommonToBaseAndLeftOnly(leftSection)
              )
          case Match.BaseAndRight(baseSection, rightSection) =>
            partialResult
              .updated(
                baseSection,
                Contribution.CommonToBaseAndRightOnly(baseSection)
              )
              .updated(
                rightSection,
                Contribution.CommonToBaseAndRightOnly(rightSection)
              )
          case Match.LeftAndRight(leftSection, rightSection) =>
            partialResult
              .updated(
                leftSection,
                Contribution.CommonToLeftAndRightOnly(leftSection)
              )
              .updated(
                rightSection,
                Contribution.CommonToLeftAndRightOnly(rightSection)
              )

      val bestContributions: Contributions =
        bestMatches.foldLeft(Map.empty)(recordContributionsFromMatch)

      val bestContributionsWithFallback =
        bestContributions.withDefault(Contribution.Difference.apply)

      def assignContributions(
          sections: IndexedSeq[Section[Element]]
      ): IndexedSeq[Contribution[Section[Element]]] =
        sections.map(bestContributionsWithFallback)

      extension (filesByPath: Map[Path, File[Element]])
        private def sectionsAt(path: Path): IndexedSeq[Section[Element]] =
          filesByPath.get(path).fold(ifEmpty = IndexedSeq.empty)(_.sections)
      end extension

      LongestCommonSubsequence(
        base = assignContributions(sectionedCode.base.sectionsAt(path)),
        left = assignContributions(sectionedCode.left.sectionsAt(path)),
        right = assignContributions(sectionedCode.right.sectionsAt(path))
      )
    end longestCommonSubsequenceOf

    def merge(using
        progressRecording: ProgressRecording
    ): (
        Map[Path, MergeResult[Element]],
        MoveDestinationsReport[Section[Element]]
    ) =
      import sectionedCode.matchesFor

      given sectionSized[X]: Sized[Section[X]] = _.size

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
        sectionedCode.base.keySet ++ sectionedCode.left.keySet ++ sectionedCode.right.keySet

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
            given ProgressRecording with
              override def newSession(label: String, maximumProgress: Int)(
                  initialProgress: Int
              ): ProgressRecordingSession =
                progressRecording.newSession(
                  s"(Merging: $path) $label",
                  maximumProgress
                )(initialProgress)
            end given

            val base  = sectionedCode.base.get(path).map(_.sections)
            val left  = sectionedCode.left.get(path).map(_.sections)
            val right = sectionedCode.right.get(path).map(_.sections)

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
              case (Some(_), None, Some(_)) =>
                // The file has disappeared on the left side. That may indicate
                // a simple deletion of the file, or may be a renaming on the
                // left.

                // Merge with fake empty content on the left...

                val firstPassMergeResult
                    : FirstPassMergeResult[Section[Element]] =
                  longestCommonSubsequenceOf(path).mergeUsing(
                    mergeAlgebra =
                      FirstPassMergeResult.mergeAlgebra(fileDeletionContext =
                        FileDeletionContext.Left
                      ),
                    label = s"Sections merged:"
                  )

                partialMergeResult.aggregate(path, firstPassMergeResult)
              case (Some(_), Some(_), None) =>
                // The file has disappeared on the right side. That may indicate
                // a simple deletion of the file, or may be a renaming on the
                // right.

                // Merge with fake empty content on the right...

                val firstPassMergeResult
                    : FirstPassMergeResult[Section[Element]] =
                  longestCommonSubsequenceOf(path).mergeUsing(
                    mergeAlgebra =
                      FirstPassMergeResult.mergeAlgebra(fileDeletionContext =
                        FileDeletionContext.Right
                      ),
                    label = s"Sections merged:"
                  )

                partialMergeResult.aggregate(path, firstPassMergeResult)
              case (
                    _,
                    Some(_),
                    Some(_)
                  ) =>
                // Mix of possibilities - the file may have been added on both
                // sides, or modified on either or both sides. There is also an
                // extraneous case where the file is on all three sides but
                // hasn't changed.

                // Whichever is the case, merge...

                val firstPassMergeResult
                    : FirstPassMergeResult[Section[Element]] =
                  longestCommonSubsequenceOf(path).mergeUsing(
                    mergeAlgebra =
                      FirstPassMergeResult.mergeAlgebra(fileDeletionContext =
                        FileDeletionContext.None
                      ),
                    label = s"Sections merged:"
                  )

                partialMergeResult.aggregate(path, firstPassMergeResult)

              case (None, None, None) =>
                throw new AssertionError(
                  s"Logic error: path $path does not exist on the base, left or right side."
                )
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
          sectionedCode.base(sectionedCode.basePathFor(sourceAnchor))

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
          moveDestinationSide: MoveDestinationSide,
          oppositeSideAnchor: OppositeSideAnchor[Section[Element]]
      ): (
          Option[IndexedSeq[Section[Element]]],
          Option[IndexedSeq[Section[Element]]]
      ) =
        val (file, preservations, coincidentInsertionsOrEdits) =
          moveDestinationSide match
            case MoveDestinationSide.Left =>
              (
                sectionedCode.right(
                  sectionedCode.rightPathFor(oppositeSideAnchor.element)
                ),
                rightPreservations,
                coincidentInsertionsOrEditsOnRight
              )
            case MoveDestinationSide.Right =>
              (
                sectionedCode.left(
                  sectionedCode.leftPathFor(oppositeSideAnchor.element)
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
            Some(
              precedingAnchoredContentUsingSelection(
                file,
                element
              )(
                selection
              )
            ) -> Some(
              succeedingAnchoredContentUsingSelection(
                file,
                element
              )(selection)
            )
          case OppositeSideAnchor.OnlyOneInMigratedEdit(element) =>
            Some(
              precedingAnchoredContentUsingSelection(
                file,
                element
              )(
                selection
              )
            ) -> Some(
              succeedingAnchoredContentUsingSelection(
                file,
                element
              )(selection)
            )
          case OppositeSideAnchor.FirstInMigratedEdit(element) =>
            Some(
              precedingAnchoredContentUsingSelection(
                file,
                element
              )(
                selection
              )
            ) -> None
          case OppositeSideAnchor.LastInMigratedEdit(element) =>
            None -> Some(
              succeedingAnchoredContentUsingSelection(
                file,
                element
              )(selection)
            )
        end match
      end anchoredContentFromOppositeSide

      def anchoredContentFromModeDestinationSide(
          moveDestinationSide: MoveDestinationSide,
          moveDestinationAnchor: Section[Element]
      ): (IndexedSeq[Section[Element]], IndexedSeq[Section[Element]]) =
        val (file, preservations, coincidentInsertionsOrEdits) =
          moveDestinationSide match
            case MoveDestinationSide.Left =>
              (
                sectionedCode.left(
                  sectionedCode.leftPathFor(moveDestinationAnchor)
                ),
                leftPreservations,
                coincidentInsertionsOrEditsOnLeft
              )
            case MoveDestinationSide.Right =>
              (
                sectionedCode.right(
                  sectionedCode.rightPathFor(moveDestinationAnchor)
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
            moveDestinationSide: MoveDestinationSide,
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
            moveDestinationSide: MoveDestinationSide,
            anchoredContentFromSource: IndexedSeq[Section[Element]],
            anchoredContentFromOppositeSide: IndexedSeq[Section[Element]],
            anchoredContentFromMoveDestinationSide: IndexedSeq[Section[Element]]
        ): MultiSidedMergeResult[Section[Element]] =
          moveDestinationSide match
            case MoveDestinationSide.Left =>
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
            case MoveDestinationSide.Right =>
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

              given ProgressRecording = SilentProgressRecording

              moveDestinationSide match
                case MoveDestinationSide.Left =>
                  mergeOf(mergeAlgebra = conflictResolvingMergeAlgebra)(
                    base = anchoredContentFromSource,
                    left = anchoredContentFromMoveDestinationSide,
                    right = anchoredContentFromOppositeSide,
                    label = "Splice merge to left destination:"
                  )
                case MoveDestinationSide.Right =>
                  mergeOf(mergeAlgebra = conflictResolvingMergeAlgebra)(
                    base = anchoredContentFromSource,
                    left = anchoredContentFromOppositeSide,
                    right = anchoredContentFromMoveDestinationSide,
                    label = "Splice merge to right destination:"
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
            .map(
              _ ++ precedingAnchoredContentFromMoveDestinationSide
            )
            .getOrElse(IndexedSeq.empty)
            ++ succeedingAnchoredContentFromOppositeSide
              .map(
                _ ++ succeedingAnchoredContentFromMoveDestinationSide
              )
              .getOrElse(IndexedSeq.empty)).toSet

        def spliceFrom(
            anchoredContentFromOppositeSide: Option[
              IndexedSeq[Section[Element]]
            ],
            anchoredContentFromSource: IndexedSeq[Section[Element]],
            anchoredContentFromMoveDestinationSide: IndexedSeq[Section[Element]]
        ) =
          anchoredContentFromOppositeSide
            .fold(ifEmpty = MergeResult.empty[MultiSided[Section[Element]]])(
              CachedAnchoredContentMerges
                .of(
                  anchoredMove.moveDestinationSide,
                  anchoredContentFromSource,
                  _,
                  anchoredContentFromMoveDestinationSide
                )
            )

        MigrationSplices(
          precedingSplice = spliceFrom(
            precedingAnchoredContentFromOppositeSide,
            precedingAnchoredContentFromSource,
            precedingAnchoredContentFromMoveDestinationSide
          ),
          succeedingSplice = spliceFrom(
            succeedingAnchoredContentFromOppositeSide,
            succeedingAnchoredContentFromSource,
            succeedingAnchoredContentFromMoveDestinationSide
          ),
          spliceMigrationSuppressions = spliceMigrationSuppressions
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

        def attemptLastMinuteResolutionViaContextOfContributions(
            mergeResult: MergeResult[MultiSided[Section[Element]]],
            anchoringSense: AnchoringSense
        ): MergeResult[MultiSided[Section[Element]]] =
          // Look for a conflict at the other end of a conflicted merge result
          // (this will be a splice) away from its *single* anchor; this may be
          // the entire splice. Then see if either side of the conflict turns
          // out to be a suffix or prefix (depending on whether the anchor
          // precedes or succeeds the splice) of whatever context originally
          // came before or after the side's contribution. Take the side with
          // the longest such affix and use it as a replacement context for the
          // other side, resolving the conflicted merge.
          Option
            .when(mergeResult.isConflicted)(anchoringSense)
            .flatMap {
              case AnchoringSense.Predecessor => mergeResult.lastOption
              case AnchoringSense.Successor   => mergeResult.headOption
            }
            .collect {
              case MergeResult.Segment.Conflicted(
                    _,
                    leftSections,
                    rightSections
                  ) =>
                val leftAffixSize = (anchoringSense match
                  case AnchoringSense.Predecessor => rightSections.lastOption
                  case AnchoringSense.Successor   => rightSections.headOption
                ).collect { case MultiSided.Unique(element) =>
                  element
                }.flatMap { adjacentToRightContext =>
                  val rightFile = sectionedCode.right(
                    sectionedCode.rightPathFor(adjacentToRightContext)
                  )

                  val Searching.Found(indexOfSection) =
                    rightFile.searchByStartOffset(
                      adjacentToRightContext.startOffset
                    ): @unchecked

                  val rightContext = anchoringSense match
                    case AnchoringSense.Predecessor =>
                      rightFile.sections
                        .drop(1 + indexOfSection)
                        .flatMap(_.content)
                    case AnchoringSense.Successor =>
                      rightFile.sections
                        .take(indexOfSection)
                        .flatMap(_.content)

                  val leftPotentialAffix = leftSections.collect {
                    case MultiSided.Unique(element) => element
                  } flatMap (_.content)

                  Option.when(
                    leftPotentialAffix.nonEmpty && (anchoringSense match
                      case AnchoringSense.Predecessor =>
                        // NOTE: like `.startsWith`, only using `Eq`.
                        rightContext
                          .take(leftPotentialAffix.size)
                          .corresponds(leftPotentialAffix)(Eq.eqv)
                      case AnchoringSense.Successor =>
                        // NOTE: like `.endsWith`, only using `Eq`.
                        rightContext
                          .takeRight(leftPotentialAffix.size)
                          .corresponds(leftPotentialAffix)(Eq.eqv))
                  )(
                    leftPotentialAffix.size
                  )
                }.getOrElse(0)

                val rightAffixSize = (anchoringSense match
                  case AnchoringSense.Predecessor => leftSections.lastOption
                  case AnchoringSense.Successor   => leftSections.headOption
                ).collect { case MultiSided.Unique(element) =>
                  element
                }.flatMap { adjacentToLeftContext =>
                  val leftFile = sectionedCode.left(
                    sectionedCode.leftPathFor(adjacentToLeftContext)
                  )

                  val Searching.Found(indexOfSection) =
                    leftFile.searchByStartOffset(
                      adjacentToLeftContext.startOffset
                    ): @unchecked

                  val leftContext = anchoringSense match
                    case AnchoringSense.Predecessor =>
                      leftFile.sections
                        .drop(1 + indexOfSection)
                        .flatMap(_.content)
                    case AnchoringSense.Successor =>
                      leftFile.sections.take(indexOfSection).flatMap(_.content)

                  val rightPotentialAffix = rightSections.collect {
                    case MultiSided.Unique(element) => element
                  } flatMap (_.content)

                  Option.when(
                    rightPotentialAffix.nonEmpty && (anchoringSense match
                      case AnchoringSense.Predecessor =>
                        // NOTE: like `.startsWith`, only using `Eq`.
                        leftContext
                          .take(rightPotentialAffix.size)
                          .corresponds(rightPotentialAffix)(Eq.eqv)
                      case AnchoringSense.Successor =>
                        // NOTE: like `.endsWith`, only using `Eq`.
                        leftContext
                          .takeRight(rightPotentialAffix.size)
                          .corresponds(rightPotentialAffix)(Eq.eqv))
                  )(
                    rightPotentialAffix.size
                  )
                }.getOrElse(0)

                if leftAffixSize > rightAffixSize then
                  // Use the left sections as replacement context for the
                  // right sections.
                  anchoringSense match
                    case AnchoringSense.Predecessor =>
                      mergeResult.tail :+ MergeResult.Segment.Resolved(
                        rightSections ++ leftSections
                      )
                    case AnchoringSense.Successor =>
                      MergeResult.Segment.Resolved(
                        leftSections ++ rightSections
                      ) +: mergeResult.init
                  end match
                else if leftAffixSize < rightAffixSize then
                  // Use the right sections as replacement context for the
                  // left sections.
                  anchoringSense match
                    case AnchoringSense.Predecessor =>
                      mergeResult.tail :+ MergeResult.Segment.Resolved(
                        leftSections ++ rightSections
                      )
                    case AnchoringSense.Successor =>
                      MergeResult.Segment.Resolved(
                        rightSections ++ leftSections
                      ) +: mergeResult.init
                  end match
                else
                  // Neither can serve as replacement context; either that or
                  // they are both as good as each other. The conflict can't be
                  // resolved.
                  mergeResult
                end if
            }
            .getOrElse(mergeResult)
        end attemptLastMinuteResolutionViaContextOfContributions

        def insertAnchoredSplices(
            side: MergeResult.Side[MultiSided[Section[Element]]]
        ): MergeResult.Side[MergeResult[MultiSided[Section[Element]]]] =
          case class Deferral(
              deferredSplice: MergeResult[MultiSided[Section[Element]]],
              anchorPrecedingDeferredSpliceIsAmbiguous: Boolean
          )

          val (
            (precedingSectionForLoggingContext, deferral),
            sideWithAccumulatedSplices
          ) = side.innerFlatMapAccumulate(
            (
              None: Option[MultiSided[Section[Element]]],
              None: Option[Deferral]
            )
          ) {
            case (
                  (
                    precedingSectionForLoggingContext,
                    deferral
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

                if precedingSpliceAlternatives.nonEmpty then
                  val uniqueSplices =
                    uniqueSortedItemsFrom(precedingSpliceAlternatives)

                  assume(uniqueSplices.nonEmpty)

                  uniqueSplices match
                    case Seq(splice) =>
                      logger.debug(
                        s"Encountered succeeding anchor destination: ${pprintCustomised(candidateAnchorDestination)} with associated preceding migration splice: ${pprintCustomised(splice)}."
                      )

                      Some(splice)
                    case _ =>
                      logger.info(
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

                      None
                  end match
                else None
                end if
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

                if succeedingSpliceAlternatives.nonEmpty then
                  val uniqueSplices =
                    uniqueSortedItemsFrom(succeedingSpliceAlternatives)

                  assume(uniqueSplices.nonEmpty)

                  uniqueSplices match
                    case Seq(splice) =>
                      logger.debug(
                        s"Encountered preceding anchor destination: ${pprintCustomised(candidateAnchorDestination)} with associated following migration splice: ${pprintCustomised(splice)}."
                      )

                      Some(splice)
                    case _ =>
                      logger.info(
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

                      None
                  end match
                else None
                end if
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
                    succeedingSplice.map(Deferral(_, anchorIsAmbiguous))
                  ) -> deferral.fold(ifEmpty =
                    Seq(MergeResult.of[MultiSided[Section[Element]]](section))
                  ) { case Deferral(deferredSplice, _) =>
                    Seq(
                      precedingSectionForLoggingContext.notingMigratedSplice(
                        attemptLastMinuteResolutionViaContextOfContributions(
                          deferredSplice,
                          AnchoringSense.Predecessor
                        )
                      ),
                      MergeResult.of[MultiSided[Section[Element]]](section)
                    )
                  }

                case Some(precedingMigrationSplice) =>
                  // We have encountered a succeeding anchor...
                  deferral.fold(ifEmpty =
                    (
                      Some(section),
                      succeedingSplice.map(Deferral(_, anchorIsAmbiguous))
                    ) -> Seq(
                      precedingSectionForLoggingContext
                        .notingMigratedSplice(
                          attemptLastMinuteResolutionViaContextOfContributions(
                            precedingMigrationSplice,
                            AnchoringSense.Successor
                          )
                        ),
                      MergeResult.of[MultiSided[Section[Element]]](section)
                    )
                  ) {
                    case Deferral(
                          deferredSplice,
                          anchorPrecedingDeferredSpliceIsAmbiguous
                        ) =>
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

                      // The previous preceding anchor from the deferred
                      // migration and the succeeding anchor just encountered
                      // splice in a bracketed shared migration.
                      def oneSpliceOnly(
                          deduplicated: MergeResult[
                            MultiSided[Section[Element]]
                          ]
                      ) =
                        (
                          Some(section),
                          succeedingSplice.map(Deferral(_, anchorIsAmbiguous))
                        ) -> Seq(
                          precedingSectionForLoggingContext
                            .notingMigratedSplice(
                              deduplicated
                            ),
                          MergeResult.of[MultiSided[Section[Element]]](section)
                        )

                      // The previous preceding anchor from the deferred
                      // migration and the succeeding anchor just encountered
                      // splice in their own migrations separately.
                      def twoSplices =
                        (
                          Some(section),
                          succeedingSplice.map(Deferral(_, anchorIsAmbiguous))
                        ) -> Seq(
                          precedingSectionForLoggingContext
                            .notingMigratedSplice(
                              attemptLastMinuteResolutionViaContextOfContributions(
                                deferredSplice,
                                AnchoringSense.Predecessor
                              )
                            ),
                          precedingSectionForLoggingContext
                            .notingMigratedSplice(
                              attemptLastMinuteResolutionViaContextOfContributions(
                                precedingMigrationSplice,
                                AnchoringSense.Successor
                              )
                            ),
                          MergeResult.of[MultiSided[Section[Element]]](section)
                        )

                      potentiallyDeduplicated.fold(ifEmpty =
                        (
                          anchorPrecedingDeferredSpliceIsAmbiguous,
                          anchorIsAmbiguous
                        ) match
                          case (true, true)   => twoSplices
                          case (false, false) => twoSplices
                          case (true, false)  =>
                            oneSpliceOnly(precedingMigrationSplice)
                          case (false, true) => oneSpliceOnly(deferredSplice)
                      )(deduplicated => oneSpliceOnly(deduplicated))
                  }
              end match
            case (
                  (precedingSectionForLoggingContext, deferral),
                  section
                ) =>
              // If this matches, then `section` is definitely not an anchor,
              // so we can splice in the deferred migration from the previous
              // preceding anchor.
              (
                Some(section),
                None
              ) -> deferral.fold(ifEmpty =
                Seq(MergeResult.of[MultiSided[Section[Element]]](section))
              ) { case Deferral(deferredSplice, _) =>
                Seq(
                  precedingSectionForLoggingContext.notingMigratedSplice(
                    attemptLastMinuteResolutionViaContextOfContributions(
                      deferredSplice,
                      AnchoringSense.Predecessor
                    )
                  ),
                  MergeResult.of[MultiSided[Section[Element]]](section)
                )
              }
          }

          deferral.fold(ifEmpty = sideWithAccumulatedSplices) {
            case Deferral(deferredSplice, _) =>
              // Splice in the remaining deferred migration from the previous
              // preceding anchor as there is nothing else left to consider.
              sideWithAccumulatedSplices.append(
                Seq(
                  precedingSectionForLoggingContext.notingMigratedSplice(
                    attemptLastMinuteResolutionViaContextOfContributions(
                      deferredSplice,
                      AnchoringSense.Predecessor
                    )
                  )
                )
              )
          }
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

            assume(uniqueSubstitutions.nonEmpty)

            uniqueSubstitutions match
              case Seq(substitution) =>
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

              case _ =>
                logger.info(
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

                IndexedSeq(section)
            end match
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

      val result =
        Using(
          progressRecording.newSession(
            label = "Post-alignment merge processing:",
            maximumProgress = 1
          )(initialProgress = 0)
        ) { progressRecordingSession =>
          val result = secondPassMergeResultsByPath
            .map(applySplices)
            .map(applySubstitutions)
            .map(resolveSections) -> moveDestinationsReport
          progressRecordingSession.upTo(1)
          result
        }.get

      result
    end merge
  end extension
end SectionedCodeExtension
