package com.sageserpent.kineticmerge.core

import cats.{Eq, Order}
import com.sageserpent.kineticmerge.core.CoreMergeAlgebra.MultiSidedMergeResult
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.{
  Contribution,
  Sized
}
import com.sageserpent.kineticmerge.core.MergeResult.given
import com.sageserpent.kineticmerge.core.MultiSided.given
import com.sageserpent.kineticmerge.core.merge.of as mergeOf
import com.sageserpent.kineticmerge.{
  NoProgressRecording,
  ProgressRecording,
  ProgressRecordingSession
}
import com.typesafe.scalalogging.StrictLogging

import scala.collection.immutable.MultiDict
import scala.collection.mutable
import scala.math.Ordering.Implicits.seqOrdering

object SectionedCodeExtension extends StrictLogging:

  private enum Side:
    case Base, Left, Right

  private case class Token[Path, Element](
      gapOrGroup: Either[Section[Element], MatchAnalysis.ParallelMatchGroup[
        Element
      ]]
  )

  /** Add merging capability to a [[SectionedCode]].
    */
  extension [Path, Element: Eq: Order](
      sectionedCode: SectionedCode[Path, Element]
  )
    def merge(using
        progressRecording: ProgressRecording
    ): (
        Map[Path, MergeResult[Element]],
        MoveDestinationsReport[Section[Element]]
    ) =
      val theSectionedCode = sectionedCode
      import theSectionedCode.{
        basePathFor,
        leftPathFor,
        matchesFor,
        rightPathFor
      }

      given sectionEquality: Eq[Section[Element]] with
        override def eqv(
            lhs: Section[Element],
            rhs: Section[Element]
        ): Boolean =
          val bothBelongToTheSameMatches =
            matchesFor(lhs).intersect(matchesFor(rhs)).nonEmpty

          bothBelongToTheSameMatches || lhs.size == rhs.size && Eq[Seq[Element]]
            .eqv(lhs.content, rhs.content)
      end sectionEquality

      given Sized[Section[Element]] = _.size

      val paths =
        sectionedCode.base.keySet ++ sectionedCode.left.keySet ++ sectionedCode.right.keySet

      val parallelMatchGroupsByBaseSection = MultiDict.from(
        sectionedCode.parallelMatchGroups.toSeq.flatMap(group =>
          group.flatMap(_.baseElementOption).map(_ -> group)
        )
      )

      val parallelMatchGroupsByLeftSection = MultiDict.from(
        sectionedCode.parallelMatchGroups.toSeq.flatMap(group =>
          group.flatMap(_.leftElementOption).map(_ -> group)
        )
      )

      val parallelMatchGroupsByRightSection = MultiDict.from(
        sectionedCode.parallelMatchGroups.toSeq.flatMap(group =>
          group.flatMap(_.rightElementOption).map(_ -> group)
        )
      )

      val allSectionsToPath: Map[Section[Element], Path] =
        sectionedCode.base.toSeq.flatMap { case (path, file) =>
          file.sections.map(_ -> path)
        }.toMap ++
          sectionedCode.left.toSeq.flatMap { case (path, file) =>
            file.sections.map(_ -> path)
          }.toMap ++
          sectionedCode.right.toSeq.flatMap { case (path, file) =>
            file.sections.map(_ -> path)
          }.toMap

      val leftSections  = sectionedCode.left.values.flatMap(_.sections).toSet
      val rightSections = sectionedCode.right.values.flatMap(_.sections).toSet

      def resolution(
          multiSided: MultiSided[Section[Element]]
      ): IndexedSeq[Element] =
        multiSided match
          case MultiSided.Unique(element) => element.content
          case MultiSided.Coincident(leftElement, rightElement) =>
            leftElement.content
          case MultiSided.Preserved(
                baseElement,
                leftElement,
                rightElement
              ) =>
            val lhsIsCompletelyUnchanged =
              baseElement.content == leftElement.content
            val rhsIsCompletelyUnchanged =
              baseElement.content == rightElement.content

            (lhsIsCompletelyUnchanged, rhsIsCompletelyUnchanged) match
              case (false, true) => leftElement.content
              case (true, false) => rightElement.content
              case _             =>
                leftElement.content
            end match

      given tokenEquality: Eq[Token[Path, Element]] with
        override def eqv(
            lhs: Token[Path, Element],
            rhs: Token[Path, Element]
        ): Boolean = (lhs.gapOrGroup, rhs.gapOrGroup) match
          case (Left(lhsGap), Left(rhsGap)) =>
            lhsGap.size == rhsGap.size && Eq[Seq[Element]]
              .eqv(lhsGap.content, rhsGap.content)
          case (Right(lhsGroup), Right(rhsGroup)) =>
            lhsGroup == rhsGroup
          case _ => false
      end tokenEquality

      given Sized[Token[Path, Element]] = _ => 1

      def tokensFor(
          sections: IndexedSeq[Section[Element]],
          parallelMatchGroupsBySection: MultiDict[Section[
            Element
          ], MatchAnalysis.ParallelMatchGroup[Element]]
      ): IndexedSeq[Token[Path, Element]] =
        val tokens = mutable.Buffer.empty[Token[Path, Element]]

        val groupToSectionsInFile = sections
          .flatMap(s =>
            parallelMatchGroupsBySection.get(s).headOption.map(_ -> s)
          )
          .groupMap(_._1)(_._2)

        val groupSpans = groupToSectionsInFile.view
          .mapValues { ss =>
            (ss.map(_.startOffset).min, ss.map(_.onePastEndOffset).max)
          }
          .toMap

        val sortedGroupsByStart = groupSpans.toSeq.sortBy(_._2._1)

        var currentOffset  = 0
        val sectionByStart = sections.map(s => s.startOffset -> s).toMap
        val fileEndOffset =
          sections.lastOption.map(_.onePastEndOffset).getOrElse(0)

        sortedGroupsByStart.foreach { case (group, (spanStart, spanEnd)) =>
          // Emit gaps before the group
          while currentOffset < spanStart do
            sectionByStart.get(currentOffset) match
              case Some(s) =>
                tokens += Token(Left(s))
                currentOffset = s.onePastEndOffset
              case None =>
                // Should not happen if sections cover the file
                currentOffset += 1

          // Emit group
          tokens += Token(Right(group))
          currentOffset = spanEnd
        }

        // Emit remaining gaps
        while currentOffset < fileEndOffset do
          sectionByStart.get(currentOffset) match
            case Some(s) =>
              tokens += Token(Left(s))
              currentOffset = s.onePastEndOffset
            case None =>
              currentOffset += 1

        tokens.toIndexedSeq
      end tokensFor

      def parallelMatchGroupLcs(
          group: MatchAnalysis.ParallelMatchGroup[Element]
      ): LongestCommonSubsequence[Section[Element]] =
        val baseContributions =
          mutable.Buffer.empty[Contribution[Section[Element]]]
        val leftContributions =
          mutable.Buffer.empty[Contribution[Section[Element]]]
        val rightContributions =
          mutable.Buffer.empty[Contribution[Section[Element]]]

        val lastBaseSectionByPath  = mutable.Map.empty[Path, Section[Element]]
        val lastLeftSectionByPath  = mutable.Map.empty[Path, Section[Element]]
        val lastRightSectionByPath = mutable.Map.empty[Path, Section[Element]]

        def gaps(
            file: File[Element],
            previousSection: Option[Section[Element]],
            currentSection: Section[Element],
            pathFor: Section[Element] => Path
        ): IndexedSeq[Section[Element]] =
          previousSection match
            case Some(previous)
                if pathFor(previous) == pathFor(currentSection) =>
              val gapSize =
                currentSection.startOffset - previous.onePastEndOffset
              if gapSize > 0 then
                file.sections.view
                  .filter(s =>
                    s.startOffset >= previous.onePastEndOffset && s.onePastEndOffset <= currentSection.startOffset
                  )
                  .toIndexedSeq
              else IndexedSeq.empty
            case _ => IndexedSeq.empty
        end gaps

        group.foreach { currentMatch =>
          currentMatch.baseElementOption.foreach { baseSection =>
            val path = basePathFor(baseSection)
            gaps(
              sectionedCode.base(path),
              lastBaseSectionByPath.get(path),
              baseSection,
              basePathFor
            )
              .foreach(gap => baseContributions += Contribution.Difference(gap))
            lastBaseSectionByPath(path) = baseSection
          }
          currentMatch.leftElementOption.foreach { leftSection =>
            val path = leftPathFor(leftSection)
            gaps(
              sectionedCode.left(path),
              lastLeftSectionByPath.get(path),
              leftSection,
              leftPathFor
            )
              .foreach(gap => leftContributions += Contribution.Difference(gap))
            lastLeftSectionByPath(path) = leftSection
          }
          currentMatch.rightElementOption.foreach { rightSection =>
            val path = rightPathFor(rightSection)
            gaps(
              sectionedCode.right(path),
              lastRightSectionByPath.get(path),
              rightSection,
              rightPathFor
            )
              .foreach(gap =>
                rightContributions += Contribution.Difference(gap)
              )
            lastRightSectionByPath(path) = rightSection
          }

          currentMatch match
            case Match.AllSides(base, left, right) =>
              baseContributions += Contribution.Common(base)
              leftContributions += Contribution.Common(left)
              rightContributions += Contribution.Common(right)
            case Match.BaseAndLeft(base, left) =>
              baseContributions += Contribution.CommonToBaseAndLeftOnly(base)
              leftContributions += Contribution.CommonToBaseAndLeftOnly(left)
            case Match.BaseAndRight(base, right) =>
              baseContributions += Contribution.CommonToBaseAndRightOnly(base)
              rightContributions += Contribution.CommonToBaseAndRightOnly(right)
            case Match.LeftAndRight(left, right) =>
              leftContributions += Contribution.CommonToLeftAndRightOnly(left)
              rightContributions += Contribution.CommonToLeftAndRightOnly(right)
        }

        LongestCommonSubsequence.from(
          baseContributions.toIndexedSeq,
          leftContributions.toIndexedSeq,
          rightContributions.toIndexedSeq
        )
      end parallelMatchGroupLcs

      val algebra = new CoreMergeAlgebra[Section[Element]]
      def precalculateGroupMerge(
          group: MatchAnalysis.ParallelMatchGroup[Element]
      ): MultiSidedMergeResult[Section[Element]] =
        val lcs = parallelMatchGroupLcs(group)
        given ProgressRecording = NoProgressRecording
        mergeOf(algebra)(lcs)
      end precalculateGroupMerge

      val groupMergeCache: Map[
        MatchAnalysis.ParallelMatchGroup[Element],
        MultiSidedMergeResult[Section[Element]]
      ] =
        sectionedCode.parallelMatchGroups
          .map(group => group -> precalculateGroupMerge(group))
          .toMap

      val topLevelMergeAlgebra =
        new com.sageserpent.kineticmerge.core.merge.MergeAlgebra[
          MultiSidedMergeResult,
          Token[Path, Element]
        ]:
          override def empty: MultiSidedMergeResult[Token[Path, Element]] =
            MergeResult.empty

          override def preservation(
              result: MultiSidedMergeResult[Token[Path, Element]],
              preservedBaseElement: Token[Path, Element],
              preservedElementOnLeft: Token[Path, Element],
              preservedElementOnRight: Token[Path, Element]
          ): MultiSidedMergeResult[Token[Path, Element]] =
            result.addResolved(
              MultiSided.Preserved(
                preservedBaseElement,
                preservedElementOnLeft,
                preservedElementOnRight
              )
            )

          override def leftInsertion(
              result: MultiSidedMergeResult[Token[Path, Element]],
              insertedElement: Token[Path, Element]
          ): MultiSidedMergeResult[Token[Path, Element]] =
            result.addResolved(MultiSided.Unique(insertedElement))

          override def rightInsertion(
              result: MultiSidedMergeResult[Token[Path, Element]],
              insertedElement: Token[Path, Element]
          ): MultiSidedMergeResult[Token[Path, Element]] =
            result.addResolved(MultiSided.Unique(insertedElement))

          override def coincidentInsertion(
              result: MultiSidedMergeResult[Token[Path, Element]],
              insertedElementOnLeft: Token[Path, Element],
              insertedElementOnRight: Token[Path, Element]
          ): MultiSidedMergeResult[Token[Path, Element]] =
            result.addResolved(
              MultiSided.Coincident(
                insertedElementOnLeft,
                insertedElementOnRight
              )
            )

          override def leftDeletion(
              result: MultiSidedMergeResult[Token[Path, Element]],
              deletedBaseElement: Token[Path, Element],
              deletedRightElement: Token[Path, Element]
          ): MultiSidedMergeResult[Token[Path, Element]] = result

          override def rightDeletion(
              result: MultiSidedMergeResult[Token[Path, Element]],
              deletedBaseElement: Token[Path, Element],
              deletedLeftElement: Token[Path, Element]
          ): MultiSidedMergeResult[Token[Path, Element]] = result

          override def coincidentDeletion(
              result: MultiSidedMergeResult[Token[Path, Element]],
              deletedElement: Token[Path, Element]
          ): MultiSidedMergeResult[Token[Path, Element]] = result

          override def leftEdit(
              result: MultiSidedMergeResult[Token[Path, Element]],
              editedBaseElement: Token[Path, Element],
              editedRightElement: Token[Path, Element],
              editElements: IndexedSeq[Token[Path, Element]]
          ): MultiSidedMergeResult[Token[Path, Element]] =
            result.addResolved(editElements.map(MultiSided.Unique.apply))

          override def rightEdit(
              result: MultiSidedMergeResult[Token[Path, Element]],
              editedBaseElement: Token[Path, Element],
              editedLeftElement: Token[Path, Element],
              editElements: IndexedSeq[Token[Path, Element]]
          ): MultiSidedMergeResult[Token[Path, Element]] =
            result.addResolved(editElements.map(MultiSided.Unique.apply))

          override def coincidentEdit(
              result: MultiSidedMergeResult[Token[Path, Element]],
              editedElement: Token[Path, Element],
              editElements: IndexedSeq[
                (Token[Path, Element], Token[Path, Element])
              ]
          ): MultiSidedMergeResult[Token[Path, Element]] =
            result.addResolved(editElements.map(MultiSided.Coincident.apply))

          override def conflict(
              result: MultiSidedMergeResult[Token[Path, Element]],
              editedElements: IndexedSeq[Token[Path, Element]],
              leftEditElements: IndexedSeq[Token[Path, Element]],
              rightEditElements: IndexedSeq[Token[Path, Element]]
          ): MultiSidedMergeResult[Token[Path, Element]] =
            result.addConflicted(
              editedElements.map(MultiSided.Unique.apply),
              leftEditElements.map(MultiSided.Unique.apply),
              rightEditElements.map(MultiSided.Unique.apply)
            )
      end topLevelMergeAlgebra

      val topLevelMergeResultsByPath =
        paths.foldLeft(
          Map.empty[Path, MultiSidedMergeResult[Token[Path, Element]]]
        ) {
          case (partialMergeResults, path) =>
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

            val baseTokens = base.fold(ifEmpty = IndexedSeq.empty)(
              tokensFor(_, parallelMatchGroupsByBaseSection)
            )
            val leftTokens = left.fold(ifEmpty = IndexedSeq.empty)(
              tokensFor(_, parallelMatchGroupsByLeftSection)
            )
            val rightTokens = right.fold(ifEmpty = IndexedSeq.empty)(
              tokensFor(_, parallelMatchGroupsByRightSection)
            )

            val topLevelMergeResult = mergeOf(topLevelMergeAlgebra)(
              base = baseTokens,
              left = leftTokens,
              right = rightTokens
            )

            partialMergeResults + (path -> topLevelMergeResult)
        }

      def resolveTokens(
          multiSidedToken: MultiSided[Token[Path, Element]],
          currentPath: Path
      ): MultiSidedMergeResult[Section[Element]] =
        val unfiltered: MultiSidedMergeResult[Section[Element]] =
          multiSidedToken match
            case MultiSided.Unique(token) =>
              token.gapOrGroup match
                case Left(gap) =>
                  MergeResult.of[MultiSided[Section[Element]]](
                    MultiSided.Unique(gap)
                  )
                case Right(group) => groupMergeCache(group)
            case MultiSided.Coincident(leftToken, rightToken) =>
              (leftToken.gapOrGroup, rightToken.gapOrGroup) match
                case (Right(group), _) => groupMergeCache(group)
                case (_, Right(group)) => groupMergeCache(group)
                case (Left(leftGap), Left(rightGap)) =>
                  MergeResult.of[MultiSided[Section[Element]]](
                    MultiSided.Coincident(leftGap, rightGap)
                  )
            case MultiSided.Preserved(baseToken, leftToken, rightToken) =>
              (
                baseToken.gapOrGroup,
                leftToken.gapOrGroup,
                rightToken.gapOrGroup
              ) match
                case (Right(group), _, _) => groupMergeCache(group)
                case (_, Right(group), _) => groupMergeCache(group)
                case (_, _, Right(group)) => groupMergeCache(group)
                case (Left(baseGap), Left(leftGap), Left(rightGap)) =>
                  MergeResult.of[MultiSided[Section[Element]]](
                    MultiSided.Preserved(baseGap, leftGap, rightGap)
                  )

        unfiltered.filter {
          case MultiSided.Preserved(_, left, right) =>
            allSectionsToPath.get(left).contains(currentPath) ||
            allSectionsToPath.get(right).contains(currentPath)
          case MultiSided.Coincident(left, right) =>
            allSectionsToPath.get(left).contains(currentPath) ||
            allSectionsToPath.get(right).contains(currentPath)
          case MultiSided.Unique(s) =>
            (leftSections.contains(s) || rightSections.contains(s)) &&
            allSectionsToPath.get(s).contains(currentPath)
        }
      end resolveTokens

      val finalResultsByPath: Map[Path, MergeResult[Element]] =
        topLevelMergeResultsByPath.view
          .map { (path, m) =>
            path -> MergeResult
              .flatten(m.map(resolveTokens(_, path)))
              .innerFlatMap(resolution)
          }
          .toMap

      val moveDestinationsBySource =
        sectionedCode.parallelMatchGroups.toSeq
          .flatMap { group =>
            group.flatMap { m =>
              m.baseElementOption.map { baseSection =>
                val destinations = m match
                  case Match.AllSides(_, left, right) =>
                    Set(
                      SpeculativeMoveDestination.Left(left),
                      SpeculativeMoveDestination.Right(right),
                      SpeculativeMoveDestination.Coincident(left, right)
                    )
                  case Match.BaseAndLeft(_, left) =>
                    Set(SpeculativeMoveDestination.Left(left))
                  case Match.BaseAndRight(_, right) =>
                    Set(SpeculativeMoveDestination.Right(right))
                  case _ =>
                    Set.empty[SpeculativeMoveDestination[Section[Element]]]

                baseSection -> destinations
              }
            }
          }
          .groupMap(_._1)(_._2)
          .view
          .mapValues { destinations =>
            val flattened = destinations.flatten.toSet
            val coincidentPairs = flattened.collect {
              case p @ SpeculativeMoveDestination.Coincident(l, r) => p
            }
            val coincidentLefts =
              coincidentPairs.map(_.elementPairAcrossLeftAndRight._1)
            val coincidentRights =
              coincidentPairs.map(_.elementPairAcrossLeftAndRight._2)

            val filtered = flattened.filter {
              case SpeculativeMoveDestination.Left(l) =>
                !coincidentLefts.contains(l)
              case SpeculativeMoveDestination.Right(r) =>
                !coincidentRights.contains(r)
              case _ => true
            }

            val finalDestinations =
              mutable.Set[SpeculativeMoveDestination[Section[Element]]]()
            val seenAll = mutable.Set[Section[Element]]()

            filtered.toSeq
              .sortBy {
                case _: SpeculativeMoveDestination.Coincident[?] => 0
                case _                                           => 1
              }
              .foreach { d =>
                val members = d match
                  case SpeculativeMoveDestination.Left(l)          => Set(l)
                  case SpeculativeMoveDestination.Right(r)         => Set(r)
                  case SpeculativeMoveDestination.Coincident(l, r) => Set(l, r)
                end members
                if members.intersect(seenAll).isEmpty then
                  finalDestinations += d
                  seenAll ++= members
                end if
              }

            new MoveDestinations(finalDestinations.toSet)
          }
          .toMap

      val moveDestinationsReport = MoveDestinationsReport(
        moveDestinationsBySource
      )

      finalResultsByPath -> moveDestinationsReport
    end merge
  end extension
end SectionedCodeExtension
