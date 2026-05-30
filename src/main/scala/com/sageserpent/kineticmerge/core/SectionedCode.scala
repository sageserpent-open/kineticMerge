package com.sageserpent.kineticmerge.core

import cats.Eq
import com.google.common.hash.{Funnel, HashFunction}
import com.sageserpent.kineticmerge
import com.sageserpent.kineticmerge.core
import com.sageserpent.kineticmerge.core.MatchAnalysis.*
import com.sageserpent.kineticmerge.core.SectionedCode.Block
import com.typesafe.scalalogging.StrictLogging

import scala.collection.immutable.SortedSet
import scala.collection.{Searching, mutable}

trait SectionedCode[Path, Element]:
  def base: Map[Path, File[Element]]
  def left: Map[Path, File[Element]]
  def right: Map[Path, File[Element]]

  def matchesFor(
      section: Section[Element]
  ): collection.Set[Match[Section[Element]]]

  def basePathFor(baseSection: Section[Element]): Path
  def leftPathFor(leftSection: Section[Element]): Path
  def rightPathFor(rightSection: Section[Element]): Path

  // TODO: trim out some if not all of the methods for getting at groups once
  // blocks have fully landed.

  def parallelMatchesGroupIdsByMatch: ParallelMatchesGroupIdsByMatch[Element]

  def groupsOfParallelMatches
      : Map[ParallelMatchesGroupId, SortedSet[GenericMatch[Element]]]

  def baseBlocksFor(path: Path): IndexedSeq[Block[Element]]

  def leftBlocksFor(path: Path): IndexedSeq[Block[Element]]

  def rightBlocksFor(path: Path): IndexedSeq[Block[Element]]
end SectionedCode

object SectionedCode extends StrictLogging:
  /** @param baseSources
    *   The common base sources from which the left and right sources are
    *   derived.
    * @param leftSources
    *   'Our' sources, from the Git standpoint...
    * @param rightSources
    *   'Their' sources, from the Git standpoint...
    * @param configuration
    *   [[Configuration]] parameter object.
    * @tparam Path
    * @return
    *   A [[SectionedCode]] that contains a breakdown into [[File]] instances
    *   and thence into [[Section]] instances for each of the three sources.
    */
  def of[Path, Element: Eq: Funnel](
      baseSources: Sources[Path, Element],
      leftSources: Sources[Path, Element],
      rightSources: Sources[Path, Element]
  )(
      configuration: AbstractConfiguration,
      suppressMatchesInvolvingOverlappingSections: Boolean = true
  )(using
      hashFunction: HashFunction
  ): Either[Throwable, SectionedCode[Path, Element]] =
    val withAllMatchesOfAtLeastTheSureFireWindowSize =
      MatchAnalysis.of(baseSources, leftSources, rightSources)(configuration)

    val withAllMatchesOfAtLeastTheMinimumWindowSize =
      withAllMatchesOfAtLeastTheSureFireWindowSize.withAllSmallFryMatches()

    val withTinyMatchesIncluded =
      withAllMatchesOfAtLeastTheMinimumWindowSize.withTinyMatches

    // TODO: this also precariously protects some downstream logic in
    // `reconcileMatches` that assumes that all matches will have a
    // parallel matches group id. Need to make this more robust.
    val parallelMatchesOnly = withTinyMatchesIncluded.parallelMatchesOnly

    try
      val matchesAndTheirSections = parallelMatchesOnly.reconcileMatches(
        suppressMatchesInvolvingOverlappingSections
      )

      val sectionsAndTheirMatches =
        matchesAndTheirSections.sectionsAndTheirMatches

      // Use the sections covered by the tiny matches to break up gap fills on
      // all sides. This gives the downstream merge a chance to make last-minute
      // matches of its own between small unmatched sections that are deleted
      // from the base and their counterparts on the left or right.
      // See https://github.com/sageserpent-open/kineticMerge/issues/42 and
      // https://github.com/sageserpent-open/kineticMerge/issues/43.

      val baseFilesByPath =
        baseSources.filesByPathUtilising(mandatorySections =
          matchesAndTheirSections.baseSections
        )
      val leftFilesByPath =
        leftSources.filesByPathUtilising(mandatorySections =
          matchesAndTheirSections.leftSections
        )
      val rightFilesByPath =
        rightSources.filesByPathUtilising(mandatorySections =
          matchesAndTheirSections.rightSections
        )

      val groupsOfParallelMatches =
        matchesAndTheirSections.groupsOfParallelMatches

      def blocksForASide(
          sectionExtractor: Match[Section[Element]] => Option[Section[Element]],
          pathExtractor: Section[Element] => Path,
          filesByPath: Map[Path, File[Element]]
      ): Map[Path, IndexedSeq[Block[Element]]] =
        def blockFrom(
            parallelMatchesGroupId: ParallelMatchesGroupId,
            parallelMatches: SortedSet[GenericMatch[Element]]
        ): Option[(Path, Block[Element])] =
          val relevantSections =
            parallelMatches.toIndexedSeq
              .flatMap(sectionExtractor)

          Option
            .when(relevantSections.nonEmpty) {
              val path = pathExtractor(relevantSections.head)

              val file = filesByPath(path)

              val Searching.Found(startingSectionIndex) =
                file.searchByStartOffset(
                  relevantSections.head.startOffset
                ): @unchecked

              val Searching.Found(endingSectionIndex) =
                file.searchByStartOffset(
                  relevantSections.last.startOffset
                ): @unchecked

              val sectionsCoveredByBlock = file.sections.slice(
                startingSectionIndex,
                1 + endingSectionIndex
              )

              path -> Block(
                Some(parallelMatchesGroupId),
                sectionsCoveredByBlock
              )
            }
        end blockFrom

        val matchedBlocksByPath = groupsOfParallelMatches.toSeq
          .map(blockFrom)
          .collect { case Some((path, block)) => path -> block }
          .groupMap(_._1)(_._2)

        filesByPath.map { (path, file) =>
          val matchedBlocks =
            matchedBlocksByPath.getOrElse(path, IndexedSeq.empty)

          val sortedMatchedBlocks = matchedBlocks.sortBy(_.startOffset)

          val fillerBlocks = mutable.Buffer.empty[Block[Element]]
          var currentEnd   = 0
          for block <- sortedMatchedBlocks do
            if block.startOffset > currentEnd then
              val Searching.Found(startingSectionIndex) =
                file.searchByStartOffset(currentEnd): @unchecked
              val Searching.Found(endingSectionIndex) =
                file.searchByStartOffset(block.startOffset): @unchecked
              fillerBlocks += Block(
                parallelMatchesGroupId = None,
                sectionsCoveredByGroup =
                  file.sections.slice(startingSectionIndex, endingSectionIndex)
              )
            end if
            currentEnd = currentEnd max block.onePastEndOffset
          end for

          if currentEnd < file.size then
            val Searching.Found(startingSectionIndex) =
              file.searchByStartOffset(currentEnd): @unchecked
            fillerBlocks += Block(
              parallelMatchesGroupId = None,
              sectionsCoveredByGroup = file.sections.drop(startingSectionIndex)
            )
          end if

          path -> (matchedBlocks ++ fillerBlocks).toIndexedSeq.sortBy(block =>
            (
              block.startOffset,
              block.onePastEndOffset,
              block.parallelMatchesGroupId
            )
          )
        }
      end blocksForASide

      val baseBlocks = blocksForASide(
        sectionExtractor = _.baseContribution,
        pathExtractor = baseSources.pathFor,
        filesByPath = baseFilesByPath
      )
      val leftBlocks = blocksForASide(
        sectionExtractor = _.leftContribution,
        pathExtractor = leftSources.pathFor,
        filesByPath = leftFilesByPath
      )
      val rightBlocks = blocksForASide(
        sectionExtractor = _.rightContribution,
        pathExtractor = rightSources.pathFor,
        filesByPath = rightFilesByPath
      )

      Right(new SectionedCode[Path, Element]:
        override def base: Map[Path, File[Element]] = baseFilesByPath

        override def left: Map[Path, File[Element]] = leftFilesByPath

        override def right: Map[Path, File[Element]] = rightFilesByPath

        override def matchesFor(
            section: Section[Element]
        ): collection.Set[Match[Section[Element]]] =
          sectionsAndTheirMatches.get(section)

        export baseSources.pathFor as basePathFor
        export leftSources.pathFor as leftPathFor
        export rightSources.pathFor as rightPathFor

        export matchesAndTheirSections.parallelMatchesGroupIdsByMatch

        export matchesAndTheirSections.groupsOfParallelMatches

        override def baseBlocksFor(path: Path): IndexedSeq[Block[Element]] =
          baseBlocks.getOrElse(path, IndexedSeq.empty)

        override def leftBlocksFor(path: Path): IndexedSeq[Block[Element]] =
          leftBlocks.getOrElse(path, IndexedSeq.empty)

        override def rightBlocksFor(path: Path): IndexedSeq[Block[Element]] =
          rightBlocks.getOrElse(path, IndexedSeq.empty)

        {
          // Invariant: the matches are referenced only by their participating
          // sections.
          val allMatchKeys = sectionsAndTheirMatches.keySet

          val allParticipatingSections = sectionsAndTheirMatches.values.toSet
            .map {
              case Match.AllSides(baseSection, leftSection, rightSection) =>
                Set(baseSection, leftSection, rightSection)
              case Match.BaseAndLeft(baseSection, leftSection) =>
                Set(baseSection, leftSection)
              case Match.BaseAndRight(baseSection, rightSection) =>
                Set(baseSection, rightSection)
              case Match.LeftAndRight(leftSection, rightSection) =>
                Set(leftSection, rightSection)
            }
            .reduceOption(_ union _)
            .getOrElse(Set.empty)

          require(allMatchKeys == allParticipatingSections)

          // Invariant - every section across all paths on all three sides is
          // unique. This is vital for `CodeMotionAnalysisExtension` to be able
          // to work with move sources, destinations and to recognise where to
          // perform substitutions.

          val allSections =
            (baseFilesByPath.values ++ leftFilesByPath.values ++ rightFilesByPath.values)
              .flatMap(_.sections)
              .toSeq

          val allDistinctSections = allSections.toSet

          require(allSections.size == allDistinctSections.size)

          // Invariant: all the match keys should belong to the breakdown
          // of sections.

          val rogueMatches =
            (allMatchKeys diff allDistinctSections).flatMap(
              sectionsAndTheirMatches.get
            )

          require(
            rogueMatches.isEmpty,
            s"Found rogue matches whose sections do not belong to the breakdown: ${pprintCustomised(rogueMatches)}."
          )
        })
    catch
      // NOTE: don't convert this to use of `Try` with a subsequent `.toEither`
      // conversion. We want most flavours of exception to propagate, as they
      // are likely to be logic errors or something just as unwholesome.
      case admissibleException: AdmissibleFailure => Left(admissibleException)
    end try
  end of

  case class Block[Element](
      parallelMatchesGroupId: Option[ParallelMatchesGroupId],
      sectionsCoveredByGroup: IndexedSeq[Section[Element]]
  ):
    require(sectionsCoveredByGroup.nonEmpty)
    sectionsCoveredByGroup.zip(sectionsCoveredByGroup.tail).foreach {
      case (predecessor, successor) =>
        require(predecessor.onePastEndOffset == successor.startOffset)
    }

    def startOffset: Int = sectionsCoveredByGroup.head.startOffset

    def onePastEndOffset: Int = sectionsCoveredByGroup.last.onePastEndOffset

    def size: Int = sectionsCoveredByGroup.map(_.size).sum
  end Block
end SectionedCode
