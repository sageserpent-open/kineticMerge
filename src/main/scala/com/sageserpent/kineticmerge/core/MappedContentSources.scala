package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.MappedContentSourcesOfTokens.{TextPosition, linebreakExtraction}
import com.typesafe.scalalogging.StrictLogging
import pprint.Tree

import java.util.Arrays as JavaArrays
import scala.collection.mutable.Map as MutableMap

trait MappedContentSources[Path, Element]
    extends Sources[Path, Element]
    with StrictLogging:
  val contentsByPath: Map[Path, IndexedSeq[Element]]
  val label: String

  override def filesByPathUtilising(
      mandatorySections: Set[Section[Element]]
  ): Map[Path, File[Element]] =
    val sectionsByPath = mandatorySections.groupBy(pathFor)

    contentsByPath.map { case (path, content) =>
      val pertinentSections = sectionsByPath.getOrElse(path, Set.empty)

      path -> File(
        if pertinentSections.nonEmpty then
          val sectionsInStartOffsetOrder =
            // Sort by the lowest start offset and then use the largest one past
            // end offset as a tiebreaker.
            pertinentSections.toSeq.sortBy(section =>
              section.startOffset -> -section.onePastEndOffset
            )

          sectionsInStartOffsetOrder
            .zip(sectionsInStartOffsetOrder.tail)
            .foreach((first, second) =>
              if first.onePastEndOffset > second.startOffset then
                require(
                  first.onePastEndOffset < second.onePastEndOffset,
                  s"Subsumed section ${pprintCustomised(second)} is subsumed by section: ${pprintCustomised(first)}."
                )
                throw new OverlappingSections(path, first, second)
            )

          val (onePastLastEndOffset, contiguousSections) =
            sectionsInStartOffsetOrder.foldLeft(
              0 -> Vector.empty[Section[Element]]
            ) { case ((onePastLastEndOffset, partialResult), section) =>
              section.onePastEndOffset ->
                ((if onePastLastEndOffset < section.startOffset then
                    val fillerSection = this.section(path)(
                      onePastLastEndOffset,
                      section.startOffset - onePastLastEndOffset
                    )
                    // Fill the gap with a new section - this may be a leading
                    // gap before the first section or between two sections.
                    logger.debug(
                      s"Filling gap on side: $label at path: $path prior to following section with: ${pprintCustomised(fillerSection)}."
                    )
                    partialResult :+ fillerSection
                  else partialResult)
                :+ section)
            }

          if content.size > onePastLastEndOffset then
            // Fill out the final gap with a new section to cover the entire
            // content.
            val fillerSection = section(path)(
              onePastLastEndOffset,
              content.size - onePastLastEndOffset
            )
            logger.debug(
              s"Filling final gap on side: $label at path: $path with ${pprintCustomised(fillerSection)}."
            )
            contiguousSections :+ fillerSection
          else contiguousSections
          end if
        else
          Vector(
            section(path = path)(
              startOffset = 0,
              size = content.length
            )
          )
      )
    }
  end filesByPathUtilising

  override def pathFor(section: Section[Element]): Path =
    section match
      case section: SectionImplementation
          if contentsByPath.contains(section.path) =>
        section.path

  override def section(path: Path)(
      startOffset: Int,
      size: Int
  ): Section[Element] = new SectionImplementation(path, startOffset, size)

  override def paths: Set[Path] = contentsByPath.keySet

  class SectionImplementation(
      val path: Path,
      override val startOffset: Int,
      override val size: Int
  ) extends Section[Element]:

    override def toString: String = pprintCustomised(this).plainText

    def render: Tree =
      Tree.Apply(
        "Section",
        Iterator(
          Tree.KeyValue("label", pprintCustomised.treeFrom(label)),
          Tree.KeyValue("path", pprintCustomised.treeFrom(path)),
          Tree.KeyValue("startOffset", pprintCustomised.treeFrom(startOffset)),
          Tree.KeyValue("size", pprintCustomised.treeFrom(size)),
          Tree.KeyValue("content", pprintCustomised.treeFrom(content))
        )
      )
    end render

    override def content: IndexedSeq[Element] =
      contentsByPath(path).slice(startOffset, onePastEndOffset)
  end SectionImplementation

  class OverlappingSections(
      path: Path,
      first: Section[Element],
      second: Section[Element]
  ) extends RuntimeException({
        val overlap = section(path)(
          startOffset = second.startOffset,
          size = first.onePastEndOffset - second.startOffset
        )

        s"Overlapping section detected on side: $label at path: $path, $first (content: ${first.content}) overlaps with start of section: $second (content: ${second.content}), overlap content: ${overlap.content}."
      }):

  end OverlappingSections
end MappedContentSources

object MappedContentSourcesOfTokens:
  private val linebreakExtraction =
    """(?m:.*$(\s+?)^)""".r // Matches a single, possibly empty line; the nested parentheses capture the terminating linebreak sequence. A final line without a terminating linebreak is not matched.

    /** @param line
      *   One-relative line number within the entire content at some path.
      * @param character
      *   Zero-relative character offset from the start of the line.
      */
  case class TextPosition(
      line: Int,
      character: Int
  )
end MappedContentSourcesOfTokens

case class MappedContentSourcesOfTokens[Path](
    override val contentsByPath: Map[Path, IndexedSeq[Token]],
    override val label: String
) extends MappedContentSources[Path, Token]:
  override def section(path: Path)(
      startOffset: Int,
      size: Int
  ): Section[Token] = new SectionImplementation(path, startOffset, size):
    override def render: Tree =
      val contentPrefixLimit = 5

      // The content is the text covered by a prefix of the section's tokens.
      val revealedContent = content
        .take(contentPrefixLimit)
        .map(_.text)
        .mkString

      Tree.Apply(
        "Section",
        Iterator(
          Tree.KeyValue("label", pprintCustomised.treeFrom(label)),
          Tree.KeyValue("path", pprintCustomised.treeFrom(this.path)),
          Tree.KeyValue(
            "start",
            pprintCustomised.treeFrom(
              LineInformation(this.path)
                .textPositionFor(
                  this.startOffset
                )
            )
          ),
          Tree.KeyValue(
            "onePastEnd",
            pprintCustomised.treeFrom(
              LineInformation(this.path)
                .textPositionFor(
                  this.startOffset + this.size
                )
            )
          ),
          Tree.KeyValue(
            "startTokenIndex",
            pprintCustomised.treeFrom(this.startOffset)
          ),
          Tree.KeyValue("sizeInTokens", pprintCustomised.treeFrom(this.size)),
          Tree.KeyValue("content", pprintCustomised.treeFrom(revealedContent))
        )
      )
    end render

  private class LineInformation(contents: IndexedSeq[Token]):
    private val cumulativeCharacterOffsetsOfTokensIncludingThePhantomOffTheEnd
        : IndexedSeq[Int] =
      (0 until contents.size)
        .scanLeft(0)((cumulativeCharacterOffset, tokenIndex) =>
          cumulativeCharacterOffset + contents(tokenIndex).text.size
        )

    private val cumulativeCharacterOffsetsOfLines: Array[Int] =
      val textCoveredBySection = contents.map(_.text).mkString

      val offsetsFollowingLinebreaks =
        linebreakExtraction.findAllMatchIn(textCoveredBySection).map(_.end(1))

      (Iterator(0) ++ offsetsFollowingLinebreaks).toArray
    end cumulativeCharacterOffsetsOfLines

    def textPositionFor(
        tokenIndex: Int
    ): TextPosition =
      val characterOffsetOfToken =
        cumulativeCharacterOffsetsOfTokensIncludingThePhantomOffTheEnd(
          tokenIndex
        )

      val locationOrInsertion = JavaArrays.binarySearch(
        cumulativeCharacterOffsetsOfLines,
        characterOffsetOfToken
      )

      val lineIndex =
        if 0 > locationOrInsertion then -(locationOrInsertion + 2)
        else locationOrInsertion

      TextPosition(
        line = 1 + lineIndex,
        character =
          characterOffsetOfToken - cumulativeCharacterOffsetsOfLines(lineIndex)
      )
    end textPositionFor
  end LineInformation

  private object LineInformation:
    private val lineInformationByPath: MutableMap[Path, LineInformation] =
      MutableMap.empty

    def apply(path: Path): LineInformation =
      lineInformationByPath.getOrElseUpdate(
        path,
        new LineInformation(contentsByPath(path))
      )
  end LineInformation

end MappedContentSourcesOfTokens
