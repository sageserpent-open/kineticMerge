package com.sageserpent.kineticmerge.core

import com.typesafe.scalalogging.StrictLogging
import pprint.Tree

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
        if content.nonEmpty then
          if pertinentSections.nonEmpty then
            val sectionsInStartOffsetOrder =
              // Sort by the lowest start offset and then use the largest one
              // past end offset as a tiebreaker.
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
        else
          // Section content is *never* empty: if there is no content at this
          // path, how could there be mandatory sections using it?
          require(pertinentSections.isEmpty)
          Vector.empty
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

case class MappedContentSourcesOfTokens[Path](
    override val contentsByPath: Map[Path, IndexedSeq[Token]],
    override val label: String
) extends MappedContentSources[Path, Token]:
  override def section(path: Path)(
      startOffset: Int,
      size: Int
  ): Section[Element] = new SectionImplementation(path, startOffset, size):
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
            "startOffset",
            pprintCustomised.treeFrom(this.startOffset)
          ),
          Tree.KeyValue("size", pprintCustomised.treeFrom(this.size)),
          Tree.KeyValue("content", pprintCustomised.treeFrom(revealedContent))
        )
      )
    end render

end MappedContentSourcesOfTokens
