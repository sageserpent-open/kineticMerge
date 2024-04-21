package com.sageserpent.kineticmerge.core

import com.typesafe.scalalogging.StrictLogging
import pprint.Tree

case class MappedContentSources[Path, Element](
    contentsByPath: Map[Path, IndexedSeq[Element]],
    label: String
) extends Sources[Path, Element]
    with StrictLogging:
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
                  s"Subsumed section detected on side: $label at path: $path, $second (content: ${second.content}) is subsumed by section: $first (content: ${first.content})."
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
                      s"Filling gap on side: $label at path: $path prior to following section with: $fillerSection."
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
              s"Filling final gap on side: $label at path: $path with $fillerSection."
            )
            contiguousSections :+ fillerSection
          else contiguousSections
          end if
        else
          Vector(
            SectionImplementation(
              path = path,
              startOffset = 0,
              size = content.length
            )
          )
      )
    }
  end filesByPathUtilising

  override def pathFor(section: Section[Element]): Path =
    section match
      case SectionImplementation(path, _, _) if contentsByPath.contains(path) =>
        path

  override def section(path: Path)(
      startOffset: Int,
      size: Int
  ): Section[Element] = SectionImplementation(path, startOffset, size)

  override def paths: Set[Path] = contentsByPath.keySet

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

  case class SectionImplementation(
      path: Path,
      override val startOffset: Int,
      override val size: Int
  ) extends Section[Element]:
    override def toString: String = pprintCustomised(this).plainText

    def render: Tree =
      val contentPrefixLimit = 5

      val revealedContent = content
        .take(contentPrefixLimit)
        .map {
          // NASTY HACK: rather than use dependency injection to
          // render an element, just use a special case to render
          // `Token`.
          case token: Token => token.text
          case anythingElse => anythingElse.toString
        }
        .mkString // TODO: this isn't correct - unless we have a sequence of tokens or possibly strings, we should show a *sequence* of elements.

      Tree.Apply(
        "Section",
        Iterator(
          Tree.KeyValue("label", pprintCustomised.treeFrom(label)),
          Tree.KeyValue("path", pprintCustomised.treeFrom(path)),
          Tree.KeyValue("startOffset", pprintCustomised.treeFrom(startOffset)),
          Tree.KeyValue("size", pprintCustomised.treeFrom(size)),
          Tree.KeyValue("content", pprintCustomised.treeFrom(revealedContent))
        )
      )
    end render

    override def content: IndexedSeq[Element] =
      contentsByPath(path).slice(startOffset, onePastEndOffset)
  end SectionImplementation
end MappedContentSources
