package com.sageserpent.kineticmerge.core

case class MappedContentSources[Path, Element](
                        contentsByPath: Map[Path, IndexedSeq[Element]],
                        label: String
                      ) extends Sources[Path, Element]:
  override def filesByPathUtilising(
                                     mandatorySections: Set[Section[Element]]
                                   ): Map[Path, File[Element]] =
    val sectionsByPath = mandatorySections.groupBy(pathFor)

    contentsByPath.map { case (path, content) =>
      val pertinentSections = sectionsByPath.getOrElse(path, Set.empty)

      path -> File(
        if pertinentSections.nonEmpty then
          val sectionsInStartOffsetOrder =
            pertinentSections.toSeq.sortBy(_.startOffset)

          sectionsInStartOffsetOrder
            .zip(sectionsInStartOffsetOrder.tail)
            .foreach((first, second) =>
              if first.onePastEndOffset > second.startOffset then
                throw new OverlappingSections(path, first, second)
            )

          val (onePastLastEndOffset, contiguousSections) =
            sectionsInStartOffsetOrder.foldLeft(
              0 -> Vector.empty[Section[Element]]
            ) { case ((onePastLastEndOffset, partialResult), section) =>
              section.onePastEndOffset ->
                ((if onePastLastEndOffset < section.startOffset then
                  partialResult :+ this.section(path)(
                    onePastLastEndOffset,
                    section.startOffset - onePastLastEndOffset
                  )
                else partialResult) :+ section)
            }

          if content.size > onePastLastEndOffset then
            contiguousSections :+ section(path)(
              onePastLastEndOffset,
              content.size - onePastLastEndOffset
            )
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

  override def section(path: Path)(
    startOffset: Int,
    size: Int
  ): Section[Element] = SectionImplementation(path, startOffset, size)

  override def pathFor(section: Section[Element]): Path =
    section match
      // If the section implementation does not come from this `FakeSources`,
      // then it can't be accepted, it's up to the client to be consistent.
      case SectionImplementation(path, _, _)
        if contentsByPath.contains(path) =>
        path

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

    s"Overlapping section detected on side: $label at path: $path: $first (content: ${first.content}) overlaps with start of section: $second (content: ${second.content}), overlap content: ${overlap.content}."
  }):

  end OverlappingSections

  case class SectionImplementation(
                                    path: Path,
                                    override val startOffset: Int,
                                    override val size: Int
                                  ) extends Section[Element]:
    override def content: IndexedSeq[Element] =
      contentsByPath(path).slice(startOffset, onePastEndOffset)
  end SectionImplementation
end MappedContentSources

