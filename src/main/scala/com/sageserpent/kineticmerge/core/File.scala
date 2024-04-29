package com.sageserpent.kineticmerge.core

/** A source file at a given path is implicitly broken down into [[Section]]
  * instances that cover sections of contiguous text in the file.
  *
  * TODO: some kind of editing that allows sections to be subdivided or merged
  * without breaking the invariant.
  */
case class File[Element](
    sections: IndexedSeq[Section[Element]]
):
  // Invariant - sections are contiguous.
  if sections.nonEmpty then
    sections.zip(sections.tail).foreach { case (first, second) =>
      require(first.onePastEndOffset == second.startOffset)
    }
  end if

  def size: Int = sections.foldLeft(0)(_ + _.size)

  def content: IndexedSeq[Element] =
    sections.foldLeft(IndexedSeq.empty)(_ ++ _.content)
end File
