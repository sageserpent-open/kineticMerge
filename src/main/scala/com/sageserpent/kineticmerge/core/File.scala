package com.sageserpent.kineticmerge.core

/** A source file at a given path is implicitly broken down into [[Section]]
  * instances that cover sections of contiguous text in the file.
  *
  * TODO: some kind of editing that allows sections to be subdivided or merged
  * without breaking the invariant.
  */
case class File(
    sections: IndexedSeq[Section]
):
  // Invariant - sections are contiguous.
  sections.zip(sections.tail).foreach { case (first, second) =>
    require(first.onePastEndOffset == second.startOffset)
  }

  def size: Int = sections.foldLeft(0)(_ + _.size)

  def contents: String = sections.foldLeft("")(_ ++ _.contents)
end File
