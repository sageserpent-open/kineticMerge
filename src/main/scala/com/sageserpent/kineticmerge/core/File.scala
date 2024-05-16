package com.sageserpent.kineticmerge.core

import scala.collection.Searching

/** A source file at a given path is implicitly broken down into [[Section]]
  * instances that cover sections of contiguous text in the file.
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

  def searchByStartOffset(startIndex: Int): Searching.SearchResult =
    sections.view.map(_.startOffset).search(startIndex)
end File
