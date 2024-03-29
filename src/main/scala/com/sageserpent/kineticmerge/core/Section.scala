package com.sageserpent.kineticmerge.core

/** A [[Section]] covers some part of a [[File]], and knows how to render the
  * content.
  */
trait Section[Element]:
  def startOffset: Int

  def size: Int

  def closedOpenInterval: (Int, Int) = startOffset -> onePastEndOffset

  def onePastEndOffset: Int = startOffset + size

  def content: IndexedSeq[Element]
end Section
