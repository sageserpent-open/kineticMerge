package com.sageserpent.kineticmerge.core

import pprint.Tree

/** A [[Section]] covers some part of a [[File]], and knows how to render the
  * content.
  */
trait Section[Element]:
  def startOffset: Int

  def size: Int

  def closedOpenInterval: (Int, Int) = startOffset -> onePastEndOffset

  def onePastEndOffset: Int = startOffset + size

  def content: IndexedSeq[Element]

  def render: Tree
end Section
