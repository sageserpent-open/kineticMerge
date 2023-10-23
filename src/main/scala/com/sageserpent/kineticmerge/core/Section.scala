package com.sageserpent.kineticmerge.core

/** A [[Section]] covers some part of a [[File]], and knows how to render the
  * content.
  *
  * At some point, it is likely that the actual content will be abstracted over
  * too, so a run of whitespace can be condensed into a single placeholder to
  * allow whitespace-insensitive comparison of sources. For now though, we just
  * use a plain [[String]].
  */
trait Section:
  def startOffset: Int

  def size: Int

  def onePastEndOffset: Int = startOffset + size

  def content: String
end Section
