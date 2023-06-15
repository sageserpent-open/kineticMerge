package com.sageserpent.kineticmerge.core

trait Section:
  def startOffset: Int

  def width: Int

  def onePastEndOffset: Int = startOffset + width

  def contents: String
end Section
