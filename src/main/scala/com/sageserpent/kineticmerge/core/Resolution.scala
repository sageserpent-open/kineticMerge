package com.sageserpent.kineticmerge.core

trait Resolution[Element]:
  def apply(multiSided: MultiSided[Element]): Element
end Resolution
