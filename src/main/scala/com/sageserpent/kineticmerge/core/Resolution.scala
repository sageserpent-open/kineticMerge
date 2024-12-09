package com.sageserpent.kineticmerge.core

trait Resolution[Element]:
  def coincident(leftElement: Element, rightElement: Element): Element

  def preserved(
      baseElement: Element,
      leftElement: Element,
      rightElement: Element
  ): Element
end Resolution
