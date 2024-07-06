package com.sageserpent.kineticmerge.core

trait Resolution[Element]:
  def apply(base: Option[Element], left: Element, right: Element): Element
end Resolution
