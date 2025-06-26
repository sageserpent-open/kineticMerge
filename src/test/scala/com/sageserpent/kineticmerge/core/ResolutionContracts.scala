package com.sageserpent.kineticmerge.core

import cats.Eq

trait ResolutionContracts[Element: Eq] extends Resolution[Element]:
  abstract override def coincident(
      leftElement: Element,
      rightElement: Element
  ): Element =
    require(Eq.eqv(leftElement, rightElement))
    super.coincident(leftElement, rightElement)
  end coincident

  abstract override def preserved(
      baseElement: Element,
      leftElement: Element,
      rightElement: Element
  ): Element =
    require(Eq.eqv(leftElement, rightElement))
    require(Eq.eqv(baseElement, leftElement))
    require(Eq.eqv(baseElement, rightElement))

    super.preserved(baseElement, leftElement, rightElement)
  end preserved
end ResolutionContracts
