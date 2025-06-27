package com.sageserpent.kineticmerge.core

import cats.Eq

enum MultiSided[Element: Eq]:
  this match
    case Unique(element)                       => ()
    case Coincident(leftElement, rightElement) =>
      require(Eq[Element].eqv(leftElement, rightElement))
    case Preserved(baseElement, leftElement, rightElement) =>
      // As equivalence is transitive, there's no need to check the left
      // element against the right element...
      Eq[Element].eqv(baseElement, leftElement)
      Eq[Element].eqv(baseElement, rightElement)
  end match

  case Unique(element: Element)(using equality: Eq[Element])
  case Coincident(leftElement: Element, rightElement: Element)(using
      equality: Eq[Element]
  )
  case Preserved(
      baseElement: Element,
      leftElement: Element,
      rightElement: Element
  )(using equality: Eq[Element])
end MultiSided
