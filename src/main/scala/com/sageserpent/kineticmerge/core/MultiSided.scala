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

  def resolveUsing(resolution: Resolution[Element]): Element =
    this match
      case Unique(element) => element
      // NOTE: the following cases are performing double-dispatch on overloads
      // of `Resolution.apply`...
      case Coincident(leftElement, rightElement) =>
        resolution.coincident(leftElement, rightElement)
      case Preserved(baseElement, leftElement, rightElement) =>
        resolution.preserved(baseElement, leftElement, rightElement)

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
