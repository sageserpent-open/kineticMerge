package com.sageserpent.kineticmerge.core

enum Match[+Element]:
  def isAnAllSidesMatch: Boolean = this match
    case _: AllSides[Element] => true
    case _                    => false

  def base: Option[Element] = this match
    case BaseAndLeft(baseElement, _)  => Some(baseElement)
    case BaseAndRight(baseElement, _) => Some(baseElement)
    case LeftAndRight(_, _)           => None
    case AllSides(baseElement, _, _)  => Some(baseElement)
  end base

  case BaseAndLeft(baseElement: Element, leftElement: Element)
  case BaseAndRight(baseElement: Element, rightElement: Element)
  case LeftAndRight(leftElement: Element, rightElement: Element)
  case AllSides(
      baseElement: Element,
      leftElement: Element,
      rightElement: Element
  )
end Match
