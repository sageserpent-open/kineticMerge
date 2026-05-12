package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.Match.*

enum Match[+Element]:
  def isAnAllSidesMatch: Boolean = this match
    case _: AllSides[Element] => true
    case _                    => false

  def baseContribution: Option[Element] = this match
    case BaseAndLeft(baseElement, _)  => Some(baseElement)
    case BaseAndRight(baseElement, _) => Some(baseElement)
    case AllSides(baseElement, _, _)  => Some(baseElement)
    case _                            => None

  def leftContribution: Option[Element] = this match
    case BaseAndLeft(_, leftElement)  => Some(leftElement)
    case LeftAndRight(leftElement, _) => Some(leftElement)
    case AllSides(_, leftElement, _)  => Some(leftElement)
    case _                            => None

  def rightContribution: Option[Element] = this match
    case BaseAndRight(_, rightElement) => Some(rightElement)
    case LeftAndRight(_, rightElement) => Some(rightElement)
    case AllSides(_, _, rightElement)  => Some(rightElement)
    case _                             => None

  case BaseAndLeft(baseElement: Element, leftElement: Element)
  case BaseAndRight(baseElement: Element, rightElement: Element)
  case LeftAndRight(leftElement: Element, rightElement: Element)
  case AllSides(
      baseElement: Element,
      leftElement: Element,
      rightElement: Element
  )
end Match
