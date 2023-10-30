package com.sageserpent.kineticmerge.core

enum Match[+Element]:
  /** @return
    *   The dominant element in the match, provided the element is part of some
    *   match.
    * @note
    *   The notion of dominance does *not* concern itself with the merge
    *   precedence of edits or deletions - that is handled downstream.
    * @note
    *   Coincident insertions are also matched across the left and right
    *   sources, so these will yield the same dominant element.
    */
  def dominantElement: Element = this match
    case BaseAndLeft(_, leftElement)   => leftElement
    case BaseAndRight(_, rightElement) => rightElement
    case LeftAndRight(leftElement, _)  => leftElement
    case AllThree(_, leftElement, _)   => leftElement

  case BaseAndLeft(baseElement: Element, leftElement: Element)
  case BaseAndRight(baseElement: Element, rightElement: Element)
  case LeftAndRight(leftElement: Element, rightElement: Element)
  case AllThree(
      baseElement: Element,
      leftElement: Element,
      rightElement: Element
  )
end Match
