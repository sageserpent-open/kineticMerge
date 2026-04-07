package com.sageserpent.kineticmerge.core

import cats.Order
import com.sageserpent.kineticmerge.core.Match.*

enum Match[+Element]:
  def isAnAllSidesMatch: Boolean = this match
    case _: AllSides[Element] => true
    case _                    => false

  case BaseAndLeft(baseElement: Element, leftElement: Element)
  case BaseAndRight(baseElement: Element, rightElement: Element)
  case LeftAndRight(leftElement: Element, rightElement: Element)
  case AllSides(
      baseElement: Element,
      leftElement: Element,
      rightElement: Element
  )
end Match

object Match:
  given matchOrdering[Element: Order]: Order[Match[Element]] with
    override def compare(x: Match[Element], y: Match[Element]): Int =
      // Use progressive fall-through matching where if the two matches aren't
      // of the same kind, there is a hierarchy of kinds.
      (x, y) match
        case (
              BaseAndLeft(xBaseElement, xLeftElement),
              BaseAndLeft(yBaseElement, yLeftElement)
            ) =>
          Order.compare(
            (xBaseElement, xLeftElement),
            (yBaseElement, yLeftElement)
          )

        case (
              BaseAndRight(xBaseElement, xRightElement),
              BaseAndRight(yBaseElement, yRightElement)
            ) =>
          Order.compare(
            (xBaseElement, xRightElement),
            (yBaseElement, yRightElement)
          )

        case (
              LeftAndRight(xLeftElement, xRightElement),
              LeftAndRight(yLeftElement, yRightElement)
            ) =>
          Order.compare(
            (xLeftElement, xRightElement),
            (yLeftElement, yRightElement)
          )

        case (
              AllSides(xBaseElement, xLeftElement, xRightElement),
              AllSides(yBaseElement, yLeftElement, yRightElement)
            ) =>
          Order.compare(
            (xBaseElement, xLeftElement, xRightElement),
            (yBaseElement, yLeftElement, yRightElement)
          )

        case (_: BaseAndLeft[Element], _)  => -1
        case (_, _: BaseAndLeft[Element])  => 1
        case (_: BaseAndRight[Element], _) => -1
        case (_, _: BaseAndRight[Element]) => 1
        case (_: LeftAndRight[Element], _) => -1
        case (_, _: LeftAndRight[Element]) => 1
  end matchOrdering
end Match
