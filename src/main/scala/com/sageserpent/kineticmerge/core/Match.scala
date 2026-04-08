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
    private val baseLeftOrder = Order.whenEqual(
      Order.by[Match.BaseAndLeft[Element], Element](_.baseElement),
      Order.by[Match.BaseAndLeft[Element], Element](_.leftElement)
    )

    private val baseRightOrder = Order
      .whenEqual(
        Order.by[Match.BaseAndRight[Element], Element](_.baseElement),
        Order.by[Match.BaseAndRight[Element], Element](_.rightElement)
      )

    private val leftRightOrder = Order
      .whenEqual(
        Order.by[Match.LeftAndRight[Element], Element](_.leftElement),
        Order.by[Match.LeftAndRight[Element], Element](_.rightElement)
      )

    private val allSidesOrder = Order
      .whenEqual(
        Order.by[Match.AllSides[Element], Element](_.baseElement),
        Order.whenEqual(
          Order.by[Match.AllSides[Element], Element](_.leftElement),
          Order.by[Match.AllSides[Element], Element](_.rightElement)
        )
      )

    override def compare(x: Match[Element], y: Match[Element]): Int =
      // Use progressive fall-through matching where if the two matches aren't
      // of the same kind, there is a hierarchy of kinds.
      (x, y) match
        case (
              left: BaseAndLeft[Element],
              right: BaseAndLeft[Element]
            ) =>
          baseLeftOrder.compare(left, right)

        case (
              left: BaseAndRight[Element],
              right: BaseAndRight[Element]
            ) =>
          baseRightOrder.compare(left, right)

        case (
              left: LeftAndRight[Element],
              right: LeftAndRight[Element]
            ) =>
          leftRightOrder.compare(left, right)

        case (
              left: AllSides[Element],
              right: AllSides[Element]
            ) =>
          allSidesOrder.compare(left, right)

        case (_: BaseAndLeft[Element], _)  => -1
        case (_, _: BaseAndLeft[Element])  => 1
        case (_: BaseAndRight[Element], _) => -1
        case (_, _: BaseAndRight[Element]) => 1
        case (_: LeftAndRight[Element], _) => -1
        case (_, _: LeftAndRight[Element]) => 1
  end matchOrdering
end Match
