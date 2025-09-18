package com.sageserpent.kineticmerge.core

import cats.Eq

object MultiSided:
  given multiSidedOrdering[Item](using
      itemOrdering: Ordering[Item]
  ): Ordering[MultiSided[Item]] with
    override def compare(x: MultiSided[Item], y: MultiSided[Item]): Int =
      (x, y) match
        case (MultiSided.Unique(uniqueX), MultiSided.Unique(uniqueY)) =>
          itemOrdering.compare(uniqueX, uniqueY)
        case (
              MultiSided.Coincident(leftX, rightX),
              MultiSided.Coincident(leftY, rightY)
            ) =>
          Ordering[(Item, Item)].compare(
            (leftX, rightX),
            (leftY, rightY)
          )
        case (
              MultiSided.Preserved(baseX, leftX, rightX),
              MultiSided.Preserved(baseY, leftY, rightY)
            ) =>
          Ordering[(Item, Item, Item)].compare(
            (baseX, leftX, rightX),
            (baseY, leftY, rightY)
          )
        case (_: MultiSided.Unique[Item], _) =>
          // A unique LHS is less than anything that isn't unique.
          -1
        case (
              _: MultiSided.Coincident[Item],
              _: MultiSided.Preserved[Item]
            ) =>
          // A coincident LHS is less than a preserved RHS.
          -1
        case _ =>
          // A coincident LHS is greater than a unique RHS, and a preserved
          // LHS is greater than anything that isn't preserved.
          +1
      end match
    end compare
  end multiSidedOrdering

  given multiSidedEq[Item](using
      itemEq: Eq[Item]
  ): Eq[MultiSided[Item]] with
    override def eqv(x: MultiSided[Item], y: MultiSided[Item]): Boolean =
      (x, y) match
        case (MultiSided.Unique(uniqueX), MultiSided.Unique(uniqueY)) =>
          itemEq.eqv(uniqueX, uniqueY)
        case (
              MultiSided.Coincident(leftX, rightX),
              MultiSided.Coincident(leftY, rightY)
            ) =>
          Eq[(Item, Item)].eqv(
            (leftX, rightX),
            (leftY, rightY)
          )
        case (
              MultiSided.Preserved(baseX, leftX, rightX),
              MultiSided.Preserved(baseY, leftY, rightY)
            ) =>
          Eq[(Item, Item, Item)].eqv(
            (baseX, leftX, rightX),
            (baseY, leftY, rightY)
          )
        case _ => false
      end match
    end eqv

  end multiSidedEq
end MultiSided

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
