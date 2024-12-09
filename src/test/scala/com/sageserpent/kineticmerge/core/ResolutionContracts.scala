package com.sageserpent.kineticmerge.core

import cats.Eq

object ResolutionContracts:
  extension [Element](matchesByElement: Map[Element, Match[Element]])
    def equivalent(lhs: Element, rhs: Element): Boolean =
      matchesByElement.get(lhs) -> matchesByElement.get(rhs) match
        case (None, None)    => false
        case (None, Some(_)) => false
        case (Some(_), None) => false
        case (Some(lhsMatch), Some(rhsMatch)) =>
          lhsMatch == rhsMatch
      end match
    end equivalent
  end extension

  def guardedLeftBiasedResolution[Element: Eq]: Resolution[Element] =
    new LeftBiasedResolution[Element] with ResolutionContracts[Element] {}

  def guardedRightBiasedResolution[Element: Eq]: Resolution[Element] =
    new RightBiasedResolution[Element] with ResolutionContracts[Element] {}

  def guardedCoinFlippingResolution[Element: Eq]: Resolution[Element] =
    new CoinFlippingResolution[Element] with ResolutionContracts[Element] {}

  trait LeftBiasedResolution[Element] extends Resolution[Element]:
    override def coincident(
        leftElement: Element,
        rightElement: Element
    ): Element = leftElement

    override def preserved(
        baseElement: Element,
        leftElement: Element,
        rightElement: Element
    ): Element = leftElement
  end LeftBiasedResolution

  trait RightBiasedResolution[Element] extends Resolution[Element]:
    override def coincident(
        leftElement: Element,
        rightElement: Element
    ): Element = rightElement

    override def preserved(
        baseElement: Element,
        leftElement: Element,
        rightElement: Element
    ): Element = rightElement
  end RightBiasedResolution

  trait CoinFlippingResolution[Element] extends Resolution[Element]:
    override def coincident(
        leftElement: Element,
        rightElement: Element
    ): Element =
      val headsItIs = 0 == (leftElement, rightElement).hashCode() % 2
      if headsItIs then leftElement else rightElement
    end coincident

    override def preserved(
        baseElement: Element,
        leftElement: Element,
        rightElement: Element
    ): Element =
      val headsItIs =
        0 == (baseElement, leftElement, rightElement).hashCode() % 2
      if headsItIs then leftElement else rightElement
    end preserved
  end CoinFlippingResolution
end ResolutionContracts

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
