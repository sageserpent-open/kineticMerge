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
    override def apply(
        base: Option[Element],
        left: Element,
        right: Element
    ): Element = left
  end LeftBiasedResolution

  trait RightBiasedResolution[Element] extends Resolution[Element]:
    override def apply(
        base: Option[Element],
        left: Element,
        right: Element
    ): Element = right
  end RightBiasedResolution

  trait CoinFlippingResolution[Element] extends Resolution[Element]:
    override def apply(
        base: Option[Element],
        left: Element,
        right: Element
    ): Element =
      val headsItIs = 0 == (base, left, right).hashCode() % 2
      if headsItIs then left else right
    end apply
  end CoinFlippingResolution
end ResolutionContracts

trait ResolutionContracts[Element: Eq] extends Resolution[Element]:
  abstract override def apply(
      base: Option[Element],
      left: Element,
      right: Element
  ): Element =
    require(Eq.eqv(left, right))

    base.foreach { payload =>
      require(Eq.eqv(payload, left))
      require(Eq.eqv(payload, right))
    }

    super.apply(base, left, right)
  end apply
end ResolutionContracts
