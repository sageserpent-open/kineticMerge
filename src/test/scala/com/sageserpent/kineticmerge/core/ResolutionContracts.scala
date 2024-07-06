package com.sageserpent.kineticmerge.core

import cats.Eq

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
