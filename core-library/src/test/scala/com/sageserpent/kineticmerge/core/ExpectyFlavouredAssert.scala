package com.sageserpent.kineticmerge.core

import com.eed3si9n.expecty.Expecty

object ExpectyFlavouredAssert:
  val assert: Expecty = new Expecty:
    override val showLocation: Boolean = true
    override val showTypes: Boolean    = true
  end assert
end ExpectyFlavouredAssert
