package com.sageserpent.kineticmerge.core

import scala.io.Source
import scala.util.Using

object AboutKineticMerge:
  val version: String = Using(
    Source
      .fromResource("version.txt")
  )(
    _.getLines().next()
  ).getOrElse("unversioned build.")
end AboutKineticMerge
