package com.sageserpent.kineticmerge.core

import de.sciss.fingertree.RangedSeq
import pprint.{PPrinter, Tree}

extension (prettyPrinter: PPrinter)
  def treeFrom(whatever: Any): Tree = prettyPrinter.treeify(
    whatever,
    prettyPrinter.defaultEscapeUnicode,
    prettyPrinter.defaultShowFieldNames
  )

val pprintCustomised: PPrinter = pprint.copy(additionalHandlers = {
  case section: Section[?] => section.render
  case rangedSeq: RangedSeq[?, ?] =>
    pprint.Tree.Apply(
      "RangedSeq",
      rangedSeq.iterator.map(pprintCustomised.treeFrom)
    )
})
