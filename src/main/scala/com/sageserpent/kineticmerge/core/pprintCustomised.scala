package com.sageserpent.kineticmerge.core

import pprint.{PPrinter, Tree}

extension (prettyPrinter: PPrinter)
  def treeFrom(whatever: Any): Tree = prettyPrinter.treeify(
    whatever,
    prettyPrinter.defaultEscapeUnicode,
    prettyPrinter.defaultShowFieldNames
  )
end extension

val pprintCustomised: PPrinter = pprint.copy(additionalHandlers = {
  case section: Section[?] => section.render
  case iterable: Iterable[?] =>
    Tree.Apply(
      "Iterable",
      iterable.iterator.map(pprintCustomised.treeFrom)
    )
})
