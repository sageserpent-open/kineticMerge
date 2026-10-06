package com.sageserpent.kineticmerge.core

import pprint.{PPrinter, Tree}

extension (prettyPrinter: PPrinter)
  def treeFrom(whatever: Any): Tree = prettyPrinter.treeify(
    whatever,
    prettyPrinter.defaultEscapeUnicode,
    prettyPrinter.defaultShowFieldNames
  )
end extension

val pprintCustomised: PPrinter = pprint.copy(colorLiteral = fansi.Attrs.Empty, colorApplyPrefix = fansi.Attrs.Empty, additionalHandlers = {
  case section: Section[?]           => section.render
  case sectionsSeen: SectionsSeen[?] =>
    Tree.Apply(
      "SectionsSeen",
      sectionsSeen.iterator.map(pprintCustomised.treeFrom)
    )
})
