package com.sageserpent.kineticmerge.core

import de.sciss.fingertree.RangedSeq
import pprint.PPrinter

val pprintCustomised: PPrinter = pprint.copy(additionalHandlers = {
  case section: Section[?] =>
    pprint.Tree.Lazy(_ => Iterator(section.render(plainTextOnly = false)))
  case rangedSeq: RangedSeq[?, ?] =>
    pprint.Tree.Apply(
      "RangedSeq",
      rangedSeq.iterator.map(part =>
        pprintCustomised.treeify(
          part,
          pprintCustomised.defaultEscapeUnicode,
          pprintCustomised.defaultShowFieldNames
        )
      )
    )
})
