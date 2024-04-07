package com.sageserpent.kineticmerge.core

import pprint.PPrinter

val pprintCustomised: PPrinter = pprint.copy(additionalHandlers = {
  case section: Section[?] => pprint.Tree.Lazy(_ => Iterator(section.toString))
})
