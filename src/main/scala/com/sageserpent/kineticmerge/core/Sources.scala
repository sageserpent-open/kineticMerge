package com.sageserpent.kineticmerge.core

trait Sources: // NOTE: there is no invariant about containment of paths - this is not necessarily a single directory tree, or even a forest.
  type Path

  case class Section(
                      path: Path,
                      startOffset: Int,
                      width: Int
                    ) extends CodeMotionAnalysis.Section:
    require(0 <= startOffset)
    require(0 < width)

    def onePastEndOffset: Int = startOffset + width

    def contents: String = ???
  end Section

end Sources
