package com.sageserpent.kineticmerge.core

/** Represents a collection of sources broken down by paths - so a working
  * directory tree, or a Git commit, or possibly even some completely unrelated
  * files sparsely scattered around several unrelated directory trees. To
  * facilitate testing, the notion of the path is left abstract - so while it
  * could be a [[String]] or [[java.nio.file.Path]], it could also be a simple
  * [[Int]] that labels a file.
  *
  * A source file at a given path is implicitly broken down into [[Section]]
  * instances that cover sections of contiguous text in the file. A [[Section]]
  * knows how to render its covered text, and can reveal the [[Path]] it belongs
  * to.
  *
  * At some point, it is likely that the actual contents will be abstracted over
  * too, so a run of whitespace can be condensed into a single placeholder to
  * allow whitespace-insensitive comparison of sources. For now though, we just
  * use a plain [[String]].
  */
trait Sources:
  type Path

  /** @return
    *   Sections that completely cover each source file - so for every [[Path]]
    *   in the [[Sources]], there is just one [[Section]] instance that covers
    *   all of the source text at that path.
    */
  def maximalSectionsByPath: Map[Path, Section]
end Sources
