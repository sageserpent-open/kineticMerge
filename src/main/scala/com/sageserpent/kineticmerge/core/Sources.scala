package com.sageserpent.kineticmerge.core

/** Represents a collection of sources broken down by paths into files - so a
  * working directory tree, or a Git commit, or possibly even some completely
  * unrelated files sparsely scattered around several unrelated directory trees.
  * To facilitate testing, the notion of the [[PathType]] is left generic - so
  * while it could be a [[String]] or [[java.nio.file.Path]], it could also be a
  * simple [[Int]] that labels a [[File]].
  */
trait Sources[PathType, ElementType]:
  type Path = PathType
  type Element = ElementType

  def filesByPath: Map[Path, File[ElementType]]
end Sources
