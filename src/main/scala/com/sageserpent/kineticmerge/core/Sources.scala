package com.sageserpent.kineticmerge.core

// TODO: is `Sources` the right name? Should it be `Side` or `SourcesFromSide` etc?
/** Represents a collection of sources broken down by paths into files - so a
  * working directory tree, or a Git commit, or possibly even some completely
  * unrelated files sparsely scattered around several unrelated directory trees.
  * To facilitate testing, the notion of the [[PathType]] is left generic - so
  * while it could be a [[String]] or [[java.nio.file.Path]], it could also be a
  * simple [[Int]] that labels a [[File]].
  */
trait Sources[PathType, ElementType]:
  type Path    = PathType
  type Element = ElementType
  val filesByPath: Map[Path, File[ElementType]] = filesByPathUtilising(
    Set.empty
  )

  def paths: Set[Path]

  /** Yield an isolated section covering part of the file at {@code path} as
    * demarcated by {@code startOffset} and {@code size}.
    * @note
    *   The file referred to by {@code path} is implied; there is no actual
    *   [[File]] object associated with the section by default.
    * @note
    *   Sections are associated with the [[Sources]] instance that yielded them;
    *   mixing up sections belonging to another [[Sources]] instance is not
    *   supported, unless both [[Sources]] instances are deemed equivalent.
    */
  def section(path: Path)(startOffset: Int, size: Int): Section[Element]

  // TODO: suppose there are sections that overlap? Is there absence a
  // precondition, or should this be an admissible failure?

  /** @return
    *   The [[Path]] that implies the file that {@code section} refers to.
    */
  def pathFor(section: Section[Element]): Path

  /** Yield a breakdown of [[File]] instances arranged by [[Path]], such that
    * each of the {@code sections} is present in a [[File]] instance.
    *
    * This adds additional sections to fill in gaps between the ones given in
    * {@code sections}, and where a path has no section from {@code sections}
    * associated with it, it will be given a [[File]] instance with one section
    * covering the entire file content.
    *
    * @note
    *   {@code sections} may be an empty set - this then yields an initial
    *   coarse breakdown into files of one section each.
    */
  def filesByPathUtilising(
      mandatorySections: Set[Section[Element]]
  ): Map[Path, File[ElementType]]
end Sources
