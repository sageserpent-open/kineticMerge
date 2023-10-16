package com.sageserpent.kineticmerge.core

import cats.Order

object CodeMotionAnalysis:

  given orderEvidence: Order[Section] = ???

  /** Analyse code motion from the sources of {@code base} to both {@code left}
    * and {@code right}, breaking them into [[File]] and thence [[Section]]
    * instances.
    *
    * Where a section moves from {@code base}, it enters into a match with one
    * or both corresponding sections in {@code left} and {@code right}; if both,
    * then one of those latter two sections is considered to be dominant and
    * therefore represents the match. If there is just one matching section,
    * that is taken to be the dominant one for the sake of picking up whitespace
    * changes.
    *
    * @note
    *   Although code motion is strictly speaking relative to the base sources,
    *   if the same section is added into both the left and right sources as a
    *   coincidental insertion (so not present in the base sources), this is
    *   treated as a match across the left and right sources anyway, so there
    *   will be a dominant section.
    * @param base
    *   The common base sources from which the left and right sources are
    *   derived.
    * @param left
    *   'Our' sources, from the Git standpoint...
    * @param right
    *   'Their' sources, from the Git standpoint...
    * @param minimumSizeFractionForMotionDetection
    *   A section's size must be at least this fraction of its containing file's
    *   size to qualify for motion detection.
    * @tparam Path
    * @return
    *   A [[CodeMotionAnalysis]] that contains a breakdown into [[File]]
    *   instances and thence into [[Section]] instances for each of the three
    *   sources.
    */
  def of[Path](
      base: Sources[Path],
      left: Sources[Path],
      right: Sources[Path]
  )(
      minimumSizeFractionForMotionDetection: Double
  ): Either[AmbiguousMatch.type, CodeMotionAnalysis[Path]] =
    require(0 < minimumSizeFractionForMotionDetection)
    require(1 >= minimumSizeFractionForMotionDetection)

    val baseSections =
      base.filesByPath

    val leftSections =
      left.filesByPath

    val rightSections =
      right.filesByPath

    Right(
      new CodeMotionAnalysis[Path]:
        override def matchFor(
            section: Section
        ): Option[Match[Section]] = None

        override def base: Map[Path, File] = baseSections

        override def left: Map[Path, File] = leftSections

        override def right: Map[Path, File] = rightSections
    )
  end of

  // TODO - what happened?
  case object AmbiguousMatch
end CodeMotionAnalysis

trait CodeMotionAnalysis[Path]:
  def base: Map[Path, File]
  def left: Map[Path, File]
  def right: Map[Path, File]

  def matchFor(section: Section): Option[Match[Section]]
end CodeMotionAnalysis
