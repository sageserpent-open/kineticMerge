package com.sageserpent.kineticmerge.core

import cats.Order
import cats.collections.DisjointSets

object CodeMotionAnalysis:

  given orderEvidence: Order[Section] = ???

  // TODO: "Something went wrong!" - "What was it?"
  case object Divergence

  /** Analyse code motion from the sources of {@code base} to both {@code left}
    * and {@code right}, breaking them into [[File]] and thence [[Section]]
    * instances.
    *
    * Where a section moves from {@code base}, it enters into a match with the
    * corresponding sections in {@code left} and {@code right}; one of those
    * latter two sections is considered to be dominant and therefore represents
    * the match.
    *
    * @note
    *   Motion has to be from {@code base}, so if the same piece of text appears
    *   as newly added or edited in both {@code left} and {@code right}, then
    *   this is treated as coincidental code duplication, *not* as movement.
    * @note
    *   If a section moves from {@code base} to only *one* of {@code left} or
    *   {@code right}, this is interpreted as being a full match across all
    *   three sources, only with the 'missing' section being filled in with the
    *   corresponding edited or deleted section, depending on context.
    * @todo
    *   What if a section moves from {@code base} to only *one* of {@code left}
    *   or {@code right}, but the missing section can't be filled in because the
    *   entire file it used to belong to in {@code base} has been deleted? Git
    *   represents this as a conflict, rather than attempting to propagate the
    *   deletion...
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
  ): Either[Divergence.type, CodeMotionAnalysis[Path]] =
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
        override def dominantMatchingSectionFor(section: Section): Option[Section] = None

        override def base: Map[Path, File] = baseSections

        override def left: Map[Path, File] = leftSections

        override def right: Map[Path, File] = rightSections
    )
  end of

end CodeMotionAnalysis

trait CodeMotionAnalysis[Path]:
  def base: Map[Path, File]
  def left: Map[Path, File]
  def right: Map[Path, File]

  /** @param section
    * @return
    *   The dominant section in the match, provided the section is part of some
    *   match. This is the *one* that has a significant edit, or outright
    *   deletion, or if the only difference(s) is/are due to whitespace changes
    *   then it will be from the left (our) contribution, as per Git merge. A
    *   post-condition is that it is not possible to have conflicting
    *   significant edits or deletions.
    */
  def dominantMatchingSectionFor(section: Section): Option[Section]
end CodeMotionAnalysis
