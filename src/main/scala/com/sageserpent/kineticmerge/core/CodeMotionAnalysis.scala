package com.sageserpent.kineticmerge.core

import cats.Order
import com.sageserpent.kineticmerge.core.CodeMotionAnalysis.Match


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
        ): Option[Match] = None

        override def base: Map[Path, File] = baseSections

        override def left: Map[Path, File] = leftSections

        override def right: Map[Path, File] = rightSections
    )
  end of
  
  // TODO - what happened?
  case object AmbiguousMatch

  enum Match:
    /** @return
     * The dominant section in the match, provided the section is part of some
     * match. If the difference(s) is/are due to whitespace changes then it
     * will be from the left (our) contribution, as per Git merge. In addition,
     * the overall match is also provided.
     * @note
     * The notion of dominance does *not* concern itself with the merge
     * precedence of edits or deletions - that is handled downstream.
     * @note
     * Coincident insertions are also matched across the left and right
     * sources, so these will yield the same dominant section.
     */
    def dominantSection: Section = this match
      case BaseAndLeft(_, leftSection) => leftSection
      case BaseAndRight(_, rightSection) => rightSection
      case LeftAndRight(leftSection, _) => leftSection
      case AllThree(_, leftSection, _) => leftSection

    case BaseAndLeft(baseSection: Section, leftSection: Section)
    case BaseAndRight(baseSection: Section, rightSection: Section)
    case LeftAndRight(leftSection: Section, rightSection: Section)
    case AllThree(
                   baseSection: Section,
                   leftSection: Section,
                   rightSection: Section
                 )
  end Match

end CodeMotionAnalysis

trait CodeMotionAnalysis[Path]:
  def base: Map[Path, File]
  def left: Map[Path, File]
  def right: Map[Path, File]

  def matchFor(section: Section): Option[Match]
end CodeMotionAnalysis


