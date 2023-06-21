package com.sageserpent.kineticmerge.core

import cats.Order
import cats.collections.DisjointSets

object CodeMotionAnalysis:

  given orderEvidence: Order[Section] = ???

  // TODO: "Something went wrong!" - "What was it?"
  case object Divergence

  def of[Path](
      base: Sources[Path],
      // 'Our' contribution, from the Git standpoint...
      left: Sources[Path],
      // 'Their' contribution, from the Git standpoint...
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

    val sections: Iterable[Section] =
      baseSections.values.flatMap(_.sections) ++ leftSections.values.flatMap(
        _.sections
      ) ++ rightSections.values.flatMap(_.sections)

    Right(
      ???
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
