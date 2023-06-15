package com.sageserpent.kineticmerge.core

import cats.Order
import cats.collections.DisjointSets

object CodeMotionAnalysis:

  given orderEvidence: Order[Section] = ???

  // TODO: "Something went wrong!" - "What was it?"
  case object Divergence

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

    val sections: Iterable[Section] =
      baseSections.values.flatMap(_.sections) ++ leftSections.values.flatMap(
        _.sections
      ) ++ rightSections.values.flatMap(_.sections)

    Right(
      CodeMotionAnalysis(
        baseSections,
        leftSections,
        rightSections,
        DisjointSets(sections.toSeq*)
      )
    )
  end of

end CodeMotionAnalysis

case class CodeMotionAnalysis[Path](
    base: Map[Path, File],
    left: Map[Path, File],
    right: Map[Path, File],
    globalSectionsOrganizedIntoSetsOfMatches: DisjointSets[Section]
)
