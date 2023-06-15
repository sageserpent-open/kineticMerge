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
      base.files

    val leftSections =
      left.files

    val rightSections =
      right.files

    val sections: Iterable[Section] =
      baseSections.flatMap(_.sections) ++ leftSections.flatMap(
        _.sections
      ) ++ rightSections.flatMap(_.sections)

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
    base: Set[? <: Sources[Path]#File],
    left: Set[? <: Sources[Path]#File],
    right: Set[? <: Sources[Path]#File],
    globalSectionSet: DisjointSets[Section]
)
