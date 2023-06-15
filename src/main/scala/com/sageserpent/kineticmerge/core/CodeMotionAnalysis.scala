package com.sageserpent.kineticmerge.core

import cats.Order
import cats.collections.DisjointSets
import com.sageserpent.kineticmerge.core.CodeMotionAnalysis.SectionedSourcesByPath

object CodeMotionAnalysis:

  given orderEvidence: Order[Sources#SectionType] = ???

  type SectionedSource = IndexedSeq[Sources#SectionType]

  type SectionedSourcesByPath = Map[Sources#Path, SectionedSource]

  // TODO: "Something went wrong!" - "What was it?"
  case object Divergence

  def of(
      base: Sources,
      left: Sources,
      right: Sources
  )(
      minimumSizeFractionForMotionDetection: Double
  ): Either[Divergence.type, CodeMotionAnalysis] =
    require(0 < minimumSizeFractionForMotionDetection)
    require(1 >= minimumSizeFractionForMotionDetection)

    val baseSections: SectionedSourcesByPath =
      base.filesByPath.view.mapValues(_.sections).toMap

    val leftSections: SectionedSourcesByPath =
      left.filesByPath.view.mapValues(_.sections).toMap

    val rightSections: SectionedSourcesByPath =
      right.filesByPath.view.mapValues(_.sections).toMap

    val sections: Iterable[Sources#SectionType] =
      baseSections.values.flatten ++ leftSections.values.flatten ++ rightSections.values.flatten

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

case class CodeMotionAnalysis(
    base: SectionedSourcesByPath,
    left: SectionedSourcesByPath,
    right: SectionedSourcesByPath,
    globalSectionSet: DisjointSets[Sources#SectionType]
)
