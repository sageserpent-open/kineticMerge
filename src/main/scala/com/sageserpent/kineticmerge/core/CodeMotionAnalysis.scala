package com.sageserpent.kineticmerge.core

import cats.Order
import cats.collections.DisjointSets
import com.sageserpent.kineticmerge.core.CodeMotionAnalysis.SectionedSources

object CodeMotionAnalysis:

  given orderEvidence: Order[Sources#SectionType] = ???

  type FileContents = IndexedSeq[Sources#SectionType]

  type SectionedSources = Map[Sources#Path, FileContents]

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

    val baseSections: SectionedSources =
      Map.empty

    val leftSections: SectionedSources =
      Map.empty

    val rightSections: SectionedSources =
      Map.empty

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
    base: SectionedSources,
    left: SectionedSources,
    right: SectionedSources,
    globalSectionSet: DisjointSets[Sources#SectionType]
)
