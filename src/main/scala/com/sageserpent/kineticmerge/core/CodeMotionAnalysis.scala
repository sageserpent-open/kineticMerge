package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.CodeMotionAnalysis.{Section, SectionedSources}

object CodeMotionAnalysis:

  trait Section:

    def onePastEndOffset: Int

    def contents: String
  end Section

  type FileContents = IndexedSeq[Section]

  type SectionedSources = Map[Sources#Path, FileContents]

  // TODO: "Something went wrong!" - "What was it?"
  case object Divergence

  def analyze(
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

    val sections: Iterable[Section] =
      baseSections.values.flatten ++ leftSections.values.flatten ++ rightSections.values.flatten

    Right(
      CodeMotionAnalysis(
        baseSections,
        leftSections,
        rightSections,
        sections.toSet
      )
    )
  end analyze

end CodeMotionAnalysis

case class CodeMotionAnalysis(
    base: SectionedSources,
    left: SectionedSources,
    right: SectionedSources,
    globalSectionSet: Set[Section]
)
