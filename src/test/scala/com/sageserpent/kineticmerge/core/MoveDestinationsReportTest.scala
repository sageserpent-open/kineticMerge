package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.MoveDestinationsReport.*
import org.junit.jupiter.api.Test
import pprint.Tree

class MoveDestinationsReportTest:
  case class FakeSection[Element](
      override val startOffset: Int,
      override val size: Int,
      override val content: IndexedSeq[Element]
  ) extends Section[Element]:
    override def render: Tree =
      pprint.Tree.Literal(s"FakeSection($startOffset, $size)")
  end FakeSection

  given [Element]: Eq[Section[Element]] = Eq.fromUniversalEquals

  @Test
  def interiorMatchesInParallelMatchesGroupAreExcludedFromMoveEvaluation(): Unit =
    val baseSection1 = FakeSection(0, 10, Vector("m1"))
    val leftSection1 = FakeSection(0, 10, Vector("m1"))

    val baseSection2 = FakeSection(10, 10, Vector("m2"))
    val leftSection2 = FakeSection(10, 10, Vector("m2"))

    val baseSection3 = FakeSection(20, 10, Vector("m3"))
    val leftSection3 = FakeSection(20, 10, Vector("m3"))

    val match1: Match[Section[String]] =
      Match.BaseAndLeft(baseSection1, leftSection1)
    val match2: Match[Section[String]] =
      Match.BaseAndLeft(baseSection2, leftSection2)
    val match3: Match[Section[String]] =
      Match.BaseAndLeft(baseSection3, leftSection3)

    val matchesBySection: Map[Section[String], Set[Match[Section[String]]]] =
      Map(
        baseSection1 -> Set(match1),
        baseSection2 -> Set(match2),
        baseSection3 -> Set(match3)
      )

    val speculativeMigrationsBySource
        : Map[Section[String], SpeculativeContentMigration[
          Section[String]
        ]] = Map(
      baseSection1 -> SpeculativeContentMigration
        .LeftEditOrDeletion(leftSection1, false),
      baseSection2 -> SpeculativeContentMigration
        .LeftEditOrDeletion(leftSection2, false),
      baseSection3 -> SpeculativeContentMigration
        .LeftEditOrDeletion(leftSection3, false)
    )

    val speculativeMoveDestinations
        : Set[SpeculativeMoveDestination[Section[String]]] = Set(
      SpeculativeMoveDestination.Left(leftSection1),
      SpeculativeMoveDestination.Left(leftSection2),
      SpeculativeMoveDestination.Left(leftSection3)
    )

    // Only match1 and match3 are vetted (first and last in parallel matches group)
    val firstOrLastMatches: Set[Match[Section[String]]] = Set(match1, match3)

    val evaluation =
      MoveDestinationsReport.evaluateSpeculativeSourcesAndDestinations[Section[String]](
        speculativeMigrationsBySource,
        speculativeMoveDestinations
      )(
        section => matchesBySection.getOrElse(section, Set.empty),
        firstOrLastMatches.contains
      )

    assert(
      evaluation.moveDestinationsReport.sources == Set(
        baseSection1,
        baseSection3
      )
    )
    assert(
      evaluation.moveDestinationsReport.all == Set(leftSection1, leftSection3)
    )
    assert(!evaluation.moveDestinationsReport.sources.contains(baseSection2))
    assert(!evaluation.moveDestinationsReport.all.contains(leftSection2))
  end interiorMatchesInParallelMatchesGroupAreExcludedFromMoveEvaluation
end MoveDestinationsReportTest
