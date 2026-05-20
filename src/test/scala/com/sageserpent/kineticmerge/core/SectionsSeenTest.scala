package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials
import com.sageserpent.americium.junit5.*
import org.junit.jupiter.api.TestFactory
import org.junit.jupiter.api.Assertions.*
import de.sciss.fingertree.RangedSeq

case class FakeSection(startOffset: Int, size: Int, id: Int = 0) extends Section[Int]:
  override def content: IndexedSeq[Int] = IndexedSeq.empty
  override def render: pprint.Tree = pprint.Tree.Literal(s"Section($startOffset, $size, $id)")

class SectionsSeenTest:
  private val sectionGen: Trials[FakeSection] = for
    start <- Trials.api.integers(0, 1000)
    size <- Trials.api.integers(1, 100)
    id <- Trials.api.integers(0, 10)
  yield FakeSection(start, size, id)

  private def sequences: Trials[Vector[Operation]] =
    Trials.api.integers(1, 100).flatMap { numberOfOperations =>
      def rangeGen(sectionsSeen: Set[FakeSection]): Trials[(Int, Int)] =
        for
          useExisting <- Trials.api.booleans
          range <- if (useExisting && sectionsSeen.nonEmpty) {
            Trials.api.choose(sectionsSeen).flatMap { s =>
              val start = s.startOffset
              val end = s.onePastEndOffset
              Trials.api.alternate(
                Trials.api.only((start, end)), // Exact match
                Trials.api.only(if (end - 1 > start + 1) (start + 1, end - 1) else (start, end)), // Nested inside
                Trials.api.only((start - 1, end + 1)), // Nested outside
                Trials.api.only((start - 5, start + 5)), // Partial overlap start
                Trials.api.only((end - 5, end + 5)) // Partial overlap end
              )
            }
          } else {
            for
              start <- Trials.api.integers(0, 1000)
              size <- Trials.api.integers(1, 100)
            yield (start, start + size)
          }
        yield range

      def generate(opsSoFar: Vector[Operation], sectionsSeen: Set[FakeSection]): Trials[Vector[Operation]] =
        if (opsSoFar.size >= numberOfOperations) Trials.api.only(opsSoFar)
        else
          val operationGen: Trials[Operation] = Trials.api.alternate(
            sectionGen.map(Operation.Add.apply),
            if (sectionsSeen.isEmpty) sectionGen.map(Operation.Remove.apply)
            else Trials.api.choose(sectionsSeen).map(Operation.Remove.apply),
            rangeGen(sectionsSeen).map(Operation.QueryIncludes.apply),
            rangeGen(sectionsSeen).map(Operation.QueryOverlaps.apply)
          )

          operationGen.flatMap { op =>
            val nextSectionsSeen = op match
              case Operation.Add(s) => sectionsSeen + s
              case Operation.Remove(s) => sectionsSeen - s
              case _ => sectionsSeen
            generate(opsSoFar :+ op, nextSectionsSeen)
          }

      generate(Vector.empty, Set.empty)
    }

  @TestFactory
  def fidelityAgainstReferenceImplementation(): DynamicTests =
    sequences.withLimit(500).dynamicTests { ops =>
      var sectionsSeen = SectionsSeen.empty[Int]
      var reference = RangedSeq.empty[FakeSection, Int](_.closedOpenInterval, Ordering.Int)
      var hitSeen = false

      ops.foreach {
        case Operation.Add(s) =>
          sectionsSeen = sectionsSeen + s
          reference = reference + s
        case Operation.Remove(s) =>
          if (reference.iterator.contains(s)) hitSeen = true
          sectionsSeen = sectionsSeen - s
          reference = reference - s
        case Operation.QueryIncludes(start, end) =>
          val result = sectionsSeen.filterIncludes((start, end)).toSet
          val expected = reference.filterIncludes((start, end)).toSet
          if (expected.nonEmpty) hitSeen = true
          assertEquals(expected, result, s"Operation: QueryIncludes($start, $end)")
        case Operation.QueryOverlaps(start, end) =>
          val result = sectionsSeen.filterOverlaps((start, end)).toSet
          val expected = reference.filterOverlaps((start, end)).toSet
          if (expected.nonEmpty) hitSeen = true
          assertEquals(expected, result, s"Operation: QueryOverlaps($start, $end)")
      }

      if (!hitSeen) Trials.reject()

      assertEquals(reference.iterator.toSet, sectionsSeen.iterator.toSet, "Final contents mismatch")
    }

  enum Operation:
    case Add(section: FakeSection)
    case Remove(section: FakeSection)
    case QueryIncludes(start: Int, end: Int)
    case QueryOverlaps(start: Int, end: Int)
