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
  val sections: Trials[FakeSection] = for
    start <- Trials.api.integers(0, 1000)
    size <- Trials.api.integers(1, 100)
    id <- Trials.api.integers(0, 10)
  yield FakeSection(start, size, id)

  val operations: Trials[Operation] = Trials.api.alternate(
    sections.map(Operation.Add.apply),
    sections.map(Operation.Remove.apply),
    for
      start <- Trials.api.integers(0, 1000)
      size <- Trials.api.integers(1, 100)
    yield Operation.QueryIncludes(start, start + size),
    for
      start <- Trials.api.integers(0, 1000)
      size <- Trials.api.integers(1, 100)
    yield Operation.QueryOverlaps(start, start + size)
  )

  @TestFactory
  def tests(): DynamicTests =
    operations.several[Vector[Operation]].withLimit(100).dynamicTests { ops =>
      var sectionsSeen = SectionsSeen.empty[Int]
      var reference = RangedSeq.empty[FakeSection, Int](_.closedOpenInterval, Ordering.Int)

      ops.foreach {
        case Operation.Add(s) =>
          sectionsSeen = sectionsSeen + s
          reference = reference + s
        case Operation.Remove(s) =>
          sectionsSeen = sectionsSeen - s
          reference = reference - s
        case Operation.QueryIncludes(start, end) =>
          val result = sectionsSeen.filterIncludes((start, end)).toSet
          val expected = reference.filterIncludes((start, end)).toSet
          assertEquals(expected, result, s"Operation: QueryIncludes($start, $end)")
        case Operation.QueryOverlaps(start, end) =>
          val result = sectionsSeen.filterOverlaps((start, end)).toSet
          val expected = reference.filterOverlaps((start, end)).toSet
          assertEquals(expected, result, s"Operation: QueryOverlaps($start, $end)")
      }

      assertEquals(reference.iterator.toSet, sectionsSeen.iterator.toSet, "Final contents mismatch")
    }

  enum Operation:
    case Add(section: FakeSection)
    case Remove(section: FakeSection)
    case QueryIncludes(start: Int, end: Int)
    case QueryOverlaps(start: Int, end: Int)
