package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import de.sciss.fingertree.RangedSeq
import org.junit.jupiter.api.TestFactory

case class FakeSection(startOffset: Int, size: Int, id: Int = 0)
    extends Section[Int]:
  override def content: IndexedSeq[Int] = IndexedSeq.empty
  override def render: pprint.Tree      =
    pprint.Tree.Literal(s"Section($startOffset, $size, $id)")
end FakeSection

class SectionsSeenTest:
  private val sections: Trials[FakeSection] = for
    start <- Trials.api.integers(0, 1000)
    size  <- Trials.api.integers(1, 100)
    id    <- Trials.api.integers(0, 10)
  yield FakeSection(start, size, id)

  @TestFactory
  def fidelityAgainstReferenceImplementation(): DynamicTests =
    sequences.withLimit(500).dynamicTests { ops =>
      var sectionsSeen = SectionsSeen.empty[Int]
      var reference    =
        RangedSeq.empty[FakeSection, Int](_.closedOpenInterval, Ordering.Int)
      var referenceWasHit = false

      ops.foreach {
        case Operation.Add(s) =>
          sectionsSeen = sectionsSeen + s
          reference = reference + s
          assert(sectionsSeen.isEmpty == reference.iterator.isEmpty)
          assert(sectionsSeen.size == reference.iterator.size)
          assert(sectionsSeen.isEmpty == sectionsSeen.iterator.isEmpty)
          assert(sectionsSeen.nonEmpty == sectionsSeen.iterator.nonEmpty)
          assert(sectionsSeen.size == sectionsSeen.iterator.size)
          assert(sectionsSeen.headOption == sectionsSeen.iterator.toSeq.headOption)
        case Operation.Remove(s) =>
          if reference.iterator.contains(s) then referenceWasHit = true
          sectionsSeen = sectionsSeen - s
          reference = reference - s
          assert(sectionsSeen.isEmpty == reference.iterator.isEmpty)
          assert(sectionsSeen.size == reference.iterator.size)
          assert(sectionsSeen.isEmpty == sectionsSeen.iterator.isEmpty)
          assert(sectionsSeen.nonEmpty == sectionsSeen.iterator.nonEmpty)
          assert(sectionsSeen.size == sectionsSeen.iterator.size)
          assert(sectionsSeen.headOption == sectionsSeen.iterator.toSeq.headOption)
        case Operation.QueryIncludes(start, end) =>
          val result   = sectionsSeen.filterIncludes((start, end)).toSet
          val expected = reference.filterIncludes((start, end)).toSet
          if expected.nonEmpty then referenceWasHit = true
          assert(
            expected == result,
            s"Operation: QueryIncludes($start, $end)"
          )
        case Operation.QueryOverlaps(start, end) =>
          val result   = sectionsSeen.filterOverlaps((start, end)).toSet
          val expected = reference.filterOverlaps((start, end)).toSet
          if expected.nonEmpty then referenceWasHit = true
          assert(
            expected == result,
            s"Operation: QueryOverlaps($start, $end)"
          )
      }

      if !referenceWasHit then Trials.reject()

      val startOffsets = sectionsSeen.iterator.map(_.startOffset).toSeq
      assert(startOffsets == startOffsets.sorted, "SectionsSeen iterator is not sorted by startOffset")

      assert(
        reference.iterator.toSeq.sortBy(s => (s.startOffset, s.size, s.id)) ==
          sectionsSeen.iterator.toSeq.sortBy(s => (s.startOffset, s.size, s.asInstanceOf[FakeSection].id)),
        "Final contents mismatch"
      )
    }

  private def sequences: Trials[Vector[Operation]] =
    Trials.api.integers(1, 100).flatMap { numberOfOperations =>
      def ranges(sectionsSeen: Set[FakeSection]): Trials[(Int, Int)] =
        for
          useExisting <- Trials.api.booleans
          range       <-
            if useExisting && sectionsSeen.nonEmpty then
              Trials.api.choose(sectionsSeen).flatMap { sectionSeen =>
                val start = sectionSeen.startOffset
                val end   = sectionSeen.onePastEndOffset

                val nestedInsidePossibility =
                  val nudgedEnd   = end - 1
                  val nudgedStart = start + 1

                  Option.when(nudgedEnd > nudgedStart)((nudgedStart, nudgedEnd))
                end nestedInsidePossibility

                val overlapWithStartPossibility =
                  val nudgedStart = start - 5
                  val nudgedEnd   = start + 5

                  Option.when(end >= nudgedEnd)((nudgedStart, nudgedEnd))
                end overlapWithStartPossibility

                val overlapWithEndPossibility =
                  val nudgedStart = end - 5
                  val nudgedEnd   = end + 5

                  Option.when(start <= nudgedStart)((nudgedStart, nudgedEnd))
                end overlapWithEndPossibility

                val vettedAlternatives = Seq(
                  nestedInsidePossibility,
                  overlapWithStartPossibility,
                  overlapWithEndPossibility
                ).flatMap(_.map(Trials.api.only))

                val exactMatchAlternative     = Trials.api.only((start, end))
                val nestingOutsideAlternative =
                  Trials.api.only((start - 1, end + 1))

                Trials.api.alternate(
                  vettedAlternatives :+ exactMatchAlternative :+ nestingOutsideAlternative
                )
              }
            else
              for
                start <- Trials.api.integers(0, 1000)
                size  <- Trials.api.integers(1, 100)
              yield (start, start + size)
        yield range

      def operationSequences(
          partialResult: Vector[Operation],
          sectionsSeen: Map[FakeSection, Int]
      ): Trials[Vector[Operation]] =
        if partialResult.size >= numberOfOperations then
          Trials.api.only(partialResult)
        else
          val operations: Trials[Operation] = Trials.api.alternate(
            if sectionsSeen.isEmpty then sections.map(Operation.Add.apply)
            else
              Trials.api.alternateWithWeights(
                4 -> sections.map(Operation.Add.apply),
                4 -> Trials.api
                  .choose(sectionsSeen.keySet)
                  .map(
                    Operation.Add.apply
                  ) // Add some duplicates in occasionally.
              )
            ,
            if sectionsSeen.isEmpty then sections.map(Operation.Remove.apply)
            else
              Trials.api.alternateWithWeights(
                6 -> Trials.api
                  .choose(sectionsSeen.keySet)
                  .map(
                    Operation.Remove.apply
                  ),
                1 -> sections
                  .filter(!sectionsSeen.contains(_))
                  .map(
                    Operation.Remove.apply
                  ) // Attempt to remove a section that isn't present occasionally.
              )
            ,
            ranges(sectionsSeen.keySet).map(Operation.QueryIncludes.apply),
            ranges(sectionsSeen.keySet).map(Operation.QueryOverlaps.apply)
          )

          operations.flatMap { op =>
            val nextSectionsSeen = op match
              case Operation.Add(s)    => sectionsSeen.updated(s, sectionsSeen.getOrElse(s, 0) + 1)
              case Operation.Remove(s) =>
                val count = sectionsSeen.getOrElse(s, 0)
                if count > 1 then sectionsSeen.updated(s, count - 1)
                else sectionsSeen - s
              case _                   => sectionsSeen
            operationSequences(partialResult :+ op, nextSectionsSeen)
          }

      operationSequences(Vector.empty, Map.empty)
    }

  enum Operation:
    case Add(section: FakeSection)
    case Remove(section: FakeSection)
    case QueryIncludes(start: Int, end: Int)
    case QueryOverlaps(start: Int, end: Int)
  end Operation
end SectionsSeenTest
