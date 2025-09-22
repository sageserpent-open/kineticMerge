package com.sageserpent.kineticmerge.core

import cats.{Eq, Traverse}
import com.sageserpent.kineticmerge.core.CoreMergeAlgebra.MultiSidedMergeResult
import com.sageserpent.kineticmerge.core.MergeResult.{
  Segment,
  coalescing,
  segmentFor
}
import com.sageserpent.kineticmerge.core.merge.MergeAlgebra

import scala.collection.decorators.*

case class MergeResult[Element: Eq] private (segments: Seq[Segment[Element]]):
  // Resolved and conflicting segments should be coalesced.
  require(segments.isEmpty || segments.zip(segments.tail).forall {
    case (Segment.Resolved(_), Segment.Conflicted(_, _)) => true
    case (Segment.Conflicted(_, _), Segment.Resolved(_)) => true
    case _                                               => false
  })

  def addResolved(element: Element): MergeResult[Element] = segments.lastOption
    .fold(ifEmpty = MergeResult(Seq(Segment.Resolved(Seq(element))))) {
      case Segment.Resolved(segmentElements) =>
        MergeResult(
          segments.init :+ Segment.Resolved(segmentElements :+ element)
        )
      case Segment.Conflicted(_, _) =>
        MergeResult(segments :+ Segment.Resolved(Seq(element)))
    }

  def addResolved(elements: Seq[Element]): MergeResult[Element] =
    segments.lastOption
      .fold(ifEmpty = MergeResult(Seq(Segment.Resolved(elements)))) {
        case Segment.Resolved(segmentElements) =>
          MergeResult(
            segments.init :+ Segment.Resolved(segmentElements ++ elements)
          )
        case Segment.Conflicted(_, _) =>
          MergeResult(segments :+ Segment.Resolved(elements))
      }

  def addConflicted(
      leftElements: Seq[Element],
      rightElements: Seq[Element]
  ): MergeResult[Element] = segments.lastOption
    .fold(ifEmpty =
      MergeResult(Seq(Segment.Conflicted(leftElements, rightElements)))
    ) {
      case Segment.Resolved(_) =>
        MergeResult(segments :+ Segment.Conflicted(leftElements, rightElements))
      case Segment.Conflicted(segmentLeftElements, segmentRightElements) =>
        MergeResult(
          segments.init :+ Segment.Conflicted(
            segmentLeftElements ++ leftElements,
            segmentRightElements ++ rightElements
          )
        )
    }

  // TODO: remove this...
  def flattenContent: Seq[Element] = segments.flatMap {
    case Segment.Resolved(elements) => elements
    case Segment.Conflicted(
          leftElements,
          rightElements
        ) =>
      // TODO: should really merge these and then flatten out the conflicting
      // parts, rather than just plonking one entire sequence after the other.
      leftElements ++ rightElements
  }

  def map[Transformed: Eq](
      transform: Element => Transformed
  ): MergeResult[Transformed] = MergeResult.coalescing(segments.flatMap {
    case Segment.Resolved(elements) =>
      segmentFor(elements.map(transform))
    case Segment.Conflicted(leftElements, rightElements) =>
      segmentFor(
        leftElements.map(transform),
        rightElements.map(transform)
      )
  })

  def innerFlatMap[Transformed: Eq](
      transform: Element => Seq[Transformed]
  ): MergeResult[Transformed] = MergeResult.coalescing(segments.flatMap {
    case Segment.Resolved(elements) =>
      segmentFor(elements.flatMap(transform))
    case Segment.Conflicted(leftElements, rightElements) =>
      segmentFor(
        leftElements.flatMap(transform),
        rightElements.flatMap(transform)
      )
  })

  def filterNot(predicate: Element => Boolean): MergeResult[Element] = filter(
    predicate.andThen(!_)
  )

  def filter(predicate: Element => Boolean): MergeResult[Element] =
    MergeResult.coalescing(
      segments.flatMap {
        case Segment.Resolved(elements) =>
          segmentFor(elements.filter(predicate))
        case Segment.Conflicted(leftElements, rightElements) =>
          segmentFor(
            leftElements.filter(predicate),
            rightElements.filter(predicate)
          )
      }
    )

  def onEachSide[Transformed: Eq](
      transform: MergeResult.Side[Element] => MergeResult.Side[Transformed]
  ): MergeResult[Transformed] =
    val leftSide = segments.zipWithIndex.flatMap {
      case (Segment.Resolved(elements), label) => elements.map(_ -> label)
      case (Segment.Conflicted(leftElements, _), label) =>
        leftElements.map(_ -> label)
    }

    val rightSide = segments.zipWithIndex.flatMap {
      case (Segment.Resolved(elements), label) => elements.map(_ -> label)
      case (Segment.Conflicted(_, rightElements), label) =>
        rightElements.map(_ -> label)
    }

    val extraSegmentLabel = 1 + leftSide.lastOption
      .fold(ifEmpty = 0)(_._2)
      .max(rightSide.lastOption.fold(ifEmpty = 0)(_._2))

    val MergeResult.Side(leftTransformed) = transform(
      MergeResult.Side(leftSide)
    )

    val MergeResult.Side(rightTransformed) = transform(
      MergeResult.Side(rightSide)
    )

    val leftSegmentGroups = leftTransformed.groupMap(_._2)(_._1)

    val rightSegmentGroups = rightTransformed.groupMap(_._2)(_._1)

    coalescing(
      leftSegmentGroups
        .mergeByKey(rightSegmentGroups)
        .values
        .flatMap((_: @unchecked) match
          case (Some(left), Some(right)) =>
            segmentFor(left, right)
          case (Some(left), None)  => segmentFor(left)
          case (None, Some(right)) => segmentFor(right))
        .toSeq
    )
  end onEachSide
end MergeResult

object MergeResult:
  private def coalescing[Element: Eq](
      segments: Seq[Segment[Element]]
  ): MergeResult[Element] =
    segments.foldLeft(empty) {
      case (partialResult, Segment.Resolved(elements)) =>
        partialResult.addResolved(elements)
      case (partialResult, Segment.Conflicted(leftElements, rightElements)) =>
        partialResult.addConflicted(leftElements, rightElements)
    }

  def empty[Element: Eq]: MergeResult[Element] = MergeResult(
    IndexedSeq.empty
  )

  private def segmentFor[Element: Eq](
      leftElements: Seq[Element],
      rightElements: Seq[Element]
  ): Option[Segment[Element]] =
    if leftElements.corresponds(rightElements)(Eq.eqv) then
      Option.unless(leftElements.isEmpty)(Segment.Resolved(leftElements))
    else
      Option.unless(leftElements.isEmpty && rightElements.isEmpty)(
        Segment.Conflicted(leftElements, rightElements)
      )

  private def segmentFor[Element: Eq](
      elements: Seq[Element]
  ): Option[Segment[Element]] =
    Option.unless(elements.isEmpty)(Segment.Resolved(elements))

  case class Side[Element: Eq](
      elementsWithSegmentLabels: Seq[(Element, Int)]
  ):
    def innerFlatMapAccumulate[State, Transformed: Eq](initialState: State)(
        statefulTransform: (State, Element) => (State, Seq[Transformed])
    ): (State, Side[Transformed]) =
      val (finalState, clumps) =
        Traverse[Seq].mapAccumulate(initialState, elementsWithSegmentLabels) {
          case (accumulatedState, (element, label)) =>
            val (nextState, transformed) =
              statefulTransform(accumulatedState, element)

            nextState -> transformed.map(_ -> label)
        }

      finalState -> Side(elementsWithSegmentLabels = clumps.flatten)
    end innerFlatMapAccumulate

    def append(elements: Seq[Element]): Side[Element] =
      Side(elementsWithSegmentLabels =
        this.elementsWithSegmentLabels ++ elements.map(_ -> Int.MaxValue)
      )
  end Side

  enum Segment[Element: Eq]:
    this match
      case Resolved(elements)                      => require(elements.nonEmpty)
      case Conflicted(leftElements, rightElements) =>
        // NOTE: it's OK if just *one* side is empty.
        require(leftElements.nonEmpty || rightElements.nonEmpty)
        require(!leftElements.corresponds(rightElements)(Eq.eqv))
    end match

    case Resolved(elements: Seq[Element])(using eq: Eq[Element])
    // Don't bother representing different flavours of conflicts, namely left
    // edit / right edit, left deletion / right edit, left edit / right deletion
    // or left insertion / right insertion. These aren't modelled separately in
    // `MergeAlgebra`, and that is fine as it is for now.
    case Conflicted(
        // If this is empty, it represents a left-deletion.
        leftElements: Seq[Element],
        // If this is empty, it represents a right-deletion.
        rightElements: Seq[Element]
    )(using eq: Eq[Element])
  end Segment
end MergeResult

extension [Element](mergeResult: MergeResult[MergeResult[Element]])
  /** This is necessary because Git doesn't model nested conflicts, where one
    * side of a conflict can contain a smaller conflict. <p>This occurs because
    * splicing can generate such nested conflicts housed within a larger
    * conflict between move destination anchors on one side (with the splice)
    * and some other conflicting content on the opposite side to the anchors.
    *
    * @see
    *   https://github.com/sageserpent-open/kineticMerge/issues/160
    */
  def flatten: MergeResult[Element] = ???
end extension

object FullyMerged:
  def unapply[Element](
      result: MergeResult[Element]
  ): Option[Seq[Element]] = Traverse[Seq].flatTraverse(result.segments) {
    case MergeResult.Segment.Resolved(elements) => Some(elements)
    case MergeResult.Segment.Conflicted(_, _)   => None
  }
end FullyMerged

object MergedWithConflicts:
  def unapply[Element](
      result: MergeResult[Element]
  ): Option[(Seq[Element], Seq[Element])] =
    Option.when(result.segments.exists {
      case MergeResult.Segment.Conflicted(_, _) => true
      case MergeResult.Segment.Resolved(_)      => false
    })(result.segments.flatMap {
      case MergeResult.Segment.Resolved(elements)          => elements
      case MergeResult.Segment.Conflicted(leftElements, _) => leftElements
    } -> result.segments.flatMap {
      case MergeResult.Segment.Resolved(elements)           => elements
      case MergeResult.Segment.Conflicted(_, rightElements) => rightElements
    })
  end unapply

end MergedWithConflicts

object CoreMergeAlgebra:
  type MultiSidedMergeResult[Element] = MergeResult[MultiSided[Element]]
end CoreMergeAlgebra

class CoreMergeAlgebra[Element: Eq]
    extends MergeAlgebra[MultiSidedMergeResult, Element]:
  override def empty: MultiSidedMergeResult[Element] = MergeResult.empty

  override def preservation(
      result: MultiSidedMergeResult[Element],
      preservedBaseElement: Element,
      preservedElementOnLeft: Element,
      preservedElementOnRight: Element
  ): MultiSidedMergeResult[Element] =
    val preserved = MultiSided.Preserved(
      preservedBaseElement,
      preservedElementOnLeft,
      preservedElementOnRight
    )

    result.addResolved(preserved)
  end preservation

  override def leftInsertion(
      result: MultiSidedMergeResult[Element],
      insertedElement: Element
  ): MultiSidedMergeResult[Element] =
    val multiSided = MultiSided.Unique(insertedElement)

    result.addResolved(multiSided)
  end leftInsertion

  override def rightInsertion(
      result: MultiSidedMergeResult[Element],
      insertedElement: Element
  ): MultiSidedMergeResult[Element] =
    val multiSided = MultiSided.Unique(insertedElement)

    result.addResolved(multiSided)
  end rightInsertion

  override def coincidentInsertion(
      result: MultiSidedMergeResult[Element],
      insertedElementOnLeft: Element,
      insertedElementOnRight: Element
  ): MultiSidedMergeResult[Element] =
    val coincident =
      MultiSided.Coincident(insertedElementOnLeft, insertedElementOnRight)

    result.addResolved(coincident)
  end coincidentInsertion

  override def leftDeletion(
      result: MultiSidedMergeResult[Element],
      deletedBaseElement: Element,
      deletedRightElement: Element
  ): MultiSidedMergeResult[Element] = result

  override def rightDeletion(
      result: MultiSidedMergeResult[Element],
      deletedBaseElement: Element,
      deletedLeftElement: Element
  ): MultiSidedMergeResult[Element] = result

  override def coincidentDeletion(
      result: MultiSidedMergeResult[Element],
      deletedElement: Element
  ): MultiSidedMergeResult[Element] = result

  override def leftEdit(
      result: MultiSidedMergeResult[Element],
      editedBaseElement: Element,
      editedRightElement: Element,
      editElements: IndexedSeq[Element]
  ): MultiSidedMergeResult[Element] =
    val multiSided = editElements map MultiSided.Unique.apply

    result.addResolved(multiSided)
  end leftEdit

  override def rightEdit(
      result: MultiSidedMergeResult[Element],
      editedBaseElement: Element,
      editedLeftElement: Element,
      editElements: IndexedSeq[Element]
  ): MultiSidedMergeResult[Element] =
    val multiSided = editElements map MultiSided.Unique.apply

    result.addResolved(multiSided)
  end rightEdit

  override def coincidentEdit(
      result: MultiSidedMergeResult[Element],
      editedElement: Element,
      editElements: IndexedSeq[(Element, Element)]
  ): MultiSidedMergeResult[Element] =
    val coincident = editElements.map {
      case (leftEditElement, rightEditElement) =>
        MultiSided.Coincident(leftEditElement, rightEditElement)
    }

    result.addResolved(coincident)
  end coincidentEdit

  override def conflict(
      result: MultiSidedMergeResult[Element],
      editedElements: IndexedSeq[Element],
      leftEditElements: IndexedSeq[Element],
      rightEditElements: IndexedSeq[Element]
  ): MultiSidedMergeResult[Element] =
    val leftResolved  = leftEditElements map MultiSided.Unique.apply
    val rightResolved = rightEditElements map MultiSided.Unique.apply

    result.addConflicted(leftResolved, rightResolved)
  end conflict
end CoreMergeAlgebra
