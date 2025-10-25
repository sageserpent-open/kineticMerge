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
import scala.math.Ordering.Implicits.seqOrdering

/** The result of a merge; it may be used for merges of an entire file or splice
  * merges. The merged elements are organised into a sequence of [[Segment]]
  * instances - these alternate between resolved runs of elements and localised
  * conflicts.<p> The usual filtration, mapping and a kind of flat-mapping are
  * provided - these will try to preserve the segment structure insofar as
  * locally conflicted segments remain so after the operation; otherwise any
  * such resolved conflict is coalesced with its neighbours if
  * appropriate.<p>Segments are never completely empty of elements, although it
  * is possible to have a conflict where just one side is empty. So if a segment
  * becomes completely empty after an operation, it will be removed.<p>It is
  * also possible to view a [[MergeResult]] from either [[Side]], applying a
  * restructuring operation to the sides (this may be optimised to just once if
  * there is just one resolved segment).
  * @param segments
  * @tparam Element
  */
case class MergeResult[Element: Eq] private (segments: Seq[Segment[Element]]):
  // Resolved and conflicting segments should be coalesced.
  require(segments.isEmpty || segments.zip(segments.tail).forall {
    case (Segment.Resolved(_), Segment.Conflicted(_, _, _)) => true
    case (Segment.Conflicted(_, _, _), Segment.Resolved(_)) => true
    case _                                                  => false
  })

  export segments.{isEmpty, nonEmpty, headOption, lastOption}

  def isConflicted: Boolean = segments match
    case Seq(Segment.Resolved(_)) => false
    case _                        => true

  def fuseWith(another: MergeResult[Element])(
      elementFusion: (Element, Element) => Option[Element]
  ): Option[MergeResult[Element]] =
    if segments.size != another.segments.size then None
    else
      Traverse[Seq]
        .traverse(segments.zip(another.segments))(_.fuseWith(_)(elementFusion))
        .map(MergeResult.apply[Element])

  def addResolved(element: Element): MergeResult[Element] = segments.lastOption
    .fold(ifEmpty = MergeResult(Seq(Segment.Resolved(Seq(element))))) {
      case Segment.Resolved(segmentElements) =>
        MergeResult(
          segments.init :+ Segment.Resolved(segmentElements :+ element)
        )
      case Segment.Conflicted(_, _, _) =>
        MergeResult(segments :+ Segment.Resolved(Seq(element)))
    }

  def addResolved(elements: Seq[Element]): MergeResult[Element] =
    segments.lastOption
      .fold(ifEmpty = MergeResult(Seq(Segment.Resolved(elements)))) {
        case Segment.Resolved(segmentElements) =>
          MergeResult(
            segments.init :+ Segment.Resolved(segmentElements ++ elements)
          )
        case Segment.Conflicted(_, _, _) =>
          MergeResult(segments :+ Segment.Resolved(elements))
      }

  def addConflicted(
      baseElements: Seq[Element],
      leftElements: Seq[Element],
      rightElements: Seq[Element]
  ): MergeResult[Element] = segments.lastOption
    .fold(ifEmpty =
      MergeResult(
        Seq(Segment.Conflicted(baseElements, leftElements, rightElements))
      )
    ) {
      case Segment.Resolved(_) =>
        MergeResult(
          segments :+ Segment.Conflicted(
            baseElements,
            leftElements,
            rightElements
          )
        )
      case Segment.Conflicted(
            segmentBaseElements,
            segmentLeftElements,
            segmentRightElements
          ) =>
        val baseElementsConcatenated  = segmentBaseElements ++ baseElements
        val leftElementsConcatenated  = segmentLeftElements ++ leftElements
        val rightElementsConcatenated = segmentRightElements ++ rightElements

        // NOTE: because the enclosing method is called by
        // `MergeResult.coalescing`, whose callers have already called
        // `MergeResult.segmentFor`; this is a bit inefficient, but we rely on
        // this being a rare occurrence.
        // NOTE: in this case, we know we have elements on at least two sides
        // from the existing segment that the new elements are being coalesced
        // with, so we don't expect the `None` case to arise,
        val Some(coalescedSegment) = segmentFor(
          baseElementsConcatenated,
          leftElementsConcatenated,
          rightElementsConcatenated
        ): @unchecked

        MergeResult(segments.init :+ coalescedSegment)

    }

  def map[Transformed: Eq](
      transform: Element => Transformed
  ): MergeResult[Transformed] = MergeResult.coalescing(segments.flatMap {
    case Segment.Resolved(elements) =>
      segmentFor(elements.map(transform))
    case Segment.Conflicted(baseElements, leftElements, rightElements) =>
      segmentFor(
        baseElements.map(transform),
        leftElements.map(transform),
        rightElements.map(transform)
      )
  })

  def innerFlatMap[Transformed: Eq](
      transform: Element => Seq[Transformed]
  ): MergeResult[Transformed] = MergeResult.coalescing(segments.flatMap {
    case Segment.Resolved(elements) =>
      segmentFor(elements.flatMap(transform))
    case Segment.Conflicted(baseElements, leftElements, rightElements) =>
      segmentFor(
        baseElements.flatMap(transform),
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
        case Segment.Conflicted(baseElements, leftElements, rightElements) =>
          segmentFor(
            baseElements.filter(predicate),
            leftElements.filter(predicate),
            rightElements.filter(predicate)
          )
      }
    )

  def onEachSide[Transformed: Eq](
      transform: MergeResult.Side[Element] => MergeResult.Side[Transformed]
  ): MergeResult[Transformed] =
    segments match
      case Seq(Segment.Resolved(elements)) =>
        val singleSide = MergeResult.Side(elements.map(_ -> 0))

        val MergeResult.Side(transformed) = transform(singleSide)

        MergeResult(segmentFor(transformed.map(_._1)).toSeq)
      case _ =>
        val baseSide = MergeResult.Side(segments.zipWithIndex.flatMap {
          case (Segment.Resolved(elements), label) => elements.map(_ -> label)
          case (Segment.Conflicted(baseElements, _, _), label) =>
            baseElements.map(_ -> label)
        })

        val leftSide = MergeResult.Side(segments.zipWithIndex.flatMap {
          case (Segment.Resolved(elements), label) => elements.map(_ -> label)
          case (Segment.Conflicted(_, leftElements, _), label) =>
            leftElements.map(_ -> label)
        })

        val rightSide = MergeResult.Side(segments.zipWithIndex.flatMap {
          case (Segment.Resolved(elements), label) => elements.map(_ -> label)
          case (Segment.Conflicted(_, _, rightElements), label) =>
            rightElements.map(_ -> label)
        })

        val MergeResult.Side(baseTransformed) = transform(baseSide)

        val MergeResult.Side(leftTransformed) = transform(leftSide)

        val MergeResult.Side(rightTransformed) = transform(rightSide)

        val baseSegmentGroups = baseTransformed.groupMap(_._2)(_._1)

        val leftSegmentGroups = leftTransformed.groupMap(_._2)(_._1)

        val rightSegmentGroups = rightTransformed.groupMap(_._2)(_._1)

        val tuples: Seq[
          (
              Int,
              (
                  Option[Seq[Transformed]],
                  Option[(Option[Seq[Transformed]], Option[Seq[Transformed]])]
              )
          )
        ] = baseSegmentGroups
          .mergeByKey(
            leftSegmentGroups
              .mergeByKey(rightSegmentGroups)
          )
          .toSeq
          .sortBy(_._1)

        coalescing(
          tuples
            .map(_._2)
            .flatMap((_: @unchecked) match
              case (base, Some((left, right))) =>
                segmentFor(
                  base.getOrElse(Seq.empty),
                  left.getOrElse(Seq.empty),
                  right.getOrElse(Seq.empty)
                )
              case (base, None) =>
                segmentFor(base.getOrElse(Seq.empty), Seq.empty, Seq.empty))
        )
  end onEachSide

  inline def tail: MergeResult[Element] = MergeResult(segments.tail)

  inline def init: MergeResult[Element] = MergeResult(segments.init)

  inline def :+(segment: Segment[Element]): MergeResult[Element] = coalescing(
    segments :+ segment
  )

  inline def +:(segment: Segment[Element]): MergeResult[Element] = coalescing(
    segment +: segments
  )
end MergeResult

object MergeResult:
  given mergeResultOrdering[Element: Ordering]: Ordering[MergeResult[Element]] =
    Ordering.by(_.segments)

  given mergeResultEquality[Element: Eq]: Eq[MergeResult[Element]] =
    Eq.by(_.segments)

  def of[Element: Eq](elements: Element*): MergeResult[Element] =
    empty.addResolved(elements)

  def flatten[Element: Eq](
      nestedMergeResults: MergeResult[MergeResult[Element]]
  ): MergeResult[Element] =
    MergeResult.coalescing(nestedMergeResults.segments.flatMap {
      case Segment.Resolved(mergeResults) =>
        mergeResults.flatMap(_.segments)
      case Segment.Conflicted(
            baseMergeResults,
            leftMergeResults,
            rightMergeResults
          ) =>
        def flattenWithinEnclosingConflict(
            segment: MergeResult.Segment[Element]
        ) =
          segment match
            case Segment.Resolved(elements)                         => elements
            case Segment.Conflicted(_, leftElements, rightElements) =>
              leftElements ++ rightElements

        MergeResult.segmentFor(
          // TODO: this is probably nonsense - what should the base contribution
          // really be?
          baseMergeResults
            .flatMap(_.segments)
            .flatMap(flattenWithinEnclosingConflict),
          leftMergeResults
            .flatMap(_.segments)
            .flatMap(flattenWithinEnclosingConflict),
          rightMergeResults
            .flatMap(_.segments)
            .flatMap(flattenWithinEnclosingConflict)
        )
    })

  private def coalescing[Element: Eq](
      segments: Seq[Segment[Element]]
  ): MergeResult[Element] =
    segments.foldLeft(empty) {
      case (partialResult, Segment.Resolved(elements)) =>
        partialResult.addResolved(elements)
      case (
            partialResult,
            Segment.Conflicted(baseElements, leftElements, rightElements)
          ) =>
        partialResult.addConflicted(baseElements, leftElements, rightElements)
    }

  def empty[Element: Eq]: MergeResult[Element] = MergeResult(
    IndexedSeq.empty
  )

  private def segmentFor[Element: Eq](
      baseElements: Seq[Element],
      leftElements: Seq[Element],
      rightElements: Seq[Element]
  ): Option[Segment[Element]] =
    // NOTE: bear in mind that these successive checks rely on the overall order
    // to draw their conclusions...
    if baseElements.corresponds(leftElements)(Eq.eqv) then
      // This turned out to be either a right-edit or a preservation.
      Option.unless(rightElements.isEmpty)(Segment.Resolved(rightElements))
    else if baseElements.corresponds(rightElements)(Eq.eqv) then
      // This turned out to be a left-edit.
      Option.unless(leftElements.isEmpty)(Segment.Resolved(leftElements))
    else if leftElements.corresponds(rightElements)(Eq.eqv) then
      // This turned out to be either a coincident edit or a coincident
      // insertion.
      Option.unless(leftElements.isEmpty)(Segment.Resolved(leftElements))
    else Some(Segment.Conflicted(baseElements, leftElements, rightElements))

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
      case Resolved(elements) => require(elements.nonEmpty)
      case Conflicted(baseElements, leftElements, rightElements) =>
        // NOTE: this also ensures that only one side can be empty.
        require(
          !baseElements.corresponds(leftElements)(Eq.eqv) && !baseElements
            .corresponds(rightElements)(Eq.eqv) && !leftElements.corresponds(
            rightElements
          )(Eq.eqv)
        )
    end match

    def fuseWith(another: Segment[Element])(
        elementFusion: (Element, Element) => Option[Element]
    ): Option[Segment[Element]] = (this, another) match
      case (Resolved(elementsOfThis), Resolved(elementsOfAnother)) =>
        for fusedElements <- Traverse[Seq]
            .traverse(elementsOfThis.zip(elementsOfAnother))(
              elementFusion(_, _)
            )
        yield Resolved(fusedElements)

      case (
            Conflicted(baseThis, leftThis, rightThis),
            Conflicted(baseAnother, leftAnother, rightAnother)
          ) =>
        for
          baseFusion <- Traverse[Seq]
            .traverse(baseThis.zip(baseAnother))(
              elementFusion(_, _)
            )
          leftFusion <- Traverse[Seq]
            .traverse(leftThis.zip(leftAnother))(
              elementFusion(_, _)
            )
          rightFusion <- Traverse[Seq]
            .traverse(rightThis.zip(rightAnother))(
              elementFusion(_, _)
            )
        yield Conflicted(baseFusion, leftFusion, rightFusion)

      case _ => None

    case Resolved(elements: Seq[Element])(using eq: Eq[Element])
    // Don't bother representing different flavours of conflicts, namely left
    // edit / right edit, left deletion / right edit, left edit / right deletion
    // or left insertion / right insertion. These aren't modelled separately in
    // `MergeAlgebra`, and that is fine as it is for now.
    case Conflicted(
        // If this is empty, it represents a left insertion versus right
        // insertion conflict.
        baseElements: Seq[Element],
        // If this is empty, it represents a left deletion versus right edit
        // conflict.
        leftElements: Seq[Element],
        // If this is empty, it represents a left edit versus right deletion
        // conflict.
        rightElements: Seq[Element]
        // Otherwise, if all sides are populated, it represents a left edit
        // versus right edit conflict.
    )(using eq: Eq[Element])
  end Segment

  given segmentOrdering[Element](using
      elementOrdering: Ordering[Element]
  ): Ordering[Segment[Element]] with
    override def compare(x: Segment[Element], y: Segment[Element]): Int =
      (x, y) match
        case (
              Segment.Resolved(elementsFromX),
              Segment.Resolved(elementFromY)
            ) =>
          Ordering[Seq[Element]].compare(elementsFromX, elementFromY)
        case (
              Segment.Conflicted(baseX, leftX, rightX),
              Segment.Conflicted(baseY, leftY, rightY)
            ) =>
          Ordering[(Seq[Element], Seq[Element], Seq[Element])]
            .compare((baseX, leftX, rightX), (baseY, leftY, rightY))
        case (Segment.Resolved(_), Segment.Conflicted(_, _, _)) => -1
        case (Segment.Conflicted(_, _, _), Segment.Resolved(_)) => 1
  end segmentOrdering

  given segmentEquality[Element](using
      elementEquality: Eq[Element]
  ): Eq[Segment[Element]] with
    override def eqv(x: Segment[Element], y: Segment[Element]): Boolean =
      (x, y) match
        case (
              Segment.Resolved(elementsFromX),
              Segment.Resolved(elementFromY)
            ) =>
          Eq[Seq[Element]].eqv(elementsFromX, elementFromY)
        case (
              Segment.Conflicted(baseX, leftX, rightX),
              Segment.Conflicted(baseY, leftY, rightY)
            ) =>
          Eq[(Seq[Element], Seq[Element], Seq[Element])]
            .eqv((baseX, leftX, rightX), (baseY, leftY, rightY))
        case _ => false
  end segmentEquality
end MergeResult

extension [Element: Eq](nestedMergeResults: MergeResult[MergeResult[Element]])
  /** This is necessary because Git doesn't model nested conflicts, where one
    * side of a conflict can contain a smaller conflict. <p>This occurs because
    * splicing can generate such nested conflicts housed within a larger
    * conflict between move destination anchors on one side (with the splice)
    * and some other conflicting content on the opposite side to the anchors.
    *
    * @see
    *   https://github.com/sageserpent-open/kineticMerge/issues/160
    */
  def flatten: MergeResult[Element] = MergeResult.flatten(nestedMergeResults)
end extension

object FullyMerged:
  def unapply[Element](
      result: MergeResult[Element]
  ): Option[Seq[Element]] = Traverse[Seq].flatTraverse(result.segments) {
    case MergeResult.Segment.Resolved(elements)  => Some(elements)
    case MergeResult.Segment.Conflicted(_, _, _) => None
  }
end FullyMerged

object MergedWithConflicts:
  def unapply[Element](
      result: MergeResult[Element]
  ): Option[(Seq[Element], Seq[Element], Seq[Element])] =
    Option.when(result.segments.exists {
      case MergeResult.Segment.Conflicted(_, _, _) => true
      case MergeResult.Segment.Resolved(_)         => false
    })(
      (
        result.segments.flatMap {
          case MergeResult.Segment.Resolved(elements)             => elements
          case MergeResult.Segment.Conflicted(baseElements, _, _) =>
            baseElements
        },
        result.segments.flatMap {
          case MergeResult.Segment.Resolved(elements)             => elements
          case MergeResult.Segment.Conflicted(_, leftElements, _) =>
            leftElements
        },
        result.segments.flatMap {
          case MergeResult.Segment.Resolved(elements)              => elements
          case MergeResult.Segment.Conflicted(_, _, rightElements) =>
            rightElements
        }
      )
    )
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
    val editedResolved = editedElements map MultiSided.Unique.apply
    val leftResolved   = leftEditElements map MultiSided.Unique.apply
    val rightResolved  = rightEditElements map MultiSided.Unique.apply

    result.addConflicted(editedResolved, leftResolved, rightResolved)
  end conflict
end CoreMergeAlgebra
