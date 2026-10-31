package com.sageserpent.kineticmerge.core

import cats.{Eq, Traverse}
import com.sageserpent.kineticmerge.core.CoreMergeAlgebra.MultiSidedMergeResult
import com.sageserpent.kineticmerge.core.MergeResult.{
  Item,
  Segment,
  coalescingItems,
  segmentForItems
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
  * also possible to view a [[MergeResult]] from any of the three sides,
  * applying a restructuring operation to the sides (this may be optimised to
  * just once if there is just one resolved segment).
  * @param segments
  * @tparam Element
  */
case class MergeResult[Element: Eq] private (
    segments: Seq[Segment[Item[Element]]]
):
  import MergeResult.given
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
    def itemFusion(
        item1: Item[Element],
        item2: Item[Element]
    ): Option[Item[Element]] = (item1: Any, item2: Any) match
      case (e1: Element @unchecked, e2: Element @unchecked)
          if !e1.isInstanceOf[MergeResult[?]] && !e2.isInstanceOf[MergeResult[?]] =>
        elementFusion(e1, e2)
      case (
            m1: MergeResult[Element] @unchecked,
            m2: MergeResult[Element] @unchecked
          ) =>
        m1.fuseWith(m2)(elementFusion)
      case _ => None

    if segments.size != another.segments.size then None
    else
      Traverse[Seq]
        .traverse(segments.zip(another.segments))(_.fuseWith(_)(itemFusion))
        .map(MergeResult.apply[Element])

  def addResolved(element: Item[Element]): MergeResult[Element] = segments.lastOption
    .fold(ifEmpty = MergeResult(Seq(Segment.Resolved[Item[Element]](Seq(element))))) {
      case Segment.Resolved(segmentItems) =>
        MergeResult(
          segments.init :+ Segment.Resolved[Item[Element]](segmentItems :+ element)
        )
      case Segment.Conflicted(_, _, _) =>
        MergeResult(segments :+ Segment.Resolved[Item[Element]](Seq(element)))
    }

  def addConflicted(
      baseElements: Seq[Item[Element]],
      leftElements: Seq[Item[Element]],
      rightElements: Seq[Item[Element]]
  ): MergeResult[Element] = segmentForItems(
    baseElements,
    leftElements,
    rightElements
  ).fold(ifEmpty = this) {
    case segment @ Segment.Conflicted(base, left, right) =>
      segments.lastOption match
        case Some(Segment.Conflicted(sb, sl, sr)) =>
          val Some(coalesced) = segmentForItems(
            sb ++ base,
            sl ++ left,
            sr ++ right
          ): @unchecked
          MergeResult(segments.init :+ coalesced)
        case _ =>
          MergeResult(segments :+ segment)
    case Segment.Resolved(items) =>
      addResolved(elements = items)
  }

  def addResolved(elements: Seq[Item[Element]]): MergeResult[Element] =
    if elements.isEmpty then this
    else
      segments.lastOption
        .fold(ifEmpty = MergeResult(Seq(Segment.Resolved[Item[Element]](elements)))) {
          case Segment.Resolved(segmentItems) =>
            MergeResult(
              segments.init :+ Segment.Resolved[Item[Element]](segmentItems ++ elements)
            )
          case Segment.Conflicted(_, _, _) =>
            MergeResult(segments :+ Segment.Resolved[Item[Element]](elements))
        }

  def map[Transformed: Eq](
      transform: Element => Transformed
  ): MergeResult[Transformed] =
    def transformItem(item: Item[Element]): Item[Transformed] = (item: Any) match
      case nested: MergeResult[Element] @unchecked =>
        nested.map(transform)
      case element: Element @unchecked =>
        transform(element)

    MergeResult.coalescingItems(segments.flatMap {
      case Segment.Resolved(items) =>
        segmentForItems(items.map(transformItem))
      case Segment.Conflicted(baseItems, leftItems, rightItems) =>
        segmentForItems(
          baseItems.map(transformItem),
          leftItems.map(transformItem),
          rightItems.map(transformItem)
        )
    })

  def innerFlatMap[Transformed: Eq](
      transform: Element => Seq[Transformed]
  ): MergeResult[Transformed] =
    def transformItem(item: Item[Element]): Seq[Item[Transformed]] = (item: Any) match
      case nested: MergeResult[Element] @unchecked =>
        Seq(nested.innerFlatMap(transform))
      case element: Element @unchecked =>
        transform(element)

    MergeResult.coalescingItems(segments.flatMap {
      case Segment.Resolved(items) =>
        segmentForItems(items.flatMap(transformItem))
      case Segment.Conflicted(baseItems, leftItems, rightItems) =>
        segmentForItems(
          baseItems.flatMap(transformItem),
          leftItems.flatMap(transformItem),
          rightItems.flatMap(transformItem)
        )
    })

  def filterNot(predicate: Element => Boolean): MergeResult[Element] = filter(
    predicate.andThen(!_)
  )

  def filter(predicate: Element => Boolean): MergeResult[Element] =
    def filterItem(item: Item[Element]): Option[Item[Element]] = (item: Any) match
      case nested: MergeResult[Element] @unchecked =>
        val filtered = nested.filter(predicate)
        Option.unless(filtered.isEmpty)(filtered)
      case element: Element @unchecked =>
        Option.when(predicate(element))(element)

    MergeResult.coalescingItems(
      segments.flatMap {
        case Segment.Resolved(items) =>
          segmentForItems(items.flatMap(filterItem))
        case Segment.Conflicted(baseItems, leftItems, rightItems) =>
          segmentForItems(
            baseItems.flatMap(filterItem),
            leftItems.flatMap(filterItem),
            rightItems.flatMap(filterItem)
          )
      }
    )

  def onEachSide[Transformed: Eq](
      transform: MergeResult.Side[Element] => MergeResult.Side[Transformed]
  ): MergeResult[Transformed] =
    onEachSideItems[Transformed] { side =>
      def transformItem(item: Item[Element]): Seq[Item[Transformed]] = (item: Any) match
        case nested: MergeResult[Element] @unchecked =>
          Seq(nested.onEachSide(transform))
        case elem: Element @unchecked =>
          val singleSide = MergeResult.Side(Seq(elem -> 0))
          val MergeResult.Side(transformed) = transform(singleSide)
          transformed.map(_._1)

      val ((), transformedSide) = side.innerFlatMapAccumulate(()) { case ((), item) =>
        () -> transformItem(item)
      }
      transformedSide
    }

  def onEachSideItems[Transformed: Eq](
      transform: MergeResult.Side[Item[Element]] => MergeResult.Side[Item[Transformed]]
  ): MergeResult[Transformed] =
    segments match
      case Seq(Segment.Resolved(items)) =>
        val singleSide = MergeResult.Side(items.map(_ -> 0))

        val MergeResult.Side(transformed) = transform(singleSide)

        coalescingItems[Transformed](segmentForItems[Transformed](transformed.map(_._1)).toSeq)
      case _ =>
        val baseSide = MergeResult.Side(segments.zipWithIndex.flatMap {
          case (Segment.Resolved(items), label) => items.map(_ -> label)
          case (Segment.Conflicted(baseItems, _, _), label) =>
            baseItems.map(_ -> label)
        })

        val leftSide = MergeResult.Side(segments.zipWithIndex.flatMap {
          case (Segment.Resolved(items), label) => items.map(_ -> label)
          case (Segment.Conflicted(_, leftItems, _), label) =>
            leftItems.map(_ -> label)
        })

        val rightSide = MergeResult.Side(segments.zipWithIndex.flatMap {
          case (Segment.Resolved(items), label) => items.map(_ -> label)
          case (Segment.Conflicted(_, _, rightItems), label) =>
            rightItems.map(_ -> label)
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
                  Option[Seq[Item[Transformed]]],
                  Option[(Option[Seq[Item[Transformed]]], Option[Seq[Item[Transformed]]])]
              )
          )
        ] = baseSegmentGroups
          .mergeByKey(
            leftSegmentGroups
              .mergeByKey(rightSegmentGroups)
          )
          .toSeq
          .sortBy(_._1)

        coalescingItems(
          tuples
            .map(_._2)
            .flatMap((_: @unchecked) match
              case (base, Some((left, right))) =>
                segmentForItems(
                  base.getOrElse(Seq.empty),
                  left.getOrElse(Seq.empty),
                  right.getOrElse(Seq.empty)
                )
              case (base, None) =>
                segmentForItems(base.getOrElse(Seq.empty), Seq.empty, Seq.empty))
        )
  end onEachSideItems

  inline def tail: MergeResult[Element] = MergeResult(segments.tail)

  inline def init: MergeResult[Element] = MergeResult(segments.init)

  inline def :+(segment: Segment[Item[Element]]): MergeResult[Element] = coalescingItems(
    segments :+ segment
  )

  inline def +:(segment: Segment[Item[Element]]): MergeResult[Element] = coalescingItems(
    segment +: segments
  )
end MergeResult

object MergeResult:
  type Item[Element] = Element | MergeResult[Element]

  given itemEquality[Element](using elementEq: Eq[Element]): Eq[Item[Element]] =
    new Eq[Item[Element]]:
      override def eqv(x: Item[Element], y: Item[Element]): Boolean =
        (x: Any, y: Any) match
          case (xRes: MergeResult[Element] @unchecked, yRes: MergeResult[Element] @unchecked) =>
            mergeResultEquality[Element].eqv(xRes, yRes)
          case (xElem: Element @unchecked, yElem: Element @unchecked)
              if !xElem.isInstanceOf[MergeResult[?]] && !yElem.isInstanceOf[MergeResult[?]] =>
            elementEq.eqv(xElem, yElem)
          case _ => false

  given itemOrdering[Element](using elementOrdering: Ordering[Element]): Ordering[Item[Element]] =
    new Ordering[Item[Element]]:
      override def compare(x: Item[Element], y: Item[Element]): Int =
        (x: Any, y: Any) match
          case (xRes: MergeResult[Element] @unchecked, yRes: MergeResult[Element] @unchecked) =>
            mergeResultOrdering[Element].compare(xRes, yRes)
          case (xElem: Element @unchecked, yElem: Element @unchecked)
              if !xElem.isInstanceOf[MergeResult[?]] && !yElem.isInstanceOf[MergeResult[?]] =>
            elementOrdering.compare(xElem, yElem)
          case (_: MergeResult[?] @unchecked, _) => 1
          case (_, _: MergeResult[?] @unchecked) => -1
          case _                                 => 0

  given mergeResultOrdering[Element: Ordering]: Ordering[MergeResult[Element]] =
    Ordering.by(_.segments)

  given mergeResultEquality[Element: Eq]: Eq[MergeResult[Element]] =
    Eq.by(_.segments)

  def of[Element: Eq](elements: Element*): MergeResult[Element] =
    empty.addResolved(elements = elements)

  def ofItems[Element: Eq](items: Item[Element]*): MergeResult[Element] =
    empty.addResolved(elements = items)

  extension [Element: Eq](result: MergeResult[Element])
    def flattenRecursively: MergeResult[Element] =
      def flattenWithinEnclosingConflict(
          segment: MergeResult.Segment[Item[Element]]
      ): Seq[Item[Element]] =
        segment match
          case Segment.Resolved(items) =>
            flattenItems(items)
          case Segment.Conflicted(_, leftItems, rightItems) =>
            flattenItems(leftItems) ++ flattenItems(rightItems)

      def flattenItems(items: Seq[Item[Element]]): Seq[Item[Element]] =
        items.flatMap {
          case nested: MergeResult[Element] @unchecked if nested.isInstanceOf[MergeResult[?]] =>
            val flattenedNested = nested.flattenRecursively
            flattenedNested.segments.flatMap {
              case Segment.Resolved(resItems) =>
                flattenItems(resItems)
              case Segment.Conflicted(_, leftItems, rightItems) =>
                flattenItems(leftItems) ++ flattenItems(rightItems)
            }
          case elem =>
            Seq(elem)
        }

      coalescingItems(result.segments.flatMap {
        case Segment.Resolved(items) =>
          items.flatMap {
            case nested: MergeResult[Element] @unchecked if nested.isInstanceOf[MergeResult[?]] =>
              nested.flattenRecursively.segments
            case elem =>
              segmentForItems(Seq(elem))
          }
        case Segment.Conflicted(baseItems, leftItems, rightItems) =>
          def flattenItemsInConflictSide(items: Seq[Item[Element]]): Seq[Item[Element]] =
            items.flatMap {
              case nested: MergeResult[Element] @unchecked if nested.isInstanceOf[MergeResult[?]] =>
                val flattened = nested.flattenRecursively
                flattened.segments.flatMap(flattenWithinEnclosingConflict)
              case elem =>
                Seq(elem)
            }

          segmentForItems(
            flattenItemsInConflictSide(baseItems),
            flattenItemsInConflictSide(leftItems),
            flattenItemsInConflictSide(rightItems)
          )
      })
  end extension

  def flatten[Element: Eq](
      nestedMergeResults: MergeResult[MergeResult[Element]]
  ): MergeResult[Element] =
    def toItemSeq(items: Seq[Item[MergeResult[Element]]]): Seq[Item[Element]] =
      items.flatMap {
        case r: MergeResult[Element] @unchecked => Seq(r)
      }

    val asMergeResultOfElement: MergeResult[Element] =
      MergeResult(nestedMergeResults.segments.map {
        case Segment.Resolved(results) =>
          Segment.Resolved(toItemSeq(results))
        case Segment.Conflicted(base, left, right) =>
          Segment.Conflicted(
            toItemSeq(base),
            toItemSeq(left),
            toItemSeq(right)
          )
      })
    asMergeResultOfElement.flattenRecursively

  private def coalescingItems[Element: Eq](
      segments: Seq[Segment[Item[Element]]]
  ): MergeResult[Element] =
    segments.foldLeft(empty) {
      case (partialResult, Segment.Resolved(items)) =>
        partialResult.addResolved(elements = items)
      case (
            partialResult,
            Segment.Conflicted(baseItems, leftItems, rightItems)
          ) =>
        partialResult.addConflicted(baseElements = baseItems, leftElements = leftItems, rightElements = rightItems)
    }

  def empty[Element: Eq]: MergeResult[Element] = MergeResult(
    IndexedSeq.empty
  )

  private def segmentForItems[Element: Eq](
      baseItems: Seq[Item[Element]],
      leftItems: Seq[Item[Element]],
      rightItems: Seq[Item[Element]]
  ): Option[Segment[Item[Element]]] =
    // NOTE: bear in mind that these successive checks rely on the overall order
    // to draw their conclusions...
    if baseItems.corresponds(leftItems)(Eq.eqv) then
      // This turned out to be either a right-edit or a preservation.
      Option.unless(rightItems.isEmpty)(Segment.Resolved(rightItems))
    else if baseItems.corresponds(rightItems)(Eq.eqv) then
      // This turned out to be a left-edit.
      Option.unless(leftItems.isEmpty)(Segment.Resolved(leftItems))
    else if leftItems.corresponds(rightItems)(Eq.eqv) then
      // This turned out to be either a coincident edit or a coincident
      // insertion.
      Option.unless(leftItems.isEmpty)(Segment.Resolved(leftItems))
    else Some(Segment.Conflicted(baseItems, leftItems, rightItems))

  private def segmentForItems[Element: Eq](
      items: Seq[Item[Element]]
  ): Option[Segment[Item[Element]]] =
    Option.unless(items.isEmpty)(Segment.Resolved(items))

  case class Side[ItemType: Eq](
      elementsWithSegmentLabels: Seq[(ItemType, Int)]
  ):
    def innerFlatMapAccumulate[State, Transformed: Eq](initialState: State)(
        statefulTransform: (State, ItemType) => (State, Seq[Transformed])
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

    def append(elements: Seq[ItemType]): Side[ItemType] =
      Side(elementsWithSegmentLabels =
        this.elementsWithSegmentLabels ++ elements.map(_ -> Int.MaxValue)
      )
  end Side

  enum Segment[Item: Eq]:
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

    def fuseWith(another: Segment[Item])(
        elementFusion: (Item, Item) => Option[Item]
    ): Option[Segment[Item]] = (this, another) match
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

    case Resolved(elements: Seq[Item])(using eq: Eq[Item])
    // Don't bother representing different flavours of conflicts, namely left
    // edit / right edit, left deletion / right edit, left edit / right deletion
    // or left insertion / right insertion. These aren't modelled separately in
    // `MergeAlgebra`, and that is fine as it is for now.
    case Conflicted(
        // If this is empty, it represents a left insertion versus right
        // insertion conflict.
        baseElements: Seq[Item],
        // If this is empty, it represents a left deletion versus right edit
        // conflict.
        leftElements: Seq[Item],
        // If this is empty, it represents a left edit versus right deletion
        // conflict.
        rightElements: Seq[Item]
        // Otherwise, if all sides are populated, it represents a left edit
        // versus right edit conflict.
    )(using eq: Eq[Item])
  end Segment

  given segmentOrdering[Item](using
      itemOrdering: Ordering[Item]
  ): Ordering[Segment[Item]] with
    override def compare(x: Segment[Item], y: Segment[Item]): Int =
      (x, y) match
        case (
              Segment.Resolved(elementsFromX),
              Segment.Resolved(elementFromY)
            ) =>
          Ordering[Seq[Item]].compare(elementsFromX, elementFromY)
        case (
              Segment.Conflicted(baseX, leftX, rightX),
              Segment.Conflicted(baseY, leftY, rightY)
            ) =>
          Ordering[(Seq[Item], Seq[Item], Seq[Item])]
            .compare((baseX, leftX, rightX), (baseY, leftY, rightY))
        case (Segment.Resolved(_), Segment.Conflicted(_, _, _)) => -1
        case (Segment.Conflicted(_, _, _), Segment.Resolved(_)) => 1
  end segmentOrdering

  given segmentEquality[Item](using
      itemEquality: Eq[Item]
  ): Eq[Segment[Item]] with
    override def eqv(x: Segment[Item], y: Segment[Item]): Boolean =
      (x, y) match
        case (
              Segment.Resolved(elementsFromX),
              Segment.Resolved(elementFromY)
            ) =>
          Eq[Seq[Item]].eqv(elementsFromX, elementFromY)
        case (
              Segment.Conflicted(baseX, leftX, rightX),
              Segment.Conflicted(baseY, leftY, rightY)
            ) =>
          Eq[(Seq[Item], Seq[Item], Seq[Item])]
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
  def unapply[Element: Eq](
      result: MergeResult[Element]
  ): Option[Seq[Element]] =
    val flattened = result.flattenRecursively
    Traverse[Seq].flatTraverse(flattened.segments) {
      case MergeResult.Segment.Resolved(items)  =>
        Some(items.collect { case elem: Element @unchecked if !elem.isInstanceOf[MergeResult[?]] => elem })
      case MergeResult.Segment.Conflicted(_, _, _) => None
    }
end FullyMerged

object MergedWithConflicts:
  def unapply[Element: Eq](
      result: MergeResult[Element]
  ): Option[(Seq[Element], Seq[Element], Seq[Element])] =
    val flattened = result.flattenRecursively
    Option.when(flattened.segments.exists {
      case MergeResult.Segment.Conflicted(_, _, _) => true
      case MergeResult.Segment.Resolved(_)         => false
    })(
      (
        flattened.segments.flatMap {
          case MergeResult.Segment.Resolved(items)             =>
            items.collect { case elem: Element @unchecked if !elem.isInstanceOf[MergeResult[?]] => elem }
          case MergeResult.Segment.Conflicted(baseItems, _, _) =>
            baseItems.collect { case elem: Element @unchecked if !elem.isInstanceOf[MergeResult[?]] => elem }
        },
        flattened.segments.flatMap {
          case MergeResult.Segment.Resolved(items)             =>
            items.collect { case elem: Element @unchecked if !elem.isInstanceOf[MergeResult[?]] => elem }
          case MergeResult.Segment.Conflicted(_, leftItems, _) =>
            leftItems.collect { case elem: Element @unchecked if !elem.isInstanceOf[MergeResult[?]] => elem }
        },
        flattened.segments.flatMap {
          case MergeResult.Segment.Resolved(items)              =>
            items.collect { case elem: Element @unchecked if !elem.isInstanceOf[MergeResult[?]] => elem }
          case MergeResult.Segment.Conflicted(_, _, rightItems) =>
            rightItems.collect { case elem: Element @unchecked if !elem.isInstanceOf[MergeResult[?]] => elem }
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
