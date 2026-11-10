package com.sageserpent.kineticmerge.core

import cats.{Eq, Traverse}
import com.sageserpent.kineticmerge.core.CoreMergeAlgebra.MultiSidedMergeResult
import com.sageserpent.kineticmerge.core.MergeResult.{
  Segment,
  SegmentItem,
  coalescing,
  coalescingItems,
  fuseItems,
  segmentFor,
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
case class MergeResult[Element: Eq] private[core] (
    segments: Seq[Segment[Element]]
):
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
        first: SegmentItem[Element],
        second: SegmentItem[Element]
    ): Option[SegmentItem[Element]] = (first, second) match
      case (SegmentItem.Leaf(e1), SegmentItem.Leaf(e2)) =>
        elementFusion(e1, e2).map(SegmentItem.Leaf.apply)
      case (SegmentItem.Nested(mr1), SegmentItem.Nested(mr2)) =>
        mr1.fuseWith(mr2)(elementFusion).map(SegmentItem.Nested.apply)
      case _ => None

    fuseItems(segments, another.segments)(_.fuseWith(_)(itemFusion))
      .map(MergeResult.apply[Element])

  def addResolved(element: Element): MergeResult[Element] = segments.lastOption
    .fold(
      ifEmpty = MergeResult(
        Seq(Segment.Resolved(Seq(SegmentItem.Leaf(element))))
      )
    ) {
      case Segment.Resolved(segmentElements) =>
        MergeResult(
          segments.init :+ Segment.Resolved(
            segmentElements :+ SegmentItem.Leaf(element)
          )
        )
      case Segment.Conflicted(_, _, _) =>
        MergeResult(
          segments :+ Segment.Resolved(Seq(SegmentItem.Leaf(element)))
        )
    }

  def addConflicted(
      baseElements: Seq[Element],
      leftElements: Seq[Element],
      rightElements: Seq[Element]
  ): MergeResult[Element] =
    addConflictedItems(
      baseElements.map(SegmentItem.Leaf.apply),
      leftElements.map(SegmentItem.Leaf.apply),
      rightElements.map(SegmentItem.Leaf.apply)
    )

  def addResolved(elements: Seq[Element]): MergeResult[Element] =
    addResolvedItems(elements.map(SegmentItem.Leaf.apply))

  private[core] def addResolvedItems(
      items: Seq[SegmentItem[Element]]
  ): MergeResult[Element] =
    if items.isEmpty then this
    else
      segments.lastOption
        .fold(ifEmpty = MergeResult(Seq(Segment.Resolved(items)))) {
          case Segment.Resolved(segmentElements) =>
            MergeResult(
              segments.init :+ Segment.Resolved(segmentElements ++ items)
            )
          case Segment.Conflicted(_, _, _) =>
            MergeResult(segments :+ Segment.Resolved(items))
        }

  private[core] def addConflictedItems(
      baseItems: Seq[SegmentItem[Element]],
      leftItems: Seq[SegmentItem[Element]],
      rightItems: Seq[SegmentItem[Element]]
  ): MergeResult[Element] = segmentForItems(
    baseItems,
    leftItems,
    rightItems
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
      addResolvedItems(items)
  }

  def map[Transformed: Eq](
      transform: Element => Transformed
  ): MergeResult[Transformed] =
    def transformItem(
        item: SegmentItem[Element]
    ): SegmentItem[Transformed] = item match
      case SegmentItem.Leaf(element) =>
        SegmentItem.Leaf(transform(element))
      case SegmentItem.Nested(mergeResult) =>
        SegmentItem.Nested(mergeResult.map(transform))

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
    def transformItem(
        item: SegmentItem[Element]
    ): Seq[SegmentItem[Transformed]] = item match
      case SegmentItem.Leaf(element) =>
        transform(element).map(SegmentItem.Leaf.apply)
      case SegmentItem.Nested(mergeResult) =>
        Seq(SegmentItem.Nested(mergeResult.innerFlatMap(transform)))

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
    def filterItem(
        item: SegmentItem[Element]
    ): Option[SegmentItem[Element]] = item match
      case SegmentItem.Leaf(element) =>
        Option.when(predicate(element))(item)
      case SegmentItem.Nested(mergeResult) =>
        val filtered = mergeResult.filter(predicate)
        Option.unless(filtered.isEmpty)(SegmentItem.Nested(filtered))

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
    // Flatten any nested merge results down to one level first so that the
    // side view sees flat elements.
    MergeResult.flattenToOneLevel(this).onEachSideImpl(transform)

  private def onEachSideImpl[Transformed: Eq](
      transform: MergeResult.Side[Element] => MergeResult.Side[Transformed]
  ): MergeResult[Transformed] =
    segments match
      case Seq(Segment.Resolved(items)) =>
        val singleSide = MergeResult.Side(items.map {
          case SegmentItem.Leaf(element) => element -> 0
          case SegmentItem.Nested(_)     =>
            // Should not occur as onEachSideImpl is called after flattenToOneLevel
            throw new IllegalStateException(
              "Unexpected nested merge result in onEachSideImpl"
            )
        })

        val MergeResult.Side(transformed) = transform(singleSide)

        MergeResult(segmentFor(transformed.map(_._1)).toSeq)
      case _ =>
        val baseSide = MergeResult.Side(segments.zipWithIndex.flatMap {
          case (Segment.Resolved(items), label) =>
            items.map {
              case SegmentItem.Leaf(element) => element -> label
              case SegmentItem.Nested(_)     =>
                throw new IllegalStateException(
                  "Unexpected nested merge result in onEachSideImpl"
                )
            }
          case (Segment.Conflicted(baseItems, _, _), label) =>
            baseItems.map {
              case SegmentItem.Leaf(element) => element -> label
              case SegmentItem.Nested(_)     =>
                throw new IllegalStateException(
                  "Unexpected nested merge result in onEachSideImpl"
                )
            }
        })

        val leftSide = MergeResult.Side(segments.zipWithIndex.flatMap {
          case (Segment.Resolved(items), label) =>
            items.map {
              case SegmentItem.Leaf(element) => element -> label
              case SegmentItem.Nested(_)     =>
                throw new IllegalStateException(
                  "Unexpected nested merge result in onEachSideImpl"
                )
            }
          case (Segment.Conflicted(_, leftItems, _), label) =>
            leftItems.map {
              case SegmentItem.Leaf(element) => element -> label
              case SegmentItem.Nested(_)     =>
                throw new IllegalStateException(
                  "Unexpected nested merge result in onEachSideImpl"
                )
            }
        })

        val rightSide = MergeResult.Side(segments.zipWithIndex.flatMap {
          case (Segment.Resolved(items), label) =>
            items.map {
              case SegmentItem.Leaf(element) => element -> label
              case SegmentItem.Nested(_)     =>
                throw new IllegalStateException(
                  "Unexpected nested merge result in onEachSideImpl"
                )
            }
          case (Segment.Conflicted(_, _, rightItems), label) =>
            rightItems.map {
              case SegmentItem.Leaf(element) => element -> label
              case SegmentItem.Nested(_)     =>
                throw new IllegalStateException(
                  "Unexpected nested merge result in onEachSideImpl"
                )
            }
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
  end onEachSideImpl

  inline def tail: MergeResult[Element] = MergeResult(segments.tail)

  inline def init: MergeResult[Element] = MergeResult(segments.init)

  inline def :+(segment: Segment[Element]): MergeResult[Element] =
    MergeResult.coalescingSegments(segments :+ segment)

  inline def +:(segment: Segment[Element]): MergeResult[Element] =
    MergeResult.coalescingSegments(segment +: segments)
end MergeResult

object MergeResult:
  enum SegmentItem[Element]:
    case Leaf(element: Element)
    case Nested(mergeResult: MergeResult[Element])(using Eq[Element])

  object SegmentItem:
    given segmentItemEquality[Element: Eq]: Eq[SegmentItem[Element]] with
      override def eqv(
          x: SegmentItem[Element],
          y: SegmentItem[Element]
      ): Boolean =
        (x, y) match
          case (Leaf(elemX), Leaf(elemY)) =>
            Eq[Element].eqv(elemX, elemY)
          case (Nested(mrX), Nested(mrY)) =>
            Eq[MergeResult[Element]].eqv(mrX, mrY)
          case _ => false

    given segmentItemOrdering[Element: Ordering: Eq]
        : Ordering[SegmentItem[Element]] with
      override def compare(
          x: SegmentItem[Element],
          y: SegmentItem[Element]
      ): Int =
        (x, y) match
          case (Leaf(elemX), Leaf(elemY)) =>
            Ordering[Element].compare(elemX, elemY)
          case (Nested(mrX), Nested(mrY)) =>
            Ordering[MergeResult[Element]].compare(mrX, mrY)
          case (Leaf(_), Nested(_)) => -1
          case (Nested(_), Leaf(_)) => 1

  private def fuseItems[Item](first: Seq[Item], second: Seq[Item])(
      itemFusion: (Item, Item) => Option[Item]
  ): Option[Seq[Item]] =
    if first.size == second.size then
      Traverse[Seq]
        .traverse(first.zip(second))(itemFusion(_, _))
    else None

  given mergeResultOrdering[Element: Ordering: Eq]: Ordering[MergeResult[Element]] =
    Ordering.by(_.segments)

  given mergeResultEquality[Element: Eq]: Eq[MergeResult[Element]] =
    Eq.by(_.segments)

  def of[Element: Eq](elements: Element*): MergeResult[Element] =
    empty.addResolved(elements)

  def flattenToOneLevel[Element: Eq](
      nestedMergeResults: MergeResult[Element]
  ): MergeResult[Element] =
    coalescing(nestedMergeResults.segments.flatMap {
      case Segment.Resolved(items) =>
        items.map {
          case SegmentItem.Leaf(element)       => of(element)
          case SegmentItem.Nested(mergeResult) => flattenToOneLevel(mergeResult)
        }.flatMap(_.segments)
      case Segment.Conflicted(
            baseItems,
            leftItems,
            rightItems
          ) =>
        def flattenWithinEnclosingConflict(
            segment: MergeResult.Segment[Element]
        ): Seq[Element] =
          segment match
            case Segment.Resolved(items) =>
              items.flatMap {
                case SegmentItem.Leaf(element)       => Seq(element)
                case SegmentItem.Nested(mergeResult) =>
                  flattenToOneLevel(mergeResult).segments.flatMap(
                    flattenWithinEnclosingConflict
                  )
              }
            case Segment.Conflicted(_, leftElements, rightElements) =>
              (leftElements ++ rightElements).flatMap {
                case SegmentItem.Leaf(element)       => Seq(element)
                case SegmentItem.Nested(mergeResult) =>
                  flattenToOneLevel(mergeResult).segments.flatMap(
                    flattenWithinEnclosingConflict
                  )
              }

        def flattenItems(
            items: Seq[SegmentItem[Element]]
        ): Seq[Element] =
          items.flatMap {
            case SegmentItem.Leaf(element)       => Seq(element)
            case SegmentItem.Nested(mergeResult) =>
              flattenToOneLevel(mergeResult).segments.flatMap(
                flattenWithinEnclosingConflict
              )
          }

        MergeResult.segmentFor(
          flattenItems(baseItems),
          flattenItems(leftItems),
          flattenItems(rightItems)
        )
    })

  private def coalescing[Element: Eq](
      segments: Seq[Segment[Element]]
  ): MergeResult[Element] =
    coalescingSegments(segments)

  private def coalescingSegments[Element: Eq](
      segments: Seq[Segment[Element]]
  ): MergeResult[Element] =
    segments.foldLeft(empty) {
      case (partialResult, Segment.Resolved(items)) =>
        partialResult.addResolvedItems(items)
      case (
            partialResult,
            Segment.Conflicted(baseItems, leftItems, rightItems)
          ) =>
        partialResult.addConflictedItems(baseItems, leftItems, rightItems)
    }

  private def coalescingItems[Element: Eq](
      segments: Seq[Segment[Element]]
  ): MergeResult[Element] =
    coalescingSegments(segments)

  def empty[Element: Eq]: MergeResult[Element] = MergeResult(
    IndexedSeq.empty
  )

  private def segmentFor[Element: Eq](
      baseElements: Seq[Element],
      leftElements: Seq[Element],
      rightElements: Seq[Element]
  ): Option[Segment[Element]] =
    segmentForItems(
      baseElements.map(SegmentItem.Leaf.apply),
      leftElements.map(SegmentItem.Leaf.apply),
      rightElements.map(SegmentItem.Leaf.apply)
    )

  private[core] def segmentForItems[Element: Eq](
      baseItems: Seq[SegmentItem[Element]],
      leftItems: Seq[SegmentItem[Element]],
      rightItems: Seq[SegmentItem[Element]]
  ): Option[Segment[Element]] =
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

  private def segmentFor[Element: Eq](
      elements: Seq[Element]
  ): Option[Segment[Element]] =
    segmentForItems(elements.map(SegmentItem.Leaf.apply))

  private[core] def segmentForItems[Element: Eq](
      items: Seq[SegmentItem[Element]]
  ): Option[Segment[Element]] =
    Option.unless(items.isEmpty)(Segment.Resolved(items))

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

  sealed trait Segment[Element]:
    def items: Seq[SegmentItem[Element]]

    def fuseWith(another: Segment[Element])(
        itemFusion: (SegmentItem[Element], SegmentItem[Element]) => Option[
          SegmentItem[Element]
        ]
    ): Option[Segment[Element]]

  object Segment:
    case class Resolved[Element: Eq](items: Seq[SegmentItem[Element]])
        extends Segment[Element]:
      require(items.nonEmpty)

      override def fuseWith(another: Segment[Element])(
          itemFusion: (SegmentItem[Element], SegmentItem[Element]) => Option[
            SegmentItem[Element]
          ]
      ): Option[Segment[Element]] = another match
        case Resolved(itemsOfAnother) =>
          for fusedItems <- fuseItems(items, itemsOfAnother)(itemFusion)
          yield Resolved(fusedItems)
        case _ => None

    // Don't bother representing different flavours of conflicts, namely left
    // edit / right edit, left deletion / right edit, left edit / right deletion
    // or left insertion / right insertion. These aren't modelled separately in
    // `MergeAlgebra`, and that is fine as it is for now.
    case class Conflicted[Element: Eq](
        // If this is empty, it represents a left insertion versus right
        // insertion conflict.
        baseItems: Seq[SegmentItem[Element]],
        // If this is empty, it represents a left deletion versus right edit
        // conflict.
        leftItems: Seq[SegmentItem[Element]],
        // If this is empty, it represents a left edit versus right deletion
        // conflict.
        rightItems: Seq[SegmentItem[Element]]
        // Otherwise, if all sides are populated, it represents a left edit
        // versus right edit conflict.
    ) extends Segment[Element]:
      require(
        !baseItems.corresponds(leftItems)(Eq.eqv) && !baseItems
          .corresponds(rightItems)(Eq.eqv) && !leftItems.corresponds(
          rightItems
        )(Eq.eqv)
      )

      override def items: Seq[SegmentItem[Element]] =
        baseItems ++ leftItems ++ rightItems

      override def fuseWith(another: Segment[Element])(
          itemFusion: (SegmentItem[Element], SegmentItem[Element]) => Option[
            SegmentItem[Element]
          ]
      ): Option[Segment[Element]] = another match
        case Conflicted(baseAnother, leftAnother, rightAnother) =>
          for
            baseFusion  <- fuseItems(baseItems, baseAnother)(itemFusion)
            leftFusion  <- fuseItems(leftItems, leftAnother)(itemFusion)
            rightFusion <- fuseItems(rightItems, rightAnother)(itemFusion)
          yield Conflicted(baseFusion, leftFusion, rightFusion)
        case _ => None
  end Segment

  given segmentOrdering[Element](using
      elementOrdering: Ordering[Element],
      elementEq: Eq[Element]
  ): Ordering[Segment[Element]] with
    override def compare(x: Segment[Element], y: Segment[Element]): Int =
      (x, y) match
        case (
              Segment.Resolved(itemsFromX),
              Segment.Resolved(itemsFromY)
            ) =>
          Ordering[Seq[SegmentItem[Element]]].compare(itemsFromX, itemsFromY)
        case (
              Segment.Conflicted(baseX, leftX, rightX),
              Segment.Conflicted(baseY, leftY, rightY)
            ) =>
          Ordering[(Seq[SegmentItem[Element]], Seq[SegmentItem[Element]], Seq[SegmentItem[Element]])]
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
              Segment.Resolved(itemsFromX),
              Segment.Resolved(itemsFromY)
            ) =>
          Eq[Seq[SegmentItem[Element]]].eqv(itemsFromX, itemsFromY)
        case (
              Segment.Conflicted(baseX, leftX, rightX),
              Segment.Conflicted(baseY, leftY, rightY)
            ) =>
          Eq[(Seq[SegmentItem[Element]], Seq[SegmentItem[Element]], Seq[SegmentItem[Element]])]
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
  def flatten: MergeResult[Element] =
    MergeResult(nestedMergeResults.segments.map {
      case MergeResult.Segment.Resolved(nestedResults) =>
        MergeResult.Segment.Resolved(nestedResults.map {
          case MergeResult.SegmentItem.Leaf(mr) =>
            MergeResult.SegmentItem.Nested(mr)
          case MergeResult.SegmentItem.Nested(doublyNested) =>
            MergeResult.SegmentItem.Nested(doublyNested.flatten)
        })
      case MergeResult.Segment.Conflicted(baseResults, leftResults, rightResults) =>
        MergeResult.Segment.Conflicted(
          baseResults.map {
            case MergeResult.SegmentItem.Leaf(mr) =>
              MergeResult.SegmentItem.Nested(mr)
            case MergeResult.SegmentItem.Nested(doublyNested) =>
              MergeResult.SegmentItem.Nested(doublyNested.flatten)
          },
          leftResults.map {
            case MergeResult.SegmentItem.Leaf(mr) =>
              MergeResult.SegmentItem.Nested(mr)
            case MergeResult.SegmentItem.Nested(doublyNested) =>
              MergeResult.SegmentItem.Nested(doublyNested.flatten)
          },
          rightResults.map {
            case MergeResult.SegmentItem.Leaf(mr) =>
              MergeResult.SegmentItem.Nested(mr)
            case MergeResult.SegmentItem.Nested(doublyNested) =>
              MergeResult.SegmentItem.Nested(doublyNested.flatten)
          }
        )
    })
end extension

object FullyMerged:
  def unapply[Element: Eq](
      result: MergeResult[Element]
  ): Option[Seq[Element]] =
    val flattened = MergeResult.flattenToOneLevel(result)
    Traverse[Seq].flatTraverse(flattened.segments) {
      case MergeResult.Segment.Resolved(items) =>
        Some(items.map {
          case MergeResult.SegmentItem.Leaf(element: Element) => element
          case MergeResult.SegmentItem.Nested(_)              =>
            throw new IllegalStateException("Unexpected nested segment after flattenToOneLevel")
        })
      case MergeResult.Segment.Conflicted(_, _, _) => None
    }
end FullyMerged

object MergedWithConflicts:
  def unapply[Element: Eq](
      result: MergeResult[Element]
  ): Option[(Seq[Element], Seq[Element], Seq[Element])] =
    val flattened = MergeResult.flattenToOneLevel(result)
    Option.when(flattened.segments.exists {
      case MergeResult.Segment.Conflicted(_, _, _) => true
      case MergeResult.Segment.Resolved(_)         => false
    })(
      (
        flattened.segments.flatMap {
          case MergeResult.Segment.Resolved(items) =>
            items.map {
              case MergeResult.SegmentItem.Leaf(element: Element) => element
              case MergeResult.SegmentItem.Nested(_)              =>
                throw new IllegalStateException("Unexpected nested segment after flattenToOneLevel")
            }
          case MergeResult.Segment.Conflicted(baseItems, _, _) =>
            baseItems.map {
              case MergeResult.SegmentItem.Leaf(element: Element) => element
              case MergeResult.SegmentItem.Nested(_)              =>
                throw new IllegalStateException("Unexpected nested segment after flattenToOneLevel")
            }
        },
        flattened.segments.flatMap {
          case MergeResult.Segment.Resolved(items) =>
            items.map {
              case MergeResult.SegmentItem.Leaf(element: Element) => element
              case MergeResult.SegmentItem.Nested(_)              =>
                throw new IllegalStateException("Unexpected nested segment after flattenToOneLevel")
            }
          case MergeResult.Segment.Conflicted(_, leftItems, _) =>
            leftItems.map {
              case MergeResult.SegmentItem.Leaf(element: Element) => element
              case MergeResult.SegmentItem.Nested(_)              =>
                throw new IllegalStateException("Unexpected nested segment after flattenToOneLevel")
            }
        },
        flattened.segments.flatMap {
          case MergeResult.Segment.Resolved(items) =>
            items.map {
              case MergeResult.SegmentItem.Leaf(element: Element) => element
              case MergeResult.SegmentItem.Nested(_)              =>
                throw new IllegalStateException("Unexpected nested segment after flattenToOneLevel")
            }
          case MergeResult.Segment.Conflicted(_, _, rightItems) =>
            rightItems.map {
              case MergeResult.SegmentItem.Leaf(element: Element) => element
              case MergeResult.SegmentItem.Nested(_)              =>
                throw new IllegalStateException("Unexpected nested segment after flattenToOneLevel")
            }
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
