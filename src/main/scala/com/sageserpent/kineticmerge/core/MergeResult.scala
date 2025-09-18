package com.sageserpent.kineticmerge.core

import cats.{Eq, Traverse}
import com.sageserpent.kineticmerge.core.CoreMergeAlgebra.MultiSidedMergeResult
import com.sageserpent.kineticmerge.core.MergedWithConflicts.resolveIfNecessary
import com.sageserpent.kineticmerge.core.MultiSided.given
import com.sageserpent.kineticmerge.core.merge.MergeAlgebra

trait MergeResult[Element: Eq]:
  // TODO: remove these two...
  def transformElementsEnMasse[TransformedElement](
      transform: IndexedSeq[Element] => IndexedSeq[TransformedElement]
  )(using equality: Eq[TransformedElement]): MergeResult[TransformedElement]
  def flattenContent: IndexedSeq[Element]

  def map[Transformed: Eq](
      transform: Element => Transformed
  ): MergeResult[Transformed]

  def innerFlatMap[Transformed: Eq](
      transform: Element => IndexedSeq[Transformed]
  ): MergeResult[Transformed]

  def filter(predicate: Element => Boolean): MergeResult[Element]

  def filterNot(predicate: Element => Boolean): MergeResult[Element] = filter(
    predicate.andThen(!_)
  )

  def onEachSide[Transformed: Eq](
      transform: MergeResult.Side[Element] => MergeResult.Side[Transformed]
  ): MergeResult[Transformed]
end MergeResult

object MergeResult:
  // TODO: ensure that `Side` can only be constructed by `MergeResult` and
  // friends...
  case class Side[Element: Eq](elements: IndexedSeq[Element]):
    def innerFlatMapAccumulate[State, Transformed: Eq](initialState: State)(
        statefulTransform: (State, Element) => (State, IndexedSeq[Transformed])
    ): (State, Side[Transformed]) =
      val (finalState, clumps) =
        Traverse[Seq].mapAccumulate(initialState, elements)(statefulTransform)

      finalState -> Side(clumps.toIndexedSeq.flatten)
    end innerFlatMapAccumulate

    def append(elements: IndexedSeq[Element]): Side[Element] = Side(
      this.elements ++ elements
    )
  end Side
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

case class FullyMerged[Element: Eq](elements: IndexedSeq[Element])
    extends MergeResult[Element]:
  override def transformElementsEnMasse[TransformedElement](
      transform: IndexedSeq[Element] => IndexedSeq[TransformedElement]
  )(using equality: Eq[TransformedElement]): MergeResult[TransformedElement] =
    FullyMerged(transform(elements))

  override def flattenContent: IndexedSeq[Element] = elements

  override def map[Transformed: Eq](
      transform: Element => Transformed
  ): MergeResult[Transformed] = FullyMerged(elements.map(transform))

  override def innerFlatMap[Transformed: Eq](
      transform: Element => IndexedSeq[Transformed]
  ): MergeResult[Transformed] = FullyMerged(elements.flatMap(transform))

  override def filter(predicate: Element => Boolean): MergeResult[Element] =
    FullyMerged(elements.filter(predicate))

  override def onEachSide[Transformed: Eq](
      transform: MergeResult.Side[Element] => MergeResult.Side[Transformed]
  ): MergeResult[Transformed] =
    FullyMerged(transform(MergeResult.Side(elements)).elements)
end FullyMerged

object MergedWithConflicts:
  private def resolveIfNecessary[Element: Eq](
      leftElements: IndexedSeq[Element],
      rightElements: IndexedSeq[Element]
  ): MergeResult[Element] =
    if leftElements.corresponds(rightElements)(Eq.eqv) then
      FullyMerged(leftElements)
    else MergedWithConflicts(leftElements, rightElements)
end MergedWithConflicts

/** @param leftElements
  *   The left hand form of the merge. Has all the clean merges, plus the left
  *   side of the conflicts.
  * @param rightElements
  *   The right hand form of the merge. Has all the clean merges, plus the right
  *   side of the conflicts.
  * @tparam Element
  */
case class MergedWithConflicts[Element: Eq](
    leftElements: IndexedSeq[Element],
    rightElements: IndexedSeq[Element]
) extends MergeResult[Element]:
  require(!leftElements.corresponds(rightElements)(Eq.eqv))

  override def transformElementsEnMasse[TransformedElement](
      transform: IndexedSeq[Element] => IndexedSeq[TransformedElement]
  )(using equality: Eq[TransformedElement]): MergeResult[TransformedElement] =
    resolveIfNecessary(transform(leftElements), transform(rightElements))

  override def flattenContent: IndexedSeq[Element] =
    // TODO: should really merge these and then flatten out the conflicting
    // parts, rather than just plonking one entire sequence after the other.
    leftElements ++ rightElements

  override def map[Transformed: Eq](
      transform: Element => Transformed
  ): MergeResult[Transformed] = resolveIfNecessary(
    leftElements.map(transform),
    rightElements.map(transform)
  )

  override def innerFlatMap[Transformed: Eq](
      transform: Element => IndexedSeq[Transformed]
  ): MergeResult[Transformed] = resolveIfNecessary(
    leftElements.flatMap(transform),
    rightElements.flatMap(transform)
  )

  override def filter(predicate: Element => Boolean): MergeResult[Element] =
    resolveIfNecessary(
      leftElements.filter(predicate),
      rightElements.filter(predicate)
    )

  override def onEachSide[Transformed: Eq](
      transform: MergeResult.Side[Element] => MergeResult.Side[Transformed]
  ): MergeResult[Transformed] =
    val leftTransformedElements = transform(
      MergeResult.Side(leftElements)
    ).elements
    val rightTransformedElements = transform(
      MergeResult.Side(rightElements)
    ).elements

    if leftTransformedElements.corresponds(rightTransformedElements)(Eq.eqv)
    then FullyMerged(leftTransformedElements)
    else MergedWithConflicts(leftTransformedElements, rightTransformedElements)
    end if
  end onEachSide
end MergedWithConflicts

object CoreMergeAlgebra:
  type MultiSidedMergeResult[Element] = MergeResult[MultiSided[Element]]
end CoreMergeAlgebra

class CoreMergeAlgebra[Element: Eq]
    extends MergeAlgebra[MultiSidedMergeResult, Element]:
  override def empty: MultiSidedMergeResult[Element] = FullyMerged(
    IndexedSeq.empty
  )

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

    result match
      case FullyMerged(elements) =>
        FullyMerged(elements :+ preserved)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements :+ preserved,
          rightElements :+ preserved
        )
    end match
  end preservation

  override def leftInsertion(
      result: MultiSidedMergeResult[Element],
      insertedElement: Element
  ): MultiSidedMergeResult[Element] =
    val multiSided = MultiSided.Unique(insertedElement)

    result match
      case FullyMerged(elements) =>
        FullyMerged(elements :+ multiSided)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements :+ multiSided,
          rightElements :+ multiSided
        )
    end match
  end leftInsertion

  override def rightInsertion(
      result: MultiSidedMergeResult[Element],
      insertedElement: Element
  ): MultiSidedMergeResult[Element] =
    val multiSided = MultiSided.Unique(insertedElement)

    result match
      case FullyMerged(elements) =>
        FullyMerged(elements :+ multiSided)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements :+ multiSided,
          rightElements :+ multiSided
        )
    end match
  end rightInsertion

  override def coincidentInsertion(
      result: MultiSidedMergeResult[Element],
      insertedElementOnLeft: Element,
      insertedElementOnRight: Element
  ): MultiSidedMergeResult[Element] =
    val coincident =
      MultiSided.Coincident(insertedElementOnLeft, insertedElementOnRight)

    result match
      case FullyMerged(elements) =>
        FullyMerged(elements :+ coincident)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements :+ coincident,
          rightElements :+ coincident
        )
    end match
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

    result match
      case FullyMerged(elements) =>
        FullyMerged(elements ++ multiSided)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements ++ multiSided,
          rightElements ++ multiSided
        )
    end match
  end leftEdit

  override def rightEdit(
      result: MultiSidedMergeResult[Element],
      editedBaseElement: Element,
      editedLeftElement: Element,
      editElements: IndexedSeq[Element]
  ): MultiSidedMergeResult[Element] =
    val multiSided = editElements map MultiSided.Unique.apply

    result match
      case FullyMerged(elements) =>
        FullyMerged(elements ++ multiSided)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements ++ multiSided,
          rightElements ++ multiSided
        )
    end match
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

    result match
      case FullyMerged(elements) =>
        FullyMerged(elements ++ coincident)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements ++ coincident,
          rightElements ++ coincident
        )
    end match
  end coincidentEdit

  override def conflict(
      result: MultiSidedMergeResult[Element],
      editedElements: IndexedSeq[Element],
      leftEditElements: IndexedSeq[Element],
      rightEditElements: IndexedSeq[Element]
  ): MultiSidedMergeResult[Element] =
    val leftResolved  = leftEditElements map MultiSided.Unique.apply
    val rightResolved = rightEditElements map MultiSided.Unique.apply

    result match
      case FullyMerged(elements) =>
        MergedWithConflicts(
          elements ++ leftResolved,
          elements ++ rightResolved
        )
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements ++ leftResolved,
          rightElements ++ rightResolved
        )
    end match
  end conflict
end CoreMergeAlgebra
