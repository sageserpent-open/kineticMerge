package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.merge.MergeAlgebra

trait MergeResult[Element]:
  def isEmpty: Boolean

  def transformElementsEnMasse[TransformedElement](
      transform: IndexedSeq[Element] => IndexedSeq[TransformedElement]
  )(using equality: Eq[TransformedElement]): MergeResult[TransformedElement]
end MergeResult

case class FullyMerged[Element](elements: IndexedSeq[Element])
    extends MergeResult[Element]:
  override def isEmpty: Boolean = elements.isEmpty

  override def transformElementsEnMasse[TransformedElement](
      transform: IndexedSeq[Element] => IndexedSeq[TransformedElement]
  )(using equality: Eq[TransformedElement]): MergeResult[TransformedElement] =
    FullyMerged(transform(elements))
end FullyMerged

/** @param leftElements
  *   The left hand form of the merge. Has all the clean merges, plus the left
  *   side of the conflicts.
  * @param rightElements
  *   The right hand form of the merge. Has all the clean merges, plus the right
  *   side of the conflicts.
  * @tparam Element
  */
case class MergedWithConflicts[Element](
    leftElements: IndexedSeq[Element],
    rightElements: IndexedSeq[Element]
) extends MergeResult[Element]:
  require(leftElements != rightElements)

  override def isEmpty: Boolean = false // The invariant guarantees this.

  override def transformElementsEnMasse[TransformedElement](
      transform: IndexedSeq[Element] => IndexedSeq[TransformedElement]
  )(using equality: Eq[TransformedElement]): MergeResult[TransformedElement] =
    val transformedLeftElements  = transform(leftElements)
    val transformedRightElements = transform(rightElements)

    // Just in case the conflict was resolved by the migrated changes...
    if transformedLeftElements.corresponds(transformedRightElements)(
        equality.eqv
      )
    then FullyMerged(transformedLeftElements)
    else
      MergedWithConflicts(
        transformedLeftElements,
        transformedRightElements
      )
    end if
  end transformElementsEnMasse
end MergedWithConflicts

class CoreMergeAlgebra[Element](resolution: Resolution[Element])
    extends MergeAlgebra[MergeResult, Element]:
  override def empty: MergeResult[Element] = FullyMerged(IndexedSeq.empty)

  override def preservation(
      result: MergeResult[Element],
      preservedBaseElement: Element,
      preservedElementOnLeft: Element,
      preservedElementOnRight: Element
  ): MergeResult[Element] =
    val resolvedPreservedElement =
      resolution(
        Some(preservedBaseElement),
        preservedElementOnLeft,
        preservedElementOnRight
      )

    result match
      case FullyMerged(elements) =>
        FullyMerged(elements :+ resolvedPreservedElement)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements :+ resolvedPreservedElement,
          rightElements :+ resolvedPreservedElement
        )
    end match
  end preservation

  override def leftInsertion(
      result: MergeResult[Element],
      insertedElement: Element
  ): MergeResult[Element] =
    result match
      case FullyMerged(elements) =>
        FullyMerged(elements :+ insertedElement)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements :+ insertedElement,
          rightElements :+ insertedElement
        )

  override def rightInsertion(
      result: MergeResult[Element],
      insertedElement: Element
  ): MergeResult[Element] =
    result match
      case FullyMerged(elements) =>
        FullyMerged(elements :+ insertedElement)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements :+ insertedElement,
          rightElements :+ insertedElement
        )

  override def coincidentInsertion(
      result: MergeResult[Element],
      insertedElementOnLeft: Element,
      insertedElementOnRight: Element
  ): MergeResult[Element] =
    val resolvedInsertedElement =
      resolution(None, insertedElementOnLeft, insertedElementOnRight)

    result match
      case FullyMerged(elements) =>
        FullyMerged(elements :+ resolvedInsertedElement)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements :+ resolvedInsertedElement,
          rightElements :+ resolvedInsertedElement
        )
    end match
  end coincidentInsertion

  override def leftDeletion(
      result: MergeResult[Element],
      deletedBaseElement: Element,
      deletedRightElement: Element
  ): MergeResult[Element] = result

  override def rightDeletion(
      result: MergeResult[Element],
      deletedBaseElement: Element,
      deletedLeftElement: Element
  ): MergeResult[Element] = result

  override def coincidentDeletion(
      result: MergeResult[Element],
      deletedElement: Element
  ): MergeResult[Element] = result

  override def leftEdit(
      result: MergeResult[Element],
      editedBaseElement: Element,
      editedRightElement: Element,
      editElements: IndexedSeq[Element]
  ): MergeResult[Element] =
    result match
      case FullyMerged(elements) =>
        FullyMerged(elements ++ editElements)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements ++ editElements,
          rightElements ++ editElements
        )

  override def rightEdit(
      result: MergeResult[Element],
      editedBaseElement: Element,
      editedLeftElement: Element,
      editElements: IndexedSeq[Element]
  ): MergeResult[Element] =
    result match
      case FullyMerged(elements) =>
        FullyMerged(elements ++ editElements)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements ++ editElements,
          rightElements ++ editElements
        )

  override def coincidentEdit(
      result: MergeResult[Element],
      editedElement: Element,
      editElements: IndexedSeq[(Element, Element)]
  ): MergeResult[Element] =
    val resolvedEditElements = editElements.map {
      case (leftEditElement, rightEditElement) =>
        resolution(None, leftEditElement, rightEditElement)
    }

    result match
      case FullyMerged(elements) =>
        FullyMerged(elements ++ resolvedEditElements)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements ++ resolvedEditElements,
          rightElements ++ resolvedEditElements
        )
    end match
  end coincidentEdit

  override def conflict(
      result: MergeResult[Element],
      editedElements: IndexedSeq[Element],
      leftEditElements: IndexedSeq[Element],
      rightEditElements: IndexedSeq[Element]
  ): MergeResult[Element] =
    result match
      case FullyMerged(elements) =>
        MergedWithConflicts(
          elements ++ leftEditElements,
          elements ++ rightEditElements
        )
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements ++ leftEditElements,
          rightElements ++ rightEditElements
        )
end CoreMergeAlgebra
