package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.CoreMergeAlgebra.MultiSidedMergeResult
import com.sageserpent.kineticmerge.core.merge.MergeAlgebra

trait MergeResult[Content]:
  def transform[TransformedContent](
      transform: Content => TransformedContent
  )(using
      equality: Eq[? >: TransformedContent]
  ): MergeResult[TransformedContent]
end MergeResult

case class FullyMerged[Content](content: Content) extends MergeResult[Content]:
  override def transform[TransformedContent](
      transform: Content => TransformedContent
  )(using
      equality: Eq[? >: TransformedContent]
  ): MergeResult[TransformedContent] =
    FullyMerged(transform(content))
end FullyMerged

/** @param leftElements
  *   The left hand form of the merge. Has all the clean merges, plus the left
  *   side of the conflicts.
  * @param rightElements
  *   The right hand form of the merge. Has all the clean merges, plus the right
  *   side of the conflicts.
  * @tparam Content
  */
case class MergedWithConflicts[Content](
    leftElements: Content,
    rightElements: Content
) extends MergeResult[Content]:
  require(leftElements != rightElements)

  // The invariant guarantees this.

  override def transform[TransformedContent](
      transform: Content => TransformedContent
  )(using
      equality: Eq[? >: TransformedContent]
  ): MergeResult[TransformedContent] =
    val transformedLeftElements  = transform(leftElements)
    val transformedRightElements = transform(rightElements)

    // Just in case the conflict was resolved by the migrated changes...
    if equality.eqv(transformedLeftElements, transformedRightElements)
    then FullyMerged(transformedLeftElements)
    else
      MergedWithConflicts(
        transformedLeftElements,
        transformedRightElements
      )
    end if
  end transform
end MergedWithConflicts

object CoreMergeAlgebra:
  type MultiSidedMergeResult[Element] =
    MergeResult[IndexedSeq[MultiSided[Element]]]
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
