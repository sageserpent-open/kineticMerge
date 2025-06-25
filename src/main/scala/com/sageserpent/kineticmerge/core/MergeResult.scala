package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.CoreMergeAlgebra.{
  MultiSided,
  MultiSidedMergeResult
}
import com.sageserpent.kineticmerge.core.merge.MergeAlgebra

trait MergeResult[Element]:
  def isEmpty: Boolean

  def transformElementsEnMasse[TransformedElement](
      transform: IndexedSeq[Element] => IndexedSeq[TransformedElement]
  )(using equality: Eq[TransformedElement]): MergeResult[TransformedElement]

  /** Yields content in a form suitable for being potentially nested within
    * either side of a larger [[MergedWithConflicts]]. This is necessary because
    * Git doesn't model nested conflicts, where one side of a conflict can
    * contain a smaller conflict. <p>This occurs because splicing can generate
    * such nested conflicts housed within a larger conflict between move
    * destination anchors on one side (with the splice) and some other
    * conflicting content on the opposite side to the anchors.
    * @return
    *   The elements as single [[IndexedSeq]].
    * @see
    *   https://github.com/sageserpent-open/kineticMerge/issues/160
    */
  def flattenContent: IndexedSeq[Element]
end MergeResult

case class FullyMerged[Element](elements: IndexedSeq[Element])
    extends MergeResult[Element]:
  override def isEmpty: Boolean = elements.isEmpty

  override def transformElementsEnMasse[TransformedElement](
      transform: IndexedSeq[Element] => IndexedSeq[TransformedElement]
  )(using equality: Eq[TransformedElement]): MergeResult[TransformedElement] =
    FullyMerged(transform(elements))

  override def flattenContent: IndexedSeq[Element] = elements
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

  override def flattenContent: IndexedSeq[Element] =
    leftElements ++ rightElements
end MergedWithConflicts

object CoreMergeAlgebra:
  type MultiSidedMergeResult[Element] = MergeResult[MultiSided[Element]]

  enum MultiSided[Element: Eq]:
    this match
      case Unique(element)                       => ()
      case Coincident(leftElement, rightElement) =>
        require(Eq[Element].eqv(leftElement, rightElement))
      case Preserved(baseElement, leftElement, rightElement) =>
        Eq[Element].eqv(baseElement, leftElement)
        Eq[Element].eqv(baseElement, rightElement)
    end match

    def resolveUsing(resolution: Resolution[Element]): Element =
      this match
        case Unique(element) => element
        // NOTE: the following cases are performing double-dispatch on overloads
        // of `Resolution.apply`...
        case Coincident(leftElement, rightElement) =>
          resolution.coincident(leftElement, rightElement)
        case Preserved(baseElement, leftElement, rightElement) =>
          resolution.preserved(baseElement, leftElement, rightElement)

    case Unique(element: Element)(using equality: Eq[Element])
    case Coincident(leftElement: Element, rightElement: Element)(using
        equality: Eq[Element]
    )
    case Preserved(
        baseElement: Element,
        leftElement: Element,
        rightElement: Element
    )(using equality: Eq[Element])
  end MultiSided
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
