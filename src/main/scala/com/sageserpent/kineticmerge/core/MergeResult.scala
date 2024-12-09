package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.CoreMergeAlgebra.{Merged, UnresolvedMergeResult}
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

object CoreMergeAlgebra:
  enum Merged[Element]:
    def resolveUsing(resolution: Resolution[Element]): Element =
      this match
        case Resolved(element) => element
        // NOTE: the following cases are performing double-dispatch on overloads
        // of `Resolution.apply`...
        case coincident: Coincident[Element] =>
          resolution.coincident(coincident.leftElement, coincident.rightElement)
        case preserved: Preserved[Element] =>
          resolution.preserved(
            preserved.baseElement,
            preserved.leftElement,
            preserved.rightElement
          )

    case Resolved(element: Element)
    case Coincident(leftElement: Element, rightElement: Element)
    case Preserved(
        baseElement: Element,
        leftElement: Element,
        rightElement: Element
    )
  end Merged

  type UnresolvedMergeResult[Element] = MergeResult[Merged[Element]]

  extension [Element: Eq](unresolvedMergeResult: UnresolvedMergeResult[Element])
    def resolveUsing(resolution: Resolution[Element]): MergeResult[Element] =
      unresolvedMergeResult.transformElementsEnMasse(
        _.map(_.resolveUsing(resolution))
      )
end CoreMergeAlgebra

class CoreMergeAlgebra[Element]
    extends MergeAlgebra[UnresolvedMergeResult, Element]:
  override def empty: UnresolvedMergeResult[Element] = FullyMerged(
    IndexedSeq.empty
  )

  override def preservation(
      result: UnresolvedMergeResult[Element],
      preservedBaseElement: Element,
      preservedElementOnLeft: Element,
      preservedElementOnRight: Element
  ): UnresolvedMergeResult[Element] =
    val preserved = Merged.Preserved(
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
      result: UnresolvedMergeResult[Element],
      insertedElement: Element
  ): UnresolvedMergeResult[Element] =
    val resolved = Merged.Resolved(insertedElement)

    result match
      case FullyMerged(elements) =>
        FullyMerged(elements :+ resolved)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements :+ resolved,
          rightElements :+ resolved
        )
    end match
  end leftInsertion

  override def rightInsertion(
      result: UnresolvedMergeResult[Element],
      insertedElement: Element
  ): UnresolvedMergeResult[Element] =
    val resolved = Merged.Resolved(insertedElement)

    result match
      case FullyMerged(elements) =>
        FullyMerged(elements :+ resolved)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements :+ resolved,
          rightElements :+ resolved
        )
    end match
  end rightInsertion

  override def coincidentInsertion(
      result: UnresolvedMergeResult[Element],
      insertedElementOnLeft: Element,
      insertedElementOnRight: Element
  ): UnresolvedMergeResult[Element] =
    val coincident =
      Merged.Coincident(insertedElementOnLeft, insertedElementOnRight)

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
      result: UnresolvedMergeResult[Element],
      deletedBaseElement: Element,
      deletedRightElement: Element
  ): UnresolvedMergeResult[Element] = result

  override def rightDeletion(
      result: UnresolvedMergeResult[Element],
      deletedBaseElement: Element,
      deletedLeftElement: Element
  ): UnresolvedMergeResult[Element] = result

  override def coincidentDeletion(
      result: UnresolvedMergeResult[Element],
      deletedElement: Element
  ): UnresolvedMergeResult[Element] = result

  override def leftEdit(
      result: UnresolvedMergeResult[Element],
      editedBaseElement: Element,
      editedRightElement: Element,
      editElements: IndexedSeq[Element]
  ): UnresolvedMergeResult[Element] =
    val resolved = editElements map Merged.Resolved.apply

    result match
      case FullyMerged(elements) =>
        FullyMerged(elements ++ resolved)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements ++ resolved,
          rightElements ++ resolved
        )
    end match
  end leftEdit

  override def rightEdit(
      result: UnresolvedMergeResult[Element],
      editedBaseElement: Element,
      editedLeftElement: Element,
      editElements: IndexedSeq[Element]
  ): UnresolvedMergeResult[Element] =
    val resolved = editElements map Merged.Resolved.apply

    result match
      case FullyMerged(elements) =>
        FullyMerged(elements ++ resolved)
      case MergedWithConflicts(leftElements, rightElements) =>
        MergedWithConflicts(
          leftElements ++ resolved,
          rightElements ++ resolved
        )
    end match
  end rightEdit

  override def coincidentEdit(
      result: UnresolvedMergeResult[Element],
      editedElement: Element,
      editElements: IndexedSeq[(Element, Element)]
  ): UnresolvedMergeResult[Element] =
    val coincident = editElements.map {
      case (leftEditElement, rightEditElement) =>
        Merged.Coincident(leftEditElement, rightEditElement)
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
      result: UnresolvedMergeResult[Element],
      editedElements: IndexedSeq[Element],
      leftEditElements: IndexedSeq[Element],
      rightEditElements: IndexedSeq[Element]
  ): UnresolvedMergeResult[Element] =
    val leftResolved  = leftEditElements map Merged.Resolved.apply
    val rightResolved = rightEditElements map Merged.Resolved.apply

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
