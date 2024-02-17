package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.merge.MergeAlgebra

object MergeResult:
  def mergeAlgebra[Element]: MergeAlgebra[MergeResult, Element] =
    new MergeAlgebra[MergeResult, Element]:
      override def empty: MergeResult[Element] = FullyMerged(IndexedSeq.empty)

      override def preservation(
          result: MergeResult[Element],
          preservedElement: Element
      ): MergeResult[Element] =
        result match
          case FullyMerged(elements) =>
            FullyMerged(elements :+ preservedElement)
          case MergedWithConflicts(leftElements, rightElements) =>
            MergedWithConflicts(
              leftElements :+ preservedElement,
              rightElements :+ preservedElement
            )

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
              rightElements
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
              leftElements,
              rightElements :+ insertedElement
            )

      override def coincidentInsertion(
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

      override def leftInsertionConflictingWithRightInsertion(
          result: MergeResult[Element],
          leftInsertedElement: Element,
          rightInsertedElement: Element
      ): MergeResult[Element] =
        result match
          case FullyMerged(elements) =>
            MergedWithConflicts(
              elements :+ leftInsertedElement,
              elements :+ rightInsertedElement
            )
          case MergedWithConflicts(leftElements, rightElements) =>
            MergedWithConflicts(
              leftElements :+ leftInsertedElement,
              rightElements :+ rightInsertedElement
            )

      override def leftDeletion(
          result: MergeResult[Element],
          deletedElement: Element
      ): MergeResult[Element] = result

      override def rightDeletion(
          result: MergeResult[Element],
          deletedElement: Element
      ): MergeResult[Element] = result

      override def coincidentDeletion(
          result: MergeResult[Element],
          deletedElement: Element
      ): MergeResult[Element] = result

      override def leftEdit(
          result: MergeResult[Element],
          editedElement: Option[Element],
          editElement: Element
      ): MergeResult[Element] =
        result match
          case FullyMerged(elements) =>
            FullyMerged(elements :+ editElement)
          case MergedWithConflicts(leftElements, rightElements) =>
            MergedWithConflicts(
              leftElements :+ editElement,
              rightElements :+ editElement
            )

      override def rightEdit(
          result: MergeResult[Element],
          editedElement: Option[Element],
          editElement: Element
      ): MergeResult[Element] =
        result match
          case FullyMerged(elements) =>
            FullyMerged(elements :+ editElement)
          case MergedWithConflicts(leftElements, rightElements) =>
            MergedWithConflicts(
              leftElements :+ editElement,
              rightElements :+ editElement
            )

      override def coincidentEdit(
          result: MergeResult[Element],
          editedElement: Element,
          editElement: Element
      ): MergeResult[Element] =
        result match
          case FullyMerged(elements) =>
            FullyMerged(elements :+ editElement)
          case MergedWithConflicts(leftElements, rightElements) =>
            MergedWithConflicts(
              leftElements :+ editElement,
              rightElements :+ editElement
            )

      override def leftEditConflictingWithRightEdit(
          result: MergeResult[Element],
          editedElement: Element,
          leftEditElement: Element,
          rightEditElement: Element
      ): MergeResult[Element] =
        result match
          case FullyMerged(elements) =>
            MergedWithConflicts(
              elements :+ leftEditElement,
              elements :+ rightEditElement
            )
          case MergedWithConflicts(leftElements, rightElements) =>
            MergedWithConflicts(
              leftElements :+ leftEditElement,
              rightElements :+ rightEditElement
            )

      override def leftEditConflictingWithRightDeletion(
          result: MergeResult[Element],
          editedOrDeletedElement: Element,
          editElement: Element
      ): MergeResult[Element] =
        result match
          case FullyMerged(elements) =>
            MergedWithConflicts(
              leftElements = elements :+ editElement,
              rightElements = elements
            )
          case MergedWithConflicts(leftElements, rightElements) =>
            MergedWithConflicts(leftElements :+ editElement, rightElements)

      override def rightEditConflictingWithLeftDeletion(
          result: MergeResult[Element],
          editedOrDeletedElement: Element,
          editElement: Element
      ): MergeResult[Element] =
        result match
          case FullyMerged(elements) =>
            MergedWithConflicts(
              leftElements = elements,
              rightElements = elements :+ editElement
            )
          case MergedWithConflicts(leftElements, rightElements) =>
            MergedWithConflicts(leftElements, rightElements :+ editElement)

end MergeResult

sealed trait MergeResult[Element]

case class FullyMerged[Element](elements: IndexedSeq[Element])
    extends MergeResult[Element]

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
) extends MergeResult[Element]
