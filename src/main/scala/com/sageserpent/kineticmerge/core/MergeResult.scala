package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.merge.MergeAlgebra

object MergeResult:
  type Resolution[Element] = (Element, Element) => Element

  def mergeAlgebra[Element](
      resolution: Resolution[Element]
  ): MergeAlgebra[MergeResult, Element] =
    new MergeAlgebra[MergeResult, Element]:
      override def empty: MergeResult[Element] = FullyMerged(IndexedSeq.empty)

      override def preservation(
          result: MergeResult[Element],
          preservedElementOnLeft: Element,
          preservedElementOnRight: Element
      ): MergeResult[Element] =
        val resolved =
          resolution(preservedElementOnLeft, preservedElementOnRight)

        result match
          case FullyMerged(elements) =>
            FullyMerged(elements :+ resolved)
          case MergedWithConflicts(leftElements, rightElements) =>
            MergedWithConflicts(
              leftElements :+ resolved,
              rightElements :+ resolved
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
        val resolved = resolution(insertedElementOnLeft, insertedElementOnRight)

        result match
          case FullyMerged(elements) =>
            FullyMerged(elements :+ resolved)
          case MergedWithConflicts(leftElements, rightElements) =>
            MergedWithConflicts(
              leftElements :+ resolved,
              rightElements :+ resolved
            )
        end match
      end coincidentInsertion

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
          editedElement: Element,
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
          editedElement: Element,
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
        val resolvedElements = editElements.map(resolution.tupled)

        result match
          case FullyMerged(elements) =>
            FullyMerged(elements ++ resolvedElements)
          case MergedWithConflicts(leftElements, rightElements) =>
            MergedWithConflicts(
              leftElements ++ resolvedElements,
              rightElements ++ resolvedElements
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
end MergeResult

trait MergeResult[Element]

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
