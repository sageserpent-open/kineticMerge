package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.CoreMergeAlgebra.MultiSidedMergeResult

/** This embellishes [[CoreMergeAlgebra]], resolving conflicts that turn out to
  * be where an edit or deletion migration was picked up.
  *
  * @param migratedEditSuppressions
  *   Conflicts are vetted against these in case they are just the sites of edit
  *   or deletions that need to be migrated.
  * @tparam Element
  * @return
  */
class ConflictResolvingMergeAlgebra[Element](
    migratedEditSuppressions: Set[Element]
) extends CoreMergeAlgebra[Element]:
  override def preservation(
      result: MultiSidedMergeResult[Element],
      preservedBaseElement: Element,
      preservedElementOnLeft: Element,
      preservedElementOnRight: Element
  ): MultiSidedMergeResult[Element] =
    if migratedEditSuppressions.contains(preservedElementOnLeft) then
      super.leftDeletion(result, preservedBaseElement, preservedElementOnRight)
    else if migratedEditSuppressions.contains(preservedElementOnRight) then
      super.rightDeletion(result, preservedBaseElement, preservedElementOnLeft)
    else
      super.preservation(
        result,
        preservedBaseElement,
        preservedElementOnLeft,
        preservedElementOnRight
      )

  override def conflict(
      result: MultiSidedMergeResult[Element],
      editedElements: IndexedSeq[Element],
      leftEditElements: IndexedSeq[Element],
      rightEditElements: IndexedSeq[Element]
  ): MultiSidedMergeResult[Element] =
    val vettedLeftElements =
      leftEditElements.filterNot(migratedEditSuppressions.contains)
    val vettedRightElements =
      rightEditElements.filterNot(migratedEditSuppressions.contains)

    // NOTE: in what follows, we allow coincident deletions to be followed
    // by one-sided insertions; these would not be permitted by the
    // `merge.of`, but are OK as a post-processing step here.
    if vettedLeftElements.isEmpty || vettedRightElements.isEmpty then
      val withCoincidentDeletions =
        editedElements.foldLeft(result)((partialResult, editedElement) =>
          super.coincidentDeletion(
            partialResult,
            editedElement
          )
        )

      if vettedLeftElements.nonEmpty then
        vettedLeftElements.foldLeft(withCoincidentDeletions)(
          (partialResult, editedElement) =>
            super.leftInsertion(
              partialResult,
              editedElement
            )
        )
      else if vettedRightElements.nonEmpty then
        vettedRightElements.foldLeft(withCoincidentDeletions)(
          (partialResult, editedElement) =>
            super.rightInsertion(
              partialResult,
              editedElement
            )
        )
      else withCoincidentDeletions
      end if
    else
      super.conflict(
        result,
        editedElements,
        vettedLeftElements,
        vettedRightElements
      )
    end if

  end conflict
end ConflictResolvingMergeAlgebra
