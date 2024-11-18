package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.merge.MergeAlgebra
import com.typesafe.scalalogging.StrictLogging
import monocle.syntax.all.*

/** @param coreMergeResult
  *   What is says on the tin: a simpler merge result that is delegated to by
  *   the operations implemented in
  *   [[MatchesContext.MergeResultDetectingMotion.mergeAlgebra]].
  * @tparam CoreResult
  * @tparam Element
  */
case class SecondPassMergeResult[CoreResult[_], Element](
    coreMergeResult: CoreResult[Element]
)

object SecondPassMergeResult extends StrictLogging:
  private type MergeResultDetectingMotionType[CoreResult[_]] =
    [Element] =>> SecondPassMergeResult[CoreResult, Element]

  def mergeAlgebra[CoreResult[_], Element](
      coreMergeAlgebra: merge.MergeAlgebra[CoreResult, Element],
      migratedEditSuppressions: Set[Element]
  ): MergeAlgebra[MergeResultDetectingMotionType[CoreResult], Element] =
    type ConfiguredMergeResultDetectingMotion[Element] =
      MergeResultDetectingMotionType[CoreResult][Element]

    new MergeAlgebra[MergeResultDetectingMotionType[CoreResult], Element]:
      override def empty: ConfiguredMergeResultDetectingMotion[Element] =
        SecondPassMergeResult(
          coreMergeResult = coreMergeAlgebra.empty
        )

      override def preservation(
          result: MergeResultDetectingMotionType[CoreResult][Element],
          preservedBaseElement: Element,
          preservedElementOnLeft: Element,
          preservedElementOnRight: Element
      ): ConfiguredMergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(
          coreMergeAlgebra.preservation(
            _,
            preservedBaseElement,
            preservedElementOnLeft,
            preservedElementOnRight
          )
        )

      override def leftInsertion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          insertedElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.coreMergeResult)
          .modify(coreMergeAlgebra.leftInsertion(_, insertedElement))
      end leftInsertion

      override def rightInsertion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          insertedElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.coreMergeResult)
          .modify(coreMergeAlgebra.rightInsertion(_, insertedElement))

      override def coincidentInsertion(
          result: MergeResultDetectingMotionType[CoreResult][Element],
          insertedElementOnLeft: Element,
          insertedElementOnRight: Element
      ): ConfiguredMergeResultDetectingMotion[Element] = result
        .focus(_.coreMergeResult)
        .modify(
          coreMergeAlgebra.coincidentInsertion(
            _,
            insertedElementOnLeft,
            insertedElementOnRight
          )
        )

      override def leftDeletion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          deletedBaseElement: Element,
          deletedRightElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.coreMergeResult)
          .modify(
            coreMergeAlgebra
              .leftDeletion(_, deletedBaseElement, deletedRightElement)
          )
      end leftDeletion

      override def rightDeletion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          deletedBaseElement: Element,
          deletedLeftElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.coreMergeResult)
          .modify(
            coreMergeAlgebra
              .rightDeletion(_, deletedBaseElement, deletedLeftElement)
          )
      end rightDeletion

      override def coincidentDeletion(
          result: ConfiguredMergeResultDetectingMotion[Element],
          deletedElement: Element
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.coreMergeResult)
          .modify(coreMergeAlgebra.coincidentDeletion(_, deletedElement))
      end coincidentDeletion

      override def leftEdit(
          result: ConfiguredMergeResultDetectingMotion[Element],
          editedBaseElement: Element,
          editedRightElement: Element,
          editElements: IndexedSeq[Element]
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.coreMergeResult)
          .modify(
            coreMergeAlgebra
              .leftEdit(
                _,
                editedBaseElement,
                editedRightElement,
                editElements
              )
          )
      end leftEdit

      override def rightEdit(
          result: ConfiguredMergeResultDetectingMotion[Element],
          editedBaseElement: Element,
          editedLeftElement: Element,
          editElements: IndexedSeq[Element]
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.coreMergeResult)
          .modify(
            coreMergeAlgebra
              .rightEdit(
                _,
                editedBaseElement,
                editedLeftElement,
                editElements
              )
          )
      end rightEdit

      override def coincidentEdit(
          result: ConfiguredMergeResultDetectingMotion[Element],
          editedElement: Element,
          editElements: IndexedSeq[(Element, Element)]
      ): ConfiguredMergeResultDetectingMotion[Element] =
        result
          .focus(_.coreMergeResult)
          .modify(
            coreMergeAlgebra.coincidentEdit(_, editedElement, editElements)
          )
      end coincidentEdit

      override def conflict(
          result: ConfiguredMergeResultDetectingMotion[Element],
          editedElements: IndexedSeq[Element],
          leftEditElements: IndexedSeq[Element],
          rightEditElements: IndexedSeq[Element]
      ): ConfiguredMergeResultDetectingMotion[Element] =
        val vettedLeftElements =
          leftEditElements.filterNot(migratedEditSuppressions.contains)
        val vettedRightElements =
          rightEditElements.filterNot(migratedEditSuppressions.contains)

        if vettedLeftElements.isEmpty || vettedRightElements.isEmpty then
          val withCoincidentDeletions =
            editedElements.foldLeft(result)((partialResult, editedElement) =>
              partialResult
                .focus(_.coreMergeResult)
                .modify(
                  coreMergeAlgebra.coincidentDeletion(
                    _,
                    editedElement
                  )
                )
            )

          if vettedLeftElements.nonEmpty then
            vettedLeftElements.foldLeft(withCoincidentDeletions)(
              (partialResult, editedElement) =>
                partialResult
                  .focus(_.coreMergeResult)
                  .modify(
                    coreMergeAlgebra.leftInsertion(
                      _,
                      editedElement
                    )
                  )
            )
          else if vettedRightElements.nonEmpty then
            vettedRightElements.foldLeft(withCoincidentDeletions)(
              (partialResult, editedElement) =>
                partialResult
                  .focus(_.coreMergeResult)
                  .modify(
                    coreMergeAlgebra.rightInsertion(
                      _,
                      editedElement
                    )
                  )
            )
          else withCoincidentDeletions
          end if
        else
          result
            .focus(_.coreMergeResult)
            .modify(
              coreMergeAlgebra.conflict(
                _,
                editedElements,
                leftEditElements,
                rightEditElements
              )
            )
        end if

      end conflict
    end new
  end mergeAlgebra
end SecondPassMergeResult
