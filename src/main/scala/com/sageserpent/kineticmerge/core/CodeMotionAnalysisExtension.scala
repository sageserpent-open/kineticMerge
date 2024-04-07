package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.merge.of as mergeOf
import com.typesafe.scalalogging.StrictLogging

import scala.collection.immutable.MultiDict

object CodeMotionAnalysisExtension extends StrictLogging:
  /** Add merging capability to a [[CodeMotionAnalysis]].
    *
    * Not sure exactly where this capability should be implemented - is it
    * really a core part of the API for [[CodeMotionAnalysis]]? Hence the
    * extension as a temporary measure.
    */
  extension [Path, Element](
      codeMotionAnalysis: CodeMotionAnalysis[Path, Element]
  )
    def merge(
        equality: Eq[Element]
    ): Map[Path, MergeResult[Element]] =
      def dominantsOf(
          section: Section[Element]
      ): collection.Set[Section[Element]] =
        codeMotionAnalysis
          .matchesFor(section)
          .map(_.dominantElement)

      /** This is most definitely *not* [[Section.equals]] - we want to compare
        * the underlying content of the dominant sections, as the sections are
        * expected to come from *different* sides. [[Section.equals]] is
        * expected to consider sections from different sides as unequal. <p>If
        * neither section is involved in a match, fall back to comparing the
        * contents; this is vital for comparing sections that would have been
        * part of a larger match if not for that match not achieving the
        * threshold size.
        */
      def sectionEqualityViaDominantsFallingBackToContentComparison(
          lhs: Section[Element],
          rhs: Section[Element]
      ): Boolean =
        val bothBelongToTheSameMatches =
          dominantsOf(lhs).intersect(dominantsOf(rhs)).nonEmpty

        bothBelongToTheSameMatches || {
          given Eq[Element] = equality

          Eq[Seq[Element]].eqv(lhs.content, rhs.content)
        }
      end sectionEqualityViaDominantsFallingBackToContentComparison

      val paths =
        codeMotionAnalysis.base.keySet ++ codeMotionAnalysis.left.keySet ++ codeMotionAnalysis.right.keySet

      val (
        mergeResultsByPath,
        changesPropagatedThroughMotion,
        excludedFromChangePropagation
      ) =
        paths.foldLeft(
          Map.empty[Path, MergeResult[Section[Element]]],
          Iterable.empty[
            (
                Section[Element],
                Option[
                  Section[Element]
                ]
            )
          ],
          Set.empty[Section[Element]]
        ) {
          case (
                (
                  mergeResultsByPath,
                  changesPropagatedThroughMotion,
                  excludedFromChangePropagation
                ),
                path
              ) =>
            val base  = codeMotionAnalysis.base.get(path).map(_.sections)
            val left  = codeMotionAnalysis.left.get(path).map(_.sections)
            val right = codeMotionAnalysis.right.get(path).map(_.sections)

            (base, left, right) match
              case (None, Some(leftSections), None) =>
                // File added only on the left; pass through as there is neither
                // anything to merge nor any sources of edits or deletions...
                (
                  mergeResultsByPath + (path -> FullyMerged(
                    leftSections
                  )),
                  changesPropagatedThroughMotion,
                  excludedFromChangePropagation
                )
              case (None, None, Some(rightSections)) =>
                // File added only on the right; pass through as there is
                // neither
                // anything to merge nor any sources of edits or deletions...
                (
                  mergeResultsByPath + (path -> FullyMerged(
                    rightSections
                  )),
                  changesPropagatedThroughMotion,
                  excludedFromChangePropagation
                )
              case (
                    optionalBaseSections,
                    optionalLeftSections,
                    optionalRightSections
                  ) =>
                // Mix of possibilities - the file may have been added on both
                // sides, or modified on either or both sides, or deleted on one
                // side and modified on the other, or deleted on one or both
                // sides. There is also an extraneous case where there is no
                // file on any of the sides, and another extraneous case where
                // the file is on all three sides but hasn't changed.

                // Whichever is the case, merge...
                val mergedSectionsResult
                    : MergeResultDetectingMotion[MergeResult, Section[
                      Element
                    ]] =
                  mergeOf(mergeAlgebra =
                    MergeResultDetectingMotion.mergeAlgebra(
                      matchesFor = codeMotionAnalysis.matchesFor,
                      coreMergeAlgebra = MergeResult.mergeAlgebra
                    )
                  )(
                    base = optionalBaseSections.getOrElse(IndexedSeq.empty),
                    left = optionalLeftSections.getOrElse(IndexedSeq.empty),
                    right = optionalRightSections.getOrElse(IndexedSeq.empty)
                  )(
                    equality =
                      sectionEqualityViaDominantsFallingBackToContentComparison,
                    elementSize = _.size
                  )

                (
                  mergeResultsByPath + (path -> mergedSectionsResult.coreMergeResult),
                  changesPropagatedThroughMotion ++ mergedSectionsResult.changesPropagatedThroughMotion,
                  excludedFromChangePropagation union mergedSectionsResult.excludedFromChangePropagation
                )
            end match
        }

      val vettedChangesPropagatedThroughMotion =
        excludedFromChangePropagation.foldLeft(
          MultiDict.from(changesPropagatedThroughMotion)
        )(_ removeKey _)

      def applyPropagatedChanges(
          path: Path,
          mergeResult: MergeResult[Section[Element]]
      ): (Path, MergeResult[Element]) =
        def elementsOf(section: Section[Element]): IndexedSeq[Element] =
          val propagatedChanges: collection.Set[Option[Section[Element]]] =
            vettedChangesPropagatedThroughMotion.get(section)
          val propagatedChange: Option[Option[Section[Element]]] =
            if 1 >= propagatedChanges.size then propagatedChanges.headOption
            else
              throw new RuntimeException(
                s"Multiple potential changes propagated to destination: $section, these are: $propagatedChanges"
              )

          // If we do have a propagated change, then there is no need to look
          // for the dominant - either the section was deleted or edited;
          // matched sections are not considered as edit candidates.
          propagatedChange.fold {
            val dominants = dominantsOf(section)

            (if dominants.isEmpty then section
             else
               // NASTY HACK: this is hokey, but essentially correct - if we
               // have ambiguous matches leading to multiple dominants, then
               // they're all just as good in terms of their content. So just
               // choose any one.
               dominants.head
            ).content
          }(
            _.fold(
              // Moved section was deleted...
              ifEmpty =
                logger.debug(
                  s"Applying propagated deletion to move destination: $section."
                )
                IndexedSeq.empty
            )(
              // Moved section was edited...
              { edit =>
                logger.debug(
                  s"Applying propagated edit into $edit to move destination: $section."
                )
                edit.content
              }
            )
          )

        end elementsOf

        path -> (mergeResult match
          case FullyMerged(elements) =>
            FullyMerged(elements =
              elements.flatMap(
                elementsOf
              )
            )
          case MergedWithConflicts(leftElements, rightElements) =>
            val leftElementsWithPropagatedChanges = leftElements.flatMap(
              elementsOf
            )
            val rightElementsWithPropagatedChanges = rightElements.flatMap(
              elementsOf
            )

            // Just in case the conflict is resolved by the propagated
            // changes...
            if leftElementsWithPropagatedChanges.corresponds(
                rightElementsWithPropagatedChanges
              )(equality.eqv)
            then FullyMerged(leftElementsWithPropagatedChanges)
            else
              MergedWithConflicts(
                leftElementsWithPropagatedChanges,
                rightElementsWithPropagatedChanges
              )
            end if
        )
      end applyPropagatedChanges

      mergeResultsByPath.map(applyPropagatedChanges)
    end merge

    // TODO: remove this method and cut over the tests to use `mergeOverPaths`;
    // this method is only used by older tests.
    @deprecated
    def mergeAt(path: Path)(
        equality: Eq[Element]
    ): MergeResult[Element] =
      merge(equality)(path)

  end extension
end CodeMotionAnalysisExtension
