package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.merge.*
import com.typesafe.scalalogging.StrictLogging

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
    def mergeAt(path: Path)(
        equality: Eq[Element]
    ): Either[Divergence.type, MergeResult[Element]] =
      // TODO: amongst other things, need to convert the sections to underlying
      // content, but irrespective of what side contributed the section. What
      // about using the match to get the dominant section?

      // The base contribution is optional.
      val baseSections: IndexedSeq[Section[Element]] =
        codeMotionAnalysis.base
          .get(path)
          .fold(ifEmpty = Vector.empty)(_.sections)

      // For now, the left and right contributions are mandatory - we are
      // merging changes made in parallel on the same path, not handling
      // addition or deletion.
      val leftSections  = codeMotionAnalysis.left(path).sections
      val rightSections = codeMotionAnalysis.right(path).sections

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

      val mergedSectionsResult: Either[
        merge.Divergence.type,
        MergeResultDetectingMotion[MergeResult, Section[Element]]
      ] =
        merge.of(mergeAlgebra =
          MergeResultDetectingMotion.mergeAlgebra(
            matchesFor = codeMotionAnalysis.matchesFor,
            coreMergeAlgebra = MergeResult.mergeAlgebra
          )
        )(
          base = baseSections,
          left = leftSections,
          right = rightSections
        )(equality = sectionEqualityViaDominantsFallingBackToContentComparison)

      def elementsOf(
          changesPropagatedThroughMotion: Map[Section[Element], Option[
            Section[Element]
          ]]
      )(section: Section[Element]): IndexedSeq[Element] =
        val propagatedChange: Option[Option[Section[Element]]] =
          changesPropagatedThroughMotion.get(section)

        // If we do have a propagated change, then there is no need to look for
        // the dominant - either the section was deleted or edited; matched
        // sections are not considered as edit candidates.
        propagatedChange.fold {
          val dominants = dominantsOf(section)

          (if dominants.isEmpty then section
           else
             // NASTY HACK: this is hokey, but essentially correct - if we have
             // ambiguous matches leading to multiple dominants, then they're
             // all just as good in terms of their content. So just choose any
             // one.
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

      mergedSectionsResult.map(mergeResultDetectingMotion =>
        mergeResultDetectingMotion.coreMergeResult match
          case FullyMerged(elements) =>
            FullyMerged(elements =
              elements.flatMap(
                elementsOf(
                  mergeResultDetectingMotion.changesPropagatedThroughMotion
                )
              )
            )
          case MergedWithConflicts(leftElements, rightElements) =>
            MergedWithConflicts(
              leftElements = leftElements.flatMap(
                elementsOf(
                  mergeResultDetectingMotion.changesPropagatedThroughMotion
                )
              ),
              rightElements = rightElements.flatMap(
                elementsOf(
                  mergeResultDetectingMotion.changesPropagatedThroughMotion
                )
              )
            )
      )
end CodeMotionAnalysisExtension
