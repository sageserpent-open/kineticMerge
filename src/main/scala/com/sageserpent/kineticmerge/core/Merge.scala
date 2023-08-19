package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.CodeMotionAnalysis.Match

object Merge:
  def of(
      base: IndexedSeq[Section],
      left: IndexedSeq[Section],
      right: IndexedSeq[Section]
  )(matchFor: Section => Option[Match]): Either[Divergence.type, Result] = ???

  // TODO: "Something went wrong!" - "What was it?"
  case object Divergence

  enum Result:
    /** @return
      *   A map from moved sections in either the left or the right to their
      *   associated rewrites, which may represent edits or outright deletions.
      *   If a section is from the base, or has not moved, or has no associated
      *   rewrite then there will be no entry for it in the map.
      * @note
      *   A section serving as an edit rewrite is wrapped in a [[Some]]
      *   instance. A deletion rewrite of a section is represented by an
      *   associated [[None]] instance.
      */
    def movedSectionRewrites: Map[Section, Option[Section]] = ???

    case FullyMerged(sections: IndexedSeq[Section])
    case MergedWithConflicts(
        leftSections: IndexedSeq[Section],
        rightSections: IndexedSeq[Section]
    )
  end Result

end Merge
