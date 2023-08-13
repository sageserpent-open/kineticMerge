package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.CodeMotionAnalysis.Match

object Merge:
  def of(
      longestCommonSubsequence: LongestCommonSubsequence[Section]
  )(matchFor: Section => Option[Match]): Either[Divergence.type, Result] = ???

  // TODO: "Something went wrong!" - "What was it?"
  case object Divergence

  enum Result:
    /** @return
      *   A map from moved sections in either the left or the right to their
      *   associated rewrites, which may be edits or outright deletion. If a
      *   section is from the base, or has not moved, or has no associated
      *   rewrite then there will be no entry for it in the map.
      * @note
      *   Deletion of a section is represented by an associated empty string.
      */
    def movedSectionRewrites: Map[Section, String] = ???

    case Merged(sections: IndexedSeq[Section])
    case Conflicted(
        leftSections: IndexedSeq[Section],
        rightSections: IndexedSeq[Section]
    )
  end Result

end Merge
