package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.Merge.Location

object Merge:
  def of[Element](
      longestCommonSubsequence: LongestCommonSubsequence[Element]
  ): Merge[Element] = ???

  /** Represents the location of an element within the merge.
    */
  case class Location(runIndex: Int, locationInRun: LocationInRun):
  end Location

  enum LocationInRun:
    case Merged(index: Int)
    case ConflictLeftSide(index: Int)
    case ConflictRightSide(index: Int)
  end LocationInRun

end Merge

case class Merge[Element](
    // An element may be present at more than one location in the merge,
    // regardless of whether the merge is clean or has conflicts. In the
    // wider scheme of things, this represents a special case of divergence
    // where a section moves to different places within the same file -
    // possibly even to both sides of the same conflict.
    elementLocations: Map[Element, Set[Location]],
    // If there are no conflicts, this gives the sequence of merged elements.
    mergedElements: Option[Seq[Element]]
):

end Merge
