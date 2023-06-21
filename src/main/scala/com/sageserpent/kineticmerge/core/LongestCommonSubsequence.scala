package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Contribution

case class LongestCommonSubsequence(
    base: IndexedSeq[Contribution],
    left: IndexedSeq[Contribution],
    right: IndexedSeq[Contribution]
):

end LongestCommonSubsequence

object LongestCommonSubsequence:
  /** NOTE: the index is taken in the context of the original base, left and
    * right elements.
    */
  enum Contribution:
    case Common(
        indexInContributor: Int
    ) // The indexed element belongs to the longest common subsequence across the base, left and right.
    case Difference(
        indexInContributor: Int
    ) // The indexed element has been added with respect to the longest common subsequence across the base, left and right.
  end Contribution

  def of[Element](
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  )(equality: Eq[Element]): LongestCommonSubsequence = ???
end LongestCommonSubsequence
