package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.ThreeWayDifference.Contribution

case class ThreeWayDifference(contributions: IndexedSeq[Contribution]):

end ThreeWayDifference

object ThreeWayDifference:
  enum Contribution:
    case Preservation(indexInBase: Int)
    case Deletion(indexInBase: Int)
    case LeftInsertion(indexInLeft: Int)
    case RightInsertion(indexInRight: Int)
    case InsertionConflict(indexInLeft: Int, indexInRight: Int)
  end Contribution

  def apply[Element](
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  ): ThreeWayDifference = ???
end ThreeWayDifference
