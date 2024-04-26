package com.sageserpent.kineticmerge.core

/** This models the various possibilities for a move: <p>1. A single move to the
  * left. Neither divergent nor ambiguous. <p>2. A single move to the right.
  * Neither divergent nor ambiguous. <p>3. Ambiguous moves to the left. Not
  * divergent. <p>4. Ambiguous moves to the right. Not divergent. <p>5. A single
  * coincident move to both the left and right. Neither divergent nor ambiguous.
  * <p>6. Ambiguous coincident moves to both the left and right. Not divergent.
  * <p>7. A mixture of 1 or 3 with 5 or 6, or a mixture of 2 or 4 with 5 or 6.
  * Not divergent, but ambiguous. <p>8. A single move to the left and a single
  * move to the right. Divergent, but not ambiguous. <p>9. All other
  * possibilities. Divergent and ambiguous.
  *
  * @param left
  * @param right
  * @param coincident
  * @tparam Element
  */
case class MoveDestinations[Element](
    left: Set[Element],
    right: Set[Element],
    coincident: Set[Element]
):
  require(left.nonEmpty || right.nonEmpty || coincident.nonEmpty)

  def isDivergent: Boolean = left.nonEmpty && right.nonEmpty

  def isAmbiguous: Boolean = 1 < (left.size max right.size) + coincident.size
end MoveDestinations
