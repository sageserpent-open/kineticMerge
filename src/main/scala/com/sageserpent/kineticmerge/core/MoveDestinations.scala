package com.sageserpent.kineticmerge.core

import monocle.syntax.all.*

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
  * @param sources
  *   Sources of the moves - may be empty in which case the move is degenerate
  *   and models matching *insertions* on the left and right hand sides of the
  *   merge.
  * @param left
  *   Destinations on the left hand side of the merge.
  * @param right
  *   Destinations on the right hand side of the merge.
  * @param coincident
  *   Destinations that coincide on the left and right hand sides of the merge.
  * @tparam Element
  */
case class MoveDestinations[Element](
    sources: collection.Set[Element],
    left: collection.Set[Element],
    right: collection.Set[Element],
    coincident: collection.Set[Element]
):
  require(
    left.nonEmpty || right.nonEmpty || coincident.nonEmpty
  )

  def isDivergent: Boolean =
    left.nonEmpty && right.nonEmpty

  def isAmbiguous: Boolean =
    1 < (left.size max right.size) + coincident.size

  def isDegenerate: Boolean = sources.isEmpty

  def mergeWith(another: MoveDestinations[Element]): MoveDestinations[Element] =
    MoveDestinations(
      sources = this.sources union another.sources,
      left = this.left union another.left,
      right = this.right union another.right,
      coincident = this.coincident union another.coincident
    )
end MoveDestinations
