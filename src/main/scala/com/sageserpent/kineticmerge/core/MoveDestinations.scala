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
  *   Sources of the moves.
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
    coincident: collection.Set[(Element, Element)]
):
  require(sources.nonEmpty)

  require(
    left.nonEmpty || right.nonEmpty || coincident.nonEmpty
  )

  require(
    left.intersect(right).isEmpty && left.intersect(sources).isEmpty && right
      .intersect(sources)
      .isEmpty
  )

  def all: collection.Set[Element] = coincident.unzip.match
    case (leftHalves, rightHalves) =>
      left union right union leftHalves union rightHalves

  def mergeWith(another: MoveDestinations[Element]): MoveDestinations[Element] =
    MoveDestinations(
      sources = this.sources union another.sources,
      left = this.left union another.left,
      right = this.right union another.right,
      coincident = this.coincident union another.coincident
    )

  def description: String =
    // What to report?
    // 1. The unambiguous and non-divergent moves.
    // 2. The ambiguous and non-divergent moves.
    // 3. The unambiguous and divergent moves.
    // 4. The ambiguous and divergent moves.
    // 5. The unambiguous coincident insertions.
    // 6. The ambiguous coincident insertions.
    // 7. The unambiguous divergent insertions.
    // 8. The ambiguous divergent insertions.

    def elementSetAsText(
        sections: collection.Set[? <: (Element | (Element, Element))]
    ): String =
      require(sections.nonEmpty)

      sections.size match
        case 1 => pprintCustomised(sections.head).toString
        case _ =>
          s"${sections.map(pprintCustomised(_).toString).mkString(",\n")}"
      end match
    end elementSetAsText

    def destinationsAsText: String =
      (left.nonEmpty, right.nonEmpty, coincident.nonEmpty) match
        // NOTE: there is no case for `(false, false, false)` as that would
        // violate the invariant.
        case (false, true, false) => s"${elementSetAsText(right)}"
        case (false, false, true) => s"${elementSetAsText(coincident)}"
        case (false, true, true) =>
          s"${elementSetAsText(right)}\n\n${elementSetAsText(coincident)}"
        case (true, false, false) => s"${elementSetAsText(left)}"
        case (true, true, false) =>
          s"${elementSetAsText(left)}\n\n${elementSetAsText(right)}"
        case (true, false, true) =>
          s"${elementSetAsText(left)}\n\n${elementSetAsText(coincident)}"
        case (true, true, true) =>
          s"${elementSetAsText(left)}\n\n${elementSetAsText(right)}\n\n${elementSetAsText(coincident)}"

    (isAmbiguous, isDivergent) match
      case (false, false) =>
        s"Single move of:\n${elementSetAsText(sources)}\nto:\n$destinationsAsText."
      case (false, true) =>
        s"Divergent move of:\n${elementSetAsText(sources)}\nto:\n$destinationsAsText."
      case (true, false) =>
        s"Ambiguous moves of:\n${elementSetAsText(sources)}\nto:\n$destinationsAsText."
      case (true, true) =>
        s"Ambiguous divergent moves of:\n${elementSetAsText(sources)}\nto:\n$destinationsAsText."
    end match
  end description

  def isDivergent: Boolean =
    left.nonEmpty && right.nonEmpty

  def isAmbiguous: Boolean =
    1 < (left.size max right.size) + coincident.size
end MoveDestinations
