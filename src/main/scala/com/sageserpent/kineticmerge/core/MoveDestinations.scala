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
// TODO: right now, the description doesn't really care about the difference
// between left-, right- and coincident move destinations. Maybe all the
// destinations should be lumped together, with flags to indicate whether
// there are left-moves, right-moves and coincident moves?
case class MoveDestinations[Element](
    sources: collection.Set[Element],
    left: collection.Set[Element],
    right: collection.Set[Element],
    coincident: collection.Set[Element]
):
  require(
    left.nonEmpty || right.nonEmpty || coincident.nonEmpty
  )

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

    def sectionSetAsText(sections: collection.Set[Element]): String =
      require(sections.nonEmpty)

      sections.size match
        case 1 => pprintCustomised(sections.head).toString
        case _ =>
          s"${sections.map(pprintCustomised(_).toString).mkString(",\n")}"
      end match
    end sectionSetAsText

    def destinationsAsText: String =
      (left.nonEmpty, right.nonEmpty, coincident.nonEmpty) match
        // NOTE: there is no case for `(false, false, false)` as that would
        // violate the invariant.
        case (false, true, false) => s"${sectionSetAsText(right)}"
        case (false, false, true) => s"${sectionSetAsText(coincident)}"
        case (false, true, true) =>
          s"${sectionSetAsText(right)}\n\n${sectionSetAsText(coincident)}"
        case (true, false, false) => s"${sectionSetAsText(left)}"
        case (true, true, false) =>
          s"${sectionSetAsText(left)}\n\n${sectionSetAsText(right)}"
        case (true, false, true) =>
          s"${sectionSetAsText(left)}\n\n${sectionSetAsText(coincident)}"
        case (true, true, true) =>
          s"${sectionSetAsText(left)}\n\n${sectionSetAsText(right)}\n\n${sectionSetAsText(coincident)}"

    (isAmbiguous, isDivergent, isDegenerate) match
      case (false, false, false) =>
        s"Single move of:\n${sectionSetAsText(sources)}\nto:\n$destinationsAsText."
      case (false, true, false) =>
        s"Divergent move of:\n${sectionSetAsText(sources)}\nto:\n$destinationsAsText."
      case (false, false, true) => s"Insertion of:\n$destinationsAsText."
      case (false, true, true) =>
        s"Divergent insertion of:\n$destinationsAsText."
      case (true, false, false) =>
        s"Ambiguous moves of:\n${sectionSetAsText(sources)}\nto:\n$destinationsAsText."
      case (true, true, false) =>
        s"Ambiguous divergent moves of:\n${sectionSetAsText(sources)}\nto:\n$destinationsAsText."
      case (true, false, true) =>
        s"Ambiguous insertions of:\n$destinationsAsText."
      case (true, true, true) =>
        s"Ambiguous divergent insertions of:\n$destinationsAsText."
    end match
  end description

  def isDivergent: Boolean =
    left.nonEmpty && right.nonEmpty

  def isAmbiguous: Boolean =
    1 < (left.size max right.size) + coincident.size

  // TODO: this isn't tested, and the existing tests are insensitive to whether
  // `sources` is empty, a singleton set or has multiple entries. Should this
  // become a trait?
  def isDegenerate: Boolean = sources.isEmpty
end MoveDestinations
