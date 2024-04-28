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

  def mergeWith(another: MoveDestinations[Element]): MoveDestinations[Element] =
    MoveDestinations(
      left = this.left union another.left,
      right = this.right union another.right,
      coincident = this.coincident union another.coincident
    )
end MoveDestinations

class MoveDestinationsSupport[Element](
    matchesFor: Element => collection.Set[Match[Element]]
):
  extension (
      moveDestinationsByDominantSet: Map[collection.Set[
        Element
      ], MoveDestinations[Element]]
  )
    private def dominantsOf(element: Element): collection.Set[Element] =
      matchesFor(element).map(_.dominantElement)

    def leftMoveOf(
        element: Element
    ): Map[collection.Set[Element], MoveDestinations[Element]] =
      val dominants = dominantsOf(element)

      if dominants.nonEmpty then
        moveDestinationsByDominantSet.updatedWith(dominants) {
          case None =>
            Some(
              MoveDestinations(
                left = Set(element),
                right = Set.empty,
                coincident = Set.empty
              )
            )
          case Some(moveDestinations) =>
            Some(moveDestinations.focus(_.left).modify(_ + element))
        }
      else moveDestinationsByDominantSet
      end if
    end leftMoveOf

    def rightMoveOf(
        element: Element
    ): Map[collection.Set[Element], MoveDestinations[Element]] =
      val dominants = dominantsOf(element)

      if dominants.nonEmpty then
        moveDestinationsByDominantSet.updatedWith(dominants) {
          case None =>
            Some(
              MoveDestinations(
                left = Set.empty,
                right = Set(element),
                coincident = Set.empty
              )
            )
          case Some(moveDestinations) =>
            Some(moveDestinations.focus(_.right).modify(_ + element))
        }
      else moveDestinationsByDominantSet
      end if
    end rightMoveOf

    def coincidentMoveOf(
        element: Element
    ): Map[collection.Set[Element], MoveDestinations[Element]] =
      val dominants = dominantsOf(element)

      if dominants.nonEmpty then
        moveDestinationsByDominantSet.updatedWith(dominants) {
          case None =>
            Some(
              MoveDestinations(
                left = Set.empty,
                right = Set.empty,
                coincident = Set(element)
              )
            )
          case Some(moveDestinations) =>
            Some(moveDestinations.focus(_.coincident).modify(_ + element))
        }
      else moveDestinationsByDominantSet
      end if
    end coincidentMoveOf
  end extension
end MoveDestinationsSupport
