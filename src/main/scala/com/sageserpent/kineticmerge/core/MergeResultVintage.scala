package com.sageserpent.kineticmerge.core

import cats.{Eq, Traverse}
import com.sageserpent.kineticmerge.core.MergedWithConflictsVintage.resolveIfNecessary

trait MergeResultVintage[Element: Eq]:
  // TODO: remove this...
  def flattenContent: Seq[Element]

  def map[Transformed: Eq](
      transform: Element => Transformed
  ): MergeResultVintage[Transformed]

  def innerFlatMap[Transformed: Eq](
      transform: Element => Seq[Transformed]
  ): MergeResultVintage[Transformed]

  def filter(predicate: Element => Boolean): MergeResultVintage[Element]

  def filterNot(predicate: Element => Boolean): MergeResultVintage[Element] =
    filter(
      predicate.andThen(!_)
    )

  def onEachSide[Transformed: Eq](
      transform: MergeResultVintage.Side[Element] => MergeResultVintage.Side[
        Transformed
      ]
  ): MergeResultVintage[Transformed]
end MergeResultVintage

object MergeResultVintage:
  // TODO: ensure that `Side` can only be constructed by `MergeResultVintage`
  // and
  // friends...
  case class Side[Element: Eq](elements: Seq[Element]):
    def innerFlatMapAccumulate[State, Transformed: Eq](initialState: State)(
        statefulTransform: (State, Element) => (State, Seq[Transformed])
    ): (State, Side[Transformed]) =
      val (finalState, clumps) =
        Traverse[Seq].mapAccumulate(initialState, elements)(statefulTransform)

      finalState -> Side(clumps.flatten)
    end innerFlatMapAccumulate

    def append(elements: Seq[Element]): Side[Element] = Side(
      this.elements ++ elements
    )
  end Side
end MergeResultVintage

case class FullyMergedVintage[Element: Eq](elements: Seq[Element])
    extends MergeResultVintage[Element]:
  override def flattenContent: Seq[Element] = elements

  override def map[Transformed: Eq](
      transform: Element => Transformed
  ): MergeResultVintage[Transformed] = FullyMergedVintage(
    elements.map(transform)
  )

  override def innerFlatMap[Transformed: Eq](
      transform: Element => Seq[Transformed]
  ): MergeResultVintage[Transformed] = FullyMergedVintage(
    elements.flatMap(transform)
  )

  override def filter(
      predicate: Element => Boolean
  ): MergeResultVintage[Element] =
    FullyMergedVintage(elements.filter(predicate))

  override def onEachSide[Transformed: Eq](
      transform: MergeResultVintage.Side[Element] => MergeResultVintage.Side[
        Transformed
      ]
  ): MergeResultVintage[Transformed] =
    FullyMergedVintage(transform(MergeResultVintage.Side(elements)).elements)
end FullyMergedVintage

object MergedWithConflictsVintage:
  private def resolveIfNecessary[Element: Eq](
      leftElements: Seq[Element],
      rightElements: Seq[Element]
  ): MergeResultVintage[Element] =
    if leftElements.corresponds(rightElements)(Eq.eqv) then
      FullyMergedVintage(leftElements)
    else MergedWithConflictsVintage(leftElements, rightElements)
end MergedWithConflictsVintage

/** @param leftElements
  *   The left hand form of the merge. Has all the clean merges, plus the left
  *   side of the conflicts.
  * @param rightElements
  *   The right hand form of the merge. Has all the clean merges, plus the right
  *   side of the conflicts.
  * @tparam Element
  */
case class MergedWithConflictsVintage[Element: Eq](
    leftElements: Seq[Element],
    rightElements: Seq[Element]
) extends MergeResultVintage[Element]:
  require(!leftElements.corresponds(rightElements)(Eq.eqv))

  override def flattenContent: Seq[Element] =
    // TODO: should really merge these and then flatten out the conflicting
    // parts, rather than just plonking one entire sequence after the other.
    leftElements ++ rightElements

  override def map[Transformed: Eq](
      transform: Element => Transformed
  ): MergeResultVintage[Transformed] = resolveIfNecessary(
    leftElements.map(transform),
    rightElements.map(transform)
  )

  override def innerFlatMap[Transformed: Eq](
      transform: Element => Seq[Transformed]
  ): MergeResultVintage[Transformed] = resolveIfNecessary(
    leftElements.flatMap(transform),
    rightElements.flatMap(transform)
  )

  override def filter(
      predicate: Element => Boolean
  ): MergeResultVintage[Element] =
    resolveIfNecessary(
      leftElements.filter(predicate),
      rightElements.filter(predicate)
    )

  override def onEachSide[Transformed: Eq](
      transform: MergeResultVintage.Side[Element] => MergeResultVintage.Side[
        Transformed
      ]
  ): MergeResultVintage[Transformed] =
    val leftTransformedElements = transform(
      MergeResultVintage.Side(leftElements)
    ).elements
    val rightTransformedElements = transform(
      MergeResultVintage.Side(rightElements)
    ).elements

    if leftTransformedElements.corresponds(rightTransformedElements)(Eq.eqv)
    then FullyMergedVintage(leftTransformedElements)
    else
      MergedWithConflictsVintage(
        leftTransformedElements,
        rightTransformedElements
      )
    end if
  end onEachSide
end MergedWithConflictsVintage
