package com.sageserpent.kineticmerge.core

import cats.Traverse
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.{DynamicTests, given}
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.MergeResultTest.nonEmptyClumps
import org.junit.jupiter.api.TestFactory

class MergeResultTest:
  // Whatever is done to a side via mapping, filtering or flat-mapping should be
  // equivalent to the same operation applied to a merge result.

  @TestFactory
  def addingOnlyResolvedElementsYieldsThemViaTheFullyMergedPattern()
      : DynamicTests =
    (for
      elements <- trialsApi.integers(0, 20).lists
      clumps   <- elements.nonEmptyClumps
    yield (elements, clumps))
      .withLimit(1000)
      .dynamicTests { (elements, clumps) =>
        assert(clumps.forall(_.nonEmpty))

        val mergeResult = clumps.foldLeft(MergeResult.empty[Int]) {
          case (partialResult, List(singleton)) =>
            partialResult.addResolved(singleton)
          case (partialResult, multiple) => partialResult.addResolved(multiple)
        }

        val FullyMerged(recoveredElements) = mergeResult: @unchecked

        assert(recoveredElements == elements)
      }
  end addingOnlyResolvedElementsYieldsThemViaTheFullyMergedPattern

  @TestFactory
  def addingTheLeftElementsOfAConflictedMergeYieldsThemViaTheMergedWithConflictsPattern()
      : DynamicTests =
    (for
      elements                       <- trialsApi.integers(0, 20).lists
      clumps                         <- elements.nonEmptyClumps
      potentiallyConflictingElements <- trialsApi
        .alternate(
          trialsApi.only(None),
          trialsApi.integers(0, 20).lists.map(Some.apply)
        )
        .listsOfSize(clumps.size)
        .filter(_.exists(_.nonEmpty))
    yield (elements, clumps.zip(potentiallyConflictingElements)))
      .withLimit(1000)
      .dynamicTests { (elements, clumpsWithPotentialConflicts) =>
        clumpsWithPotentialConflicts.foreach {
          case (left, Some(right)) if left == right =>
            // When building up a `MergeResult`, it is a precondition that any
            // conflicts should be genuine.
            Trials.reject()
          case _ =>
        }

        val mergeResult =
          clumpsWithPotentialConflicts.foldLeft(MergeResult.empty[Int]) {
            case (partialResult, (List(singleton), None)) =>
              partialResult.addResolved(singleton)
            case (partialResult, (multiple, None)) =>
              partialResult.addResolved(multiple)
            case (partialResult, (left, Some(right))) =>
              partialResult.addConflicted(left, right)
          }

        val MergedWithConflicts(recoveredLeftElements, _) =
          mergeResult: @unchecked

        assert(recoveredLeftElements == elements)
      }

  @TestFactory
  def addingTheRightElementsOfAConflictedMergeYieldsThemViaTheMergedWithConflictsPattern()
      : DynamicTests =
    (for
      elements                       <- trialsApi.integers(0, 20).lists
      clumps                         <- elements.nonEmptyClumps
      potentiallyConflictingElements <- trialsApi
        .alternate(
          trialsApi.only(None),
          trialsApi.integers(0, 20).lists.map(Some.apply)
        )
        .listsOfSize(clumps.size)
        .filter(_.exists(_.nonEmpty))
    yield (elements, potentiallyConflictingElements.zip(clumps)))
      .withLimit(1000)
      .dynamicTests { (elements, clumpsWithPotentialConflicts) =>
        clumpsWithPotentialConflicts.foreach {
          case (Some(left), right) if left == right =>
            // When building up a `MergeResult`, it is a precondition that any
            // conflicts should be genuine.
            Trials.reject()
          case _ =>
        }

        val mergeResult =
          clumpsWithPotentialConflicts.foldLeft(MergeResult.empty[Int]) {
            case (partialResult, (None, List(singleton))) =>
              partialResult.addResolved(singleton)
            case (partialResult, (None, multiple)) =>
              partialResult.addResolved(multiple)
            case (partialResult, (Some(left), right)) =>
              partialResult.addConflicted(left, right)
          }

        val MergedWithConflicts(_, recoveredRightElements) =
          mergeResult: @unchecked

        assert(recoveredRightElements == elements)
      }

end MergeResultTest

object MergeResultTest:
  extension [Element](elements: List[Element])
    def nonEmptyClumps: Trials[Vector[List[Element]]] =
      if elements.nonEmpty then
        for
          numberOfClumps  <- trialsApi.integers(1, elements.size)
          clumpEndIndices <- trialsApi.indexCombinations(
            elements.size - 1,
            numberOfClumps - 1
          )
          cumulativeSizes = clumpEndIndices.map(1 + _)
        yield
          val ((_, leftovers), clumps) =
            Traverse[Vector].mapAccumulate(0 -> elements, cumulativeSizes) {
              case ((predecessorSize, remainingElements), size) =>
                val clumpSize          = size - predecessorSize
                val (clump, leftovers) =
                  remainingElements.splitAt(clumpSize)
                (size, leftovers) -> clump
            }

          clumps :+ leftovers
      else trialsApi.only(Vector.empty)
  end extension
end MergeResultTest
