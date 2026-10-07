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
          (trialsApi
            .integers(0, 20)
            .lists and trialsApi.integers(0, 20).lists).trials.map(Some.apply)
        )
        .listsOfSize(clumps.size)
        .filter(_.exists(_.nonEmpty))
    yield (elements, clumps.zip(potentiallyConflictingElements)))
      .withLimit(1000)
      .dynamicTests { (elements, clumpsWithPotentialConflicts) =>
        clumpsWithPotentialConflicts.foreach {
          case (left, Some((base, right)))
              if base == left || base == right || left == right =>
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
            case (partialResult, (left, Some((base, right)))) =>
              partialResult.addConflicted(base, left, right)
          }

        val MergedWithConflicts(_, recoveredLeftElements, _) =
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
          (trialsApi
            .integers(0, 20)
            .lists and trialsApi.integers(0, 20).lists).trials.map(Some.apply)
        )
        .listsOfSize(clumps.size)
        .filter(_.exists(_.nonEmpty))
    yield (elements, potentiallyConflictingElements.zip(clumps)))
      .withLimit(1000)
      .dynamicTests { (elements, clumpsWithPotentialConflicts) =>
        clumpsWithPotentialConflicts.foreach {
          case (Some((base, left)), right)
              if base == left || base == right || left == right =>
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
            case (partialResult, (Some((base, left)), right)) =>
              partialResult.addConflicted(base, left, right)
          }

        val MergedWithConflicts(_, _, recoveredRightElements) =
          mergeResult: @unchecked

        assert(recoveredRightElements == elements)
      }

  @TestFactory
  def addingTheBaseElementsOfAConflictedMergeYieldsThemViaTheMergedWithConflictsPattern()
      : DynamicTests =
    (for
      elements                       <- trialsApi.integers(0, 20).lists
      clumps                         <- elements.nonEmptyClumps
      potentiallyConflictingElements <- trialsApi
        .alternate(
          trialsApi.only(None),
          (trialsApi
            .integers(0, 20)
            .lists and trialsApi.integers(0, 20).lists).trials.map(Some.apply)
        )
        .listsOfSize(clumps.size)
        .filter(_.exists(_.nonEmpty))
    yield (elements, clumps.zip(potentiallyConflictingElements)))
      .withLimit(1000)
      .dynamicTests { (elements, clumpsWithPotentialConflicts) =>
        clumpsWithPotentialConflicts.foreach {
          case (base, Some((left, right)))
              if base == left || base == right || left == right =>
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
            case (partialResult, (base, Some((left, right)))) =>
              partialResult.addConflicted(base, left, right)
          }

        val MergedWithConflicts(recoveredBaseElements, _, _) =
          mergeResult: @unchecked

        assert(recoveredBaseElements == elements)
      }

  @TestFactory
  def localisationOfConflicts(): DynamicTests =
    (for sequencesOfResolvedRunsAlternatingWithConflictsInPairs <- trialsApi
        .integers(0, 20)
        .lists
        .flatMap(_.nonEmptyClumps)
        .flatMap(resolvedRuns =>
          (trialsApi.integers(0, 20).lists and trialsApi
            .integers(0, 20)
            .lists and trialsApi.integers(0, 20).lists).trials
            .filter { case (base, left, right) =>
              base != left && base != right && left != right
            }
            .listsOfSize(resolvedRuns.size)
            .map(resolvedRuns zip _)
        )
    yield sequencesOfResolvedRunsAlternatingWithConflictsInPairs)
      .withLimit(1000)
      .dynamicTests { resolvedRunsAlternatingWithConflictsInPairs =>
        val mergeResult = resolvedRunsAlternatingWithConflictsInPairs.foldLeft(
          MergeResult.empty[Int]
        ) { case (partialResult, (resolvedRun, (base, left, right))) =>
          partialResult
            .addResolved(resolvedRun)
            .addConflicted(base, left, right)
        }

        // As the resolved runs and conflicts alternate, there is no possibility
        // of coalescence of either. So each pair should contribute a resolved
        // and a conflicted segment...

        assert(
          resolvedRunsAlternatingWithConflictsInPairs.size == mergeResult.segments
            .count {
              case MergeResult.Segment.Resolved(_)         => true
              case MergeResult.Segment.Conflicted(_, _, _) => false
            }
        )

        assert(
          resolvedRunsAlternatingWithConflictsInPairs.size == mergeResult.segments
            .count {
              case MergeResult.Segment.Resolved(_)         => false
              case MergeResult.Segment.Conflicted(_, _, _) => true
            }
        )
      }

  @TestFactory
  def preservationOfLocalisedConflicts(): DynamicTests =
    (for sequencesOfResolvedRunsAlternatingWithConflictsInPairs <- trialsApi
        .integers(0, 20)
        .lists
        .flatMap(_.nonEmptyClumps)
        .flatMap(resolvedRuns =>
          (trialsApi.integers(0, 20).lists and trialsApi
            .integers(0, 20)
            .lists and trialsApi.integers(0, 20).lists).trials
            .filter { case (base, left, right) =>
              base != left && base != right && left != right
            }
            .listsOfSize(resolvedRuns.size)
            .map(resolvedRuns zip _)
        )
    yield sequencesOfResolvedRunsAlternatingWithConflictsInPairs)
      .withLimit(1000)
      .dynamicTests { resolvedRunsAlternatingWithConflictsInPairs =>
        val mergeResult = resolvedRunsAlternatingWithConflictsInPairs.foldLeft(
          MergeResult.empty[Int]
        ) { case (partialResult, (resolvedRun, (base, left, right))) =>
          partialResult
            .addResolved(resolvedRun)
            .addConflicted(base, left, right)
        }

        // As the resolved runs and conflicts alternate, there is no possibility
        // of coalescence of either in the initial merge result. Each pair
        // should contribute a resolved and a conflicted segment. This pairing
        // of segments should be preserved when transforming via a lossless
        // round trip.

        {
          // Use the direct API to round-trip...

          val mergeResultSharingSameConflictLocalisation =
            mergeResult
              .innerFlatMap(element => Seq(2 * element, 1 + 2 * element))

          val roundTrippedMergeResultSharingSameConflictLocalisation =
            mergeResultSharingSameConflictLocalisation
              .filter(1 == _ % 2)
              .map(_ / 2)

          assert(
            mergeResult.segments == roundTrippedMergeResultSharingSameConflictLocalisation.segments
          )
        }

        {
          // Use the side-based API to round-trip...

          val mergeResultSharingSameConflictLocalisation =
            mergeResult.onEachSide(
              _.innerFlatMapAccumulate(())((state, element) =>
                state -> Seq(2 * element, 1 + 2 * element)
              )._2
            )

          val roundTrippedMergeResultSharingSameConflictLocalisation =
            mergeResultSharingSameConflictLocalisation.onEachSide(
              _.innerFlatMapAccumulate(())((state, element) =>
                state -> Seq(element).filter(1 == _ % 2).map(_ / 2)
              )._2
            )

          assert(
            mergeResult.segments == roundTrippedMergeResultSharingSameConflictLocalisation.segments
          )
        }
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
