package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials
import org.junit.jupiter.api.Test
import ExpectyFlavouredAssert.assert

class MergeResultTest:
  // Whatever is done to a side via mapping, filtering or flat-mapping should be
  // equivalent to the same operation applied to a merge result.

  @Test
  def addingOnlyResolvedElementsYieldsThemViaTheFullyMergedPattern(): Unit =
    Trials.api
      .integers(0, 20)
      .lists
      .lists
      .map(_.filter(_.nonEmpty))
      .withLimit(20)
      .supplyTo { elementsInClumps =>
        val elements = elementsInClumps.flatten

        val mergeResult = elementsInClumps.foldLeft(MergeResult.empty[Int])(_.addResolved(_))

        val FullyMerged(recoveredElements) = mergeResult: @unchecked
        
        assert(recoveredElements == elements)
      }

end MergeResultTest
