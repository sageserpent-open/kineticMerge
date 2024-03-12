package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import org.junit.jupiter.api.Test

class MergeResultDetectingMotionTest:
  @Test
  def baseAndLeftMatchWhenMergeDoesNotAlignTheTwoMatchingSections: Unit =
    // Test plan:
    // 1. Set up base and left match.
    // 2. Apply coincident deletion.
    // 3. Expect the core merge result to be a left deletion.
    // 4. Expect an edit of the left section to nothing.

    assert(true)
end MergeResultDetectingMotionTest
