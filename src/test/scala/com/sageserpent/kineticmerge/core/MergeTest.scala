package com.sageserpent.kineticmerge.core

import com.eed3si9n.expecty
import com.eed3si9n.expecty.Expecty
import com.sageserpent.americium.junit5.DynamicTests
import org.junit.jupiter.api.{DynamicTest, TestFactory}

class MergeTest:
  @TestFactory
  def example: DynamicTests = ???

  /* Test ideas:
   * 1. Start with a merged sequence of sections and confabulate base, left and
   * right sequences by diverting each section into just the left (a left
   * insertion), just the right (a right insertion) or both the left and right
   * (coincident insertion) or all three (preservation). In a similar vein,
   * additional sections can be added into the base and left (right deletion),
   * the base and right (left deletion) or just the base (coincident deletion).
   * Insertions and deletions can be mixed as long as they don't make
   * overlapping claims in the base / left / right and also do not mix a left or
   * right insertion with a coincident deletion. That tests a simple, clean
   * merge without moves.
   *
   * 2. Same as #1, only add in combinations of a left or right insertion with a
   * coincident deletion to create edit / delete conflicts and combinations of
   * left and right insertions with different elements to create edit conflicts.
   *
   * 3. Same as #1, only associate left or right deletions with an insertion
   * elsewhere of the same section to create a move.
   *
   * 4. Same as #1, only combine coincident deletions with left or right
   * insertions and associate them with an insertion elsewhere of the inserted
   * section to create an edited move.
   *
   * 5. Same as #1, only associate coincident deletions with an insertion
   * elsewhere of the same section in the same place in the left and right to
   * create a coincident move. A coincident deletion may be combined with a left
   * / right or coincident insertion that is *not* treated as an edit of either
   * move.
   *
   * 6. Same as #5, only move to different locations to make divergence. A
   * coincident deletion may be combined with a left / right or coincident
   * insertion that is *not* treated as an edit of either move.
   *
   * NOTE: have to create a synthetic match for a section that is present in
   * more than one input, with a dominant section that should appear in the
   * merged result.
   *
   * An easier way to generate the test cases might be to make triples of
   * optional sections, filtering out (None, None, None). Each section appearing
   * in a triple can be put into a match if desired, and that match be made to
   * yield a mocked dominant section that goes into the expected output. */
end MergeTest
