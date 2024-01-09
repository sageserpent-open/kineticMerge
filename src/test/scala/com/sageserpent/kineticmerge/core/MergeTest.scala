package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.MergeTest.*
import com.sageserpent.kineticmerge.core.MergeTest.Move.*
import com.sageserpent.kineticmerge.core.merge.{FullyMerged, MergedWithConflicts, Result}
import monocle.syntax.all.*
import org.junit.jupiter.api.{Test, TestFactory}
import pprint.*

class MergeTest:
  private val fullyMergedTestCases: Trials[MergeTestCase] =
    simpleMergeTestCases(
      allowConflicts = false,
      predecessorMove = Preservation
    )(partialResult = emptyMergeTestCase(allowConflicts = false))

  private val possiblyConflictedMergeTestCases: Trials[MergeTestCase] =
    simpleMergeTestCases(
      allowConflicts = true,
      predecessorMove = Preservation
    )(
      partialResult = emptyMergeTestCase(allowConflicts = true)
    )

  @Test
  def edit(): Unit =
    val a    = 1
    val base = Vector(a)

    val b    = 2
    val left = Vector(b)

    val c     = 3
    val right = Vector(c)

    val ab = Match.BaseAndLeft(baseElement = a, leftElement = b)

    val matchesByElement: Map[Element, Match[Element]] = Map(a -> ab, b -> ab)

    // NOTE: we expect a clean merge of the edit of `a` into `c`.
    val expectedMerge = FullyMerged(elements = Vector(c))

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end edit

  @Test
  def editFollowedByAnInsertionOnTheOtherSide(): Unit =
    val a    = 1
    val base = Vector(a)

    val b    = 2
    val c    = 3
    val left = Vector(b, c)

    val d     = 4
    val right = Vector(d)

    val ab = Match.BaseAndLeft(baseElement = a, leftElement = b)

    val matchesByElement: Map[Element, Match[Element]] = Map(a -> ab, b -> ab)

    // NOTE: we expect a clean merge of the edit of `a` into `d` followed by an
    // insertion of `c`.
    val expectedMerge = FullyMerged(elements = Vector(d, c))

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end editFollowedByAnInsertionOnTheOtherSide

  @Test
  def insertionFollowedByADeletionOnTheOtherSide(): Unit =
    val a    = 1
    val base = Vector(a)

    val b    = 2
    val c    = 3
    val left = Vector(b, c)

    val right = Vector()

    val ac = Match.BaseAndLeft(baseElement = a, leftElement = c)

    val matchesByElement: Map[Element, Match[Element]] = Map(a -> ac, c -> ac)

    // NOTE: we expect a clean merge of the insertion of `b` followed by a
    // deletion of `a`.
    val expectedMerge = FullyMerged(elements = Vector(b))

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end insertionFollowedByADeletionOnTheOtherSide

  @Test
  def insertionFollowedByAnEditOnTheOtherSide(): Unit =
    val a = 1

    val base = Vector(a)

    val b    = 2
    val c    = 3
    val left = Vector(b, c)

    val d     = 4
    val right = Vector(d)

    val ac = Match.BaseAndLeft(baseElement = a, leftElement = c)

    val matchesByElement: Map[Element, Match[Element]] = Map(a -> ac, c -> ac)

    // NOTE: we expect a clean merge of the insertion of `b` followed by an edit
    // of `a` into `d`.
    val expectedMerge = FullyMerged(elements = Vector(b, d))

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end insertionFollowedByAnEditOnTheOtherSide

  @Test
  def coincidentDeletionFollowedByAnEdit(): Unit =
    val a    = 1
    val b    = 2
    val base = Vector(a, b)

    val c    = 3
    val left = Vector(c)

    val d     = 4
    val right = Vector(d)

    val bc = Match.BaseAndLeft(baseElement = b, leftElement = c)

    val matchesByElement: Map[Element, Match[Element]] = Map(b -> bc, c -> bc)

    // NOTE: we expect a clean merge of the coincident deletion of `a` followed
    // by the edit of `b` into `d`
    val expectedMerge = FullyMerged(elements = Vector(d))

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end coincidentDeletionFollowedByAnEdit

  @Test
  def coincidentDeletionWithCoincidentInsertionAndThenASingleSideInsertion()
      : Unit =
    val a    = 1
    val base = Vector(a)

    val b    = 2
    val left = Vector(b)

    val c     = 3
    val d     = 4
    val right = Vector(c, d)

    val bc = Match.LeftAndRight(leftElement = b, rightElement = c)

    val matchesByElement: Map[Element, Match[Element]] = Map(b -> bc, c -> bc)

    // NOTE: we expect a clean merge of the coincident deletion of `a` with a
    // coincident insertion of `b` followed by insertion of `d`.
    val expectedMerge = FullyMerged(elements = Vector(b, d))

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end coincidentDeletionWithCoincidentInsertionAndThenASingleSideInsertion

  @Test
  def singleSideInsertionFollowedByCoincidentDeletionWithCoincidentInsertion()
      : Unit =
    val a    = 1
    val base = Vector(a)

    val b    = 2
    val left = Vector(b)

    val c     = 3
    val d     = 4
    val right = Vector(c, d)

    val bd = Match.LeftAndRight(leftElement = b, rightElement = d)

    val matchesByElement: Map[Element, Match[Element]] = Map(b -> bd, d -> bd)

    // NOTE: we expect a clean merge of the insertion of `c`, followed by a
    // coincident deletion of `a` with a coincident insertion of `b`.
    val expectedMerge = FullyMerged(elements = Vector(c, b))

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end singleSideInsertionFollowedByCoincidentDeletionWithCoincidentInsertion

  @Test
  def editOnOneSideFollowedByADeletionOnTheSameSide(): Unit =
    val a    = 1
    val b    = 2
    val base = Vector(a, b)

    val c    = 3
    val d    = 4
    val left = Vector(c, d)

    val e     = 5
    val right = Vector(e)

    val ac = Match.BaseAndLeft(baseElement = a, leftElement = c)
    val bd = Match.BaseAndLeft(baseElement = b, leftElement = d)

    val matchesByElement: Map[Element, Match[Element]] =
      Map(a -> ac, c -> ac, b -> bd, d -> bd)

    // NOTE: we expect a clean merge of the edit of `a` into `e` followed by a
    // deletion of `b`. Merging is supposed to look eagerly for edits, so `e` is
    // considered to be an edit of `a` rather than of `b`.
    val expectedMerge = FullyMerged(elements = Vector(e))

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end editOnOneSideFollowedByADeletionOnTheSameSide

  @Test
  def editOnOneSideFollowedByADeletionOnTheSameSideThenAnInsertionOnTheOppositeSide()
      : Unit =
    val a    = 1
    val b    = 2
    val base = Vector(a, b)

    val c    = 3
    val d    = 4
    val e    = 5
    val left = Vector(c, d, e)

    val f     = 6
    val right = Vector(f)

    val ac = Match.BaseAndLeft(baseElement = a, leftElement = c)
    val bd = Match.BaseAndLeft(baseElement = b, leftElement = d)

    val matchesByElement: Map[Element, Match[Element]] =
      Map(a -> ac, c -> ac, b -> bd, d -> bd)

    // NOTE: we expect a clean merge of the edit of `a` into `f` followed by a
    // deletion of `b` and then an insertion of `e`. Merging is supposed to look
    // eagerly for edits, so `f` is considered to be an edit of `a` rather than
    // of `b`.
    val expectedMerge = FullyMerged(elements = Vector(f, e))

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end editOnOneSideFollowedByADeletionOnTheSameSideThenAnInsertionOnTheOppositeSide

  @Test
  def editOnOneSideFollowedByAnInsertionOnTheOppositeSideThenADeletionOnTheOriginalSide()
      : Unit =
    val a    = 1
    val b    = 2
    val base = Vector(a, b)

    val c    = 3
    val d    = 4
    val e    = 5
    val left = Vector(c, d, e)

    val f     = 6
    val right = Vector(f)

    val ac = Match.BaseAndLeft(baseElement = a, leftElement = c)
    val be = Match.BaseAndLeft(baseElement = b, leftElement = e)

    val matchesByElement: Map[Element, Match[Element]] =
      Map(a -> ac, c -> ac, b -> be, e -> be)

    // NOTE: we expect a clean merge of the edit of `a` into `f` followed by an
    // insertion of `d` and finally deletion of `b`. Merging is supposed to look
    // eagerly for edits, so `f` is considered to be an edit of `a` rather than
    // of `b`.
    val expectedMerge = FullyMerged(elements = Vector(f, d))

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end editOnOneSideFollowedByAnInsertionOnTheOppositeSideThenADeletionOnTheOriginalSide

  @Test
  def editOnOneSideFollowedByAnInsertionOnTheOppositeSideThenAnEditOnTheOriginalSide()
      : Unit =
    val a    = 1
    val b    = 2
    val base = Vector(a, b)

    val c    = 3
    val d    = 4
    val e    = 5
    val left = Vector(c, d, e)

    val f     = 6
    val g     = 7
    val right = Vector(f, g)

    val ac = Match.BaseAndLeft(baseElement = a, leftElement = c)
    val be = Match.BaseAndLeft(baseElement = b, leftElement = e)

    val matchesByElement: Map[Element, Match[Element]] =
      Map(a -> ac, c -> ac, b -> be, e -> be)

    // NOTE: we expect a clean merge of the edit of `a` into `f` followed by an
    // insertion of `d` and finally a merge of the edit of `b` into `g`.
    val expectedMerge = FullyMerged(elements = Vector(f, d, g))

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end editOnOneSideFollowedByAnInsertionOnTheOppositeSideThenAnEditOnTheOriginalSide

  @Test
  def editOnOneSideFollowedByAnInsertionOnTheSameSide(): Unit =
    val a    = 1
    val base = Vector(a)

    val b    = 2
    val left = Vector(b)

    val c     = 3
    val d     = 4
    val right = Vector(c, d)

    val ab = Match.BaseAndLeft(baseElement = a, leftElement = b)

    val matchesByElement: Map[Element, Match[Element]] =
      Map(a -> ab, b -> ab)

    // NOTE: we expect a clean merge of the edit of `a` into `c` coalesced with
    // an insertion of `d`.
    val expectedMerge = FullyMerged(elements = Vector(c, d))

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end editOnOneSideFollowedByAnInsertionOnTheSameSide

  @Test
  def editOnOneSideFollowedByAnInsertionOnTheSameSideThenAnInsertionOnTheOppositeSide()
      : Unit =
    val a    = 1
    val base = Vector(a)

    val b    = 2
    val c    = 3
    val left = Vector(b, c)

    val d     = 4
    val e     = 5
    val right = Vector(d, e)

    val ab = Match.BaseAndLeft(baseElement = a, leftElement = b)

    val matchesByElement: Map[Element, Match[Element]] =
      Map(a -> ab, b -> ab)

    // NOTE: we expect a clean merge of the edit of `a` into `d` coalesced with
    // an insertion of `e` followed by an insertion of `c`.
    val expectedMerge = FullyMerged(elements = Vector(d, e, c))

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end editOnOneSideFollowedByAnInsertionOnTheSameSideThenAnInsertionOnTheOppositeSide

  @Test
  def deletionFollowedByAnInsertionOnTheOtherSide(): Unit =
    val a    = 1
    val base = Vector(a)

    val b    = 2
    val c    = 3
    val left = Vector(b, c)

    val right = Vector()

    val ab = Match.BaseAndLeft(baseElement = a, leftElement = b)

    val matchesByElement: Map[Element, Match[Element]] = Map(a -> ab, b -> ab)

    // NOTE: we expect a clean merge of the deletion of `a` followed by an
    // insertion of `c`.
    val expectedMerge = FullyMerged(elements = Vector(c))

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end deletionFollowedByAnInsertionOnTheOtherSide

  @Test
  def deletionOnOneSideFollowedByADeletionOnTheOtherSide(): Unit =
    val a    = 1
    val b    = 2
    val base = Vector(a, b)

    val c    = 3
    val left = Vector(c)

    val d     = 4
    val right = Vector(d)

    val ac = Match.BaseAndLeft(baseElement = a, leftElement = c)
    val bd = Match.BaseAndRight(baseElement = b, rightElement = d)

    val matchesByElement: Map[Element, Match[Element]] =
      Map(a -> ac, c -> ac, b -> bd, d -> bd)

    // NOTE: we expect a clean merge of the deletion of `a` followed by a
    // deletion of `b`.
    val expectedMerge = FullyMerged(elements = Vector())

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end deletionOnOneSideFollowedByADeletionOnTheOtherSide

  @Test
  def editConflict(): Unit =
    val a    = 1
    val b    = 2
    val base = Vector(a, b)

    val c    = 3
    val d    = 4
    val left = Vector(c, d)

    val e     = 5
    val f     = 6
    val right = Vector(e, f)

    val bdf =
      Match.AllThree(baseElement = b, leftElement = d, rightElement = f)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      b -> bdf,
      d -> bdf,
      f -> bdf
    )

    // NOTE: we expect a clean merge of `d` after the initial edit conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(c, d),
        rightElements = Vector(e, d)
      )

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end editConflict

  @Test
  def leftEditVersusRightDeletionConflictDueToFollowingRightEdit(): Unit =
    val a    = 1
    val b    = 2
    val c    = 3
    val base = Vector(a, b, c)

    val d    = 4
    val e    = 5
    val f    = 6
    val left = Vector(d, e, f)

    val g     = 7
    val h     = 8
    val i     = 9
    val right = Vector(g, h, i)

    val be  = Match.BaseAndLeft(baseElement = b, leftElement = e)
    val cfi = Match.AllThree(baseElement = c, leftElement = f, rightElement = i)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      b -> be,
      e -> be,
      c -> cfi,
      f -> cfi,
      i -> cfi
    )

    // NOTE: we expect a clean merge of `g`, `h` and `f` after the initial
    // left edit versus right deletion conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(d, g, h, f),
        rightElements = Vector(g, h, f)
      )

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end leftEditVersusRightDeletionConflictDueToFollowingRightEdit

  @Test
  def rightEditVersusLeftDeletionConflictDueToFollowingLeftEdit(): Unit =
    val a    = 1
    val b    = 2
    val c    = 3
    val base = Vector(a, b, c)

    val d    = 4
    val e    = 5
    val f    = 6
    val left = Vector(d, e, f)

    val g     = 7
    val h     = 8
    val i     = 9
    val right = Vector(g, h, i)

    val bh  = Match.BaseAndRight(baseElement = b, rightElement = h)
    val cfi = Match.AllThree(baseElement = c, leftElement = f, rightElement = i)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      b -> bh,
      h -> bh,
      c -> cfi,
      f -> cfi,
      i -> cfi
    )

    // NOTE: we expect a clean merge of `d`, `e` and `f` after the initial
    // right edit versus left deletion conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(d, e, f),
        rightElements = Vector(g, d, e, f)
      )

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end rightEditVersusLeftDeletionConflictDueToFollowingLeftEdit

  @Test
  def insertionConflict(): Unit =
    val a    = 1
    val base = Vector(a)

    val b    = 2
    val c    = 3
    val left = Vector(b, c)

    val d     = 4
    val e     = 5
    val right = Vector(d, e)

    val ace =
      Match.AllThree(baseElement = a, leftElement = c, rightElement = e)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      a -> ace,
      c -> ace,
      e -> ace
    )

    // NOTE: we expect a clean merge of `d` after the initial conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(b, c),
        rightElements = Vector(d, c)
      )

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end insertionConflict

  @Test
  def editConflictFollowedByCoincidentInsertion(): Unit =
    val a    = 1
    val b    = 2
    val base = Vector(a, b)

    val c    = 3
    val d    = 4
    val e    = 5
    val left = Vector(c, d, e)

    val f     = 6
    val g     = 7
    val h     = 8
    val right = Vector(f, g, h)

    val dg = Match.LeftAndRight(leftElement = d, rightElement = g)

    val beh =
      Match.AllThree(baseElement = b, leftElement = e, rightElement = h)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      d -> dg,
      g -> dg,
      b -> beh,
      e -> beh,
      h -> beh
    )

    // NOTE: we expect a clean merge of `d` and `g` after the initial edit
    // conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(c, d, e),
        rightElements = Vector(f, d, e)
      )

    val Right(result) =
      merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end editConflictFollowedByCoincidentInsertion

  @TestFactory
  def fullMerge: DynamicTests =
    fullyMergedTestCases
      .withLimit(20000)
      .dynamicTests: testCase =>
        println("*************")
        pprintln(testCase)

        val Right(result) =
          merge.of(testCase.base, testCase.left, testCase.right)(
            equivalent(testCase.matchesByElement)
          ): @unchecked

        testCase.validate(result)

  @TestFactory
  def conflictedMerge: DynamicTests =
    possiblyConflictedMergeTestCases
      .withLimit(2000)
      .dynamicTests: testCase =>
        val Right(result) =
          merge.of(testCase.base, testCase.left, testCase.right)(
            equivalent(testCase.matchesByElement)
          ): @unchecked

        result match
          case MergedWithConflicts(_, _) =>
            println("*************")
            pprintln(testCase)

            testCase.validate(result)
          case FullyMerged(_) => Trials.reject()
        end match

  def simpleMergeTestCases(
      allowConflicts: Boolean,
      predecessorMove: Move,
      // The next two flags express the decision to have the SUT eagerly
      // pick up edits, so a merged edit can be followed by a merged deletion on
      // the same side, but not the other way around. The test confabulates the
      // inputs for the merge based on an expected result, so it has to make
      // sure that it doesn't start with a result where an edit follows a
      // deletion. This is only relevant when the edit and deletion are both on
      // the same side and the only other moves between them are insertions on
      // the opposite side, or coincident deletions. All other moves lift the
      // block, as they make the interpretation as to which is the edit and
      // which the deletion unambiguous.
      // In a similar vein, insertions also block following edits on the same
      // side, because we expect the SUT to eagerly pick up an edit first.
      priorDeletionOrInsertionBlockingLeftEdit: Boolean = false,
      priorDeletionOrInsertionBlockingRightEdit: Boolean = false,
      // The next two flags stop misleading move sequences from being generated,
      // where a deletion is followed by an insertion on the same side and the
      // only other moves between them are insertions on the opposite side.
      // Edits already capture this notion more economically.
      priorDeletionBlockingLeftInsertion: Boolean = false,
      priorDeletionBlockingRightInsertion: Boolean = false,
      // The next two flags stop misleading move sequences from being generated,
      // where an insertion is followed by a deletion on the same side and the
      // only other moves between them are insertions on the opposite side.
      // Edits already capture this notion more economically.
      priorInsertionBlockingLeftDeletion: Boolean = false,
      priorInsertionBlockingRightDeletion: Boolean = false,
      // The next two flags allow move sequences to pair insertions across sides
      // when there is a prior edit that could coalesce with one of the
      // insertions to avoid a conflict.
      priorEditClaimingLeftInsertion: Boolean = false,
      priorEditClaimingRightInsertion: Boolean = false
  )(partialResult: MergeTestCase): Trials[MergeTestCase] =
    def leftInsertionFrequency(allow: Boolean) =
      (if allow && (!priorDeletionBlockingLeftInsertion || priorEditClaimingRightInsertion)
       then 7
       else 0) -> LeftInsertion

    def rightInsertionFrequency(allow: Boolean) =
      (if allow && (!priorDeletionBlockingRightInsertion || priorEditClaimingLeftInsertion)
       then 7
       else 0) -> RightInsertion

    def leftEditFrequency() =
      (if !priorDeletionOrInsertionBlockingLeftEdit then 7
       else 0) -> LeftEdit

    def rightEditFrequency() =
      (if !priorDeletionOrInsertionBlockingRightEdit then 7
       else 0) -> RightEdit

    def leftDeletionFrequency() =
      (if !priorInsertionBlockingLeftDeletion then 7 else 0) -> LeftDeletion

    def rightDeletionFrequency() =
      (if !priorInsertionBlockingRightDeletion then 7 else 0) -> RightDeletion

    val extendedMergeTestCases =
      val zeroRelativeElements: Trials[Element] = trialsApi.uniqueIds

      val choices = predecessorMove match
        case LeftInsertion =>
          trialsApi
            .chooseWithWeights(
              leftInsertionFrequency(allow = true),
              rightInsertionFrequency(allow = allowConflicts),
              coincidentInsertionFrequency,
              preservationFrequency,
              leftEditFrequency(),
              rightEditFrequency(),
              leftDeletionFrequency(),
              rightDeletionFrequency()
            )
        case RightInsertion =>
          trialsApi
            .chooseWithWeights(
              leftInsertionFrequency(allow = allowConflicts),
              rightInsertionFrequency(allow = true),
              coincidentInsertionFrequency,
              preservationFrequency,
              leftEditFrequency(),
              rightEditFrequency(),
              leftDeletionFrequency(),
              rightDeletionFrequency()
            )

        case CoincidentInsertion | Preservation | LeftEdit | RightEdit =>
          trialsApi
            .chooseWithWeights(
              leftInsertionFrequency(allow = true),
              rightInsertionFrequency(allow = true),
              coincidentInsertionFrequency,
              preservationFrequency,
              leftEditFrequency(),
              rightEditFrequency(),
              leftDeletionFrequency(),
              rightDeletionFrequency(),
              coincidentDeletionFrequency
            )

        case LeftDeletion =>
          trialsApi
            .chooseWithWeights(
              leftInsertionFrequency(allow = true),
              coincidentInsertionFrequency,
              preservationFrequency,
              leftEditFrequency(),
              rightEditFrequency(),
              leftDeletionFrequency(),
              rightDeletionFrequency(),
              coincidentDeletionFrequency
            )

        case RightDeletion =>
          trialsApi
            .chooseWithWeights(
              leftInsertionFrequency(allow = true),
              coincidentInsertionFrequency,
              preservationFrequency,
              leftEditFrequency(),
              rightEditFrequency(),
              leftDeletionFrequency(),
              rightDeletionFrequency(),
              coincidentDeletionFrequency
            )

        case CoincidentDeletion =>
          trialsApi
            .chooseWithWeights(
              leftInsertionFrequency(allow = allowConflicts),
              rightInsertionFrequency(allow = allowConflicts),
              coincidentInsertionFrequency,
              preservationFrequency,
              leftEditFrequency(),
              rightEditFrequency(),
              leftDeletionFrequency(),
              rightDeletionFrequency(),
              coincidentDeletionFrequency
            )
      choices flatMap:
        case LeftInsertion =>
          for
            leftElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorMove = LeftInsertion,
              priorDeletionOrInsertionBlockingLeftEdit = true,
              priorDeletionOrInsertionBlockingRightEdit =
                priorDeletionOrInsertionBlockingRightEdit,
              priorDeletionBlockingRightInsertion =
                priorDeletionBlockingRightInsertion,
              priorInsertionBlockingLeftDeletion = true,
              priorInsertionBlockingRightDeletion =
                priorInsertionBlockingRightDeletion,
              priorEditClaimingLeftInsertion = priorEditClaimingLeftInsertion,
              priorEditClaimingRightInsertion = priorEditClaimingRightInsertion
            )(
              partialResult
                .focus(_.left)
                .modify(_ :+ leftElement)
                .focus(_.expectedMerge.some)
                .modify:
                  case FullyMerged(elements) =>
                    FullyMerged(elements :+ leftElement)
                .focus(_.moves)
                .modify(_ :+ LeftInsertion)
            )
          yield result
          end for

        case RightInsertion =>
          for
            rightElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorMove = RightInsertion,
              priorDeletionOrInsertionBlockingLeftEdit =
                priorDeletionOrInsertionBlockingLeftEdit,
              priorDeletionOrInsertionBlockingRightEdit = true,
              priorDeletionBlockingLeftInsertion =
                priorDeletionBlockingLeftInsertion,
              priorInsertionBlockingLeftDeletion =
                priorInsertionBlockingLeftDeletion,
              priorInsertionBlockingRightDeletion = true,
              priorEditClaimingLeftInsertion = priorEditClaimingLeftInsertion,
              priorEditClaimingRightInsertion = priorEditClaimingRightInsertion
            )(
              partialResult
                .focus(_.right)
                .modify(_ :+ rightElement)
                .focus(_.expectedMerge.some)
                .modify:
                  case FullyMerged(elements) =>
                    FullyMerged(elements :+ rightElement)
                .focus(_.moves)
                .modify(_ :+ RightInsertion)
            )
          yield result
          end for

        case CoincidentInsertion =>
          for
            leftElement  <- zeroRelativeElements
            rightElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorMove = CoincidentInsertion
            ):
              val elementMatch = Match.LeftAndRight(
                leftElement = leftElement,
                rightElement = rightElement
              )

              partialResult
                .focus(_.left)
                .modify(_ :+ leftElement)
                .focus(_.right)
                .modify(_ :+ rightElement)
                .focus(_.matchesByElement)
                .modify(
                  _ + (leftElement -> elementMatch) + (rightElement -> elementMatch)
                )
                .focus(_.expectedMerge.some)
                .modify:
                  case FullyMerged(elements) =>
                    FullyMerged(elements :+ elementMatch.dominantElement)
                .focus(_.moves)
                .modify(_ :+ CoincidentInsertion)
          yield result
          end for

        case Preservation =>
          for
            baseElement  <- zeroRelativeElements
            leftElement  <- zeroRelativeElements
            rightElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorMove = Preservation
            ):
              val elementMatch =
                Match.AllThree(
                  baseElement = baseElement,
                  leftElement = leftElement,
                  rightElement = rightElement
                )

              partialResult
                .focus(_.base)
                .modify(_ :+ baseElement)
                .focus(_.left)
                .modify(_ :+ leftElement)
                .focus(_.right)
                .modify(_ :+ rightElement)
                .focus(_.matchesByElement)
                .modify(
                  _ + (baseElement -> elementMatch) + (leftElement -> elementMatch) + (rightElement -> elementMatch)
                )
                .focus(_.expectedMerge.some)
                .modify:
                  case FullyMerged(elements) =>
                    FullyMerged(
                      elements :+ elementMatch.dominantElement
                    )
                .focus(_.moves)
                .modify(_ :+ Preservation)
          yield result
          end for

        case LeftEdit =>
          for
            baseElement  <- zeroRelativeElements
            leftElement  <- zeroRelativeElements
            rightElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorMove = LeftEdit,
              priorEditClaimingLeftInsertion = true
            ):
              val elementMatch = Match.BaseAndRight(
                baseElement = baseElement,
                rightElement = rightElement
              )

              partialResult
                .focus(_.base)
                .modify(_ :+ baseElement)
                .focus(_.left)
                .modify(_ :+ leftElement)
                .focus(_.right)
                .modify(_ :+ rightElement)
                .focus(_.matchesByElement)
                .modify(
                  _ + (baseElement -> elementMatch) + (rightElement -> elementMatch)
                )
                .focus(_.expectedMerge.some)
                .modify:
                  case FullyMerged(elements) =>
                    FullyMerged(elements :+ leftElement)
                .focus(_.moves)
                .modify(_ :+ LeftEdit)
          yield result
          end for

        case RightEdit =>
          for
            baseElement  <- zeroRelativeElements
            leftElement  <- zeroRelativeElements
            rightElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorMove = RightEdit,
              priorEditClaimingRightInsertion = true
            ):
              val elementMatch = Match.BaseAndLeft(
                baseElement = baseElement,
                leftElement = leftElement
              )

              partialResult
                .focus(_.base)
                .modify(_ :+ baseElement)
                .focus(_.left)
                .modify(_ :+ leftElement)
                .focus(_.right)
                .modify(_ :+ rightElement)
                .focus(_.matchesByElement)
                .modify(
                  _ + (baseElement -> elementMatch) + (leftElement -> elementMatch)
                )
                .focus(_.expectedMerge.some)
                .modify:
                  case FullyMerged(elements) =>
                    FullyMerged(elements :+ rightElement)
                .focus(_.moves)
                .modify(_ :+ RightEdit)
          yield result
          end for

        case LeftDeletion =>
          for
            baseElement  <- zeroRelativeElements
            rightElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorMove = LeftDeletion,
              priorDeletionOrInsertionBlockingLeftEdit = true,
              priorDeletionBlockingLeftInsertion = true
            ):
              val elementMatch = Match.BaseAndRight(
                baseElement = baseElement,
                rightElement = rightElement
              )

              partialResult
                .focus(_.base)
                .modify(_ :+ baseElement)
                .focus(_.right)
                .modify(_ :+ rightElement)
                .focus(_.matchesByElement)
                .modify(
                  _ + (baseElement -> elementMatch) + (rightElement -> elementMatch)
                )
                .focus(_.moves)
                .modify(_ :+ LeftDeletion)
          yield result
          end for

        case RightDeletion =>
          for
            baseElement <- zeroRelativeElements
            leftElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorMove = RightDeletion,
              priorDeletionOrInsertionBlockingRightEdit = true,
              priorDeletionBlockingRightInsertion = true
            ):
              val elementMatch = Match.BaseAndLeft(
                baseElement = baseElement,
                leftElement = leftElement
              )

              partialResult
                .focus(_.base)
                .modify(_ :+ baseElement)
                .focus(_.left)
                .modify(_ :+ leftElement)
                .focus(_.matchesByElement)
                .modify(
                  _ + (baseElement -> elementMatch) + (leftElement -> elementMatch)
                )
                .focus(_.moves)
                .modify(_ :+ RightDeletion)
          yield result
          end for

        case CoincidentDeletion =>
          for
            baseElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorMove = CoincidentDeletion,
              priorDeletionOrInsertionBlockingLeftEdit,
              priorDeletionOrInsertionBlockingRightEdit,
              priorDeletionBlockingLeftInsertion,
              priorDeletionBlockingRightInsertion,
              priorInsertionBlockingLeftDeletion,
              priorInsertionBlockingRightDeletion,
              priorEditClaimingLeftInsertion,
              priorEditClaimingRightInsertion
            ):
              partialResult
                .focus(_.base)
                .modify(_ :+ baseElement)
                .focus(_.moves)
                .modify(_ :+ CoincidentDeletion)
          yield result
          end for

    end extendedMergeTestCases

    trialsApi.complexities.flatMap(complexity =>
      trialsApi.alternateWithWeights(
        complexity -> trialsApi.only(partialResult),
        50         -> extendedMergeTestCases
      )
    )

  end simpleMergeTestCases

end MergeTest

object MergeTest:
  type Element = Int

  private val coincidentInsertionFrequency = 4  -> CoincidentInsertion
  private val preservationFrequency        = 10 -> Preservation
  private val coincidentDeletionFrequency  = 4  -> CoincidentDeletion

  def equivalent(
      matchesByElement: Map[Element, Match[Element]]
  )(lhs: Element, rhs: Element): Boolean =
    matchesByElement.get(lhs) -> matchesByElement.get(rhs) match
      case (None, None)    => false
      case (None, Some(_)) => false
      case (Some(_), None) => false
      case (Some(lhsMatch), Some(rhsMatch)) =>
        lhsMatch.dominantElement == rhsMatch.dominantElement

  private def emptyMergeTestCase(allowConflicts: Boolean): MergeTestCase =
    MergeTestCase(
      base = IndexedSeq.empty,
      left = IndexedSeq.empty,
      right = IndexedSeq.empty,
      matchesByElement = Map.empty,
      expectedMerge = Option.unless(allowConflicts) {
        FullyMerged(elements = IndexedSeq.empty)
      },
      moves = IndexedSeq.empty
    )

  case class MergeTestCase(
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element],
      matchesByElement: Map[Element, Match[Element]],
      expectedMerge: Option[FullyMerged[Element]],
      moves: IndexedSeq[Move]
  ):
    def validate(result: Result[Element]): Unit =
      def baseIsPreservedCorrectlyIn(
          elements: IndexedSeq[Element]
      ): Set[Element] =
        val preserved = base.collect(baseElement =>
          matchesByElement.get(baseElement) match
            case Some(allThree: Match.AllThree[Element]) =>
              allThree.dominantElement
        )

        val _ = preserved.isSubsequenceOf(elements)

        preserved.toSet

      end baseIsPreservedCorrectlyIn

      def leftAppearsCorrectlyIn(elements: IndexedSeq[Element]): Set[Element] =
        val appears = left.collect(leftElement =>
          matchesByElement.get(leftElement) match
            case Some(
                  allThree: (Match.AllThree[Element] |
                    Match.LeftAndRight[Element])
                ) =>
              allThree.dominantElement
            case None => leftElement
        )

        val _ = appears.isSubsequenceOf(elements)

        appears.toSet
      end leftAppearsCorrectlyIn

      def rightAppearsCorrectlyIn(elements: IndexedSeq[Element]): Set[Element] =
        val appears = right.collect(rightElement =>
          matchesByElement.get(rightElement) match
            case Some(
                  allThree: (Match.AllThree[Element] |
                    Match.LeftAndRight[Element])
                ) =>
              allThree.dominantElement
            case None => rightElement
        )

        val _ = appears.isSubsequenceOf(elements)

        appears.toSet
      end rightAppearsCorrectlyIn

      def allPresentAndCorrectIn(result: Result[Element]): Unit =
        result match
          case FullyMerged(elements) =>
            val basePreservations = baseIsPreservedCorrectlyIn(elements)
            val leftAppearances   = leftAppearsCorrectlyIn(elements)
            val rightAppearances  = rightAppearsCorrectlyIn(elements)

            assert(
              (basePreservations union leftAppearances union rightAppearances) == elements.toSet
            )

          case MergedWithConflicts(leftElements, rightElements) =>
            val basePreservationsOnLeft = baseIsPreservedCorrectlyIn(
              leftElements
            )
            val basePreservationsOnRight = baseIsPreservedCorrectlyIn(
              rightElements
            )
            val leftAppearances = leftAppearsCorrectlyIn(leftElements)
            val rightAppearances = rightAppearsCorrectlyIn(
              rightElements
            )

            assert(
              basePreservationsOnLeft == basePreservationsOnRight
            )

            // NOTE: use a subset rather than an equality check, as it is
            // possible for right elements that cleanly merged to appear in
            // `leftElements`.
            assert(
              (basePreservationsOnLeft union leftAppearances) subsetOf leftElements.toSet
            )
            // NOTE: use a subset rather than an equality check, as it is
            // possible for left elements that cleanly merged to appear in
            // `rightElements`.
            assert(
              (basePreservationsOnRight union rightAppearances) subsetOf rightElements.toSet
            )

      allPresentAndCorrectIn(result)

      expectedMerge.foreach(merge =>
        // Perform a self-check on the expected merge.
        allPresentAndCorrectIn(result)

        // This assertion is stronger than `allPresentAndCorrectIn` because it
        // includes the precise merge resolution.
        assert(
          result == merge
        )
      )

    end validate

  end MergeTestCase

  enum Move:
    case LeftInsertion
    case RightInsertion
    case CoincidentInsertion
    case Preservation
    case LeftEdit
    case RightEdit
    case LeftDeletion
    case RightDeletion
    case CoincidentDeletion
  end Move

  enum MoveBias:
    case Left // Can be followed by another left or switch to over to a neutral or coincident deletion.
    case Right // Can be followed by another right or switch to over to a neutral or coincident deletion.
    case Neutral // Can be followed by anything.
    case CoincidentDeletion // Can only be followed by another coincident deletion or a neutral.
  end MoveBias
end MergeTest
