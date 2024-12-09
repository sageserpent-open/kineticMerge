package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.CoreMergeAlgebra.{Merged, UnresolvedMergeResult}
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.{Sized, defaultElementSize}
import com.sageserpent.kineticmerge.core.MergeTest.*
import com.sageserpent.kineticmerge.core.MergeTest.DelegatingMergeAlgebraWithContracts.{AugmentedMergeResult, State}
import com.sageserpent.kineticmerge.core.MergeTest.Move.*
import com.sageserpent.kineticmerge.core.ResolutionContracts.*
import monocle.syntax.all.*
import org.junit.jupiter.api.{Assertions, Test, TestFactory}
import pprintCustomised.*

class MergeTest:
  private val fullyMergedTestCases: Trials[MergeTestCase] =
    simpleMergeTestCases(MoveConstraints.empty(allowConflicts = false))(
      partialResult = emptyMergeTestCase(allowConflicts = false)
    )

  private val possiblyConflictedMergeTestCases: Trials[MergeTestCase] =
    simpleMergeTestCases(MoveConstraints.empty(allowConflicts = true))(
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end editOnOneSideFollowedByAnInsertionOnTheSameSideThenAnInsertionOnTheOppositeSide

  @Test
  def editOnOneSideFollowedByAnInsertionOnTheSameSideThenACoincidentEdit()
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

    val af = Match.BaseAndRight(baseElement = a, rightElement = f)
    val eg = Match.LeftAndRight(leftElement = e, rightElement = g)

    val matchesByElement: Map[Element, Match[Element]] =
      Map(a -> af, f -> af, e -> eg, g -> eg)

    // NOTE: we expect a clean merge of the left edit of `a` into `c` coalesced
    // with the left insertion of `d` and finally a coincident edit of `b` into
    // `e`.
    val expectedMerge = FullyMerged(elements = Vector(c, d, e))

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end editOnOneSideFollowedByAnInsertionOnTheSameSideThenACoincidentEdit

  @Test
  def editOnOneSideFollowedByADeletionOnTheOppositeSide(): Unit =
    val a    = 1
    val b    = 2
    val base = Vector(a, b)

    val c    = 3
    val left = Vector(c)

    val d     = 4
    val e     = 5
    val right = Vector(d, e)

    val ac = Match.BaseAndLeft(baseElement = a, leftElement = c)
    val be = Match.BaseAndRight(baseElement = b, rightElement = e)

    val matchesByElement: Map[Element, Match[Element]] =
      Map(a -> ac, c -> ac, b -> be, e -> be)

    // NOTE: we expect a clean merge of the edit of `a` into `d` followed by a
    // deletion of `b`.
    val expectedMerge = FullyMerged(elements = Vector(d))

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end editOnOneSideFollowedByADeletionOnTheOppositeSide

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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end deletionOnOneSideFollowedByADeletionOnTheOtherSide

  @Test
  def deletionOnOneSideFollowedByAnEditOnTheOppositeSide(): Unit =
    val a    = 1
    val b    = 2
    val base = Vector(a, b)

    val c    = 3
    val d    = 4
    val left = Vector(c, d)

    val e     = 5
    val right = Vector(e)

    val ac = Match.BaseAndLeft(baseElement = a, leftElement = c)
    val be = Match.BaseAndRight(baseElement = b, rightElement = e)

    val matchesByElement: Map[Element, Match[Element]] =
      Map(a -> ac, c -> ac, b -> be, e -> be)

    // NOTE: we expect a clean merge of the deletion of 'a' followed by an edit
    // of `b` into `d` followed by a deletion of `b`.
    val expectedMerge = FullyMerged(elements = Vector(d))

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end deletionOnOneSideFollowedByAnEditOnTheOppositeSide

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
      Match.AllSides(baseElement = b, leftElement = d, rightElement = f)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end editConflict

  @Test
  def editConflictFollowedByAnotherEditConflict(): Unit =
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

    val cfi =
      Match.AllSides(baseElement = c, leftElement = f, rightElement = i)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      c -> cfi,
      f -> cfi,
      i -> cfi
    )

    // NOTE: we expect a clean merge of `f` after the two separate edit
    // conflicts.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(d, e, f),
        rightElements = Vector(g, h, f)
      )

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end editConflictFollowedByAnotherEditConflict

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
    val cfi = Match.AllSides(baseElement = c, leftElement = f, rightElement = i)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
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
    val cfi = Match.AllSides(baseElement = c, leftElement = f, rightElement = i)
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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end rightEditVersusLeftDeletionConflictDueToFollowingLeftEdit

  @Test
  def leftInsertionWithFollowingLeftInsertionVersusRightInsertionConflict()
      : Unit =
    val a    = 1
    val b    = 2
    val base = Vector(a, b)

    val c    = 3
    val d    = 4
    val e    = 5
    val f    = 6
    val left = Vector(c, d, e, f)

    val g     = 7
    val h     = 8
    val i     = 9
    val right = Vector(g, h, i)

    val acg = Match.AllSides(baseElement = a, leftElement = c, rightElement = g)
    val bfi = Match.AllSides(baseElement = b, leftElement = f, rightElement = i)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      a -> acg,
      c -> acg,
      g -> acg,
      b -> bfi,
      f -> bfi,
      i -> bfi
    )

    // NOTE: we expect a clean merge of `f` after the conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(c, d, e, f),
        rightElements = Vector(c, h, f)
      )

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end leftInsertionWithFollowingLeftInsertionVersusRightInsertionConflict

  @Test
  def leftEditVersusRightDeletionConflictWithFollowingLeftInsertion(): Unit =
    val a    = 1
    val b    = 2
    val c    = 3
    val base = Vector(a, b, c)

    val d    = 4
    val e    = 5
    val f    = 6
    val g    = 7
    val left = Vector(d, e, f, g)

    val h     = 8
    val i     = 9
    val right = Vector(h, i)

    val adh = Match.AllSides(baseElement = a, leftElement = d, rightElement = h)
    val cgi = Match.AllSides(baseElement = c, leftElement = g, rightElement = i)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      a -> adh,
      d -> adh,
      h -> adh,
      c -> cgi,
      g -> cgi,
      i -> cgi
    )

    // NOTE: we expect a clean merge of `g` after the coalesced conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(d, e, f, g),
        rightElements = Vector(d, g)
      )

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end leftEditVersusRightDeletionConflictWithFollowingLeftInsertion

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
      Match.AllSides(baseElement = a, leftElement = c, rightElement = e)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      a -> ace,
      c -> ace,
      e -> ace
    )

    // NOTE: we expect a clean merge of `c` after the initial conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(b, c),
        rightElements = Vector(d, c)
      )

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end insertionConflict

  @Test
  def insertionConflictFollowedByAnotherInsertionConflict(): Unit =
    val a    = 1
    val base = Vector(a)

    val b    = 2
    val c    = 3
    val d    = 4
    val left = Vector(b, c, d)

    val e     = 5
    val f     = 6
    val g     = 7
    val right = Vector(e, f, g)

    val adg =
      Match.AllSides(baseElement = a, leftElement = d, rightElement = g)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      a -> adg,
      d -> adg,
      g -> adg
    )

    // NOTE: we expect a clean merge of `d` after the two separate insertion
    // conflicts.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(b, c, d),
        rightElements = Vector(e, f, d)
      )

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end insertionConflictFollowedByAnotherInsertionConflict

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
      Match.AllSides(baseElement = b, leftElement = e, rightElement = h)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      d -> dg,
      g -> dg,
      b -> beh,
      e -> beh,
      h -> beh
    )

    // NOTE: we expect a clean merge of `d` and `e` after the initial edit
    // conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(c, d, e),
        rightElements = Vector(f, d, e)
      )

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end editConflictFollowedByCoincidentInsertion

  @Test
  def editConflictFollowedByCoincidentDeletion(): Unit =
    val a    = 1
    val b    = 2
    val c    = 3
    val base = Vector(a, b, c)

    val d    = 4
    val e    = 5
    val left = Vector(d, e)

    val f     = 6
    val g     = 7
    val right = Vector(f, g)

    val ceg =
      Match.AllSides(baseElement = c, leftElement = e, rightElement = g)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      c -> ceg,
      e -> ceg,
      g -> ceg
    )

    // NOTE: we expect a clean merge of `e` after the initial edit
    // conflict coalesces with the coincident deletion of `b`.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(d, e),
        rightElements = Vector(f, e)
      )

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end editConflictFollowedByCoincidentDeletion

  @Test
  def editConflictFollowedByALeftInsertionConflictingWithARightInsertion()
      : Unit =
    val a    = 1
    val base = Vector(a)

    val b    = 2
    val c    = 3
    val left = Vector(b, c)

    val d     = 4
    val e     = 5
    val right = Vector(d, e)

    val matchesByElement: Map[Element, Match[Element]] = Map.empty

    // NOTE: we expect the initial edit conflict to coalesce with the following
    // insertion conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(b, c),
        rightElements = Vector(d, e)
      )

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end editConflictFollowedByALeftInsertionConflictingWithARightInsertion

  @Test
  def leftEditVersusRightDeletionConflictWithFollowingLeftInsertionThenACoincidentInsertion()
      : Unit =
    val a    = 1
    val base = Vector(a)

    val b    = 2
    val c    = 3
    val d    = 4
    val left = Vector(b, c, d)

    val e     = 5
    val f     = 6
    val right = Vector(e, f)

    val df = Match.LeftAndRight(leftElement = d, rightElement = f)

    val matchesByElement: Map[Element, Match[Element]] = Map(d -> df, f -> df)

    // NOTE: we expect the initial edit conflict to coalesce with the following
    // insertion conflict, then be followed by the coincident insertion.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(b, c, d),
        rightElements = Vector(e, d)
      )

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end leftEditVersusRightDeletionConflictWithFollowingLeftInsertionThenACoincidentInsertion

  @Test
  def leftEditVersusRightDeletionConflictDueToFollowingRightDeletionAndThenLeftEdit()
      : Unit =
    val a    = 1
    val b    = 2
    val c    = 3
    val base = Vector(a, b, c)

    val d    = 4
    val e    = 5
    val f    = 6
    val left = Vector(d, e, f)

    val g     = 7
    val right = Vector(g)

    val be = Match.BaseAndLeft(baseElement = b, leftElement = e)
    val cg = Match.BaseAndRight(baseElement = c, rightElement = g)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      b -> be,
      e -> be,
      c -> cg,
      g -> cg
    )

    // NOTE: we expect a clean merge of the deletion of 'b' and the edit of 'c'
    // into 'f' after the initial left edit versus right deletion conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(d, f),
        rightElements = Vector(f)
      )

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end leftEditVersusRightDeletionConflictDueToFollowingRightDeletionAndThenLeftEdit

  @Test
  def leftEditVersusRightDeletionConflictWithFollowingLeftInsertionAndThenRightEdit()
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

    val be = Match.BaseAndLeft(baseElement = b, leftElement = e)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      b -> be,
      e -> be
    )

    // NOTE: we expect a left edit versus right deletion conflict that claims
    // both `c` and `d` on the left, followed by a right-edit of `b` into `f`.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(c, d, f),
        rightElements = Vector(f)
      )

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end leftEditVersusRightDeletionConflictWithFollowingLeftInsertionAndThenRightEdit

  @Test
  def rightEditVersusLeftDeletionConflictDueToFollowingLeftDeletionAndThenRightEdit()
      : Unit =
    val a    = 1
    val b    = 2
    val c    = 3
    val base = Vector(a, b, c)

    val d    = 4
    val left = Vector(d)

    val e     = 5
    val f     = 6
    val g     = 7
    val right = Vector(e, f, g)

    val bf = Match.BaseAndRight(baseElement = b, rightElement = f)
    val cd = Match.BaseAndLeft(baseElement = c, leftElement = d)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      b -> bf,
      f -> bf,
      c -> cd,
      d -> cd
    )

    // NOTE: we expect a clean merge of the deletion of 'b' and the edit of 'c'
    // into 'g' after the initial left edit versus right deletion conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(g),
        rightElements = Vector(e, g)
      )

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end rightEditVersusLeftDeletionConflictDueToFollowingLeftDeletionAndThenRightEdit

  @Test
  def rightEditVersusLeftDeletionConflictWithFollowingRightInsertionAndThenLeftEdit()
      : Unit =
    val a    = 1
    val b    = 2
    val base = Vector(a, b)

    val c    = 3
    val left = Vector(c)

    val d     = 4
    val e     = 5
    val f     = 6
    val right = Vector(d, e, f)

    val bf = Match.BaseAndRight(baseElement = b, rightElement = f)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      b -> bf,
      f -> bf
    )

    // NOTE: we expect a right edit versus left deletion conflict that claims
    // both `d` and `e` on the right, followed by a left-edit of `b` into `c`.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(c),
        rightElements = Vector(d, e, c)
      )

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end rightEditVersusLeftDeletionConflictWithFollowingRightInsertionAndThenLeftEdit

  @Test
  def coincidentEditFollowedByACoincidentInsertion(): Unit =
    val a    = 1
    val base = Vector(a)

    val b    = 2
    val c    = 3
    val left = Vector(b, c)

    val d     = 4
    val e     = 5
    val right = Vector(d, e)

    val bd = Match.LeftAndRight(leftElement = b, rightElement = d)
    val ce = Match.LeftAndRight(leftElement = c, rightElement = e)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      b -> bd,
      d -> bd,
      c -> ce,
      e -> ce
    )

    // NOTE: we expect a a clean merge of a coincident edit coalesced with the
    // following coincident insertion, editing `a` into `b` and `c`.
    val expectedMerge =
      FullyMerged(elements = Vector(b, c))

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result.resolveUsing(guardedLeftBiasedResolution) == expectedMerge)
  end coincidentEditFollowedByACoincidentInsertion

  @TestFactory
  def fullMerge: DynamicTests =
    fullyMergedTestCases
      .withLimit(20000)
      .dynamicTests: testCase =>
        println("*************")
        pprintln(testCase)

        given Eq[Element]    = equivalent(testCase.matchesByElement)
        given Sized[Element] = defaultElementSize

        val AugmentedMergeResult(_, result) =
          merge.of(mergeAlgebra =
            DelegatingMergeAlgebraWithContracts(
              new CoreMergeAlgebra
            )
          )(
            testCase.base,
            testCase.left,
            testCase.right
          ): @unchecked

        testCase.validate(result.resolveUsing(guardedCoinFlippingResolution))

  @TestFactory
  def conflictedMerge: DynamicTests =
    possiblyConflictedMergeTestCases
      .withLimit(4000)
      .dynamicTests: testCase =>
        given Eq[Element]    = equivalent(testCase.matchesByElement)
        given Sized[Element] = defaultElementSize

        val AugmentedMergeResult(_, result) =
          merge.of(mergeAlgebra =
            DelegatingMergeAlgebraWithContracts(
              new CoreMergeAlgebra
            )
          )(
            testCase.base,
            testCase.left,
            testCase.right
          ): @unchecked

        val resolvedResult = result.resolveUsing(guardedCoinFlippingResolution)

        resolvedResult match
          case MergedWithConflicts(_, _) =>
            println("*************")
            pprintln(testCase)

            testCase.validate(resolvedResult)
          case FullyMerged(_) => Trials.reject()
        end match

  def simpleMergeTestCases(
      moveConstraints: MoveConstraints
  )(partialResult: MergeTestCase): Trials[MergeTestCase] =
    val extendedMergeTestCases =
      val zeroRelativeElements: Trials[Element] = trialsApi.uniqueIds

      val choices = moveConstraints.choices

      choices flatMap:
        case LeftInsertion =>
          for
            leftElement <- zeroRelativeElements
            result <- simpleMergeTestCases(moveConstraints.leftInsertion)(
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
            result <- simpleMergeTestCases(moveConstraints.rightInsertion)(
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
            result <- simpleMergeTestCases(moveConstraints.coincidentInsertion):
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
                    FullyMerged(elements :+ coinFlippingResolution.coincident(leftElement, rightElement))
                .focus(_.moves)
                .modify(_ :+ CoincidentInsertion)
          yield result
          end for

        case Preservation =>
          for
            baseElement  <- zeroRelativeElements
            leftElement  <- zeroRelativeElements
            rightElement <- zeroRelativeElements
            result <- simpleMergeTestCases(moveConstraints.preservation):
              val elementMatch =
                Match.AllSides(
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
                      elements :+ coinFlippingResolution.preserved(
                        baseElement,
                        leftElement,
                        rightElement
                      )
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
            result <- simpleMergeTestCases(moveConstraints.leftEdit):
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
            result <- simpleMergeTestCases(moveConstraints.rightEdit):
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

        case CoincidentEdit =>
          for
            baseElement  <- zeroRelativeElements
            leftElement  <- zeroRelativeElements
            rightElement <- zeroRelativeElements
            result <- simpleMergeTestCases(moveConstraints.coincidentEdit):
              val elementMatch = Match.LeftAndRight(
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
                  _ + (leftElement -> elementMatch) + (rightElement -> elementMatch)
                )
                .focus(_.expectedMerge.some)
                .modify:
                  case FullyMerged(elements) =>
                    FullyMerged(
                      elements :+ coinFlippingResolution.coincident(
                        leftElement,
                        rightElement
                      )
                    )
                .focus(_.moves)
                .modify(_ :+ CoincidentEdit)
          yield result
          end for

        case LeftDeletion =>
          for
            baseElement  <- zeroRelativeElements
            rightElement <- zeroRelativeElements
            result <- simpleMergeTestCases(moveConstraints.leftDeletion):
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
            result <- simpleMergeTestCases(moveConstraints.rightDeletion):
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
            result <- simpleMergeTestCases(moveConstraints.coincidentDeletion):
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
        (if moveConstraints.allowConflicts then 500
         else 50) -> extendedMergeTestCases
      )
    )

  end simpleMergeTestCases

end MergeTest

object MergeTest:
  type Element = Int

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
    def validate(result: MergeResult[Element]): Unit =
      def baseIsPreservedCorrectlyIn(
          elements: IndexedSeq[Element]
      ): Set[Element] =
        val preserved = base.collect(baseElement =>
          matchesByElement.get(baseElement) match
            case Some(Match.AllSides(baseElement, leftElement, rightElement)) =>
              coinFlippingResolution.preserved(
                baseElement,
                leftElement,
                rightElement
              )
        )

        val _ = preserved.isSubsequenceOf(elements)

        preserved.toSet

      end baseIsPreservedCorrectlyIn

      def leftAppearsCorrectlyIn(elements: IndexedSeq[Element]): Set[Element] =
        val appears = left.collect(leftElement =>
          matchesByElement.get(leftElement) match
            case Some(Match.AllSides(baseElement, leftElement, rightElement)) =>
              coinFlippingResolution.preserved(
                baseElement,
                leftElement,
                rightElement
              )
            case Some(Match.LeftAndRight(leftElement, rightElement)) =>
              coinFlippingResolution.coincident(leftElement, rightElement)
            case None => leftElement
        )

        val _ = appears.isSubsequenceOf(elements)

        appears.toSet
      end leftAppearsCorrectlyIn

      def rightAppearsCorrectlyIn(elements: IndexedSeq[Element]): Set[Element] =
        val appears = right.collect(rightElement =>
          matchesByElement.get(rightElement) match
            case Some(Match.AllSides(baseElement, leftElement, rightElement)) =>
              coinFlippingResolution.preserved(
                baseElement,
                leftElement,
                rightElement
              )
            case Some(Match.LeftAndRight(leftElement, rightElement)) =>
              coinFlippingResolution.coincident(leftElement, rightElement)
            case None => rightElement
        )

        val _ = appears.isSubsequenceOf(elements)

        appears.toSet
      end rightAppearsCorrectlyIn

      def allPresentAndCorrectIn(result: MergeResult[Element]): Unit =
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
        allPresentAndCorrectIn(merge)

        // This assertion is stronger than `allPresentAndCorrectIn` because it
        // includes the precise merge resolution.
        assert(
          result == merge
        )
      )

    end validate

  end MergeTestCase

  case class MoveConstraints(
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
      priorDeletionOrInsertionBlockingCoincidentEdit: Boolean = false,
      // The next two flags stop misleading move sequences from being generated,
      // where a deletion is followed by an insertion on the same side and the
      // only other moves between them are insertions on the opposite side.
      // Edits already capture this notion more economically.
      priorDeletionBlockingLeftInsertion: Boolean = false,
      priorDeletionBlockingRightInsertion: Boolean = false,
      priorDeletionBlockingCoincidentInsertion: Boolean = false,
      // The next two flags stop misleading move sequences from being generated,
      // where an insertion is followed by a deletion on the same side and the
      // only other moves between them are insertions on the opposite side.
      // Edits already capture this notion more economically.
      priorInsertionBlockingLeftDeletion: Boolean = false,
      priorInsertionBlockingRightDeletion: Boolean = false,
      priorInsertionBlockingCoincidentDeletion: Boolean = false,
      // The next two flags allow move sequences to pair insertions across sides
      // when there is a prior edit that could coalesce with one of the
      // insertions to avoid a conflict.
      priorEditClaimingLeftInsertion: Boolean = false,
      priorEditClaimingRightInsertion: Boolean = false
  ):
    import MoveConstraints.*

    def choices: Trials[Move] = predecessorMove match
      case LeftInsertion =>
        trialsApi
          .chooseWithWeights(
            leftInsertionFrequency(allow = true),
            rightInsertionFrequency(allow = allowConflicts),
            coincidentInsertionFrequency(),
            preservationFrequency(),
            leftEditFrequency(),
            rightEditFrequency(),
            coincidentEditFrequency(),
            leftDeletionFrequency(),
            rightDeletionFrequency()
          )

      case RightInsertion =>
        trialsApi
          .chooseWithWeights(
            leftInsertionFrequency(allow = allowConflicts),
            rightInsertionFrequency(allow = true),
            coincidentInsertionFrequency(),
            preservationFrequency(),
            leftEditFrequency(),
            rightEditFrequency(),
            coincidentEditFrequency(),
            leftDeletionFrequency(),
            rightDeletionFrequency()
          )

      case CoincidentInsertion | Preservation | LeftEdit | RightEdit |
          CoincidentEdit =>
        trialsApi
          .chooseWithWeights(
            leftInsertionFrequency(allow = true),
            rightInsertionFrequency(allow = true),
            coincidentInsertionFrequency(),
            preservationFrequency(),
            leftEditFrequency(),
            rightEditFrequency(),
            coincidentEditFrequency(),
            leftDeletionFrequency(),
            rightDeletionFrequency(),
            coincidentDeletionFrequency()
          )

      case LeftDeletion =>
        trialsApi
          .chooseWithWeights(
            rightInsertionFrequency(allow = true),
            coincidentInsertionFrequency(),
            preservationFrequency(),
            leftEditFrequency(),
            rightEditFrequency(),
            coincidentEditFrequency(),
            leftDeletionFrequency(),
            rightDeletionFrequency(),
            coincidentDeletionFrequency()
          )

      case RightDeletion =>
        trialsApi
          .chooseWithWeights(
            leftInsertionFrequency(allow = true),
            coincidentInsertionFrequency(),
            preservationFrequency(),
            leftEditFrequency(),
            rightEditFrequency(),
            coincidentEditFrequency(),
            leftDeletionFrequency(),
            rightDeletionFrequency(),
            coincidentDeletionFrequency()
          )

      case CoincidentDeletion =>
        trialsApi
          .chooseWithWeights(
            leftInsertionFrequency(allow = allowConflicts),
            rightInsertionFrequency(allow = allowConflicts),
            coincidentInsertionFrequency(),
            preservationFrequency(),
            leftEditFrequency(),
            rightEditFrequency(),
            coincidentEditFrequency(),
            leftDeletionFrequency(),
            rightDeletionFrequency(),
            coincidentDeletionFrequency()
          )

    private def leftInsertionFrequency(allow: Boolean) =
      (if allow && (!priorDeletionBlockingLeftInsertion || priorEditClaimingRightInsertion)
       then 7
       else 0) -> LeftInsertion

    private def rightInsertionFrequency(allow: Boolean) =
      (if allow && (!priorDeletionBlockingRightInsertion || priorEditClaimingLeftInsertion)
       then 7
       else 0) -> RightInsertion

    private def coincidentInsertionFrequency() =
      (if !priorDeletionBlockingCoincidentInsertion then 4
       else 0) -> CoincidentInsertion

    private def preservationFrequency() = 10 -> Preservation

    private def leftEditFrequency() =
      (if !priorDeletionOrInsertionBlockingLeftEdit then 7
       else 0) -> LeftEdit

    private def rightEditFrequency() =
      (if !priorDeletionOrInsertionBlockingRightEdit then 7
       else 0) -> RightEdit

    private def coincidentEditFrequency() =
      (if !priorDeletionOrInsertionBlockingCoincidentEdit then 4
       else 0) -> CoincidentEdit

    private def leftDeletionFrequency() =
      (if !priorInsertionBlockingLeftDeletion then 7
       else 0) -> LeftDeletion

    private def rightDeletionFrequency() =
      (if !priorInsertionBlockingRightDeletion then 7
       else 0) -> RightDeletion

    private def coincidentDeletionFrequency() =
      (if !priorInsertionBlockingCoincidentDeletion then 4
       else 0) -> CoincidentDeletion

    def leftInsertion: MoveConstraints =
      copy(
        predecessorMove = LeftInsertion,
        priorDeletionOrInsertionBlockingLeftEdit = true,
        priorDeletionBlockingLeftInsertion = false,
        priorInsertionBlockingLeftDeletion = true
      )

    def rightInsertion: MoveConstraints =
      copy(
        predecessorMove = RightInsertion,
        priorDeletionOrInsertionBlockingRightEdit = true,
        priorDeletionBlockingRightInsertion = false,
        priorInsertionBlockingRightDeletion = true
      )

    def coincidentInsertion: MoveConstraints =
      MoveConstraints(
        allowConflicts = allowConflicts,
        predecessorMove = CoincidentInsertion,
        priorDeletionOrInsertionBlockingCoincidentEdit = true,
        priorInsertionBlockingCoincidentDeletion = true
      )

    def preservation: MoveConstraints = MoveConstraints(
      allowConflicts = allowConflicts,
      predecessorMove = Preservation
    )

    def leftEdit: MoveConstraints = MoveConstraints(
      allowConflicts = allowConflicts,
      predecessorMove = LeftEdit,
      priorEditClaimingLeftInsertion = true
    )

    def rightEdit: MoveConstraints = MoveConstraints(
      allowConflicts = allowConflicts,
      predecessorMove = RightEdit,
      priorEditClaimingRightInsertion = true
    )

    def coincidentEdit: MoveConstraints = MoveConstraints(
      allowConflicts = allowConflicts,
      predecessorMove = CoincidentEdit
    )

    def leftDeletion: MoveConstraints = MoveConstraints(
      allowConflicts = allowConflicts,
      predecessorMove = LeftDeletion,
      priorDeletionOrInsertionBlockingLeftEdit = true,
      priorDeletionBlockingLeftInsertion = true
    )

    def rightDeletion: MoveConstraints = MoveConstraints(
      allowConflicts = allowConflicts,
      predecessorMove = RightDeletion,
      priorDeletionOrInsertionBlockingRightEdit = true,
      priorDeletionBlockingRightInsertion = true
    )

    def coincidentDeletion: MoveConstraints =
      copy(
        predecessorMove = CoincidentDeletion,
        priorDeletionOrInsertionBlockingCoincidentEdit = true,
        priorDeletionBlockingCoincidentInsertion = true
      )
  end MoveConstraints

  enum Move:
    case LeftInsertion
    case RightInsertion
    case CoincidentInsertion
    case Preservation
    case LeftEdit
    case RightEdit
    case CoincidentEdit
    case LeftDeletion
    case RightDeletion
    case CoincidentDeletion
  end Move

  class DelegatingMergeAlgebraWithContracts(
      coreMergeAlgebra: merge.MergeAlgebra[UnresolvedMergeResult, Element]
  ) extends merge.MergeAlgebra[AugmentedMergeResult, Element]:
    override def empty: AugmentedMergeResult[Element] =
      AugmentedMergeResult(state = State.Neutral, coreMergeAlgebra.empty)

    override def preservation(
        result: AugmentedMergeResult[Element],
        preservedBaseElement: Element,
        preservedElementOnLeft: Element,
        preservedElementOnRight: Element
    ): AugmentedMergeResult[Element] =
      AugmentedMergeResult(
        state = State.Neutral,
        coreMergeResult = coreMergeAlgebra.preservation(
          result.coreMergeResult,
          preservedBaseElement,
          preservedElementOnLeft,
          preservedElementOnRight
        )
      )

    override def leftInsertion(
        result: AugmentedMergeResult[Element],
        insertedElement: Element
    ): AugmentedMergeResult[Element] =
      result.state match
        case State.Neutral | State.RightEdit | State.CoincidentEdit =>
        case _ => Assertions.fail()
      end match
      AugmentedMergeResult(
        state =
          if State.RightEdit == result.state then result.state
          else State.Neutral,
        coreMergeResult = coreMergeAlgebra.leftInsertion(
          result.coreMergeResult,
          insertedElement
        )
      )
    end leftInsertion

    override def rightInsertion(
        result: AugmentedMergeResult[Element],
        insertedElement: Element
    ): AugmentedMergeResult[Element] =
      result.state match
        case State.Neutral | State.LeftEdit | State.CoincidentEdit =>
        case _ => Assertions.fail()
      end match
      AugmentedMergeResult(
        state =
          if State.LeftEdit == result.state then result.state
          else State.Neutral,
        coreMergeResult = coreMergeAlgebra.rightInsertion(
          result.coreMergeResult,
          insertedElement
        )
      )
    end rightInsertion

    override def coincidentInsertion(
        result: AugmentedMergeResult[Element],
        insertedElementOnLeft: Element,
        insertedElementOnRight: Element
    ): AugmentedMergeResult[Element] =
      result.state match
        case State.Conflict | State.Neutral | State.LeftEdit |
            State.RightEdit =>
        case _ => Assertions.fail()
      end match
      AugmentedMergeResult(
        state = State.Neutral,
        coreMergeResult = coreMergeAlgebra.coincidentInsertion(
          result.coreMergeResult,
          insertedElementOnLeft,
          insertedElementOnRight
        )
      )
    end coincidentInsertion

    override def leftDeletion(
        result: AugmentedMergeResult[Element],
        deletedBaseElement: Element,
        deletedRightElement: Element
    ): AugmentedMergeResult[Element] = AugmentedMergeResult(
      state = State.Neutral,
      coreMergeResult = coreMergeAlgebra.leftDeletion(
        result.coreMergeResult,
        deletedBaseElement,
        deletedRightElement
      )
    )

    override def rightDeletion(
        result: AugmentedMergeResult[Element],
        deletedBaseElement: Element,
        deletedLeftElement: Element
    ): AugmentedMergeResult[Element] = AugmentedMergeResult(
      state = State.Neutral,
      coreMergeResult = coreMergeAlgebra.rightDeletion(
        result.coreMergeResult,
        deletedBaseElement,
        deletedLeftElement
      )
    )

    override def coincidentDeletion(
        result: AugmentedMergeResult[Element],
        deletedElement: Element
    ): AugmentedMergeResult[Element] = AugmentedMergeResult(
      state = State.Neutral,
      coreMergeResult = coreMergeAlgebra.coincidentDeletion(
        result.coreMergeResult,
        deletedElement
      )
    )

    override def leftEdit(
        result: AugmentedMergeResult[Element],
        editedBaseElement: Element,
        editedRightElement: Element,
        editElements: IndexedSeq[Element]
    ): AugmentedMergeResult[Element] =
      AugmentedMergeResult(
        state = State.LeftEdit,
        coreMergeResult = coreMergeAlgebra.leftEdit(
          result.coreMergeResult,
          editedBaseElement,
          editedRightElement,
          editElements
        )
      )
    end leftEdit

    override def rightEdit(
        result: AugmentedMergeResult[Element],
        editedBaseElement: Element,
        editedLeftElement: Element,
        editElements: IndexedSeq[Element]
    ): AugmentedMergeResult[Element] = AugmentedMergeResult(
      state = State.RightEdit,
      coreMergeResult = coreMergeAlgebra.rightEdit(
        result.coreMergeResult,
        editedBaseElement,
        editedLeftElement,
        editElements
      )
    )

    override def coincidentEdit(
        result: AugmentedMergeResult[Element],
        editedElement: Element,
        editElements: IndexedSeq[(Element, Element)]
    ): AugmentedMergeResult[Element] = AugmentedMergeResult(
      state = State.CoincidentEdit,
      coreMergeResult = coreMergeAlgebra.coincidentEdit(
        result.coreMergeResult,
        editedElement,
        editElements
      )
    )

    override def conflict(
        result: AugmentedMergeResult[Element],
        editedElements: IndexedSeq[Element],
        leftEditElements: IndexedSeq[Element],
        rightEditElements: IndexedSeq[Element]
    ): AugmentedMergeResult[Element] =
      // Should not follow a conflict directly with another conflict - they
      // should be coalesced instead.
      require(State.Conflict != result.state)
      // This is a conflict, so we can't just have one side with different
      // elements.
      require(
        // Left edit versus right edit...
        editedElements.nonEmpty && leftEditElements.nonEmpty && rightEditElements.nonEmpty ||
          // Left edit versus right deletion...
          editedElements.nonEmpty && leftEditElements.nonEmpty ||
          // Right edit versus left deletion...
          editedElements.nonEmpty && rightEditElements.nonEmpty ||
          // Left insertion versus right insertion...
          leftEditElements.nonEmpty && rightEditElements.nonEmpty
      )

      AugmentedMergeResult(
        state = State.Conflict,
        coreMergeResult = coreMergeAlgebra.conflict(
          result.coreMergeResult,
          editedElements,
          leftEditElements,
          rightEditElements
        )
      )
    end conflict
  end DelegatingMergeAlgebraWithContracts

  object leftBiasedResolution extends LeftBiasedResolution[Element]

  object coinFlippingResolution extends CoinFlippingResolution[Element]

  object DelegatingMergeAlgebraWithContracts:
    // Leave this as an enumeration rather than as a Boolean as left- and
    // right-edits might need to be checked later, too.
    enum State:
      case Neutral
      case Conflict
      case LeftEdit
      case RightEdit
      case CoincidentEdit
    end State

    case class AugmentedMergeResult[Element](
        state: State,
        coreMergeResult: UnresolvedMergeResult[Element]
    ) extends UnresolvedMergeResult[Element]:
      export coreMergeResult.*

  end DelegatingMergeAlgebraWithContracts

  object MoveConstraints:
    def empty(allowConflicts: Boolean): MoveConstraints = MoveConstraints(
      allowConflicts = allowConflicts,
      predecessorMove = Preservation
    )
  end MoveConstraints

end MergeTest
