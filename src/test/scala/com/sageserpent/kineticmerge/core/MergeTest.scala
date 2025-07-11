package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.CoreMergeAlgebra.MultiSidedMergeResult
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.{
  Sized,
  defaultElementSize
}
import com.sageserpent.kineticmerge.core.MergeTest.*
import com.sageserpent.kineticmerge.core.MergeTest.DelegatingMergeAlgebraWithContracts.{
  AugmentedMergeResult,
  State
}
import com.sageserpent.kineticmerge.core.MergeTest.Move.*
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a clean merge of the edit of `a` into `c`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(c)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize
    
    // NOTE: we expect a clean merge of the edit of `a` into `d` followed by an
    // insertion of `c`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(d), MultiSided.Unique(c)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a clean merge of the insertion of `b` followed by a
    // deletion of `a`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(b)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a clean merge of the insertion of `b` followed by an edit
    // of `a` into `d`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(b), MultiSided.Unique(d)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a clean merge of the coincident deletion of `a` followed
    // by the edit of `b` into `d`
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(d)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a clean merge of the coincident deletion of `a` with a
    // coincident insertion of `b` and `c`, followed by insertion of `d`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Coincident(b, c), MultiSided.Unique(d)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize
    
    // NOTE: we expect a clean merge of the insertion of `c`, followed by a
    // coincident deletion of `a` with a coincident insertion of `b` and `d`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(c), MultiSided.Coincident(b, d)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

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
    
    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a clean merge of the edit of `a` into `e` followed by a
    // deletion of `b`. Merging is supposed to look eagerly for edits, so `e` is
    // considered to be an edit of `a` rather than of `b`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(e)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a clean merge of the edit of `a` into `f` followed by a
    // deletion of `b` and then an insertion of `e`. Merging is supposed to look
    // eagerly for edits, so `f` is considered to be an edit of `a` rather than
    // of `b`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(f), MultiSided.Unique(e)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

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

    given Eq[Element]    = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a clean merge of the edit of `a` into `f` followed by an
    // insertion of `d` and finally deletion of `b`. Merging is supposed to look
    // eagerly for edits, so `f` is considered to be an edit of `a` rather than
    // of `b`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(f), MultiSided.Unique(d)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a clean merge of the edit of `a` into `f` followed by an
    // insertion of `d` and finally a merge of the edit of `b` into `g`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(f), MultiSided.Unique(d), MultiSided.Unique(g)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a clean merge of the edit of `a` into `c` coalesced with
    // an insertion of `d`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(c), MultiSided.Unique(d)))
    
    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize
    
    // NOTE: we expect a clean merge of the edit of `a` into `d` coalesced with
    // an insertion of `e` followed by an insertion of `c`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(d), MultiSided.Unique(e), MultiSided.Unique(c)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize
    
    // NOTE: we expect a clean merge of the left edit of `a` into `c` coalesced
    // with the left insertion of `d` and finally a coincident edit of `b` into
    // `e` and `g`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(c), MultiSided.Unique(d), MultiSided.Coincident(e, g)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize
    
    // NOTE: we expect a clean merge of the edit of `a` into `d` followed by a
    // deletion of `b`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(d)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize
    
    // NOTE: we expect a clean merge of the deletion of `a` followed by an
    // insertion of `c`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(c)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize
    
    // NOTE: we expect a clean merge of the deletion of `a` followed by a
    // deletion of `b`.
    val expectedMerge = FullyMerged(elements = Vector())

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize
    
    // NOTE: we expect a clean merge of the deletion of 'a' followed by an edit
    // of `b` into `d` followed by a deletion of `b`.
    val expectedMerge = FullyMerged(elements = Vector(MultiSided.Unique(d)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect `b` to be preserved as `d` and `f` after the initial edit conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Unique(c), MultiSided.Preserved(b, d, f)),
        rightElements = Vector(MultiSided.Unique(e), MultiSided.Preserved(b, d, f))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a `c` to be preserved as `f` and `i` after the two separate edit conflicts.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Unique(d), MultiSided.Unique(e), MultiSided.Preserved(c, f, i)),
        rightElements = Vector(MultiSided.Unique(g), MultiSided.Unique(h), MultiSided.Preserved(c, f, i))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a right edit of `b` into `g` and then `h` after the initial left edit versus right deletion conflict, followed by `c` being preserved as `f` and `i`.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Unique(d), MultiSided.Unique(g), MultiSided.Unique(h), MultiSided.Preserved(c, f, i)),
        rightElements = Vector(MultiSided.Unique(g), MultiSided.Unique(h), MultiSided.Preserved(c, f, i))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

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
    val cfi = Match.AllSides(baseElement = c, leftElement = f, rightElement = i)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      b -> bh,
      h -> bh,
      c -> cfi,
      f -> cfi,
      i -> cfi
    )

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a left edit of `b` into `d` and then `e` after the initial right edit versus left deletion conflict, followed by `c` being preserved as `f` and `i`.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Unique(d), MultiSided.Unique(e), MultiSided.Preserved(c, f, i)),
        rightElements = Vector(MultiSided.Unique(g), MultiSided.Unique(d), MultiSided.Unique(e), MultiSided.Preserved(c, f, i))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect `a` to be preserved as `c` and `g` before the conflict, with `b` being preserved as `f` and `i` after.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Preserved(a, c, g), MultiSided.Unique(d), MultiSided.Unique(e), MultiSided.Preserved(b, f, i)),
        rightElements = Vector(MultiSided.Preserved(a, c, g), MultiSided.Unique(h), MultiSided.Preserved(b, f, i))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect `a` to be preserved as `d` and `h` before the coalesced conflict, with `c` to be preserved as `g` and `i` after.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Preserved(a, d, h), MultiSided.Unique(e), MultiSided.Unique(f), MultiSided.Preserved(c, g, i)),
        rightElements = Vector(MultiSided.Preserved(a, d, h), MultiSided.Preserved(c, g, i))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
  end leftEditVersusRightDeletionConflictWithFollowingLeftInsertion

  @Test
  def leftEditVersusRightDeletionConflictWithFollowingCoincidentDeletion(): Unit =
    val a = 1
    val b = 2
    val c = 3
    val d = 4
    val base = Vector(a, b, c, d)

    val e = 5
    val f = 6
    val h = 7
    val left = Vector(e, f, h)

    val i = 8
    val j = 9
    val right = Vector(i, j)

    val aei = Match.AllSides(baseElement = a, leftElement = e, rightElement = i)
    val dhj = Match.AllSides(baseElement = d, leftElement = h, rightElement = j)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      a -> aei,
      e -> aei,
      i -> aei,
      d -> dhj,
      h -> dhj,
      j -> dhj
    )

    given Eq[Element] = matchesByElement.equivalent

    given Sized[Element] = defaultElementSize

    // NOTE: we expect `a` to be preserved as `e` and `i` before the coalesced conflict, with `d` to be preserved as `h` and `j` after the initial conflict coalesces with the coincident deletion of `c`.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Preserved(a, e, i), MultiSided.Unique(f), MultiSided.Preserved(d, h, j)),
        rightElements = Vector(MultiSided.Preserved(a, e, i), MultiSided.Preserved(d, h, j))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
  end leftEditVersusRightDeletionConflictWithFollowingCoincidentDeletion

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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect `a` to be preserved as `c` and `e` after the initial conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Unique(b), MultiSided.Preserved(a, c, e)),
        rightElements = Vector(MultiSided.Unique(d), MultiSided.Preserved(a, c, e))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect `a` to be preserved as `d` and `g` after the coalesced insertion conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Unique(b), MultiSided.Unique(c), MultiSided.Preserved(a, d, g)),
        rightElements = Vector(MultiSided.Unique(e), MultiSided.Unique(f), MultiSided.Preserved(a, d, g))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a coincident insertion of `d` and `g` and then `b` to be preserved as `e` and `h` after the initial edit conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Unique(c), MultiSided.Coincident(d, g), MultiSided.Preserved(b, e, h)),
        rightElements = Vector(MultiSided.Unique(f), MultiSided.Coincident(d, g), MultiSided.Preserved(b, e, h))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect `c` to be preserved as `e` and `g` after the initial edit conflict coalesces with the coincident deletion of `b`.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Unique(d), MultiSided.Preserved(c, e, g)),
        rightElements = Vector(MultiSided.Unique(f), MultiSided.Preserved(c, e, g))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect the initial edit conflict to coalesce with the following
    // insertion conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Unique(b), MultiSided.Unique(c)),
        rightElements = Vector(MultiSided.Unique(d), MultiSided.Unique(e))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect the initial edit conflict to coalesce with the following
    // insertion conflict, then be followed by the coincident insertion of `d` and `f`.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Unique(b), MultiSided.Unique(c), MultiSided.Coincident(d, f)),
        rightElements = Vector(MultiSided.Unique(e), MultiSided.Coincident(d, f))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a clean merge of the deletion of 'b' and the edit of 'c'
    // into 'f' after the initial left edit versus right deletion conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Unique(d), MultiSided.Unique(f)),
        rightElements = Vector(MultiSided.Unique(f))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a left edit versus right deletion conflict that claims
    // both `c` and `d` on the left, followed by a right-edit of `b` into `f`.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Unique(c), MultiSided.Unique(d), MultiSided.Unique(f)),
        rightElements = Vector(MultiSided.Unique(f))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
  end leftEditVersusRightDeletionConflictWithFollowingLeftInsertionAndThenRightEdit

  @Test
  def rightEditVersusLeftDeletionConflictWithFollowingCoincidentDeletion(): Unit =
    val a = 1
    val b = 2
    val c = 3
    val d = 4
    val base = Vector(a, b, c, d)

    val e = 5
    val f = 6
    val left = Vector(e, f)

    val g = 7
    val h = 8
    val i = 9
    val right = Vector(g, h, i)

    val aeg = Match.AllSides(baseElement = a, leftElement = e, rightElement = g)
    val dfi = Match.AllSides(baseElement = d, leftElement = f, rightElement = i)
    val matchesByElement: Map[Element, Match[Element]] = Map(
      a -> aeg,
      e -> aeg,
      g -> aeg,
      d -> dfi,
      f -> dfi,
      i -> dfi
    )

    given Eq[Element] = matchesByElement.equivalent

    given Sized[Element] = defaultElementSize

    // NOTE: we expect `a` to be preserved as `e` and `g` before the coalesced conflict, with `d` to be preserved as `f` and `i` after the initial conflict coalesces with the coincident deletion of `c`.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Preserved(a, e, g), MultiSided.Preserved(d, f, i)),
        rightElements = Vector(MultiSided.Preserved(a, e, g), MultiSided.Unique(h), MultiSided.Preserved(d, f, i))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
  end rightEditVersusLeftDeletionConflictWithFollowingCoincidentDeletion


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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a clean merge of the deletion of 'b' and the edit of 'c'
    // into 'g' after the initial left edit versus right deletion conflict.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Unique(g)),
        rightElements = Vector(MultiSided.Unique(e), MultiSided.Unique(g))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a right edit versus left deletion conflict that claims
    // both `d` and `e` on the right, followed by a left-edit of `b` into `c`.
    val expectedMerge =
      MergedWithConflicts(
        leftElements = Vector(MultiSided.Unique(c)),
        rightElements = Vector(MultiSided.Unique(d), MultiSided.Unique(e), MultiSided.Unique(c))
      )

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

    given Eq[Element] = matchesByElement.equivalent
    given Sized[Element] = defaultElementSize

    // NOTE: we expect a clean merge of a coincident edit coalesced with the
    // following coincident insertion, editing `a` into `b` and `d` and then `c` and `e`.
    val expectedMerge =
      FullyMerged(elements = Vector(MultiSided.Coincident(b, d), MultiSided.Coincident(c, e)))

    val AugmentedMergeResult(_, result) =
      merge.of(mergeAlgebra =
        DelegatingMergeAlgebraWithContracts(
          new CoreMergeAlgebra
        )
      )(base, left, right): @unchecked

    assert(result == expectedMerge)
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

        testCase.validate(result)

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

        result match
          case MergedWithConflicts(_, _) =>
            println("*************")
            pprintln(testCase)

            testCase.validate(result)
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
                    FullyMerged(elements :+ MultiSided.Unique(leftElement))
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
                    FullyMerged(elements :+ MultiSided.Unique(rightElement))
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
                    FullyMerged(elements :+ MultiSided.Coincident(leftElement, rightElement)
                      // As we're defining the match between the left and right elements at the same time anyway, we just jemmy the equivalence.
                      (using (_, _) => true))
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
                      elements :+ MultiSided.Preserved(
                        baseElement,
                        leftElement,
                        rightElement
                      )
                      // As we're defining the match between the base, left and right elements at the same time anyway, we just jemmy the equivalence.
                      (using (_, _) => true)
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
                    FullyMerged(elements :+ MultiSided.Unique(leftElement))
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
                    FullyMerged(elements :+ MultiSided.Unique(rightElement))
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
                      elements :+ MultiSided.Coincident(
                        leftElement,
                        rightElement
                      ) 
                      // As we're defining the match between the left and right elements at the same time anyway, we just jemmy the equivalence.
                      (using (_, _) => true)
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

  extension (matchesByElement: Map[Element, Match[Element]])
    def equivalent(lhs: Element, rhs: Element): Boolean =
      matchesByElement.get(lhs) -> matchesByElement.get(rhs) match
        case (None, None) => false
        case (None, Some(_)) => false
        case (Some(_), None) => false
        case (Some(lhsMatch), Some(rhsMatch)) =>
          lhsMatch == rhsMatch
      end match
    end equivalent
  end extension

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
      expectedMerge: Option[FullyMerged[MultiSided[Element]]],
      moves: IndexedSeq[Move]
  ):
    given Eq[Element] = equivalent(matchesByElement)
    
    def validate(result: MultiSidedMergeResult[Element]): Unit =
      def baseIsPreservedCorrectlyIn(
          elements: IndexedSeq[MultiSided[Element]]
      ): Set[MultiSided[Element]] =
        val preserved = base.collect(baseElement =>
          matchesByElement.get(baseElement) match
            case Some(Match.AllSides(baseElement, leftElement, rightElement)) =>
              MultiSided.Preserved(
                baseElement,
                leftElement,
                rightElement
              )
        )

        val _ = preserved.isSubsequenceOf(elements)

        preserved.toSet

      end baseIsPreservedCorrectlyIn

      def leftAppearsCorrectlyIn(elements: IndexedSeq[MultiSided[Element]]): Set[MultiSided[Element]] =
        val appears = left.collect(leftElement =>
          matchesByElement.get(leftElement) match
            case Some(Match.AllSides(baseElement, leftElement, rightElement)) =>
              MultiSided.Preserved(
                baseElement,
                leftElement,
                rightElement
              )
            case Some(Match.LeftAndRight(leftElement, rightElement)) =>
              MultiSided.Coincident(leftElement, rightElement)
            case None => MultiSided.Unique(leftElement)
        )

        val _ = appears.isSubsequenceOf(elements)

        appears.toSet
      end leftAppearsCorrectlyIn

      def rightAppearsCorrectlyIn(elements: IndexedSeq[MultiSided[Element]]): Set[MultiSided[Element]] =
        val appears = right.collect(rightElement =>
          matchesByElement.get(rightElement) match
            case Some(Match.AllSides(baseElement, leftElement, rightElement)) =>
              MultiSided.Preserved(
                baseElement,
                leftElement,
                rightElement
              )
            case Some(Match.LeftAndRight(leftElement, rightElement)) =>
              MultiSided.Coincident(leftElement, rightElement)
            case None => MultiSided.Unique(rightElement)
        )

        val _ = appears.isSubsequenceOf(elements)

        appears.toSet
      end rightAppearsCorrectlyIn

      def allPresentAndCorrectIn(result: MultiSidedMergeResult[Element]): Unit =
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
      coreMergeAlgebra: merge.MergeAlgebra[MultiSidedMergeResult, Element]
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
    ): AugmentedMergeResult[Element] =
      // Should not follow a conflict directly with a coincident deletion - they should be coalesced instead.
      require(State.Conflict != result.state)
      
      AugmentedMergeResult(
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

  object DelegatingMergeAlgebraWithContracts:
    enum State:
      case Neutral
      case Conflict
      case LeftEdit
      case RightEdit
      case CoincidentEdit
    end State

    case class AugmentedMergeResult[Element](
        state: State,
        coreMergeResult: MultiSidedMergeResult[Element]
    ) extends MultiSidedMergeResult[Element]:
      export coreMergeResult.*

  end DelegatingMergeAlgebraWithContracts

  object MoveConstraints:
    def empty(allowConflicts: Boolean): MoveConstraints = MoveConstraints(
      allowConflicts = allowConflicts,
      predecessorMove = Preservation
    )
  end MoveConstraints

end MergeTest
