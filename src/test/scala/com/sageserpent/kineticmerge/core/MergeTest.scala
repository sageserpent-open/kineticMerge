package com.sageserpent.kineticmerge.core

import cats.syntax.all.*
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.Merge.Result
import com.sageserpent.kineticmerge.core.Merge.Result.*
import com.sageserpent.kineticmerge.core.MergeTest.*
import monocle.syntax.all.*
import org.junit.jupiter.api.{Test, TestFactory}
import pprint.*

import scala.collection.mutable.Map as MutableMap

import ExpectyFlavouredAssert.assert

class MergeTest:
  private val fullyMergedTestCases: Trials[MergeTestCase] =
    simpleMergeTestCases(allowConflicts = false)(partialResult =
      emptyMergeTestCase(allowConflicts = false)
    )

  private val possiblyConflictedMergeTestCases: Trials[MergeTestCase] =
    simpleMergeTestCases(allowConflicts = true)(partialResult =
      emptyMergeTestCase(allowConflicts = true)
    )

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
      Merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end editConflict

  @Test
  def simpleMergeOfAnEdit(): Unit =
    val a    = 1
    val base = Vector(a)

    val b    = 1
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
      Merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end simpleMergeOfAnEdit

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
      Merge.of(base, left, right)(
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
      Merge.of(base, left, right)(
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
      Merge.of(base, left, right)(
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
      Merge.of(base, left, right)(
        equivalent(matchesByElement)
      ): @unchecked

    assert(result == expectedMerge)
  end editConflictFollowedByCoincidentInsertion

  /* Test ideas:
   * 1. Start with a merged sequence of elements and confabulate base, left and
   * right sequences by diverting each element into just the left (a left
   * insertion), just the right (a right insertion) or both the left and right
   * (coincident insertion) or all three (preservation). In a similar vein,
   * additional elements can be added into the base and left (right deletion),
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
   * elsewhere of the same element to create a move.
   *
   * 4. Same as #1, only combine coincident deletions with left or right
   * insertions and associate them with an insertion elsewhere of the inserted
   * element to create an edited move.
   *
   * 5. Same as #1, only associate coincident deletions with an insertion
   * elsewhere of the same element in the same place in the left and right to
   * create a coincident move. A coincident deletion may be combined with a left
   * / right or coincident insertion that is *not* treated as an edit of either
   * move.
   *
   * 6. Same as #5, only move to different locations to make divergence. A
   * coincident deletion may be combined with a left / right or coincident
   * insertion that is *not* treated as an edit of either move.
   *
   * NOTE: have to create a synthetic match for a element that is present in
   * more than one input, with a dominant element that should appear in the
   * merged result.
   *
   * An easier way to generate the test cases might be to make triples of
   * optional elements, filtering out (None, None, None). Each element appearing
   * in a triple can be put into a match if desired, and that match be made to
   * yield a mocked dominant element that goes into the expected output. */

  @TestFactory
  def fullMerge: DynamicTests =
    fullyMergedTestCases
      .withLimit(2000)
      .dynamicTests: testCase =>
        println("*************")
        pprintln(testCase)

        val Right(result) =
          Merge.of(testCase.base, testCase.left, testCase.right)(
            equivalent(testCase.matchesByElement)
          ): @unchecked

        testCase.validate(result)

  @TestFactory
  def conflictedMerge: DynamicTests =
    possiblyConflictedMergeTestCases
      .withLimit(2000)
      .dynamicTests: testCase =>
        val Right(result) =
          Merge.of(testCase.base, testCase.left, testCase.right)(
            equivalent(testCase.matchesByElement)
          ): @unchecked

        result match
          case Result.MergedWithConflicts(_, _) =>
            println("*************")
            pprintln(testCase)

            testCase.validate(result)
          case Result.FullyMerged(_) => Trials.reject()
        end match

  def simpleMergeTestCases(
      allowConflicts: Boolean,
      predecessorBias: MoveBias = MoveBias.Neutral,
      precedingLeftDeletions: Boolean = false,
      precedingRightDeletions: Boolean = false
  )(partialResult: MergeTestCase): Trials[MergeTestCase] =
    val extendedMergeTestCases =
      def zeroRelativeElements: Trials[Element] =
        // Using the complexity provides unique element labels.
        for
          complexity <- trialsApi.complexities
          _ <- trialsApi.choose(
            Iterable.single(0)
          ) // NASTY HACK - force an increase in complexity so that successive calls do not yield the same label.
        yield complexity
        end for
      end zeroRelativeElements

      val choices = predecessorBias match
        case _ if allowConflicts =>
          trialsApi
            .chooseWithWeights(
              leftInsertionFrequency(allow = true),
              rightInsertionFrequency(allow = true),
              coincidentInsertionFrequency,
              preservationFrequency,
              leftDeletionFrequency,
              rightDeletionFrequency,
              coincidentDeletionFrequency
            )

        case MoveBias.Left =>
          trialsApi
            .chooseWithWeights(
              leftInsertionFrequency(allow = !precedingRightDeletions),
              coincidentInsertionFrequency,
              preservationFrequency,
              leftDeletionFrequency,
              rightDeletionFrequency
            )
        case MoveBias.Right =>
          trialsApi
            .chooseWithWeights(
              rightInsertionFrequency(allow = !precedingLeftDeletions),
              coincidentInsertionFrequency,
              preservationFrequency,
              leftDeletionFrequency,
              rightDeletionFrequency
            )
        case MoveBias.Neutral =>
          trialsApi
            .chooseWithWeights(
              leftInsertionFrequency(allow = !precedingRightDeletions),
              rightInsertionFrequency(allow = !precedingLeftDeletions),
              coincidentInsertionFrequency,
              preservationFrequency,
              leftDeletionFrequency,
              rightDeletionFrequency,
              coincidentDeletionFrequency
            )
        case MoveBias.CoincidentDeletion =>
          trialsApi
            .chooseWithWeights(
              coincidentInsertionFrequency,
              preservationFrequency,
              coincidentDeletionFrequency
            )
      choices flatMap:
        case Move.LeftInsertion =>
          for
            leftElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorBias = MoveBias.Left,
              precedingLeftDeletions = precedingLeftDeletions
            )(
              partialResult
                .focus(_.left)
                .modify(_ :+ leftElement)
                .focus(_.expectedMerge.some)
                .modify:
                  case Result.FullyMerged(elements) =>
                    Result.FullyMerged(elements :+ leftElement)
                .focus(_.moves)
                .modify(_ :+ Move.LeftInsertion)
            )
          yield result
          end for

        case Move.RightInsertion =>
          for
            rightElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorBias = MoveBias.Right,
              precedingRightDeletions = precedingRightDeletions
            )(
              partialResult
                .focus(_.right)
                .modify(_ :+ rightElement)
                .focus(_.expectedMerge.some)
                .modify:
                  case Result.FullyMerged(elements) =>
                    Result.FullyMerged(elements :+ rightElement)
                .focus(_.moves)
                .modify(_ :+ Move.RightInsertion)
            )
          yield result
          end for

        case Move.CoincidentInsertion =>
          for
            leftElement  <- zeroRelativeElements
            rightElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorBias = MoveBias.Neutral
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
                  case Result.FullyMerged(elements) =>
                    Result.FullyMerged(elements :+ elementMatch.dominantElement)
                .focus(_.moves)
                .modify(_ :+ Move.CoincidentInsertion)
          yield result
          end for

        case Move.Preservation =>
          for
            baseElement  <- zeroRelativeElements
            leftElement  <- zeroRelativeElements
            rightElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorBias = MoveBias.Neutral
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
                  case Result.FullyMerged(elements) =>
                    Result.FullyMerged(
                      elements :+ elementMatch.dominantElement
                    )
                .focus(_.moves)
                .modify(_ :+ Move.Preservation)
          yield result
          end for

        case Move.LeftDeletion =>
          for
            baseElement  <- zeroRelativeElements
            rightElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorBias = MoveBias.Neutral,
              precedingLeftDeletions = true,
              precedingRightDeletions = precedingRightDeletions
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
                .modify(_ :+ Move.LeftDeletion)
          yield result
          end for

        case Move.RightDeletion =>
          for
            baseElement <- zeroRelativeElements
            leftElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorBias = MoveBias.Neutral,
              precedingLeftDeletions = precedingLeftDeletions,
              precedingRightDeletions = true
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
                .modify(_ :+ Move.RightDeletion)
          yield result
          end for

        case Move.CoincidentDeletion =>
          for
            baseElement <- zeroRelativeElements
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorBias = MoveBias.CoincidentDeletion,
              precedingLeftDeletions = precedingLeftDeletions,
              precedingRightDeletions = precedingRightDeletions
            ):
              partialResult
                .focus(_.base)
                .modify(_ :+ baseElement)
                .focus(_.moves)
                .modify(_ :+ Move.CoincidentDeletion)
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

  private val leftInsertionFrequency       = 7  -> Move.LeftInsertion
  private val rightInsertionFrequency      = 7  -> Move.RightInsertion
  private val coincidentInsertionFrequency = 4  -> Move.CoincidentInsertion
  private val preservationFrequency        = 10 -> Move.Preservation
  private val leftDeletionFrequency        = 7  -> Move.LeftDeletion
  private val rightDeletionFrequency       = 7  -> Move.RightDeletion
  private val coincidentDeletionFrequency  = 4  -> Move.CoincidentDeletion

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
        Result.FullyMerged(elements = IndexedSeq.empty)
      },
      moves = IndexedSeq.empty
    )

  private def leftInsertionFrequency(allow: Boolean) =
    (if allow then 7 else 0) -> Move.LeftInsertion

  private def rightInsertionFrequency(allow: Boolean) =
    (if allow then 7 else 0) -> Move.RightInsertion

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

  object FakeSection:
    private val startOffsetCache: MutableMap[Int, Int] = MutableMap.empty
  end FakeSection

end MergeTest
