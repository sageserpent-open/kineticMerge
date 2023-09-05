package com.sageserpent.kineticmerge.core

import cats.syntax.all.*
import com.eed3si9n.expecty
import com.eed3si9n.expecty.Expecty
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Contribution
import com.sageserpent.kineticmerge.core.Merge.Result
import com.sageserpent.kineticmerge.core.Merge.Result.*
import com.sageserpent.kineticmerge.core.MergeTest.*
import com.sageserpent.kineticmerge.core.MergeTest.FakeSection.startOffsetCache
import monocle.syntax.all.*
import org.junit.jupiter.api.{DynamicTest, Test, TestFactory}
import pprint.*

import scala.collection.mutable.Map as MutableMap

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
    val a    = FakeSection(zeroRelativeLabel = 1)
    val b    = FakeSection(zeroRelativeLabel = 2)
    val base = Vector(a, b)

    val c    = FakeSection(zeroRelativeLabel = 3)
    val d    = FakeSection(zeroRelativeLabel = 4)
    val left = Vector(c, d)

    val e     = FakeSection(zeroRelativeLabel = 5)
    val f     = FakeSection(zeroRelativeLabel = 6)
    val right = Vector(e, f)

    val bdf =
      Match.AllThree(baseElement = b, leftElement = d, rightElement = f)
    val matchesBySection: Map[Section, Match[Section]] = Map(
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
        matchesBySection.get
      ): @unchecked

    assert(result == expectedMerge)
  end editConflict

  @Test
  def leftEditVersusRightDeletionConflictDueToFollowingRightEdit(): Unit =
    val a    = FakeSection(zeroRelativeLabel = 1)
    val b    = FakeSection(zeroRelativeLabel = 2)
    val c    = FakeSection(zeroRelativeLabel = 3)
    val base = Vector(a, b, c)

    val d    = FakeSection(zeroRelativeLabel = 4)
    val e    = FakeSection(zeroRelativeLabel = 5)
    val f    = FakeSection(zeroRelativeLabel = 6)
    val left = Vector(d, e, f)

    val g     = FakeSection(zeroRelativeLabel = 7)
    val h     = FakeSection(zeroRelativeLabel = 8)
    val i     = FakeSection(zeroRelativeLabel = 9)
    val right = Vector(g, h, i)

    val be  = Match.BaseAndLeft(baseElement = b, leftElement = e)
    val cfi = Match.AllThree(baseElement = c, leftElement = f, rightElement = i)
    val matchesBySection: Map[Section, Match[Section]] = Map(
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
        matchesBySection.get
      ): @unchecked

    assert(result == expectedMerge)
  end leftEditVersusRightDeletionConflictDueToFollowingRightEdit

  @Test
  def rightEditVersusLeftDeletionConflictDueToFollowingLeftEdit(): Unit =
    val a    = FakeSection(zeroRelativeLabel = 1)
    val b    = FakeSection(zeroRelativeLabel = 2)
    val c    = FakeSection(zeroRelativeLabel = 3)
    val base = Vector(a, b, c)

    val d    = FakeSection(zeroRelativeLabel = 4)
    val e    = FakeSection(zeroRelativeLabel = 5)
    val f    = FakeSection(zeroRelativeLabel = 6)
    val left = Vector(d, e, f)

    val g     = FakeSection(zeroRelativeLabel = 7)
    val h     = FakeSection(zeroRelativeLabel = 8)
    val i     = FakeSection(zeroRelativeLabel = 9)
    val right = Vector(g, h, i)

    val bh  = Match.BaseAndRight(baseElement = b, rightElement = h)
    val cfi = Match.AllThree(baseElement = c, leftElement = f, rightElement = i)
    val matchesBySection: Map[Section, Match[Section]] = Map(
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
        matchesBySection.get
      ): @unchecked

    assert(result == expectedMerge)
  end rightEditVersusLeftDeletionConflictDueToFollowingLeftEdit

  @Test
  def insertionConflict(): Unit =
    val a    = FakeSection(zeroRelativeLabel = 1)
    val base = Vector(a)

    val b    = FakeSection(zeroRelativeLabel = 2)
    val c    = FakeSection(zeroRelativeLabel = 3)
    val left = Vector(b, c)

    val d     = FakeSection(zeroRelativeLabel = 4)
    val e     = FakeSection(zeroRelativeLabel = 5)
    val right = Vector(d, e)

    val ace =
      Match.AllThree(baseElement = a, leftElement = c, rightElement = e)
    val matchesBySection: Map[Section, Match[Section]] = Map(
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
        matchesBySection.get
      ): @unchecked

    assert(result == expectedMerge)
  end insertionConflict

  @Test
  def editConflictFollowedByCoincidentInsertion(): Unit =
    val a    = FakeSection(zeroRelativeLabel = 1)
    val b    = FakeSection(zeroRelativeLabel = 2)
    val base = Vector(a, b)

    val c    = FakeSection(zeroRelativeLabel = 3)
    val d    = FakeSection(zeroRelativeLabel = 4)
    val e    = FakeSection(zeroRelativeLabel = 5)
    val left = Vector(c, d, e)

    val f     = FakeSection(zeroRelativeLabel = 6)
    val g     = FakeSection(zeroRelativeLabel = 7)
    val h     = FakeSection(zeroRelativeLabel = 8)
    val right = Vector(f, g, h)

    val dg = Match.LeftAndRight(leftElement = d, rightElement = g)

    val beh =
      Match.AllThree(baseElement = b, leftElement = e, rightElement = h)
    val matchesBySection: Map[Section, Match[Section]] = Map(
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
        matchesBySection.get
      ): @unchecked

    assert(result == expectedMerge)
  end editConflictFollowedByCoincidentInsertion

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

  @TestFactory
  def fullMerge: DynamicTests =
    fullyMergedTestCases
      .withLimit(2000)
      .dynamicTests: testCase =>
        println("*************")
        pprint.pprintln(testCase)

        val Right(result) =
          Merge.of(testCase.base, testCase.left, testCase.right)(
            testCase.matchesBySection.get
          ): @unchecked

        testCase.validate(result)

  @TestFactory
  def conflictedMerge: DynamicTests =
    possiblyConflictedMergeTestCases
      .withLimit(2000)
      .dynamicTests: testCase =>
        val Right(result) =
          Merge.of(testCase.base, testCase.left, testCase.right)(
            testCase.matchesBySection.get
          ): @unchecked

        result match
          case Result.MergedWithConflicts(_, _) =>
            println("*************")
            pprint.pprintln(testCase)

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
      def zeroRelativeSections: Trials[Section] =
        // Using the complexity provides unique section labels.
        for
          complexity <- trialsApi.complexities
          _ <- trialsApi.choose(
            Iterable.single(0)
          ) // NASTY HACK - force an increase in complexity so that successive calls do not yield the same label.
        yield FakeSection(complexity)
        end for
      end zeroRelativeSections

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
            leftSection <- zeroRelativeSections
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorBias = MoveBias.Left,
              precedingLeftDeletions = precedingLeftDeletions
            )(
              partialResult
                .focus(_.left)
                .modify(_ :+ leftSection)
                .focus(_.expectedMerge.some)
                .modify:
                  case Result.FullyMerged(sections) =>
                    Result.FullyMerged(sections :+ leftSection)
                .focus(_.moves)
                .modify(_ :+ Move.LeftInsertion)
            )
          yield result
          end for

        case Move.RightInsertion =>
          for
            rightSection <- zeroRelativeSections
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorBias = MoveBias.Right,
              precedingRightDeletions = precedingRightDeletions
            )(
              partialResult
                .focus(_.right)
                .modify(_ :+ rightSection)
                .focus(_.expectedMerge.some)
                .modify:
                  case Result.FullyMerged(sections) =>
                    Result.FullyMerged(sections :+ rightSection)
                .focus(_.moves)
                .modify(_ :+ Move.RightInsertion)
            )
          yield result
          end for

        case Move.CoincidentInsertion =>
          for
            leftSection  <- zeroRelativeSections
            rightSection <- zeroRelativeSections
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorBias = MoveBias.Neutral
            ):
              val sectionMatch = Match.LeftAndRight(
                leftElement = leftSection,
                rightElement = rightSection
              )

              partialResult
                .focus(_.left)
                .modify(_ :+ leftSection)
                .focus(_.right)
                .modify(_ :+ rightSection)
                .focus(_.matchesBySection)
                .modify(
                  _ + (leftSection -> sectionMatch) + (rightSection -> sectionMatch)
                )
                .focus(_.expectedMerge.some)
                .modify:
                  case Result.FullyMerged(sections) =>
                    Result.FullyMerged(sections :+ sectionMatch.dominantElement)
                .focus(_.moves)
                .modify(_ :+ Move.CoincidentInsertion)
          yield result
          end for

        case Move.Preservation =>
          for
            baseSection  <- zeroRelativeSections
            leftSection  <- zeroRelativeSections
            rightSection <- zeroRelativeSections
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorBias = MoveBias.Neutral
            ):
              val sectionMatch =
                Match.AllThree(
                  baseElement = baseSection,
                  leftElement = leftSection,
                  rightElement = rightSection
                )

              partialResult
                .focus(_.base)
                .modify(_ :+ baseSection)
                .focus(_.left)
                .modify(_ :+ leftSection)
                .focus(_.right)
                .modify(_ :+ rightSection)
                .focus(_.matchesBySection)
                .modify(
                  _ + (baseSection -> sectionMatch) + (leftSection -> sectionMatch) + (rightSection -> sectionMatch)
                )
                .focus(_.expectedMerge.some)
                .modify:
                  case Result.FullyMerged(sections) =>
                    Result.FullyMerged(
                      sections :+ sectionMatch.dominantElement
                    )
                .focus(_.moves)
                .modify(_ :+ Move.Preservation)
          yield result
          end for

        case Move.LeftDeletion =>
          for
            baseSection  <- zeroRelativeSections
            rightSection <- zeroRelativeSections
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorBias = MoveBias.Neutral,
              precedingLeftDeletions = true,
              precedingRightDeletions = precedingRightDeletions
            ):
              val sectionMatch = Match.BaseAndRight(
                baseElement = baseSection,
                rightElement = rightSection
              )

              partialResult
                .focus(_.base)
                .modify(_ :+ baseSection)
                .focus(_.right)
                .modify(_ :+ rightSection)
                .focus(_.matchesBySection)
                .modify(
                  _ + (baseSection -> sectionMatch) + (rightSection -> sectionMatch)
                )
                .focus(_.moves)
                .modify(_ :+ Move.LeftDeletion)
          yield result
          end for

        case Move.RightDeletion =>
          for
            baseSection <- zeroRelativeSections
            leftSection <- zeroRelativeSections
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorBias = MoveBias.Neutral,
              precedingLeftDeletions = precedingLeftDeletions,
              precedingRightDeletions = true
            ):
              val sectionMatch = Match.BaseAndLeft(
                baseElement = baseSection,
                leftElement = leftSection
              )

              partialResult
                .focus(_.base)
                .modify(_ :+ baseSection)
                .focus(_.left)
                .modify(_ :+ leftSection)
                .focus(_.matchesBySection)
                .modify(
                  _ + (baseSection -> sectionMatch) + (leftSection -> sectionMatch)
                )
                .focus(_.moves)
                .modify(_ :+ Move.RightDeletion)
          yield result
          end for

        case Move.CoincidentDeletion =>
          for
            baseSection <- zeroRelativeSections
            result <- simpleMergeTestCases(
              allowConflicts = allowConflicts,
              predecessorBias = MoveBias.CoincidentDeletion,
              precedingLeftDeletions = precedingLeftDeletions,
              precedingRightDeletions = precedingRightDeletions
            ):
              partialResult
                .focus(_.base)
                .modify(_ :+ baseSection)
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
  val assert: Expecty = new Expecty:
    override val showLocation: Boolean = true
    override val showTypes: Boolean    = true
  end assert
  private val leftInsertionFrequency       = 7  -> Move.LeftInsertion
  private val rightInsertionFrequency      = 7  -> Move.RightInsertion
  private val coincidentInsertionFrequency = 4  -> Move.CoincidentInsertion
  private val preservationFrequency        = 10 -> Move.Preservation
  private val leftDeletionFrequency        = 7  -> Move.LeftDeletion
  private val rightDeletionFrequency       = 7  -> Move.RightDeletion
  private val coincidentDeletionFrequency  = 4  -> Move.CoincidentDeletion

  private def emptyMergeTestCase(allowConflicts: Boolean): MergeTestCase =
    MergeTestCase(
      base = IndexedSeq.empty,
      left = IndexedSeq.empty,
      right = IndexedSeq.empty,
      matchesBySection = Map.empty,
      expectedMerge = Option.unless(allowConflicts) {
        Result.FullyMerged(elements = IndexedSeq.empty)
      },
      moves = IndexedSeq.empty
    )

  private def leftInsertionFrequency(allow: Boolean) =
    (if allow then 7 else 0) -> Move.LeftInsertion

  private def rightInsertionFrequency(allow: Boolean) =
    (if allow then 7 else 0) -> Move.RightInsertion

  /** A fake section's contents are the textual form of its label; the sections
    * can be thought of covering an overall text that is the concatenation of
    * the infinite sequence of label texts in increasing label order, starting
    * from zero. This is only done to make sure that equality is clearly defined
    * on fake sections, which is necessary to get a {@code matchLookup} to
    * supply to [[Merge.of]]. In particular, the test is expected to use matches
    * between sections that are *not* equivalent, simulating whitespace
    * insensitivity.
    *
    * The labels are just to make it easier to debug test failures by
    * identifying sections - so don't be surprised if a section of contents "1"
    * matches a section of contents "99", this is just an extreme case of
    * flexible matching!
    *
    * @param zeroRelativeLabel
    *   This provides an identity for the section, so that sections with the
    *   same label are equivalent.
    */
  case class FakeSection(zeroRelativeLabel: Int) extends Section:
    require(0 <= zeroRelativeLabel)
    override def startOffset: Int =
      startOffsetCache.getOrElseUpdate(
        zeroRelativeLabel, {
          // Indirectly recurse down the labels to zero, hence the caching...
          if 0 == zeroRelativeLabel then 0
          else FakeSection(zeroRelativeLabel - 1).onePastEndOffset
        }
      )

    override def size: Int        = contents.length
    override def contents: String = zeroRelativeLabel.toString
  end FakeSection

  case class MergeTestCase(
      base: IndexedSeq[Section],
      left: IndexedSeq[Section],
      right: IndexedSeq[Section],
      matchesBySection: Map[Section, Match[Section]],
      expectedMerge: Option[FullyMerged[Section]],
      moves: IndexedSeq[Move]
  ):
    def validate(result: Result[Section]): Unit =
      def baseIsPreservedCorrectlyIn(
          sections: IndexedSeq[Section]
      ): Set[Section] =
        val preserved = base.collect(baseSection =>
          matchesBySection.get(baseSection) match
            case Some(allThree: Match.AllThree[Section]) =>
              allThree.dominantElement
        )

        val _ = preserved.isSubsequenceOf(sections)

        preserved.toSet

      end baseIsPreservedCorrectlyIn

      def leftAppearsCorrectlyIn(sections: IndexedSeq[Section]): Set[Section] =
        val appears = left.collect(leftSection =>
          matchesBySection.get(leftSection) match
            case Some(
                  allThree: (Match.AllThree[Section] |
                    Match.LeftAndRight[Section])
                ) =>
              allThree.dominantElement
            case None => leftSection
        )

        val _ = appears.isSubsequenceOf(sections)

        appears.toSet
      end leftAppearsCorrectlyIn

      def rightAppearsCorrectlyIn(sections: IndexedSeq[Section]): Set[Section] =
        val appears = right.collect(rightSection =>
          matchesBySection.get(rightSection) match
            case Some(
                  allThree: (Match.AllThree[Section] |
                    Match.LeftAndRight[Section])
                ) =>
              allThree.dominantElement
            case None => rightSection
        )

        val _ = appears.isSubsequenceOf(sections)

        appears.toSet
      end rightAppearsCorrectlyIn

      def allPresentAndCorrectIn(result: Result[Section]): Unit =
        result match
          case FullyMerged(sections) =>
            val basePreservations = baseIsPreservedCorrectlyIn(sections)
            val leftAppearances   = leftAppearsCorrectlyIn(sections)
            val rightAppearances  = rightAppearsCorrectlyIn(sections)

            assert(
              (basePreservations union leftAppearances union rightAppearances) == sections.toSet
            )

          case MergedWithConflicts(leftSections, rightSections) =>
            val basePreservationsOnLeft = baseIsPreservedCorrectlyIn(
              leftSections
            )
            val basePreservationsOnRight = baseIsPreservedCorrectlyIn(
              rightSections
            )
            val leftAppearances = leftAppearsCorrectlyIn(leftSections)
            val rightAppearances = rightAppearsCorrectlyIn(
              rightSections
            )

            assert(
              basePreservationsOnLeft == basePreservationsOnRight
            )

            // NOTE: use a subset rather than an equality check, as it is
            // possible for right sections that cleanly merged to appear in
            // `leftSections`.
            assert(
              (basePreservationsOnLeft union leftAppearances) subsetOf leftSections.toSet
            )
            // NOTE: use a subset rather than an equality check, as it is
            // possible for left sections that cleanly merged to appear in
            // `rightSections`.
            assert(
              (basePreservationsOnRight union rightAppearances) subsetOf rightSections.toSet
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

  object FakeSection:
    private val startOffsetCache: MutableMap[Int, Int] = MutableMap.empty
  end FakeSection

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

end MergeTest
