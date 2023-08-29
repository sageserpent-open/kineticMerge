package com.sageserpent.kineticmerge.core

import cats.syntax.all.*
import com.eed3si9n.expecty
import com.eed3si9n.expecty.Expecty
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.CodeMotionAnalysis.Match
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
  val simpleMergeTestCases: Trials[MergeTestCase] =
    simpleMergeTestCases()(partialResult = emptyMergeTestCase)

  @Test
  def bugReproduction(): Unit =
    val testCase = MergeTestCase(
      base = Vector(
        FakeSection(zeroRelativeLabel = 5),
        FakeSection(zeroRelativeLabel = 9),
        FakeSection(zeroRelativeLabel = 12)
      ),
      left = Vector(
        FakeSection(zeroRelativeLabel = 2),
        FakeSection(zeroRelativeLabel = 6),
        FakeSection(zeroRelativeLabel = 15)
      ),
      right = Vector(
        FakeSection(zeroRelativeLabel = 16),
        FakeSection(zeroRelativeLabel = 19)
      ),
      matchesBySection = Map(
        FakeSection(zeroRelativeLabel = 5) -> Match.BaseAndLeft(
          baseSection = FakeSection(zeroRelativeLabel = 5),
          leftSection = FakeSection(zeroRelativeLabel = 6)
        ),
        FakeSection(zeroRelativeLabel = 6) -> Match.BaseAndLeft(
          baseSection = FakeSection(zeroRelativeLabel = 5),
          leftSection = FakeSection(zeroRelativeLabel = 6)
        ),
        FakeSection(zeroRelativeLabel = 15) -> Match.LeftAndRight(
          leftSection = FakeSection(zeroRelativeLabel = 15),
          rightSection = FakeSection(zeroRelativeLabel = 16)
        ),
        FakeSection(zeroRelativeLabel = 16) -> Match.LeftAndRight(
          leftSection = FakeSection(zeroRelativeLabel = 15),
          rightSection = FakeSection(zeroRelativeLabel = 16)
        )
      ),
      expectedMerge = Some(
        FullyMerged(
          sections = Vector(
            FakeSection(zeroRelativeLabel = 2),
            FakeSection(zeroRelativeLabel = 15),
            FakeSection(zeroRelativeLabel = 19)
          )
        )
      ),
      moves = Vector(
        Move.LeftInsertion,
        Move.RightDeletion,
        Move.CoincidentDeletion,
        Move.CoincidentDeletion,
        Move.CoincidentInsertion,
        Move.RightInsertion
      )
    )

    pprint.pprintln(testCase)

    val Right(result) =
      Merge.of(testCase.base, testCase.left, testCase.right)(
        testCase.matchesBySection.get
      ): @unchecked

    testCase.validate(result)
  end bugReproduction

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
    simpleMergeTestCases
      .withLimit(2000)
      .dynamicTests: testCase =>
        println("*************")
        pprint.pprintln(testCase)

        val Right(result) =
          Merge.of(testCase.base, testCase.left, testCase.right)(
            testCase.matchesBySection.get
          ): @unchecked

        testCase.validate(result)

  def simpleMergeTestCases(
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
              predecessorBias = MoveBias.Neutral
            ):
              val sectionMatch = Match.LeftAndRight(
                leftSection = leftSection,
                rightSection = rightSection
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
                    Result.FullyMerged(sections :+ sectionMatch.dominantSection)
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
              predecessorBias = MoveBias.Neutral
            ):
              val sectionMatch =
                Match.AllThree(
                  baseSection = baseSection,
                  leftSection = leftSection,
                  rightSection = rightSection
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
                      sections :+ sectionMatch.dominantSection
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
              predecessorBias = MoveBias.Neutral,
              precedingLeftDeletions = true,
              precedingRightDeletions = precedingRightDeletions
            ):
              val sectionMatch = Match.BaseAndRight(
                baseSection = baseSection,
                rightSection = rightSection
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
              predecessorBias = MoveBias.Neutral,
              precedingLeftDeletions = precedingLeftDeletions,
              precedingRightDeletions = true
            ):
              val sectionMatch = Match.BaseAndLeft(
                baseSection = baseSection,
                leftSection = leftSection
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

    partialResult.expectedMerge match
      case _ =>
        trialsApi.complexities.flatMap(complexity =>
          trialsApi.alternateWithWeights(
            complexity -> trialsApi.only(partialResult),
            50         -> extendedMergeTestCases
          )
        )
    end match

  end simpleMergeTestCases

end MergeTest

object MergeTest:
  val assert: Expecty = new Expecty:
    override val showLocation: Boolean = true
    override val showTypes: Boolean    = true
  end assert
  private val emptyMergeTestCase: MergeTestCase = MergeTestCase(
    base = IndexedSeq.empty,
    left = IndexedSeq.empty,
    right = IndexedSeq.empty,
    matchesBySection = Map.empty,
    expectedMerge = Some(Result.FullyMerged(sections = IndexedSeq.empty)),
    moves = IndexedSeq.empty
  )
  private val leftInsertionFrequency       = 7  -> Move.LeftInsertion
  private val rightInsertionFrequency      = 7  -> Move.RightInsertion
  private val coincidentInsertionFrequency = 4  -> Move.CoincidentInsertion
  private val preservationFrequency        = 10 -> Move.Preservation
  private val leftDeletionFrequency        = 7  -> Move.LeftDeletion
  private val rightDeletionFrequency       = 7  -> Move.RightDeletion
  private val coincidentDeletionFrequency  = 4  -> Move.CoincidentDeletion

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
      matchesBySection: Map[Section, Match],
      expectedMerge: Option[FullyMerged],
      moves: IndexedSeq[Move]
  ):
    def validate(result: Result): Unit =
      expectedMerge match
        case Some(merge) =>
          assert(
            result == merge
          ) // TODO: perform an initial self-validation of the expected merge.
        case None => // TODO: find all the pieces and check they're present and correct.
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
