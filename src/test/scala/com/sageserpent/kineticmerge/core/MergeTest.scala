package com.sageserpent.kineticmerge.core

import cats.syntax.all.*
import com.eed3si9n.expecty
import com.eed3si9n.expecty.Expecty
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.CodeMotionAnalysis.Match
import com.sageserpent.kineticmerge.core.Merge.Result
import com.sageserpent.kineticmerge.core.MergeTest.*
import com.sageserpent.kineticmerge.core.MergeTest.FakeSection.startOffsetCache
import monocle.syntax.all.*
import org.junit.jupiter.api.{DynamicTest, TestFactory}
import pprint.*

import scala.collection.mutable.Map as MutableMap

class MergeTest:
  @TestFactory
  def example: DynamicTests =
    simpleMergeTestCases().withLimit(100).dynamicTests(pprint.pprintln(_))

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

  def simpleMergeTestCases(
      mergeContributorBias: MergeContributorBias = MergeContributorBias.Neutral
  ): Trials[MergeTestCase] =

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

      // TODO - decide whether to generate conflict test cases, these are
      // commented out for now...
      val choices = mergeContributorBias match
        case MergeContributorBias.Left =>
          trialsApi
            .chooseWithWeights(
              leftInsertionFrequency,
              coincidentInsertionFrequency,
              preservationFrequency,
              leftDeletionFrequency,
              rightDeletionFrequency,
              coincidentDeletionFrequency
            )
        case MergeContributorBias.Right =>
          trialsApi
            .chooseWithWeights(
              rightInsertionFrequency,
              coincidentInsertionFrequency,
              preservationFrequency,
              leftDeletionFrequency,
              rightDeletionFrequency,
              coincidentDeletionFrequency
            )
        case MergeContributorBias.Neutral =>
          trialsApi
            .chooseWithWeights(
              leftInsertionFrequency,
              rightInsertionFrequency,
              coincidentInsertionFrequency,
              preservationFrequency,
              leftDeletionFrequency,
              rightDeletionFrequency,
              coincidentDeletionFrequency
            )
        case MergeContributorBias.Fragile =>
          trialsApi
            .chooseWithWeights(
              coincidentInsertionFrequency,
              preservationFrequency,
              coincidentDeletionFrequency
            )
      choices flatMap:
        case MergeOutcome.LeftInsertion =>
          for
            leftSection          <- zeroRelativeSections
            simplerMergeTestCase <- simpleMergeTestCases(mergeContributorBias = MergeContributorBias.Left)
          yield simplerMergeTestCase
            .focus(_.left)
            .modify(_ :+ leftSection)
            .focus(_.expectedMerge)
            .modify:
              case Result.FullyMerged(sections) =>
                Result.FullyMerged(sections :+ leftSection)
              case Result.MergedWithConflicts(
                    leftSections,
                    rightSections
                  ) =>
                Result.MergedWithConflicts(
                  leftSections = leftSections :+ leftSection,
                  rightSections = rightSections
                )
          end for

        case MergeOutcome.RightInsertion =>
          for
            rightSection         <- zeroRelativeSections
            simplerMergeTestCase <- simpleMergeTestCases(mergeContributorBias = MergeContributorBias.Right)
          yield simplerMergeTestCase
            .focus(_.right)
            .modify(_ :+ rightSection)
            .focus(_.expectedMerge)
            .modify:
              case Result.FullyMerged(sections) =>
                Result.FullyMerged(sections :+ rightSection)
              case Result.MergedWithConflicts(
                    leftSections,
                    rightSections
                  ) =>
                Result.MergedWithConflicts(
                  leftSections = leftSections,
                  rightSections = rightSections :+ rightSection
                )
          end for

        case MergeOutcome.CoincidentInsertion =>
          for
            leftSection       <- zeroRelativeSections
            rightSection      <- zeroRelativeSections
            simplerMergeTestCase <- simpleMergeTestCases(mergeContributorBias =
              MergeContributorBias.Neutral
            )
          yield
            val sectionMatch = Match.LeftAndRight(
              leftSection = leftSection,
              rightSection = rightSection
            )

            simplerMergeTestCase
              .focus(_.left)
              .modify(_ :+ leftSection)
              .focus(_.right)
              .modify(_ :+ rightSection)
              .focus(_.matchesBySection)
              .modify(
                _ + (leftSection -> sectionMatch) + (rightSection -> sectionMatch)
              )
              .focus(_.expectedMerge)
              .modify:
                case Result.FullyMerged(sections) =>
                  Result.FullyMerged(sections :+ sectionMatch.dominantSection)
                case Result.MergedWithConflicts(
                      leftSections,
                      rightSections
                    ) =>
                  Result.MergedWithConflicts(
                    leftSections = leftSections :+ sectionMatch.dominantSection,
                    rightSections =
                      rightSections :+ sectionMatch.dominantSection
                  )
          end for

        case MergeOutcome.Preservation =>
          for
            baseSection  <- zeroRelativeSections
            leftSection  <- zeroRelativeSections
            rightSection <- zeroRelativeSections
            simplerMergeTestCase <- simpleMergeTestCases(mergeContributorBias =
              MergeContributorBias.Neutral
            )
          yield
            val sectionMatch =
              Match.AllThree(
                baseSection = baseSection,
                leftSection = leftSection,
                rightSection = rightSection
              )

            simplerMergeTestCase
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
              .focus(_.expectedMerge)
              .modify:
                case Result.FullyMerged(sections) =>
                  Result.FullyMerged(sections :+ sectionMatch.dominantSection)
                case Result.MergedWithConflicts(
                      leftSections,
                      rightSections
                    ) =>
                  Result.MergedWithConflicts(
                    leftSections = leftSections :+ sectionMatch.dominantSection,
                    rightSections =
                      rightSections :+ sectionMatch.dominantSection
                  )
          end for

        case MergeOutcome.LeftDeletion =>
          for
            baseSection  <- zeroRelativeSections
            rightSection <- zeroRelativeSections
            simplerMergeTestCase <- simpleMergeTestCases(mergeContributorBias =
              MergeContributorBias.Neutral
            )
          yield
            val sectionMatch = Match.BaseAndRight(
              baseSection = baseSection,
              rightSection = rightSection
            )

            simplerMergeTestCase
              .focus(_.base)
              .modify(_ :+ baseSection)
              .focus(_.right)
              .modify(_ :+ rightSection)
              .focus(_.matchesBySection)
              .modify(
                _ + (baseSection -> sectionMatch) + (rightSection -> sectionMatch)
              )
          end for

        case MergeOutcome.RightDeletion =>
          for
            baseSection <- zeroRelativeSections
            leftSection <- zeroRelativeSections
            simplerMergeTestCase <- simpleMergeTestCases(mergeContributorBias =
              MergeContributorBias.Neutral
            )
          yield
            val sectionMatch = Match.BaseAndLeft(
              baseSection = baseSection,
              leftSection = leftSection
            )

            simplerMergeTestCase
              .focus(_.base)
              .modify(_ :+ baseSection)
              .focus(_.left)
              .modify(_ :+ leftSection)
              .focus(_.matchesBySection)
              .modify(
                _ + (baseSection -> sectionMatch) + (leftSection -> sectionMatch)
              )
          end for

        case MergeOutcome.CoincidentDeletion =>
          for
            baseSection <- zeroRelativeSections
            simplerMergeTestCase <- simpleMergeTestCases(mergeContributorBias =
              MergeContributorBias.Fragile
            )
          yield simplerMergeTestCase
            .focus(_.base)
            .modify(_ :+ baseSection)
          end for

    end extendedMergeTestCases

    trialsApi.complexities.flatMap(complexity =>
      trialsApi.alternateWithWeights(
        complexity -> trialsApi.only(emptyMergeTestCase),
        50         -> extendedMergeTestCases
      )
    )
  end simpleMergeTestCases

end MergeTest

object MergeTest:
  private val emptyMergeTestCase: MergeTestCase = MergeTestCase(
    base = IndexedSeq.empty,
    left = IndexedSeq.empty,
    right = IndexedSeq.empty,
    matchesBySection = Map.empty,
    expectedMerge = Result.FullyMerged(sections = IndexedSeq.empty)
  )

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
      expectedMerge: Result
  )

  object FakeSection:
    private val startOffsetCache: MutableMap[Int, Int] = MutableMap.empty
  end FakeSection

  enum MergeOutcome:
    case LeftInsertion
    case RightInsertion
    case CoincidentInsertion
    case Preservation
    case LeftDeletion
    case RightDeletion
    case CoincidentDeletion
  end MergeOutcome

  private val leftInsertionFrequency = 7 -> MergeOutcome.LeftInsertion

  private val rightInsertionFrequency = 7 -> MergeOutcome.RightInsertion

  private val coincidentInsertionFrequency = 4 -> MergeOutcome.CoincidentInsertion

  private val preservationFrequency = 10 -> MergeOutcome.Preservation

  private val leftDeletionFrequency = 7 -> MergeOutcome.LeftDeletion

  private val rightDeletionFrequency = 7 -> MergeOutcome.RightDeletion

  private val coincidentDeletionFrequency = 4 -> MergeOutcome.CoincidentDeletion

  enum MergeContributorBias:
    case Left // Can be followed by another left or switch to over to a neutral or fragile.
    case Right // Can be followed by another right or switch to over to a neutral or fragile.
    case Neutral // Can be followed by anything.
    case Fragile // Can only be followed by another fragile or a neutral.
  end MergeContributorBias

end MergeTest
