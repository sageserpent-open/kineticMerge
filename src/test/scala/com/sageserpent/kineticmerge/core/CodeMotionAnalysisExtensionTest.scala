package com.sageserpent.kineticmerge.core

import cats.{Eq, Order}
import com.google.common.hash.{Funnel, HashFunction, Hashing}
import com.sageserpent.americium.Trials
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.CodeMotionAnalysis.Configuration
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisExtension.*
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisExtensionTest.{FakePath, reconstituteTextFrom, given}
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.Token.tokens
import org.junit.jupiter.api.Assertions.fail
import org.junit.jupiter.api.{Test, TestFactory}

import scala.util.Right

object CodeMotionAnalysisExtensionTest:
  type FakePath = String

  def reconstituteTextFrom(tokens: IndexedSeq[Token]) =
    tokens.map(_.text).mkString

  given Eq[Token]     = Token.equality
  given Order[Token]  = Token.comparison
  given Funnel[Token] = Token.funnel
  given HashFunction  = Hashing.murmur3_32_fixed()

end CodeMotionAnalysisExtensionTest

class CodeMotionAnalysisExtensionTest extends ProseExamples:
  @Test
  def issue23BugReproduction(): Unit =
    val configuration = Configuration(
      minimumMatchSize = 4,
      thresholdSizeFractionForMatching = 0.1,
      minimumAmbiguousMatchSize = 0
    )

    val placeholderPath: FakePath = "*** STUNT DOUBLE ***"

    val tokenRegex = raw"(SAFE|INTRUDER|FIZZY|BANG|.)+?".r.anchored

    def stuntDoubleTokens(content: String): Vector[Token] = tokenRegex
      .findAllMatchIn(content)
      .map(_.group(1))
      .map(Token.Significant.apply)
      .toVector

    val baseSources = MappedContentSourcesOfTokens(
      contentsByPath =
        Map(placeholderPath -> stuntDoubleTokens(issue23BugReproductionBase)),
      label = "base"
    )
    val leftSources = MappedContentSourcesOfTokens(
      contentsByPath =
        Map(placeholderPath -> stuntDoubleTokens(issue23BugReproductionLeft)),
      label = "left"
    )
    val rightSources = MappedContentSourcesOfTokens(
      contentsByPath =
        Map(placeholderPath -> stuntDoubleTokens(issue23BugReproductionRight)),
      label = "right"
    )

    val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
      base = baseSources,
      left = leftSources,
      right = rightSources
    )(configuration): @unchecked

    val (mergeResultsByPath, _) =
      codeMotionAnalysis.merge

    verifyContent(placeholderPath, mergeResultsByPath)(
      stuntDoubleTokens(issue23BugReproductionExpectedMerge)
    )
  end issue23BugReproduction

  @TestFactory
  def codeMotionWithPropagatedInsertion(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 2,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0
    )

    val placeholderPath: FakePath = "*** STUNT DOUBLE ***"

    val tokenRegex =
      raw"(Fish|Chips|MushyPeas|Ketchup|Muesli|Toast|Tea|Coffee|Kippers|Noodles|Sandwich|Cake|Pudding|Figs|.)+?".r.anchored

    def stuntDoubleTokens(content: FakePath): Vector[Token] = tokenRegex
      .findAllMatchIn(content)
      .map(_.group(1))
      .map(Token.Significant.apply)
      .toVector

    Trials.api
      .choose(
        (
          migratedSurroundedInsertionExampleBase,
          migratedSurroundedInsertionExampleLeft,
          migratedSurroundedInsertionExampleRight,
          migratedSurroundedInsertionExampleExpectedMerge
        ),
        (
          migratedLeadingInsertionExampleBase,
          migratedLeadingInsertionExampleLeft,
          migratedLeadingInsertionExampleRight,
          migratedLeadingInsertionExampleExpectedMerge
        ),
        (
          migratedTrailingInsertionExampleBase,
          migratedTrailingInsertionExampleLeft,
          migratedTrailingInsertionExampleRight,
          migratedTrailingInsertionExampleExpectedMerge
        )
      )
      .withLimit(10)
      .dynamicTests {
        (
            baseText: String,
            leftText: String,
            rightText: String,
            expectedText: String
        ) =>
          val baseSources = MappedContentSourcesOfTokens(
            contentsByPath =
              Map(placeholderPath -> stuntDoubleTokens(baseText)),
            label = "base"
          )
          val leftSources = MappedContentSourcesOfTokens(
            contentsByPath = Map(
              placeholderPath -> stuntDoubleTokens(
                leftText
              )
            ),
            label = "left"
          )
          val rightSources = MappedContentSourcesOfTokens(
            contentsByPath =
              Map(placeholderPath -> stuntDoubleTokens(rightText)),
            label = "right"
          )

          val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
            base = baseSources,
            left = leftSources,
            right = rightSources
          )(configuration): @unchecked

          val (mergeResultsByPath, _) =
            codeMotionAnalysis.merge

          verifyContent(placeholderPath, mergeResultsByPath)(
            stuntDoubleTokens(expectedText)
          )
      }

  end codeMotionWithPropagatedInsertion

  @TestFactory
  def issue42BugReproduction(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 2,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0
    )

    val placeholderPath: FakePath = "*** STUNT DOUBLE ***"

    val tokenRegex =
      raw"([}]|Fish|Chips|MushyPeas|Ketchup|Muesli|Toast|Tea|Coffee|Kippers|Noodles|Sandwich|Cake|Pudding|Porridge|Figs|BlackPudding|Bangers|.)+?".r.anchored

    def stuntDoubleTokens(content: FakePath): Vector[Token] = tokenRegex
      .findAllMatchIn(content)
      .map(_.group(1))
      .map(Token.Significant.apply)
      .toVector

    Trials.api
      .choose(
        (
          issue42BugReproductionTrailingTokenBase,
          issue42BugReproductionTrailingTokenLeft,
          issue42BugReproductionTrailingTokenRight,
          issue42BugReproductionTrailingTokenMerge
        ),
        (
          issue42BugReproductionTrailingTokenAndThenRightEditBase,
          issue42BugReproductionTrailingTokenAndThenRightEditLeft,
          issue42BugReproductionTrailingTokenAndThenRightEditRight,
          issue42BugReproductionTrailingTokenAndThenRightEditMerge
        ),
        (
          issue42BugReproductionLeadingTokenBase,
          issue42BugReproductionLeadingTokenLeft,
          issue42BugReproductionLeadingTokenRight,
          issue42BugReproductionLeadingTokenMerge
        ),
        (
          issue42BugReproductionRightEditAndThenLeadingTokenBase,
          issue42BugReproductionRightEditAndThenLeadingTokenLeft,
          issue42BugReproductionRightEditAndThenLeadingTokenRight,
          issue42BugReproductionRightEditAndThenLeadingTokenMerge
        )
      )
      .and(Trials.api.booleans)
      .withLimit(10)
      .dynamicTests {
        case (
              (
                baseText: String,
                leftText: String,
                rightText: String,
                expectedText: String
              ),
              mirrorImage: Boolean
            ) =>
          val baseSources = MappedContentSourcesOfTokens(
            contentsByPath =
              Map(placeholderPath -> stuntDoubleTokens(baseText)),
            label = "base"
          )
          val leftSources = MappedContentSourcesOfTokens(
            contentsByPath = Map(
              placeholderPath -> stuntDoubleTokens(
                if mirrorImage then rightText else leftText
              )
            ),
            label = "left"
          )
          val rightSources = MappedContentSourcesOfTokens(
            contentsByPath = Map(
              placeholderPath -> stuntDoubleTokens(
                if mirrorImage then leftText else rightText
              )
            ),
            label = "right"
          )

          val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
            base = baseSources,
            left = leftSources,
            right = rightSources
          )(configuration): @unchecked

          val (mergeResultsByPath, _) =
            codeMotionAnalysis.merge

          verifyContent(placeholderPath, mergeResultsByPath)(
            stuntDoubleTokens(expectedText)
          )
      }

  end issue42BugReproduction

  @Test
  def codeMotion(): Unit =
    val configuration = Configuration(
      minimumMatchSize = 4,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0
    )

    val placeholderPath: FakePath = "*** STUNT DOUBLE ***"

    val baseSources = MappedContentSourcesOfTokens(
      contentsByPath =
        Map(placeholderPath -> tokens(codeMotionExampleBase).get),
      label = "base"
    )
    val leftSources = MappedContentSourcesOfTokens(
      contentsByPath =
        Map(placeholderPath -> tokens(codeMotionExampleLeft).get),
      label = "left"
    )
    val rightSources = MappedContentSourcesOfTokens(
      contentsByPath =
        Map(placeholderPath -> tokens(codeMotionExampleRight).get),
      label = "right"
    )

    val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
      base = baseSources,
      left = leftSources,
      right = rightSources
    )(configuration): @unchecked

    val (mergeResultsByPath, _) =
      codeMotionAnalysis.merge

    verifyContent(placeholderPath, mergeResultsByPath)(
      tokens(codeMotionExampleExpectedMerge).get
    )
  end codeMotion

  @Test
  def codeMotionWithSplit(): Unit =
    val configuration = Configuration(
      minimumMatchSize = 4,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 5
    )

    val originalPath: FakePath = "*** ORIGINAL ***"
    val hivedOffPath: FakePath = "*** HIVED OFF ***"

    val baseSources = MappedContentSourcesOfTokens(
      contentsByPath =
        Map(originalPath -> tokens(codeMotionExampleWithSplitOriginalBase).get),
      label = "base"
    )
    val leftSources = MappedContentSourcesOfTokens(
      contentsByPath =
        Map(originalPath -> tokens(codeMotionExampleWithSplitOriginalLeft).get),
      label = "left"
    )
    val rightSources = MappedContentSourcesOfTokens(
      contentsByPath = Map(
        originalPath -> tokens(codeMotionExampleWithSplitOriginalRight).get,
        hivedOffPath -> tokens(codeMotionExampleWithSplitHivedOffRight).get
      ),
      label = "right"
    )

    val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
      base = baseSources,
      left = leftSources,
      right = rightSources
    )(configuration): @unchecked

    val (mergeResultsByPath, _) =
      codeMotionAnalysis.merge

    verifyContent(originalPath, mergeResultsByPath)(
      tokens(codeMotionExampleWithSplitOriginalExpectedMerge).get
    )

    verifyContent(hivedOffPath, mergeResultsByPath)(
      tokens(codeMotionExampleWithSplitHivedOffExpectedMerge).get
    )
  end codeMotionWithSplit

  @TestFactory
  def merging(): DynamicTests =
    val minimumMatchSizes = Trials.api.integers(2, 10)

    minimumMatchSizes.withLimit(30).dynamicTests { minimumMatchSize =>
      val configuration = Configuration(
        minimumMatchSize = minimumMatchSize,
        thresholdSizeFractionForMatching = 0,
        minimumAmbiguousMatchSize = 4
      )

      val prosePath: FakePath    = "prose"
      val sbtBuildPath: FakePath = "sbtBuild"

      val baseSources =
        MappedContentSourcesOfTokens(
          contentsByPath = Map(
            prosePath    -> tokens(wordsworth).get,
            sbtBuildPath -> tokens(baseSbtBuild).get
          ),
          label = "base"
        )
      val leftSources =
        MappedContentSourcesOfTokens(
          contentsByPath = Map(
            prosePath    -> tokens(jobsworth).get,
            sbtBuildPath -> tokens(leftSbtBuild).get
          ),
          label = "left"
        )
      val rightSources =
        MappedContentSourcesOfTokens(
          contentsByPath = Map(
            prosePath    -> tokens(emsworth).get,
            sbtBuildPath -> tokens(rightSbtBuild).get
          ),
          label = "right"
        )

      val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
        base = baseSources,
        left = leftSources,
        right = rightSources
      )(configuration): @unchecked

      val (mergeResultsByPath, _) = codeMotionAnalysis.merge

      def merge(path: FakePath): Unit =
        mergeResultsByPath(path) match
          case FullyMerged(result) =>
            println(fansi.Color.Yellow("Fully merged result..."))
            println(fansi.Color.Green(reconstituteTextFrom(result)))
          case MergedWithConflicts(leftResult, rightResult) =>
            println(fansi.Color.Red(s"Left result..."))
            println(fansi.Color.Green(reconstituteTextFrom(leftResult)))
            println(fansi.Color.Red(s"Right result..."))
            println(fansi.Color.Green(reconstituteTextFrom(rightResult)))
      end merge

      merge(prosePath)
      merge(sbtBuildPath)
    }
  end merging

  @TestFactory
  def whitespaceOnlyEditing(): DynamicTests =
    (Trials.api.booleans and Trials.api.booleans).withLimit(4).dynamicTests {
      (leftEdited, rightEdited) =>
        val configuration = Configuration(
          minimumMatchSize = 4,
          thresholdSizeFractionForMatching = 0.1,
          minimumAmbiguousMatchSize = 0
        )

        val placeholderPath: FakePath = "*** STUNT DOUBLE ***"

        val baseSources = MappedContentSourcesOfTokens(
          contentsByPath =
            Map(placeholderPath -> tokens(whitespaceOnlyChangeExampleBase).get),
          label = "base"
        )
        val leftSources = MappedContentSourcesOfTokens(
          contentsByPath = Map(
            placeholderPath -> tokens(
              if leftEdited then whitespaceOnlyChangeExampleLeftEdited
              else whitespaceOnlyChangeExampleBase
            ).get
          ),
          label = "left"
        )
        val rightSources = MappedContentSourcesOfTokens(
          contentsByPath = Map(
            placeholderPath -> tokens(
              if rightEdited then whitespaceOnlyChangeExampleRightEdited
              else whitespaceOnlyChangeExampleBase
            ).get
          ),
          label = "right"
        )

        val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
          base = baseSources,
          left = leftSources,
          right = rightSources
        )(configuration): @unchecked

        val expected = tokens(
          if leftEdited && rightEdited then
            // The left takes precedence over the right when both have
            // whitespace changes.
            whitespaceOnlyChangeExampleLeftEdited
          else if leftEdited then whitespaceOnlyChangeExampleLeftEdited
          else if rightEdited then
            // NOTE: this one is in contrast to what Git does.
            whitespaceOnlyChangeExampleRightEdited
          else whitespaceOnlyChangeExampleBase
        ).get

        val (mergeResultsByPath, _) =
          codeMotionAnalysis.merge

        verifyContent(placeholderPath, mergeResultsByPath)(expected, _ == _)
    }
  end whitespaceOnlyEditing

  @TestFactory
  def whitespaceOnlyEditingWithCodeMotion(): DynamicTests =
    enum RenamingSide:
      case Left
      case Right
      case Both
    end RenamingSide

    (Trials.api.booleans and Trials.api.booleans and Trials.api.choose(
      RenamingSide.values
    ))
      .withLimit(12)
      .dynamicTests { (leftEdited, rightEdited, renamingSide) =>
        val configuration = Configuration(
          minimumMatchSize = 4,
          thresholdSizeFractionForMatching = 0.1,
          minimumAmbiguousMatchSize = 0
        )

        val originalPath: FakePath             = "*** ORIGINAL ***"
        val renamedForCodeMotionPath: FakePath = "*** RENAMED ***"

        val baseSources = MappedContentSourcesOfTokens(
          contentsByPath =
            Map(originalPath -> tokens(whitespaceOnlyChangeExampleBase).get),
          label = "base"
        )

        val (leftPath, rightPath) =
          renamingSide match
            case RenamingSide.Left => renamedForCodeMotionPath -> originalPath
            case RenamingSide.Right => originalPath -> renamedForCodeMotionPath
            case RenamingSide.Both =>
              renamedForCodeMotionPath -> renamedForCodeMotionPath

        val leftSources = MappedContentSourcesOfTokens(
          contentsByPath = Map(
            leftPath -> tokens(
              if leftEdited then whitespaceOnlyChangeExampleLeftEdited
              else whitespaceOnlyChangeExampleBase
            ).get
          ),
          label = "left"
        )
        val rightSources = MappedContentSourcesOfTokens(
          contentsByPath = Map(
            rightPath -> tokens(
              if rightEdited then whitespaceOnlyChangeExampleRightEdited
              else whitespaceOnlyChangeExampleBase
            ).get
          ),
          label = "right"
        )

        val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
          base = baseSources,
          left = leftSources,
          right = rightSources
        )(configuration): @unchecked

        val expected = tokens(
          if leftEdited && rightEdited then
            // The left takes precedence over the right when both have
            // whitespace changes.
            whitespaceOnlyChangeExampleLeftEdited
          else if leftEdited then whitespaceOnlyChangeExampleLeftEdited
          else if rightEdited then
            // NOTE: this one is in contrast to what Git does.
            whitespaceOnlyChangeExampleRightEdited
          else whitespaceOnlyChangeExampleBase
        ).get

        val (mergeResultsByPath, _) =
          codeMotionAnalysis.merge

        verifyAbsenceOfContent(originalPath, mergeResultsByPath)

        verifyContent(renamedForCodeMotionPath, mergeResultsByPath)(
          expected,
          _ == _
        )
      }
  end whitespaceOnlyEditingWithCodeMotion

  @TestFactory
  def codeMotionAcrossAFileRename(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 2,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0
    )

    val originalPath: FakePath = "*** ORIGINAL ***"
    val renamedPath: FakePath  = "*** RENAMED ***"

    Trials.api
      .choose(
        (
          "Inserted context migrated across the file rename.",
          heyDiddleDiddleInModernForm,
          heyDiddleDiddleInArchaicForm,
          heyDiddleDiddleWithInsertions,
          heyDiddleDiddleWithInsertionsExpectedMerge
        ),
        (
          "Intra-file code motion migrated across the file rename.",
          heyDiddleDiddleInModernForm,
          heyDiddleDiddleInArchaicForm,
          heyDiddleDiddleWithIntraFileMove,
          heyDiddleDiddleWithIntraFileMoveExpectedMerge
        ),
        (
          "Intra-file code motion with surrounding inserted context migrated across the file rename.",
          heyDiddleDiddleInModernForm,
          heyDiddleDiddleInArchaicForm,
          heyDiddleDiddleWithIntraFileMoveAndSurroundingInsertions,
          heyDiddleDiddleWithIntraFileMoveAndSurroundingInsertionsExpectedMerge
        ),
        // TODO - reinstate this, probably with the expected merge amended...
//        (
//          "Intra-file code motion migrated across the file rename - variation.",
//          heyDiddleDiddleInModernForm,
//          heyDiddleDiddleInPsychoticForm,
//          heyDiddleDiddleWithIntraFileMove,
//          heyDiddleDiddleInPsychoticFormExpectedMerge
//        ),
        (
          "Inserted context migrated across the file rename with a deletion at the destination.",
          heyDiddleDiddleInModernForm,
          heyDiddleDiddleWithDeletionAtDestination,
          heyDiddleDiddleWithInsertions,
          heyDiddleDiddleWithInsertionsAndADeletionAtDestinationExpectedMerge
        )
      )
      .and(Trials.api.booleans)
      .withLimit(10)
      .dynamicTests {
        case (
              (
                label,
                baseContent,
                leftContent,
                rightContent,
                expectedMergeContent
              ),
              swapSides
            ) =>
          println(fansi.Color.Yellow(s"*** $label ***"))

          val baseSources = MappedContentSourcesOfTokens(
            contentsByPath = Map(
              originalPath -> tokens(baseContent).get
            ),
            label = "base"
          )

          val leftSources = MappedContentSourcesOfTokens(
            contentsByPath = Map(
              renamedPath -> tokens(leftContent).get
            ),
            label = "left"
          )

          val rightSources = MappedContentSourcesOfTokens(
            contentsByPath = Map(
              originalPath -> tokens(
                rightContent
              ).get
            ),
            label = "right"
          )

          val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
            base = baseSources,
            left = if swapSides then rightSources else leftSources,
            right = if swapSides then leftSources else rightSources
          )(configuration): @unchecked

          val (mergeResultsByPath, moveDestinationsReport) =
            codeMotionAnalysis.merge

          println(fansi.Color.Yellow(s"Final move destinations report...\n"))
          println(
            fansi.Color
              .Green(moveDestinationsReport.summarizeInText.mkString("\n"))
          )

          verifyContent(renamedPath, mergeResultsByPath)(
            tokens(expectedMergeContent).get
          )

          verifyAbsenceOfContent(originalPath, mergeResultsByPath)
      }
  end codeMotionAcrossAFileRename

  private def verifyAbsenceOfContent(
      path: FakePath,
      mergeResultsByPath: Map[FakePath, MergeResult[Token]]
  ): Unit =
    mergeResultsByPath(path) match
      case FullyMerged(result) =>
        assert(
          result.isEmpty,
          fansi.Color
            .Yellow(
              s"\nShould not have this content at $path...\n"
            )
            .render + fansi.Color
            .Green(
              reconstituteTextFrom(result)
            )
            .render
        )
      case MergedWithConflicts(leftResult, rightResult) =>
        fail(
          fansi.Color
            .Yellow(
              s"\nShould not have this content at $path...\n"
            )
            .render + fansi.Color.Red(s"\nLeft result...\n")
            + fansi.Color.Green(reconstituteTextFrom(leftResult)).render
            + fansi.Color.Red(s"\nRight result...\n").render
            + fansi.Color.Green(reconstituteTextFrom(rightResult)).render
        )
    end match
  end verifyAbsenceOfContent

  private def verifyContent(
      path: FakePath,
      mergeResultsByPath: Map[FakePath, MergeResult[Token]]
  )(
      expectedTokens: IndexedSeq[Token],
      equality: (Token, Token) => Boolean = Token.equality
  ): Unit =
    println(fansi.Color.Yellow(s"Checking $path...\n"))
    println(fansi.Color.Yellow("Expected..."))
    println(fansi.Color.Green(reconstituteTextFrom(expectedTokens)))

    mergeResultsByPath(path) match
      case FullyMerged(result) =>
        println(fansi.Color.Yellow("Fully merged result..."))
        println(fansi.Color.Green(reconstituteTextFrom(result)))
        assert(result.corresponds(expectedTokens)(equality))
      case MergedWithConflicts(leftResult, rightResult) =>
        println(fansi.Color.Red(s"Left result..."))
        println(fansi.Color.Green(reconstituteTextFrom(leftResult)))
        println(fansi.Color.Red(s"Right result..."))
        println(fansi.Color.Green(reconstituteTextFrom(rightResult)))

        fail("Should have seen a clean merge.")
    end match
  end verifyContent

  @TestFactory
  def codeMotionAcrossTwoFilesWhoseContentIsCombinedTogetherToMakeANewReplacementFile()
      : DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 2,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0
    )

    val proverbsPath: FakePath    = "*** PROVERBS ***"
    val palindromesPath: FakePath = "*** PALINDROMES ***"
    val combinedPath: FakePath    = "*** COMBINED ***"

    Trials.api
      .choose(
        (
          "Concatenation.",
          (proverbs, palindromes),
          (proverbsMeetAgileConsultant, palindromesMeetAgileConsultant),
          concatenatedWordPlay,
          concatenatedWordPlayExpectedMerge
        ),
        (
          "Jumbling.",
          (proverbs, palindromes),
          (proverbsMeetAgileConsultant, palindromesMeetAgileConsultant),
          jumbledWordPlay,
          jumbledWordPlayExpectedMerge
        ),
        (
          "One line moves out of the first half past the end of the second half.",
          (proverbs, palindromes),
          (proverbsMeetAgileConsultant, palindromesMeetAgileConsultant),
          moveToTheEndWordPlay,
          moveToTheEndWordPlayExpectedMerge
        ),
        (
          "Leading lines from both halves are made adjacent.",
          (proverbs, palindromes),
          (proverbsMeetAgileConsultant, palindromesMeetAgileConsultant),
          makeLeadingLinesFromBothHalvesAdjacentWordPlay,
          makeLeadingLinesFromBothHalvesAdjacentWordPlayExpectedMerge
        ),
        (
          "Leading lines from both halves are made adjacent with some leading deletion.",
          (proverbs, palindromes),
          (proverbsMeetAgileConsultant, palindromesMeetAgileConsultant),
          makeLeadingLinesFromBothHalvesAdjacentWithSomeLeadingDeletionWordPlay,
          makeLeadingLinesFromBothHalvesAdjacentWithSomeLeadingDeletionWordPlayExpectedMerge
        )
      )
      .withLimit(5)
      .dynamicTests {
        case (
              (
                label,
                (proverbsBaseContent, palindromesBaseContent),
                (proverbsLeftContent, palindromesLeftContent),
                combinedRightContent,
                expectedMergeContent
              )
            ) =>
          println(fansi.Color.Yellow(s"*** $label ***"))

          val baseSources = MappedContentSourcesOfTokens(
            contentsByPath = Map(
              proverbsPath    -> tokens(proverbsBaseContent).get,
              palindromesPath -> tokens(palindromesBaseContent).get
            ),
            label = "base"
          )

          val leftSources = MappedContentSourcesOfTokens(
            contentsByPath = Map(
              proverbsPath    -> tokens(proverbsLeftContent).get,
              palindromesPath -> tokens(palindromesLeftContent).get
            ),
            label = "left"
          )

          val rightSources = MappedContentSourcesOfTokens(
            contentsByPath = Map(
              combinedPath -> tokens(
                combinedRightContent
              ).get
            ),
            label = "right"
          )

          val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
            base = baseSources,
            left = leftSources,
            right = rightSources
          )(configuration): @unchecked

          val (mergeResultsByPath, moveDestinationsReport) =
            codeMotionAnalysis.merge

          println(fansi.Color.Yellow(s"Final move destinations report...\n"))
          println(
            fansi.Color
              .Green(moveDestinationsReport.summarizeInText.mkString("\n"))
          )

          verifyContent(combinedPath, mergeResultsByPath)(
            tokens(expectedMergeContent).get
          )

          verifyAbsenceOfContent(proverbsPath, mergeResultsByPath)

          verifyAbsenceOfContent(palindromesPath, mergeResultsByPath)
      }
  end codeMotionAcrossTwoFilesWhoseContentIsCombinedTogetherToMakeANewReplacementFile

  @Test
  def furtherMigrationOfAMigratedEditAsAnInsertion(): Unit =
    val configuration = Configuration(
      minimumMatchSize = 2,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0
    )

    val proverbsPath: FakePath        = "*** PROVERBS ***"
    val excisedProverbsPath: FakePath = "*** EXCISED PROVERBS ***"

    val baseSources = MappedContentSourcesOfTokens(
      contentsByPath = Map(
        proverbsPath -> tokens(proverbs).get
      ),
      label = "base"
    )

    val leftSources = MappedContentSourcesOfTokens(
      contentsByPath = Map(
        proverbsPath -> tokens(proverbsWithIntraFileMove).get
      ),
      label = "left"
    )

    val rightSources = MappedContentSourcesOfTokens(
      contentsByPath = Map(
        proverbsPath        -> tokens(leftoverProverbsWithEdit).get,
        excisedProverbsPath -> tokens(excisedProverbs).get
      ),
      label = "right"
    )

    val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
      base = baseSources,
      left = leftSources,
      right = rightSources
    )(configuration): @unchecked

    val (mergeResultsByPath, moveDestinationsReport) =
      codeMotionAnalysis.merge

    println(fansi.Color.Yellow(s"Final move destinations report...\n"))
    println(
      fansi.Color
        .Green(moveDestinationsReport.summarizeInText.mkString("\n"))
    )

    verifyContent(excisedProverbsPath, mergeResultsByPath)(
      tokens(excisedProverbsExpectedMerge).get
    )

    verifyContent(proverbsPath, mergeResultsByPath)(
      tokens(leftOverProverbsExpectedMerge).get
    )

  end furtherMigrationOfAMigratedEditAsAnInsertion

  @Test
  def codeMotionAmbiguousWithAPreservation(): Unit =
    val configuration = Configuration(
      minimumMatchSize = 2,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0
    )

    val ehs  = "A".repeat(10)
    val ease = "e".repeat(11)
    val ayes = "I".repeat(12)
    val owes = "o".repeat(13)
    val ewes = "U".repeat(14)
    val wise = "y".repeat(5)

    val bees   = "b".repeat(20)
    val seize  = "c".repeat(21)
    val peas   = "p".repeat(7)
    val queues = "q".repeat(22)

    // On the left, the initial `ease` and the `ayes` go to the end, swapping
    // around. There is however another fixed `ease`, isolated by `peas` being
    // inserted on the left and `queues` being inserted on the right. The moving
    // `ease` is edited into `bees`, and the `ayes` anchors the `seize`.

    val baseText = ehs ++ ease ++ ayes ++ owes ++ ewes ++ ease ++ wise

    val leftText = ehs ++ owes ++ ewes ++ peas ++ ease ++ wise ++ ayes ++ ease

    val rightText =
      ehs ++ bees ++ ayes ++ seize ++ owes ++ ewes ++ ease ++ queues ++ wise

    val expectedMergeText =
      ehs ++ owes ++ ewes ++ peas ++ ease ++ queues ++ wise ++ ayes ++ seize ++ bees

    val placeholderPath: FakePath = "*** STUNT DOUBLE ***"

    val tokenRegex = raw".".r.anchored

    def stuntDoubleTokens(content: String): Vector[Token] = tokenRegex
      .findAllMatchIn(content)
      .map(_.group(0))
      .map(Token.Significant.apply)
      .toVector

    val baseSources = MappedContentSourcesOfTokens(
      contentsByPath = Map(
        placeholderPath -> stuntDoubleTokens(baseText)
      ),
      label = "base"
    )

    val leftSources = MappedContentSourcesOfTokens(
      contentsByPath = Map(
        placeholderPath -> stuntDoubleTokens(leftText)
      ),
      label = "left"
    )

    val rightSources = MappedContentSourcesOfTokens(
      contentsByPath = Map(
        placeholderPath -> stuntDoubleTokens(rightText)
      ),
      label = "right"
    )

    val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
      base = baseSources,
      left = leftSources,
      right = rightSources
    )(configuration): @unchecked

    val (mergeResultsByPath, moveDestinationsReport) =
      codeMotionAnalysis.merge

    println(fansi.Color.Yellow(s"Final move destinations report...\n"))
    println(
      fansi.Color
        .Green(moveDestinationsReport.summarizeInText.mkString("\n"))
    )

    verifyContent(placeholderPath, mergeResultsByPath)(
      stuntDoubleTokens(expectedMergeText)
    )
  end codeMotionAmbiguousWithAPreservation

  @Test
  def coincidences(): Unit =
    val configuration = Configuration(
      minimumMatchSize = 4,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0
    )

    val placeholderPath: FakePath = "*** STUNT DOUBLE ***"

    val baseSources = MappedContentSourcesOfTokens(
      contentsByPath = Map(placeholderPath -> tokens(coincidencesBase).get),
      label = "base"
    )
    val leftSources = MappedContentSourcesOfTokens(
      contentsByPath = Map(placeholderPath -> tokens(coincidencesLeft).get),
      label = "left"
    )
    val rightSources = MappedContentSourcesOfTokens(
      contentsByPath = Map(placeholderPath -> tokens(coincidencesRight).get),
      label = "right"
    )

    val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
      base = baseSources,
      left = leftSources,
      right = rightSources
    )(configuration): @unchecked

    val (mergeResultsByPath, _) =
      codeMotionAnalysis.merge

    verifyContent(placeholderPath, mergeResultsByPath)(
      tokens(coincidencesExpectedMerge).get
    )
  end coincidences

end CodeMotionAnalysisExtensionTest

trait ProseExamples:
  protected val issue23BugReproductionBase: String =
    """
      |chipsSAFEketchupSAFEnoodlesFIZZYsandwichSAFEpudding
      |""".stripMargin

  protected val issue23BugReproductionLeft: String =
    """
      |chipsSAFEketchupSAFEnoodlesBANGsandwichSAFEpudding
      |""".stripMargin

  protected val issue23BugReproductionRight: String =
    """
      |chipsINTRUDERketchupINTRUDERnoodlesFIZZYsandwichINTRUDERpudding
      |""".stripMargin

  protected val issue23BugReproductionExpectedMerge: String =
    """
      |chipsINTRUDERketchupINTRUDERnoodlesBANGsandwichINTRUDERpudding
      |""".stripMargin

  protected val migratedSurroundedInsertionExampleBase: String =
    """
      |FishChipsMushyPeasKetchupMuesliToastTeaKippersNoodlesSandwichCakePudding
      |""".stripMargin

  protected val migratedSurroundedInsertionExampleLeft: String =
    """
      |MuesliToastTeaKippersNoodlesSandwichCakeFishChipsMushyPeasKetchupPudding
      |""".stripMargin

  protected val migratedSurroundedInsertionExampleRight: String =
    """
      |FishChipsCurrySauceMushyPeasKetchupMuesliToastCoffeeKippersNoodlesSandwichCakePudding
      |""".stripMargin

  protected val migratedSurroundedInsertionExampleExpectedMerge: String =
    """
      |MuesliToastCoffeeKippersNoodlesSandwichCakeFishChipsCurrySauceMushyPeasKetchupPudding
      |""".stripMargin

  protected val migratedLeadingInsertionExampleBase: String =
    """
      |FishChipsMushyPeasKetchupMuesliToastTeaKippersNoodlesSandwichCakePudding
      |""".stripMargin

  protected val migratedLeadingInsertionExampleLeft: String =
    """
      |MuesliToastTeaKippersNoodlesSandwichCakeFishChipsMushyPeasKetchupPudding
      |""".stripMargin

  protected val migratedLeadingInsertionExampleRight: String =
    """
      |CurrySauceFishChipsMushyPeasKetchupMuesliToastCoffeeKippersNoodlesSandwichCakePudding
      |""".stripMargin

  protected val migratedLeadingInsertionExampleExpectedMerge: String =
    """
      |MuesliToastCoffeeKippersNoodlesSandwichCakeCurrySauceFishChipsMushyPeasKetchupPudding
      |""".stripMargin

  protected val migratedTrailingInsertionExampleBase: String =
    """
      |FishChipsMushyPeasKetchupMuesliToastTeaKippersNoodlesSandwichCakePudding
      |""".stripMargin

  protected val migratedTrailingInsertionExampleLeft: String =
    """
      |FishChipsMushyPeasKetchupSandwichCakePuddingMuesliToastTeaKippersNoodles
      |""".stripMargin

  protected val migratedTrailingInsertionExampleRight: String =
    """
      |FishChipsMushyPeasKetchupMuesliToastCoffeeKippersNoodlesSandwichCakePuddingFigs
      |""".stripMargin

  protected val migratedTrailingInsertionExampleExpectedMerge: String =
    """
      |FishChipsMushyPeasKetchupSandwichCakePuddingFigsMuesliToastCoffeeKippersNoodles
      |""".stripMargin

  protected val issue42BugReproductionTrailingTokenBase: String =
    """
      |FishChipsMushyPeasKetchupMuesliToastTeaKippers}
      |""".stripMargin

  protected val issue42BugReproductionTrailingTokenLeft: String =
    """
      |MuesliToastTeaKippersFishChipsMushyPeasKetchup
      |""".stripMargin

  protected val issue42BugReproductionTrailingTokenRight: String =
    """
      |FishChipsMushyPeasKetchupPorridge}
      |""".stripMargin

  protected val issue42BugReproductionTrailingTokenMerge: String =
    """
      |PorridgeFishChipsMushyPeasKetchup
      |""".stripMargin

  protected val issue42BugReproductionTrailingTokenAndThenRightEditBase
      : String =
    """
      |FishChipsMushyPeasKetchupMuesliToastTeaKippers}NoodlesSandwichCakePudding
      |""".stripMargin

  protected val issue42BugReproductionTrailingTokenAndThenRightEditLeft
      : String =
    """
      |MuesliToastTeaKippersFishChipsMushyPeasKetchupNoodlesSandwichCakePudding
      |""".stripMargin

  protected val issue42BugReproductionTrailingTokenAndThenRightEditRight
      : String =
    """
      |FishChipsMushyPeasKetchupPorridge}CakePudding
      |""".stripMargin

  protected val issue42BugReproductionTrailingTokenAndThenRightEditMerge
      : String =
    """
      |PorridgeFishChipsMushyPeasKetchupCakePudding
      |""".stripMargin

  protected val issue42BugReproductionLeadingTokenBase: String =
    """
      |FishChipsMushyPeasKetchup{MuesliToastTeaKippers
      |""".stripMargin

  protected val issue42BugReproductionLeadingTokenLeft: String =
    """
      |MuesliToastTeaKippersFishChipsMushyPeasKetchupBangers
      |""".stripMargin

  protected val issue42BugReproductionLeadingTokenRight: String =
    """
      |FishChipsMushyPeasKetchupBangers{Porridge
      |""".stripMargin

  protected val issue42BugReproductionLeadingTokenMerge: String =
    """
      |PorridgeFishChipsMushyPeasKetchupBangers
      |""".stripMargin

  protected val issue42BugReproductionRightEditAndThenLeadingTokenBase: String =
    """
      |FishChipsMushyPeasKetchupNoodlesSandwich{MuesliToastTeaKippersCakePudding
      |""".stripMargin

  protected val issue42BugReproductionRightEditAndThenLeadingTokenLeft: String =
    """
      |MuesliToastTeaKippersFishChipsMushyPeasKetchupNoodlesSandwichCakePudding
      |""".stripMargin

  protected val issue42BugReproductionRightEditAndThenLeadingTokenRight
      : String =
    """
      |FishChipsMushyPeasKetchup{PorridgeCakePudding
      |""".stripMargin

  protected val issue42BugReproductionRightEditAndThenLeadingTokenMerge
      : String =
    """
      |PorridgeFishChipsMushyPeasKetchupCakePudding
      |""".stripMargin

  protected val wordsworth: String =
    """
      |I wandered lonely as a cloud
      |That floats on high o'er vales and hills,
      |When all at once I saw a crowd,
      |A host, of golden daffodils;
      |Beside the lake, beneath the trees,
      |Fluttering and dancing in the breeze.
      |
      |Continuous as the stars that shine
      |And twinkle on the milky way,
      |They stretched in never-ending line
      |Along the margin of a bay:
      |Ten thousand saw I at a glance,
      |Tossing their heads in sprightly dance.
      |
      |The waves beside them danced; but they
      |Out-did the sparkling waves in glee:
      |A poet could not but be gay,
      |In such a jocund company:
      |I gazed—and gazed—but little thought
      |What wealth the show to me had brought:
      |
      |For oft, when on my couch I lie
      |In vacant or in pensive mood,
      |They flash upon that inward eye
      |Which is the bliss of solitude;
      |And then my heart with pleasure fills,
      |And dances with the daffodils.
      |""".stripMargin

  protected val jobsworth: String =
    """
      |I wandered lonely as a cloud
      |That floats on high o'er vales and hills,
      |When all at once I saw a crowd,
      |A host, of golden daffodils;
      |Beside the lake, beneath the trees,
      |Fluttering and dancing in the breeze.
      |
      |I thought, 'Was this part of the job role?'.
      |'Should I be expected to deal with flowers?'
      |The waves beside them danced; but they
      |Out-did the sparkling waves in glee:
      |A poet could not but be gay,
      |In such a jocund company:
      |I gazed—and gazed—but thought only of
      |raising this in the next Zoom meeting.
      |
      |For oft, when on my Aeron I slouch
      |In vacant or in pensive mood,
      |They flash upon that inward eye
      |Which is the bliss of solitude;
      |And then my heart with pleasure fills,
      |And sends an email to human resources.
      |""".stripMargin

  protected val emsworth: String =
    """
      |I wandered lonely as a cloud
      |That floats on high o'er vales and hills,
      |When all at once I saw a crowd,
      |A host, of small fishing boats;
      |Astride the sea, beneath the quay,
      |Rocking and swaying in the breeze.
      |
      |Why this allusion?
      |I Havant a clue!
      |Along the margin of a bay:
      |Ten thousand (well, maybe not quite) saw I at a glance,
      |Tossing their heads in sprightly dance.
      |
      |The waves beside them danced; but they
      |Out-did the sparkling waves in glee:
      |A poet could not but be gay,
      |In such a jocund company:
      |I gazed—and gazed—but little thought
      |What wealth the show to me had brought:
      |
      |For oft, when on my couch I lie
      |In vacant or in pensive mood,
      |They flash upon that inward eye
      |Which is the bliss of solitude;
      |And then my heart with pleasure fills,
      |And sashays with the fishing boats.
      |""".stripMargin

  protected val baseSbtBuild: String =
    """
      |import scala.sys.process.*
      |import scala.language.postfixOps
      |import sbtrelease.ReleaseStateTransformations.*
      |import xerial.sbt.Sonatype.*
      |
      |lazy val javaVersion = "14"
      |
      |ThisBuild / version := "0.1.0-SNAPSHOT"
      |
      |ThisBuild / scalaVersion := "3.3.0"
      |
      |ThisBuild / javacOptions ++= Seq("-source", javaVersion, "-target", javaVersion)
      |
      |lazy val packageExecutable =
      |  taskKey[String]("Package an executable with Coursier")
      |
      |lazy val root = (project in file("."))
      |  .settings(
      |    publishTo              := sonatypePublishToBundle.value,
      |    pomIncludeRepository   := { _ => false },
      |    sonatypeCredentialHost := "s01.oss.sonatype.org",
      |    publishMavenStyle      := true,
      |    licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
      |    organization     := "com.sageserpent",
      |    organizationName := "sageserpent",
      |    description := "Merge branches in the presence of code motion within and between files.",
      |    sonatypeProjectHosting := Some(
      |      GitHubHosting(
      |        user = "sageserpent-open",
      |        repository = "kineticMerge",
      |        email = "gjmurphy1@icloud.com"
      |      )
      |    ),
      |    releaseCrossBuild := false, // No cross-building here - just Scala 3.
      |    releaseProcess := Seq[ReleaseStep](
      |      checkSnapshotDependencies,
      |      inquireVersions,
      |      runClean,
      |      runTest,
      |      setReleaseVersion,
      |      commitReleaseVersion,
      |      tagRelease,
      |      releaseStepCommandAndRemaining(
      |        "publishSigned"
      |      ), // ... finally the publishing step using SBT's own mechanism.
      |      releaseStepCommand("sonatypeBundleRelease"),
      |      releaseStepCommand("packageExecutable"),
      |      setNextVersion,
      |      commitNextVersion,
      |      pushChanges
      |    ),
      |    scalacOptions ++= List("-source:future"),
      |    name := "kinetic-merge",
      |    packageExecutable := {
      |      val _ = publishLocal.value; (rabinFingerprint / publishLocal).value
      |
      |      val localArtifactCoordinates =
      |        s"${organization.value}:${name.value}_${scalaBinaryVersion.value}:${version.value}"
      |
      |      val executablePath = s"${target.value}${Path.sep}${name.value}"
      |
      |      s"cs bootstrap --verbose --bat=true --scala-version ${scalaBinaryVersion.value} -f $localArtifactCoordinates -o $executablePath" !
      |
      |      name.value
      |    })
      |""".stripMargin

  protected val leftSbtBuild: String =
    """
      |import scala.sys.process.*
      |import scala.language.postfixOps
      |import sbtrelease.ReleaseStateTransformations.*
      |import xerial.sbt.Sonatype.*
      |import scala.xml.transform.{RuleTransformer, RewriteRule}
      |import scala.xml.{Node, Elem}
      |
      |enablePlugins(ShadingPlugin)
      |
      |lazy val javaVersion = "14"
      |
      |ThisBuild / version := "0.1.0-SNAPSHOT"
      |
      |ThisBuild / scalaVersion := "3.3.0"
      |
      |ThisBuild / javacOptions ++= Seq("-source", javaVersion, "-target", javaVersion)
      |
      |lazy val packageExecutable =
      |  taskKey[String]("Package an executable with Coursier")
      |
      |lazy val versionResource =
      |  settingKey[File]("Location of generated version resource file.")
      |
      |lazy val root = (project in file("."))
      |  .settings(
      |    publishTo              := sonatypePublishToBundle.value,
      |    pomIncludeRepository   := { _ => false },
      |    sonatypeCredentialHost := "s01.oss.sonatype.org",
      |    publishMavenStyle      := true,
      |    pomPostProcess := { node =>
      |      val rejectedDependencyArtifact =
      |        (rabinFingerprint / Compile / packageBin / artifact).value.name
      |
      |      object removeRejectedDependencyArtifact extends RewriteRule {
      |        override def transform(subtreeNode: Node): Seq[Node] =
      |          subtreeNode match {
      |            case element: Elem if element.label == "dependency" =>
      |              if (
      |                element.child
      |                  .filter(_.label == "artifactId")
      |                  .exists(rejectedDependencyArtifact == _.text)
      |              ) Seq.empty
      |              else Seq(subtreeNode)
      |            case _ => Seq(subtreeNode)
      |          }
      |      }
      |
      |      val transformer = new RuleTransformer(removeRejectedDependencyArtifact)
      |
      |      transformer.transform(node).head
      |    },
      |    licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
      |    organization     := "com.sageserpent",
      |    organizationName := "sageserpent",
      |    description := "Merge branches in the presence of code motion within and between files.",
      |    sonatypeProjectHosting := Some(
      |      GitHubHosting(
      |        user = "sageserpent-open",
      |        repository = "kineticMerge",
      |        email = "gjmurphy1@icloud.com"
      |      )
      |    ),
      |    releaseCrossBuild := false, // No cross-building here - just Scala 3.
      |    releaseProcess := Seq[ReleaseStep](
      |      checkSnapshotDependencies,
      |      inquireVersions,
      |      runClean,
      |      runTest,
      |      setReleaseVersion,
      |      commitReleaseVersion,
      |      tagRelease,
      |      releaseStepCommand("packageExecutable"),
      |      releaseStepCommand(
      |        "publishSigned"
      |      ), // ... finally the publishing step using SBT's own mechanism.
      |      releaseStepCommand("sonatypeBundleRelease"),
      |      setNextVersion,
      |      commitNextVersion,
      |      pushChanges
      |    ),
      |    scalacOptions ++= List("-source:future"),
      |    name := "kinetic-merge",
      |    versionResource := {
      |      val additionalResourcesDirectory = (Compile / resourceManaged).value
      |
      |      additionalResourcesDirectory.toPath.resolve("version.txt").toFile
      |    },
      |    Compile / resourceGenerators += Def.task {
      |      val location = versionResource.value
      |
      |      val packagingVersion = (ThisBuild / version).value
      |
      |      println(
      |        s"Generating version resource: $location for version: $packagingVersion"
      |      )
      |
      |      IO.write(location, packagingVersion)
      |
      |      Seq(location)
      |    }.taskValue,
      |    packageExecutable := {
      |      val packagingVersion = (ThisBuild / version).value
      |
      |      println(s"Packaging executable with version: $packagingVersion")
      |
      |      val localArtifactCoordinates =
      |        s"${organization.value}:${name.value}_${scalaBinaryVersion.value}:$packagingVersion"
      |
      |      val executablePath = s"${target.value}${Path.sep}${name.value}"
      |
      |      s"cs bootstrap --verbose --bat=true --scala-version ${scalaBinaryVersion.value} -f $localArtifactCoordinates -o $executablePath" !
      |
      |      name.value
      |    })
      |""".stripMargin

  protected val rightSbtBuild: String =
    """
      |import scala.sys.process.*
      |import scala.language.postfixOps
      |import sbtrelease.ReleaseStateTransformations.*
      |import xerial.sbt.Sonatype.*
      |
      |lazy val javaVersion = "14"
      |
      |ThisBuild / version := "0.1.0-SNAPSHOT"
      |
      |ThisBuild / scalaVersion := "3.3.0"
      |
      |ThisBuild / javacOptions ++= Seq("-source", javaVersion, "-target", javaVersion)
      |
      |lazy val packageExecutable =
      |  taskKey[String]("Package an executable with Coursier")
      |
      |lazy val root = (project in file("."))
      |  .settings(
      |    publishTo              := sonatypePublishToBundle.value,
      |    pomIncludeRepository   := { _ => false },
      |    sonatypeCredentialHost := "s01.oss.sonatype.org",
      |    publishMavenStyle      := true,
      |    licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
      |    organization     := "com.sageserpent",
      |    organizationName := "sageserpent",
      |    description := "Merge branches in the presence of code motion within and between files.",
      |    sonatypeProjectHosting := Some(
      |      GitHubHosting(
      |        user = "sageserpent-open",
      |        repository = "kineticMerge",
      |        email = "gjmurphy1@icloud.com"
      |      )
      |    ),
      |    releaseCrossBuild := false, // No cross-building here - just Scala 3.
      |    releaseProcess := Seq[ReleaseStep](
      |      checkSnapshotDependencies,
      |      inquireVersions,
      |      runClean,
      |      runTest,
      |      setReleaseVersion,
      |      commitReleaseVersion,
      |      tagRelease,
      |      releaseStepCommandAndRemaining(
      |        "publishSigned"
      |      ), // ... finally the publishing step using SBT's own mechanism.
      |      releaseStepCommand("sonatypeBundleRelease"),
      |      releaseStepCommand("packageExecutable"),
      |      setNextVersion,
      |      commitNextVersion,
      |      pushChanges
      |    ),
      |    scalacOptions ++= List("-source:future"),
      |    name := "kinetic-merge",
      |    packageExecutable := {
      |      val _ = publishLocal.value
      |
      |      val localArtifactCoordinates =
      |        s"${organization.value}:${name.value}_${scalaBinaryVersion.value}:${version.value}"
      |
      |      val executablePath = s"${target.value}${Path.sep}${name.value}"
      |
      |      s"cs bootstrap --verbose --bat=true --scala-version ${scalaBinaryVersion.value} -f $localArtifactCoordinates -o $executablePath" !
      |
      |      name.value
      |    })
      |""".stripMargin

  protected val codeMotionExampleBase: String =
    """
      |package com.sageserpent.kineticmerge.core
      |
      |import com.eed3si9n.expecty.Expecty
      |
      | // Using Kinetic Merge will improve your software engineering practices...
      |object ExpectyFlavouredAssert:
      |  val assert: Expecty = new Expecty:
      |    override val showLocation: Boolean = true
      |    override val showTypes: Boolean    = true
      |  end assert
      |end ExpectyFlavouredAssert
      |""".stripMargin

  protected val codeMotionExampleLeft: String =
    """
      |package com.sageserpent.kineticmerge.core
      |
      |import com.eed3si9n.expecty.Expecty
      |
      | // Using Kinetic Merge will help you improvise in your software engineering experience...
      |object ExpectyFlavouredAssert:
      |  val assert: Expecty = new Expecty:
      |    // Swapped the next two lines around...
      |    override val showTypes: Boolean    = true
      |    override val showLocation: Boolean = true
      |
      |  end assert
      |end ExpectyFlavouredAssert
      |""".stripMargin

  protected val codeMotionExampleRight: String =
    """
      |package com.sageserpent.kineticmerge.core
      |
      |import com.eed3si9n.expecty.Expecty
      |
      |object ExpectyFlavouredAssert:
      |  val assert: Expecty = new Expecty:
      |    override val showLocation: Boolean = true
      |    override val showTypes: Boolean    = false
      |  end assert
      |end ExpectyFlavouredAssert
      |  // Using Kinetic Merge will improve your software engineering practices...
      |""".stripMargin

  protected val codeMotionExampleExpectedMerge: String =
    """
      |package com.sageserpent.kineticmerge.core
      |
      |import com.eed3si9n.expecty.Expecty
      |
      |object ExpectyFlavouredAssert:
      |  val assert: Expecty = new Expecty:
      |    // Swapped the next two lines around...
      |    override val showTypes: Boolean    = false
      |    override val showLocation: Boolean = true
      |
      |  end assert
      |end ExpectyFlavouredAssert
      | // Using Kinetic Merge will help you improvise in your software engineering experience...
      |""".stripMargin

  protected val codeMotionExampleWithSplitOriginalBase: String =
    """
      |package com.sageserpent.americium.java;
      |
      |import com.google.common.base.Preconditions;
      |
      |import java.time.Duration;
      |import java.time.Instant;
      |import java.util.function.Consumer;
      |import java.util.function.Function;
      |import java.util.function.Predicate;
      |
      |import static scala.jdk.javaapi.DurationConverters.toJava;
      |
      |/**
      | * Strategy used to limit the emission of cases by the implementation of
      | * {@link Trials}. These are supplied by client code when calling
      | * {@link Trials#withStrategy(Function)}.
      | *
      | * @apiNote Instances are expected to be stateful, so they should not be
      | * reused when calling the aforementioned overloads.
      | */
      |public interface CasesLimitStrategy {
      |    /**
      |     * Limits test case emission using a time budget that starts when the
      |     * strategy is first consulted via {@link CasesLimitStrategy#moreToDo()}.
      |     *
      |     * @param timeBudget How long to allow a testing cycle to continue to
      |     *                   emit cases.
      |     * @return A fresh strategy instance - the time budget is not consumed
      |     * until the first call to {@link CasesLimitStrategy#moreToDo()}.
      |     */
      |    static CasesLimitStrategy timed(final Duration timeBudget) {
      |        return new CasesLimitStrategy() {
      |            Instant deadline = Instant.MAX;
      |
      |            @Override
      |            public boolean moreToDo() {
      |                if (deadline.equals(Instant.MAX)) {
      |                    deadline = Instant.now().plus(timeBudget);
      |                }
      |
      |                return !Instant.now().isAfter(deadline);
      |            }
      |
      |            @Override
      |            public void noteRejectionOfCase() {
      |
      |            }
      |
      |            @Override
      |            public void noteEmissionOfCase() {
      |
      |            }
      |
      |            @Override
      |            public void noteStarvation() {
      |
      |            }
      |
      |            @Override
      |            public boolean legacyMethod(int whatIsThisFor){ return true; }
      |        };
      |    }
      |
      |    /**
      |     * Limits test case emission using a time budget that starts when the
      |     * strategy is first consulted via {@link CasesLimitStrategy#moreToDo()}.
      |     *
      |     * @param timeBudget How long to allow a testing cycle to continue to
      |     *                   emit cases.
      |     * @return A fresh strategy instance - the time budget is not consumed
      |     * until the first call to {@link CasesLimitStrategy#moreToDo()}.
      |     */
      |    static CasesLimitStrategy timed(
      |            final scala.concurrent.duration.FiniteDuration timeBudget) {
      |        return timed(toJava(timeBudget));
      |    }
      |
      |    /**
      |     * Emulation of Scalacheck's approach to limiting emission of test cases.
      |     *
      |     * @param maximumNumberOfCases   *Upper* limit on the number of cases
      |     *                               emitted. <b>For Scalacheck aficionados:
      |     *                               the name reflects the fact that this is
      |     *                               a limit, contrast with Scalacheck's
      |     *                               {@code minSuccessfulTests}.</b>
      |     * @param maximumStarvationRatio Maximum ratio of case starvation versus
      |     *                               case emission.
      |     * @return A fresh strategy instance.
      |     * @implNote Like Scalacheck, the strategy will allow {@code
      |     * maximumNumberOfCases * maximumStarvationRatio} starvation to take
      |     * place before giving up.
      |     */
      |    static CasesLimitStrategy counted(int maximumNumberOfCases,
      |                                      double maximumStarvationRatio) {
      |        return new CasesLimitStrategy() {
      |            int numberOfCasesEmitted = 0;
      |            int starvationCount = 0;
      |
      |            {
      |                Preconditions.checkArgument(0 <= maximumNumberOfCases);
      |                Preconditions.checkArgument(0 <= maximumStarvationRatio);
      |            }
      |
      |            @Override
      |            public boolean moreToDo() {
      |                return maximumNumberOfCases > numberOfCasesEmitted &&
      |                       starvationCount <=
      |                       maximumNumberOfCases * maximumStarvationRatio;
      |            }
      |
      |            @Override
      |            public void noteRejectionOfCase() {
      |                numberOfCasesEmitted -= 1;
      |                starvationCount += 1;
      |            }
      |
      |            @Override
      |            public void noteEmissionOfCase() {
      |                numberOfCasesEmitted += 1;
      |            }
      |
      |            @Override
      |            public void noteStarvation() {
      |                starvationCount += 1;
      |            }
      |
      |            @Override
      |            public boolean legacyMethod(int whatIsThisFor){ return true; }
      |        };
      |    }
      |
      |    /**
      |     * Query used by the implementation of {@link Trials} to control the
      |     * emission of new cases.
      |     *
      |     * @return True to signal that more cases should be emitted, false to
      |     * stop emission.
      |     * @apiNote Once a call returns false, there should be no further
      |     * interaction with the strategy by the implementation of {@link Trials}
      |     * except for additional calls to this method.
      |     */
      |    boolean moreToDo();
      |
      |    /**
      |     * Notes that inlined case filtration in a test body has rejected a case.
      |     *
      |     * @apiNote This is <b>not</b> called when the filtration provided by
      |     * {@link Trials#filter(Predicate)} rejects a case. When this method is
      |     * called, there should have been a corresponding call to
      |     * {@link CasesLimitStrategy#noteEmissionOfCase} concerning the same
      |     * implied test case that is being backed out of by this method's call.
      |     */
      |    void noteRejectionOfCase();
      |
      |    /**
      |     * Notes that a case has been successfully emitted. The case is
      |     * guaranteed to have been constructed in a different way from all others
      |     * emitted within a call to
      |     * {@link Trials.SupplyToSyntax#supplyTo(Consumer)}.
      |     *
      |     * @apiNote Although each emitted case has been uniquely constructed,
      |     * this does not mean that it is definitely unique in terms of equality;
      |     * for one thing, the equality may be unable to distinguish between
      |     * instances constructed in different ways and for another, the rendition
      |     * of a test case may flatten information causing collisions between test
      |     * cases built in different ways.
      |     */
      |    void noteEmissionOfCase();
      |
      |    /**
      |     * Notes that a case has not been successfully emitted. This can be due
      |     * to it being a duplicate of an earlier case emitted previously in a
      |     * call to {@link Trials.SupplyToSyntax#supplyTo(Consumer)}, or may be
      |     * due to the filtration provided by {@link Trials#filter(Predicate)}
      |     * rejecting a case, or may be due to the complexity limit being breached.
      |     *
      |     * @apiNote This is  <b>not</b> called due to inlined test filtration -
      |     * that is handled by {@link CasesLimitStrategy#noteRejectionOfCase}.
      |     */
      |    void noteStarvation();
      |
      |    boolean legacyMethod(int whatIsThisFor);
      |}
      |""".stripMargin

  protected val codeMotionExampleWithSplitOriginalLeft: String =
    """
      |package com.sageserpent.americium.java;
      |
      |import com.google.common.base.Preconditions;
      |
      |import java.time.Duration;
      |import java.time.Instant;
      |import java.util.function.Consumer;
      |import java.util.function.Function;
      |import java.util.function.Predicate;
      |
      |import static scala.jdk.javaapi.DurationConverters.toJava;
      |
      |/**
      | * Strategy used to limit the emission of cases by the implementation of
      | * {@link Trials}. These are supplied by client code when calling
      | * {@link Trials#withStrategy(Function)}.
      | *
      | * @apiNote Instances are expected to be stateful, so they should not be
      | * reused when calling the aforementioned overloads.
      | */
      |public interface CasesLimitStrategy {
      |    /**
      |     * Limits test case emission using a time budget that starts when the
      |     * strategy is first consulted via
      |     * {@link CasesLimitStrategy#moreCasesToDo()}.
      |     *
      |     * @param timeBudget How long to allow a testing cycle to continue to
      |     *                   emit cases.
      |     * @return A fresh strategy instance - the time budget is not consumed
      |     * until the first call to {@link CasesLimitStrategy#moreCasesToDo()}.
      |     */
      |    static CasesLimitStrategy timed(final Duration timeBudget) {
      |        return new CasesLimitStrategy() {
      |            Instant deadline = null;
      |
      |            @Override
      |            public boolean moreCasesToDo() {
      |                if (null == deadline) {
      |                    deadline = Instant.now().plus(timeBudget);
      |                }
      |
      |                return !Instant.now().isAfter(deadline);
      |            }
      |
      |            @Override
      |            public void noteRejectionOfCase() {
      |
      |            }
      |
      |            @Override
      |            public void noteEmissionOfCase() {
      |
      |            }
      |
      |            @Override
      |            public void noteStarvation() {
      |
      |            }
      |        };
      |    }
      |
      |    /**
      |     * Limits test case emission using a time budget that starts when the
      |     * strategy is first consulted via
      |     * {@link CasesLimitStrategy#moreCasesToDo()}.
      |     *
      |     * @param timeBudget How long to allow a testing cycle to continue to
      |     *                   emit cases.
      |     * @return A fresh strategy instance - the time budget is not consumed
      |     * until the first call to {@link CasesLimitStrategy#moreCasesToDo()}.
      |     */
      |    static CasesLimitStrategy timed(
      |            final scala.concurrent.duration.FiniteDuration timeBudget) {
      |        return timed(toJava(timeBudget));
      |    }
      |
      |    /**
      |     * Emulation of Scalacheck's approach to limiting emission of test cases.
      |     *
      |     * @param maximumNumberOfCases   *Upper* limit on the number of cases
      |     *                               emitted. <b>For Scalacheck aficionados:
      |     *                               the name reflects the fact that this is
      |     *                               a limit, contrast with Scalacheck's
      |     *                               {@code minSuccessfulTests}.</b>
      |     * @param maximumStarvationRatio Maximum ratio of case starvation versus
      |     *                               case emission.
      |     * @return A fresh strategy instance.
      |     * @implNote Like Scalacheck, the strategy will allow {@code
      |     * maximumNumberOfCases * maximumStarvationRatio} starvation to take
      |     * place before giving up.
      |     */
      |    static CasesLimitStrategy counted(int maximumNumberOfCases,
      |                                      double maximumStarvationRatio) {
      |        return new CasesLimitStrategy() {
      |            int numberOfCasesEmitted = 0;
      |            int starvationCount = 0;
      |
      |            {
      |                Preconditions.checkArgument(0 <= maximumNumberOfCases);
      |                Preconditions.checkArgument(0 <= maximumStarvationRatio);
      |            }
      |
      |            @Override
      |            public boolean moreCasesToDo() {
      |                return maximumNumberOfCases > numberOfCasesEmitted &&
      |                       starvationCount <=
      |                       maximumNumberOfCases * maximumStarvationRatio;
      |            }
      |
      |            @Override
      |            public void noteRejectionOfCase() {
      |                numberOfCasesEmitted -= 1;
      |                starvationCount += 1;
      |            }
      |
      |            @Override
      |            public void noteEmissionOfCase() {
      |                numberOfCasesEmitted += 1;
      |            }
      |
      |            @Override
      |            public void noteStarvation() {
      |                starvationCount += 1;
      |            }
      |        };
      |    }
      |
      |    /**
      |     * Query used by the implementation of {@link Trials} to control the
      |     * emission of new cases.
      |     *
      |     * @return True to signal that more cases should be emitted, false to
      |     * stop emission.
      |     * @apiNote Once a call returns false, there should be no further
      |     * interaction with the strategy by the implementation of {@link Trials}
      |     * except for additional calls to this method.
      |     */
      |    boolean moreCasesToDo(); // This rename should migrate.
      |
      |    /**
      |     * Notes that inlined case filtration in a test body has rejected a case.
      |     *
      |     * @apiNote This is <b>not</b> called when the filtration provided by
      |     * {@link Trials#filter(Predicate)} rejects a case. When this method is
      |     * called, there should have been a corresponding call to
      |     * {@link CasesLimitStrategy#noteEmissionOfCase} concerning the same
      |     * implied test case that is being backed out of by this method's call.
      |     */
      |    void noteRejectionOfCase();
      |
      |    /**
      |     * Notes that a case has been successfully emitted. The case is
      |     * guaranteed to have been constructed in a different way from all others
      |     * emitted within a call to
      |     * {@link Trials.SupplyToSyntax#supplyTo(Consumer)}.
      |     *
      |     * @apiNote Although each emitted case has been uniquely constructed,
      |     * this does not mean that it is definitely unique in terms of equality;
      |     * for one thing, the equality may be unable to distinguish between
      |     * instances constructed in different ways and for another, the rendition
      |     * of a test case may flatten information causing collisions between test
      |     * cases built in different ways.
      |     */
      |    void noteEmissionOfCase();
      |
      |    /**
      |     * Notes that a case has not been successfully emitted. This can be due
      |     * to it being a duplicate of an earlier case emitted previously in a
      |     * call to {@link Trials.SupplyToSyntax#supplyTo(Consumer)}, or may be
      |     * due to the filtration provided by {@link Trials#filter(Predicate)}
      |     * rejecting a case, or may be due to the complexity limit being breached.
      |     *
      |     * @apiNote This is  <b>not</b> called due to inlined test filtration -
      |     * that is handled by {@link CasesLimitStrategy#noteRejectionOfCase}.
      |     */
      |    void noteStarvation();
      |}
      |""".stripMargin

  protected val codeMotionExampleWithSplitOriginalRight: String =
    """
      |package com.sageserpent.americium.java;
      |
      |import java.util.function.Consumer;
      |import java.util.function.Function;
      |import java.util.function.Predicate;
      |
      |/**
      | * Strategy used to limit the emission of cases by the implementation of
      | * {@link Trials}. These are supplied by client code when calling
      | * {@link Trials#withStrategy(Function)}.
      | *
      | * @apiNote Instances are expected to be stateful, so they should not be
      | * reused when calling the aforementioned overloads.
      | */
      |public interface CasesLimitStrategy {
      |    /**
      |     * Query used by the implementation of {@link Trials} to control the
      |     * emission of new cases.
      |     *
      |     * @return True to signal that more cases should be emitted, false to
      |     * stop emission.
      |     * @apiNote Once a call returns false, there should be no further
      |     * interaction with the strategy by the implementation of {@link Trials}
      |     * except for additional calls to this method.
      |     */
      |    boolean moreToDo();
      |
      |    /**
      |     * Notes that inlined case filtration in a test body has rejected a case.
      |     *
      |     * @apiNote This is <b>not</b> called when the filtration provided by
      |     * {@link Trials#filter(Predicate)} rejects a case. When this method is
      |     * called, there should have been a corresponding call to
      |     * {@link CasesLimitStrategy#noteEmissionOfCase} concerning the same
      |     * implied test case that is being backed out of by this method's call.
      |     */
      |    void noteRejectionOfCase();
      |
      |    /**
      |     * Notes that a case has been successfully emitted. The case is
      |     * guaranteed to have been constructed in a different way from all others
      |     * emitted within a call to
      |     * {@link Trials.SupplyToSyntax#supplyTo(Consumer)}.
      |     *
      |     * @apiNote Although each emitted case has been uniquely constructed,
      |     * this does not mean that it is definitely unique in terms of equality;
      |     * for one thing, the equality may be unable to distinguish between
      |     * instances constructed in different ways and for another, the rendition
      |     * of a test case may flatten information causing collisions between test
      |     * cases built in different ways.
      |     */
      |    void noteEmissionOfCase();
      |
      |    /**
      |     * Notes that a case has not been successfully emitted. This can be due
      |     * to it being a duplicate of an earlier case emitted previously in a
      |     * call to {@link Trials.SupplyToSyntax#supplyTo(Consumer)}, or may be
      |     * due to the filtration provided by {@link Trials#filter(Predicate)}
      |     * rejecting a case, or may be due to the complexity limit being breached.
      |     *
      |     * @apiNote This is  <b>not</b> called due to inlined test filtration -
      |     * that is handled by {@link CasesLimitStrategy#noteRejectionOfCase}.
      |     */
      |    void noteStarvation();
      |
      |    boolean legacyMethod(int whatIsThisFor);
      |}
      |""".stripMargin

  protected val codeMotionExampleWithSplitHivedOffRight: String =
    """
      |package com.sageserpent.americium.java;
      |
      |import com.google.common.base.Preconditions;
      |
      |import java.time.Duration;
      |import java.time.Instant;
      |
      |import static scala.jdk.javaapi.DurationConverters.toJava;
      |
      |public interface CasesLimitStrategies {
      |    /**
      |     * Limits test case emission using a time budget that starts when the
      |     * strategy is first consulted via {@link CasesLimitStrategy#moreToDo()}.
      |     *
      |     * @param timeBudget How long to allow a testing cycle to continue to
      |     *                   emit cases.
      |     * @return A fresh strategy instance - the time budget is not consumed
      |     * until the first call to {@link CasesLimitStrategy#moreToDo()}.
      |     */
      |    static CasesLimitStrategy timed(final Duration timeBudget) {
      |        return new CasesLimitStrategy() {
      |            Instant deadline = Instant.MAX;
      |
      |            @Override
      |            public boolean moreToDo() {
      |                if (deadline.equals(Instant.MAX)) {
      |                    deadline = Instant.now().plus(timeBudget);
      |                }
      |
      |                return !Instant.now().isAfter(deadline);
      |            }
      |
      |            @Override
      |            public void noteRejectionOfCase() {
      |
      |            }
      |
      |            @Override
      |            public void noteEmissionOfCase() {
      |
      |            }
      |
      |            @Override
      |            public void noteStarvation() {
      |
      |            }
      |
      |            @Override
      |            public boolean legacyMethod(int whatIsThisFor){ return true; }
      |        };
      |    }
      |
      |    /**
      |     * Limits test case emission using a time budget that starts when the
      |     * strategy is first consulted via {@link CasesLimitStrategy#moreToDo()}.
      |     *
      |     * @param timeBudget How long to allow a testing cycle to continue to
      |     *                   emit cases.
      |     * @return A fresh strategy instance - the time budget is not consumed
      |     * until the first call to {@link CasesLimitStrategy#moreToDo()}.
      |     */
      |    static CasesLimitStrategy timed(
      |            final scala.concurrent.duration.FiniteDuration timeBudget) {
      |        return timed(toJava(timeBudget));
      |    }
      |
      |    /**
      |     * Emulation of Scalacheck's approach to limiting emission of test cases.
      |     *
      |     * @param maximumNumberOfCases   *Upper* limit on the number of cases
      |     *                               emitted. <b>For Scalacheck aficionados:
      |     *                               the name reflects the fact that this is
      |     *                               a limit, contrast with Scalacheck's
      |     *                               {@code minSuccessfulTests}.</b>
      |     * @param maximumStarvationRatio Maximum ratio of case starvation versus
      |     *                               case emission.
      |     * @return A fresh strategy instance.
      |     * @implNote Like Scalacheck, the strategy will allow {@code
      |     * maximumNumberOfCases * maximumStarvationRatio} starvation to take
      |     * place before giving up.
      |     */
      |    static CasesLimitStrategy counted(int maximumNumberOfCases,
      |                                      double maximumStarvationRatio) {
      |        return new CasesLimitStrategy() {
      |            int numberOfCasesEmitted = 0;
      |            int starvationCount = 0;
      |
      |            {
      |                Preconditions.checkArgument(0 <= maximumNumberOfCases);
      |                Preconditions.checkArgument(0 <= maximumStarvationRatio);
      |            }
      |
      |            @Override
      |            public boolean moreToDo() {
      |                return maximumNumberOfCases > numberOfCasesEmitted &&
      |                       starvationCount <=
      |                       maximumNumberOfCases * maximumStarvationRatio;
      |            }
      |
      |            @Override
      |            public void noteRejectionOfCase() {
      |                numberOfCasesEmitted -= 1;
      |                starvationCount += 1;
      |            }
      |
      |            @Override
      |            public void noteEmissionOfCase() {
      |                numberOfCasesEmitted += 1;
      |            }
      |
      |            @Override
      |            public void noteStarvation() {
      |                starvationCount += 1;
      |            }
      |
      |            @Override
      |            public boolean legacyMethod(int whatIsThisFor){ return true; }
      |        };
      |    }
      |
      |
      |}
      |""".stripMargin

  protected val codeMotionExampleWithSplitOriginalExpectedMerge: String =
    """
      |package com.sageserpent.americium.java;
      |
      |import java.util.function.Consumer;
      |import java.util.function.Function;
      |import java.util.function.Predicate;
      |
      |/**
      | * Strategy used to limit the emission of cases by the implementation of
      | * {@link Trials}. These are supplied by client code when calling
      | * {@link Trials#withStrategy(Function)}.
      | *
      | * @apiNote Instances are expected to be stateful, so they should not be
      | * reused when calling the aforementioned overloads.
      | */
      |public interface CasesLimitStrategy {
      |    /**
      |     * Query used by the implementation of {@link Trials} to control the
      |     * emission of new cases.
      |     *
      |     * @return True to signal that more cases should be emitted, false to
      |     * stop emission.
      |     * @apiNote Once a call returns false, there should be no further
      |     * interaction with the strategy by the implementation of {@link Trials}
      |     * except for additional calls to this method.
      |     */
      |    boolean moreCasesToDo(); // This rename should migrate.
      |
      |    /**
      |     * Notes that inlined case filtration in a test body has rejected a case.
      |     *
      |     * @apiNote This is <b>not</b> called when the filtration provided by
      |     * {@link Trials#filter(Predicate)} rejects a case. When this method is
      |     * called, there should have been a corresponding call to
      |     * {@link CasesLimitStrategy#noteEmissionOfCase} concerning the same
      |     * implied test case that is being backed out of by this method's call.
      |     */
      |    void noteRejectionOfCase();
      |
      |    /**
      |     * Notes that a case has been successfully emitted. The case is
      |     * guaranteed to have been constructed in a different way from all others
      |     * emitted within a call to
      |     * {@link Trials.SupplyToSyntax#supplyTo(Consumer)}.
      |     *
      |     * @apiNote Although each emitted case has been uniquely constructed,
      |     * this does not mean that it is definitely unique in terms of equality;
      |     * for one thing, the equality may be unable to distinguish between
      |     * instances constructed in different ways and for another, the rendition
      |     * of a test case may flatten information causing collisions between test
      |     * cases built in different ways.
      |     */
      |    void noteEmissionOfCase();
      |
      |    /**
      |     * Notes that a case has not been successfully emitted. This can be due
      |     * to it being a duplicate of an earlier case emitted previously in a
      |     * call to {@link Trials.SupplyToSyntax#supplyTo(Consumer)}, or may be
      |     * due to the filtration provided by {@link Trials#filter(Predicate)}
      |     * rejecting a case, or may be due to the complexity limit being breached.
      |     *
      |     * @apiNote This is  <b>not</b> called due to inlined test filtration -
      |     * that is handled by {@link CasesLimitStrategy#noteRejectionOfCase}.
      |     */
      |    void noteStarvation();
      |}
      |""".stripMargin

  protected val codeMotionExampleWithSplitHivedOffExpectedMerge: String =
    """
      |package com.sageserpent.americium.java;
      |
      |import com.google.common.base.Preconditions;
      |
      |import java.time.Duration;
      |import java.time.Instant;
      |
      |import static scala.jdk.javaapi.DurationConverters.toJava;
      |
      |public interface CasesLimitStrategies {
      |    /**
      |     * Limits test case emission using a time budget that starts when the
      |     * strategy is first consulted via
      |     * {@link CasesLimitStrategy#moreCasesToDo()}.
      |     *
      |     * @param timeBudget How long to allow a testing cycle to continue to
      |     *                   emit cases.
      |     * @return A fresh strategy instance - the time budget is not consumed
      |     * until the first call to {@link CasesLimitStrategy#moreCasesToDo()}.
      |     */
      |    static CasesLimitStrategy timed(final Duration timeBudget) {
      |        return new CasesLimitStrategy() {
      |            Instant deadline = null;
      |
      |            @Override
      |            public boolean moreCasesToDo() {
      |                if (null == deadline) {
      |                    deadline = Instant.now().plus(timeBudget);
      |                }
      |
      |                return !Instant.now().isAfter(deadline);
      |            }
      |
      |            @Override
      |            public void noteRejectionOfCase() {
      |
      |            }
      |
      |            @Override
      |            public void noteEmissionOfCase() {
      |
      |            }
      |
      |            @Override
      |            public void noteStarvation() {
      |
      |            }
      |        };
      |    }
      |
      |    /**
      |     * Limits test case emission using a time budget that starts when the
      |     * strategy is first consulted via
      |     * {@link CasesLimitStrategy#moreCasesToDo()}.
      |     *
      |     * @param timeBudget How long to allow a testing cycle to continue to
      |     *                   emit cases.
      |     * @return A fresh strategy instance - the time budget is not consumed
      |     * until the first call to {@link CasesLimitStrategy#moreCasesToDo()}.
      |     */
      |    static CasesLimitStrategy timed(
      |            final scala.concurrent.duration.FiniteDuration timeBudget) {
      |        return timed(toJava(timeBudget));
      |    }
      |
      |    /**
      |     * Emulation of Scalacheck's approach to limiting emission of test cases.
      |     *
      |     * @param maximumNumberOfCases   *Upper* limit on the number of cases
      |     *                               emitted. <b>For Scalacheck aficionados:
      |     *                               the name reflects the fact that this is
      |     *                               a limit, contrast with Scalacheck's
      |     *                               {@code minSuccessfulTests}.</b>
      |     * @param maximumStarvationRatio Maximum ratio of case starvation versus
      |     *                               case emission.
      |     * @return A fresh strategy instance.
      |     * @implNote Like Scalacheck, the strategy will allow {@code
      |     * maximumNumberOfCases * maximumStarvationRatio} starvation to take
      |     * place before giving up.
      |     */
      |    static CasesLimitStrategy counted(int maximumNumberOfCases,
      |                                      double maximumStarvationRatio) {
      |        return new CasesLimitStrategy() {
      |            int numberOfCasesEmitted = 0;
      |            int starvationCount = 0;
      |
      |            {
      |                Preconditions.checkArgument(0 <= maximumNumberOfCases);
      |                Preconditions.checkArgument(0 <= maximumStarvationRatio);
      |            }
      |
      |            @Override
      |            public boolean moreCasesToDo() {
      |                return maximumNumberOfCases > numberOfCasesEmitted &&
      |                       starvationCount <=
      |                       maximumNumberOfCases * maximumStarvationRatio;
      |            }
      |
      |            @Override
      |            public void noteRejectionOfCase() {
      |                numberOfCasesEmitted -= 1;
      |                starvationCount += 1;
      |            }
      |
      |            @Override
      |            public void noteEmissionOfCase() {
      |                numberOfCasesEmitted += 1;
      |            }
      |
      |            @Override
      |            public void noteStarvation() {
      |                starvationCount += 1;
      |            }
      |        };
      |    }
      |
      |
      |}
      |""".stripMargin

  protected val whitespaceOnlyChangeExampleBase: String =
    """
      |{
      |  "array": [
      |    1,
      |    2,
      |    3
      |  ],
      |  "boolean": true,
      |  "color": "gold",
      |  "null": null,
      |  "number": 123,
      |  "object": {
      |    "a": "b",
      |    "c": "d"
      |  },
      |  "string": "Hello World"
      |}
      |""".stripMargin

  protected val whitespaceOnlyChangeExampleLeftEdited: String =
    """
        |{
        |  "array": [
        |    1,
        |    2,
        |    3
        |  ],
        |  "boolean": true,
        |  "color": "gold",
        |  "null": null,
        |  "number": 123,
        |      "object": {
        |        "a": "b",
        |        "c": "d"
        |      },
        |  "string": "Hello World"
        |}
        |""".stripMargin

  protected val whitespaceOnlyChangeExampleRightEdited: String =
    """
      |{
      |  "array": [
      |    1,
      |    2,
      |    3
      |  ],
      |      "boolean": true,
      |  "color": "gold",
      |  "null": null,
      |  "number": 123,
      |  "object": {
      |    "a": "b",
      |    "c": "d"
      |  },
      |    "string": "Hello World"
      |}
      |""".stripMargin

  protected val heyDiddleDiddleInModernForm: String =
    """
      |Hey diddle diddle,
      |The cat and the fiddle,
      |The cow jumped over the moon;
      |The little dog laughed
      |To see such sport,
      |And the dish ran away with the spoon.
      |""".stripMargin

  protected val heyDiddleDiddleInArchaicForm: String =
    """
      |Hey diddle diddle,
      |The cat and the fiddle,
      |The cow jump'd over the moon;
      |The little dog laugh'd
      |To see such craft,
      |And the fork ran away with the spoon.
      |""".stripMargin

  protected val heyDiddleDiddleWithInsertions: String =
    """
      |Hey diddle diddle,
      |The cat and the fiddle,
      |The cow jumped over the moon;
      |The little dog laughed
      |To see such sport, (they know how to have a party)
      |And the dish ran away with the spoon.
      |""".stripMargin

  protected val heyDiddleDiddleWithInsertionsExpectedMerge: String =
    """
      |Hey diddle diddle,
      |The cat and the fiddle,
      |The cow jump'd over the moon;
      |The little dog laugh'd
      |To see such craft, (they know how to have a party)
      |And the fork ran away with the spoon.
      |""".stripMargin

  protected val heyDiddleDiddleWithIntraFileMove: String =
    """
      |Hey diddle diddle,
      |The cat and the fiddle,
      |The cow jumped!
      |The little dog laughed
      |To see such sport,
      |And the dish ran away with the spoon over the moon.
      |""".stripMargin

  protected val heyDiddleDiddleWithIntraFileMoveExpectedMerge: String =
    """
      |Hey diddle diddle,
      |The cat and the fiddle,
      |The cow jump'd!
      |The little dog laugh'd
      |To see such craft,
      |And the fork ran away with the spoon over the moon.
      |""".stripMargin

  protected val heyDiddleDiddleWithIntraFileMoveAndSurroundingInsertions
      : String =
    """
      |Hey diddle diddle,
      |The cat and the fiddle,
      |The cow jumped!
      |The little dog laughed
      |To see such sport, (in fact he was over the moon)
      |And the dish ran away with the spoon.
      |""".stripMargin

  protected val heyDiddleDiddleWithIntraFileMoveAndSurroundingInsertionsExpectedMerge
      : String =
    """
      |Hey diddle diddle,
      |The cat and the fiddle,
      |The cow jump'd!
      |The little dog laugh'd
      |To see such craft, (in fact he was over the moon)
      |And the fork ran away with the spoon.
      |""".stripMargin

  protected val heyDiddleDiddleInPsychoticForm: String =
    """
      |Hey diddle diddle,
      |The cat and the fiddle,
      |The cow, laughing madly, ran away with the spoon and then jump'd over the moon;
      |The little dog laugh'd
      |To see such craft,
      |And the fork ran away with the spoon.
      |""".stripMargin

  // TODO: review the expected merge in the context of
  // https://github.com/sageserpent-open/kineticMerge/issues/83...
  protected val heyDiddleDiddleInPsychoticFormExpectedMerge: String =
    """
      |Hey diddle diddle,
      |The cat and the fiddle,
      |The cow, laughing madly, ran away with the spoon and then jump'd!
      |The little dog laugh'd
      |To see such craft,
      |And the fork ran away with the spoon over the moon.
      |""".stripMargin

  protected val heyDiddleDiddleWithDeletionAtDestination: String =
    """
      |Hey diddle diddle,
      |The cat and the fiddle,
      |The cow jumped over the moon;
      |The little dog laughed
      |To see such
      |And the dish ran away with the spoon.
      |""".stripMargin

  protected val heyDiddleDiddleWithInsertionsAndADeletionAtDestinationExpectedMerge
      : String =
    """
      |Hey diddle diddle,
      |The cat and the fiddle,
      |The cow jumped over the moon;
      |The little dog laughed
      |To see such (they know how to have a party)
      |And the dish ran away with the spoon.
      |""".stripMargin

  protected val proverbs: String =
    """
      |A bird in hand is worth two in the bush.
      |A stitch in time saves nine.
      |Fools rush in.
      |All's well that ends well.
      |Better a gramme than a damn.
      |""".stripMargin

  protected val palindromes: String =
    // Thank you, Wikipedia, for the last two entries! :-)
    """
      |Able was I ere I saw Elba
      |A man, a plan, a canal, Panama
      |Rats live on no evil star
      |No one made killer apparel like Dame Noon
      |""".stripMargin

  protected val proverbsMeetAgileConsultant: String =
    """
      |A bird in hand is worth two in the bush.
      |A stitch in time saves nine (but you aren't going to need it).
      |Fools rush in.
      |All's well that ends well.
      |Better a gramme than a damn.
      |""".stripMargin

  protected val palindromesMeetAgileConsultant: String =
    """
      |Able was I ere I saw Elba
      |A man, a plan (but you aren't going to need it), a canal, Panama
      |Rats live on no evil star
      |No one made killer apparel like Dame Noon
      |""".stripMargin

  protected val concatenatedWordPlay: String =
    """
      |A bird in hand is worth two in the bush.
      |A stitch in time saves nine.
      |Fools rush in.
      |All's well that ends well.
      |Better a gramme than a damn.
      |Able was I ere I saw Elba
      |A man, a plan, a canal, Panama
      |Rats live on no evil star
      |No one made killer apparel like Dame Noon
      |""".stripMargin

  protected val concatenatedWordPlayExpectedMerge: String =
    """
      |A bird in hand is worth two in the bush.
      |A stitch in time saves nine (but you aren't going to need it).
      |Fools rush in.
      |All's well that ends well.
      |Better a gramme than a damn.
      |Able was I ere I saw Elba
      |A man, a plan (but you aren't going to need it), a canal, Panama
      |Rats live on no evil star
      |No one made killer apparel like Dame Noon
      |""".stripMargin

  protected val jumbledWordPlay: String =
    // NOTE: the first line is not preceded by a linebreak, this is just to work
    // around the finicky treatment of a leading whitespace token when comparing
    // sequences of tokens for equality.
    """Fools rush in.
      |All's well that ends well.
      |Better a gramme than a damn.
      |A bird in hand is worth two in the bush.
      |A stitch in time saves nine, a canal, Panama.
      |Able was I ere I saw Elba
      |A man, a plan
      |Rats live on no evil star
      |No one made killer apparel like Dame Noon
      |""".stripMargin

  protected val jumbledWordPlayExpectedMerge: String =
    // NOTE: the first line is not preceded by a linebreak, this is just to work
    // around the finicky treatment of a leading whitespace token when comparing
    // sequences of tokens for equality.
    """(but you aren't going to need it) Fools rush in.
      |All's well that ends well.
      |Better a gramme than a damn.
      |A bird in hand is worth two in the bush.
      |A stitch in time saves nine (but you aren't going to need it), a canal, Panama.
      |Able was I ere I saw Elba
      |A man, a plan (but you aren't going to need it)
      |Rats live on no evil star
      |No one made killer apparel like Dame Noon
      |""".stripMargin

  protected val moveToTheEndWordPlay: String =
    """
      |A bird in hand is worth two in the bush.
      |Fools rush in.
      |All's well that ends well.
      |Better a gramme than a damn.
      |Able was I ere I saw Elba
      |A man, a plan, a canal, Panama
      |Rats live on no evil star
      |No one made killer apparel like Dame Noon
      |A stitch in time saves nine.
      |""".stripMargin

  protected val moveToTheEndWordPlayExpectedMerge: String =
    """
      |A bird in hand is worth two in the bush (but you aren't going to need it).
      |Fools rush in.
      |All's well that ends well.
      |Better a gramme than a damn.
      |Able was I ere I saw Elba
      |A man, a plan (but you aren't going to need it), a canal, Panama
      |Rats live on no evil star
      |No one made killer apparel like Dame Noon
      |A stitch in time saves nine (but you aren't going to need it).
      |""".stripMargin

  protected val makeLeadingLinesFromBothHalvesAdjacentWordPlay: String =
    """
      |A bird in hand is worth two in the bush.
      |A stitch in time saves nine.
      |A man, a plan, a canal, Panama
      |Fools rush in.
      |All's well that ends well.
      |Better a gramme than a damn.
      |Able was I ere I saw Elba
      |Rats live on no evil star
      |No one made killer apparel like Dame Noon
      |""".stripMargin

  protected val makeLeadingLinesFromBothHalvesAdjacentWordPlayExpectedMerge
      : String =
    """
      |A bird in hand is worth two in the bush.
      |A stitch in time saves nine (but you aren't going to need it).
      |A man, a plan (but you aren't going to need it), a canal, Panama
      |(but you aren't going to need it)
      |Fools rush in.
      |All's well that ends well.
      |Better a gramme than a damn.
      |Able was I ere I saw Elba
      |Rats live on no evil star
      |No one made killer apparel like Dame Noon
      |""".stripMargin

  protected val makeLeadingLinesFromBothHalvesAdjacentWithSomeLeadingDeletionWordPlay
      : String =
    """
      |A bird in hand is worth two in the bush.
      |A stitch in time saves.
      |A man, a plan, a canal, Panama
      |Fools rush in.
      |All's well that ends well.
      |Better a gramme than a damn.
      |Able was I ere I saw Elba
      |Rats live on no evil star
      |No one made killer apparel like Dame Noon
      |""".stripMargin

  protected val makeLeadingLinesFromBothHalvesAdjacentWithSomeLeadingDeletionWordPlayExpectedMerge
      : String =
    // NASTY HACK: note the line:
    // "A stitch in time saves. (but you aren't going to need it)".
    // This is to acknowledge that when a preceding anchor ("a stitch in time
    // saves") is followed by another preceding anchor ("A man, a plan"), then
    // the handling of deferred content at the destination is really clumsy.
    """
      |A bird in hand is worth two in the bush.
      |A stitch in time saves. (but you aren't going to need it)
      |A man, a plan (but you aren't going to need it), a canal, Panama
      |(but you aren't going to need it)
      |Fools rush in.
      |All's well that ends well.
      |Better a gramme than a damn.
      |Able was I ere I saw Elba
      |Rats live on no evil star
      |No one made killer apparel like Dame Noon
      |""".stripMargin

  protected val proverbsWithIntraFileMove: String =
    """
      |A bird in hand is worth two in the bush.
      |Better a gramme than a damn.
      |A stitch in time saves nine.
      |Fools rush in.
      |All's well that ends well.
      |""".stripMargin

  protected val leftoverProverbsWithEdit: String =
    // FIXME: once https://github.com/sageserpent-open/kineticMerge/issues/83 is
    // in, amend the *last* line to end in an exclamation mark instead of a full
    // stop.
    """Fools rush in.
      |All's well that ends well.
      |Better eat gram flour, not the damned flowers.
      |""".stripMargin

  protected val excisedProverbs: String =
    """
      |A bird in hand is worth two in the bush.
      |A stitch in time saves nine.
      |""".stripMargin

  protected val excisedProverbsExpectedMerge: String =
    // FIXME: once https://github.com/sageserpent-open/kineticMerge/issues/83 is
    // in, amend the *second* line to end in an exclamation mark instead of a
    // full stop.
    """
      |A bird in hand is worth two in the bush.
      |Better eat gram flour, not the damned flowers.
      |A stitch in time saves nine.
      |""".stripMargin

  protected val leftOverProverbsExpectedMerge: String =
    """Fools rush in.
      |All's well that ends well.
      |""".stripMargin

  protected val coincidencesBase: String =
    """
      |Fancy meeting you here!
      |I've got to drop from this call and jump on another - oh, you too?
      |Many of these new employees happen to be a relative of the same person.
      |""".stripMargin

  protected val coincidencesLeft: String =
    """
      |I support the left...
      |I've got to drop from this call and jump on another - oh, you too?
      |Many of these new employees happen to be a relative of the same person.
      |It seems everyone wants to pay less tax and expects better council repairs.
      |Fancy meeting you here!
      |""".stripMargin

  protected val coincidencesRight: String =
    """
      |I've got to drop from this call and jump on another - oh, you too?
      |... although I'm leaning to the right.
      |Many of these new employees happen to be a relative of the same person.
      |It seems everyone wants to pay less tax and expects better council repairs.
      |Fancy meeting you here!
      |""".stripMargin

  protected val coincidencesExpectedMerge: String =
    """
      |I support the left...
      |I've got to drop from this call and jump on another - oh, you too?
      |... although I'm leaning to the right.
      |Many of these new employees happen to be a relative of the same person.
      |It seems everyone wants to pay less tax and expects better council repairs.
      |Fancy meeting you here!
      |""".stripMargin
end ProseExamples
