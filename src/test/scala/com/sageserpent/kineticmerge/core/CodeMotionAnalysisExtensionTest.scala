package com.sageserpent.kineticmerge.core

import cats.{Eq, Order}
import com.google.common.hash.{Funnel, HashFunction, Hashing}
import com.sageserpent.americium.Trials
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.CodeMotionAnalysis.Configuration
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisExtension.*
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisExtensionTest.{
  FakePath,
  reconstituteTextFrom,
  given
}
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
  @TestFactory
  def migrationScenariosFromExcalidrawDocuments(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 1,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    Trials.api
      .choose(
        (
          "ATheStripedMoverBC",
          "ABCTheStripedMover",
          "ABCTheStripedMover",
          "ABCTheStripedMover",
          "https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/coincidentMove.excalidraw.svg"
        ),
        (
          "ATheStripedMoverBC",
          "ABCTheStripedMover",
          "TheStripedMoverABC",
          "TheStripedMoverABCTheStripedMover",
          "https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/divergentMoves.excalidraw.svg"
        ),
        (
          "ATheStripedMoverBC",
          "ABCTheStripedMover",
          "ABC",
          "ABC",
          "https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/migratingADeletion.excalidraw.svg"
        ),
        (
          "ATheStripedMoverBC",
          "ATheCoincidentStripedEditBCTheStripedMover",
          "ATheCoincidentStripedEditBC",
          "ATheCoincidentStripedEditBC",
          "https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/migratingADeletionWithACoincidentEdit.excalidraw.svg"
        ),
        (
          "ATheStripedMoverBC",
          "APBCTheStripedMover",
          "ABC",
          "APBC",
          "https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/migratingADeletionWithAnEditOnTheMoveSide.excalidraw.svg"
        ),
        (
          "ATheStripedMoverBC",
          "ABCTheStripedMover",
          "APBC",
          "ABCP",
          "https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/migratingAnEdit.excalidraw.svg"
        ),
        (
          "ATheStripedMoverBC",
          "APBCTheStripedMover",
          "AQBC",
          "APBCQ",
          "https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/migratingAnEditWithAnEditOnTheMoveSide.excalidraw.svg"
        ),
        (
          "ATheStripedMoverBC",
          "ABCTheStripedMover",
          "ATheStripedMoverBC",
          "ABCTheStripedMover",
          "https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/simpleMoveWithADeletionOnTheMoveSide.excalidraw.svg"
        ),
        (
          "ATheStripedMoverBC",
          "APBCTheStripedMover",
          "ATheStripedMoverBC",
          "APBCTheStripedMover",
          "https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/simpleMoveWithAnEditOnTheMoveSide.excalidraw.svg"
        )
      )
      .and(Trials.api.booleans)
      .withLimit(20)
      .dynamicTests {
        case (
              (baseText, leftText, rightText, expectedMergeText, link),
              mirrorImage
            ) =>
          println(s"See: $link")

          val placeholderPath: FakePath = "*** STUNT DOUBLE ***"

          val tokenRegex =
            raw"TheStripedMover|TheCoincidentStripedEdit|.".r.anchored

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
            baseSources = baseSources,
            leftSources = leftSources,
            rightSources = rightSources
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
      }
  end migrationScenariosFromExcalidrawDocuments

  @Test
  def issue23BugReproduction(): Unit =
    val configuration = Configuration(
      minimumMatchSize = 4,
      thresholdSizeFractionForMatching = 0.1,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
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
      baseSources = baseSources,
      leftSources = leftSources,
      rightSources = rightSources
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
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
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
            baseSources = baseSources,
            leftSources = leftSources,
            rightSources = rightSources
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
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
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
            baseSources = baseSources,
            leftSources = leftSources,
            rightSources = rightSources
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
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
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
      baseSources = baseSources,
      leftSources = leftSources,
      rightSources = rightSources
    )(configuration): @unchecked

    val (mergeResultsByPath, _) =
      codeMotionAnalysis.merge

    verifyContent(placeholderPath, mergeResultsByPath)(
      tokens(codeMotionExampleExpectedMerge).get
    )
  end codeMotion

  @TestFactory
  def codeMotionWithSplit(): DynamicTests =
    Trials.api
      .integers(lowerBound = 1, upperBound = 10)
      .withLimit(10)
      .dynamicTests { minimumMatchSize =>
        val configuration = Configuration(
          minimumMatchSize = minimumMatchSize,
          thresholdSizeFractionForMatching = 0,
          minimumAmbiguousMatchSize = minimumMatchSize,
          ambiguousMatchesThreshold = 10
        )

        val originalPath: FakePath = "*** ORIGINAL ***"
        val hivedOffPath: FakePath = "*** HIVED OFF ***"

        val baseSources = MappedContentSourcesOfTokens(
          contentsByPath = Map(
            originalPath -> tokens(codeMotionExampleWithSplitOriginalBase).get
          ),
          label = "base"
        )
        val leftSources = MappedContentSourcesOfTokens(
          contentsByPath = Map(
            originalPath -> tokens(codeMotionExampleWithSplitOriginalLeft).get
          ),
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
          baseSources = baseSources,
          leftSources = leftSources,
          rightSources = rightSources
        )(configuration): @unchecked

        val (mergeResultsByPath, _) =
          codeMotionAnalysis.merge

        verifyContent(originalPath, mergeResultsByPath)(
          tokens(codeMotionExampleWithSplitOriginalExpectedMerge).get
        )

        verifyContent(hivedOffPath, mergeResultsByPath)(
          tokens(codeMotionExampleWithSplitHivedOffExpectedMerge).get
        )
      }
  end codeMotionWithSplit

  @TestFactory
  def merging(): DynamicTests =
    val minimumMatchSizes = Trials.api.integers(2, 10)

    minimumMatchSizes.withLimit(30).dynamicTests { minimumMatchSize =>
      val configuration = Configuration(
        minimumMatchSize = minimumMatchSize,
        thresholdSizeFractionForMatching = 0,
        minimumAmbiguousMatchSize = 4,
        ambiguousMatchesThreshold = 10
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
        baseSources = baseSources,
        leftSources = leftSources,
        rightSources = rightSources
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
          minimumAmbiguousMatchSize = 0,
          ambiguousMatchesThreshold = 10
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
          baseSources = baseSources,
          leftSources = leftSources,
          rightSources = rightSources
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
          minimumAmbiguousMatchSize = 0,
          ambiguousMatchesThreshold = 10
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
            case RenamingSide.Both  =>
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
          baseSources = baseSources,
          leftSources = leftSources,
          rightSources = rightSources
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

        renamingSide match
          case RenamingSide.Both =>
            assert(!mergeResultsByPath.contains(originalPath))
          case _ => verifyAbsenceOfContent(originalPath, mergeResultsByPath)
        end match

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
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
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
        // This has been thrown into the Too-Hard-Basket once again...
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
              // NOTE: when the test case is mirrored, the migrations becomes
              // captures instead.
              mirrorImage
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
              renamedPath -> tokens(
                if mirrorImage then rightContent else leftContent
              ).get
            ),
            label = "left"
          )

          val rightSources = MappedContentSourcesOfTokens(
            contentsByPath = Map(
              originalPath -> tokens(
                if mirrorImage then leftContent else rightContent
              ).get
            ),
            label = "right"
          )

          val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
            baseSources = baseSources,
            leftSources = leftSources,
            rightSources = rightSources
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

  @TestFactory
  def codeMotionAcrossTwoFilesWhoseContentIsCombinedTogetherToMakeANewReplacementFile()
      : DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 2,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
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
            baseSources = baseSources,
            leftSources = leftSources,
            rightSources = rightSources
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

  @Test
  def furtherMigrationOfAMigratedEditAsAnInsertion(): Unit =
    val configuration = Configuration(
      minimumMatchSize = 2,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
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
      baseSources = baseSources,
      leftSources = leftSources,
      rightSources = rightSources
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
      minimumAmbiguousMatchSize = 0,
      // Need a large value for this, as the single-character tokenization
      // yields runs of repeated tokens, thus causing lots of ambiguous
      // overlaps.
      ambiguousMatchesThreshold = 200
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
      baseSources = baseSources,
      leftSources = leftSources,
      rightSources = rightSources
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
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
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
      baseSources = baseSources,
      leftSources = leftSources,
      rightSources = rightSources
    )(configuration): @unchecked

    val (mergeResultsByPath, _) =
      codeMotionAnalysis.merge

    verifyContent(placeholderPath, mergeResultsByPath)(
      tokens(coincidencesExpectedMerge).get
    )
  end coincidences

  @TestFactory
  def simpleCodeMotionAcrossAFileRenameExamples(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 1,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    val originalPath: FakePath = "*** ORIGINAL ***"
    val renamedPath: FakePath  = "*** RENAMED ***"

    Trials.api
      .choose(
        (
          "Pure move.",
          "Anchor",
          "Anchor",
          "Anchor",
          "Anchor"
        ),
        (
          "Edit migration.",
          "Edited",
          "Edited",
          "MigratedEdit",
          "MigratedEdit"
        ),
        (
          "Edit migration preceded by a pure move.",
          "PrecedingAnchor Edited",
          "PrecedingAnchor Edited",
          "PrecedingAnchor MigratedEdit",
          "PrecedingAnchor MigratedEdit"
        ),
        (
          "Edit migration succeeded by a pure move.",
          "Edited SucceedingAnchor",
          "Edited SucceedingAnchor",
          "MigratedEdit SucceedingAnchor",
          "MigratedEdit SucceedingAnchor"
        ),
        (
          "Edit migration surrounded by pure moves.",
          "PrecedingAnchor Edited SucceedingAnchor",
          "PrecedingAnchor Edited SucceedingAnchor",
          "PrecedingAnchor MigratedEdit SucceedingAnchor",
          "PrecedingAnchor MigratedEdit SucceedingAnchor"
        ),
        // This has been commented out, because nothing actually moves; in fact
        // the original path is just deleted.
//        (
//          "Edit capture.",
//          "Edited",
//          "CapturedEdit",
//          "Edited",
//          "CapturedEdit"
//        ),
        (
          "Edit capture preceded by a pure move.",
          "PrecedingAnchor Edited",
          "PrecedingAnchor CapturedEdit",
          "PrecedingAnchor Edited",
          "PrecedingAnchor CapturedEdit"
        ),
        (
          "Edit capture succeeded by a pure move.",
          "Edited SucceedingAnchor",
          "CapturedEdit SucceedingAnchor",
          "Edited SucceedingAnchor",
          "CapturedEdit SucceedingAnchor"
        ),
        (
          "Edit capture surrounded by pure moves.",
          "PrecedingAnchor Edited SucceedingAnchor",
          "PrecedingAnchor CapturedEdit SucceedingAnchor",
          "PrecedingAnchor Edited SucceedingAnchor",
          "PrecedingAnchor CapturedEdit SucceedingAnchor"
        ),
        (
          "Deletion migration.",
          "Deleted",
          "Deleted",
          "",
          ""
        ),
        (
          "Deletion migration preceded by a pure move.",
          "PrecedingAnchor Deleted",
          "PrecedingAnchor Deleted",
          "PrecedingAnchor",
          "PrecedingAnchor"
        ),
        (
          "Deletion migration succeeded by a pure move.",
          "Deleted SucceedingAnchor",
          "Deleted SucceedingAnchor",
          "MigratedEdit",
          "MigratedEdit"
        ),
        (
          "Deletion migration surrounded by pure moves.",
          "PrecedingAnchor Deleted SucceedingAnchor",
          "PrecedingAnchor Deleted SucceedingAnchor",
          "PrecedingAnchor SucceedingAnchor",
          "PrecedingAnchor SucceedingAnchor"
        ),
        // This has been commented out, because nothing actually moves; in fact
        // the original path is just deleted.
//        (
//          "Deletion capture.",
//          "Deleted",
//          "",
//          "Deleted",
//          ""
//        ),
        (
          "Deletion capture preceded by a pure move.",
          "PrecedingAnchor Deleted",
          "PrecedingAnchor",
          "PrecedingAnchor Deleted",
          "PrecedingAnchor"
        ),
        (
          "Deletion capture succeeded by a pure move.",
          "Deleted SucceedingAnchor",
          "SucceedingAnchor",
          "Deleted SucceedingAnchor",
          "SucceedingAnchor"
        ),
        (
          "Deletion capture surrounded by pure moves.",
          "PrecedingAnchor Deleted SucceedingAnchor",
          "PrecedingAnchor SucceedingAnchor",
          "PrecedingAnchor Deleted SucceedingAnchor",
          "PrecedingAnchor SucceedingAnchor"
        ),
        (
          "Insertion migration preceded by a pure move.",
          "PrecedingAnchor",
          "PrecedingAnchor",
          "PrecedingAnchor MigratedInsertion",
          "PrecedingAnchor MigratedInsertion"
        ),
        (
          "Insertion migration succeeded by a pure move.",
          "SucceedingAnchor",
          "SucceedingAnchor",
          "MigratedInsertion SucceedingAnchor",
          "MigratedInsertion SucceedingAnchor"
        ),
        (
          "Insertion migration surrounded by pure moves.",
          "PrecedingAnchor SucceedingAnchor",
          "PrecedingAnchor SucceedingAnchor",
          "PrecedingAnchor MigratedInsertion SucceedingAnchor",
          "PrecedingAnchor MigratedInsertion SucceedingAnchor"
        ),
        (
          "Insertion capture preceded by a pure move.",
          "PrecedingAnchor",
          "PrecedingAnchor CapturedInsertion",
          "PrecedingAnchor",
          "PrecedingAnchor CapturedInsertion"
        ),
        (
          "Insertion capture succeeded by a pure move.",
          "SucceedingAnchor",
          "CapturedInsertion SucceedingAnchor",
          "SucceedingAnchor",
          "CapturedInsertion SucceedingAnchor"
        ),
        (
          "Insertion capture surrounded by pure moves.",
          "PrecedingAnchor SucceedingAnchor",
          "PrecedingAnchor CapturedInsertion SucceedingAnchor",
          "PrecedingAnchor SucceedingAnchor",
          "PrecedingAnchor CapturedInsertion SucceedingAnchor"
        )
      )
      .and(Trials.api.booleans)
      .withLimit(100)
      .dynamicTests {
        case (
              (
                label,
                baseOriginalContent,
                leftRenamedContent,
                rightOriginalContent,
                expectedRenamedMergeContent
              ),
              mirrorImage
            ) =>
          println(fansi.Color.Yellow(s"*** $label ***"))

          val baseSources = MappedContentSourcesOfTokens(
            contentsByPath = Map(
              originalPath -> tokens(baseOriginalContent).get
            ),
            label = "base"
          )

          val (leftSources, rightSources) =
            if mirrorImage then
              (
                MappedContentSourcesOfTokens(
                  contentsByPath = Map(
                    originalPath -> tokens(rightOriginalContent).get
                  ),
                  label = "left"
                ),
                MappedContentSourcesOfTokens(
                  contentsByPath = Map(
                    renamedPath -> tokens(leftRenamedContent).get
                  ),
                  label = "right"
                )
              )
            else
              (
                MappedContentSourcesOfTokens(
                  contentsByPath = Map(
                    renamedPath -> tokens(leftRenamedContent).get
                  ),
                  label = "left"
                ),
                MappedContentSourcesOfTokens(
                  contentsByPath = Map(
                    originalPath -> tokens(rightOriginalContent).get
                  ),
                  label = "right"
                )
              )

          val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
            baseSources = baseSources,
            leftSources = leftSources,
            rightSources = rightSources
          )(configuration): @unchecked

          val (mergeResultsByPath, moveDestinationsReport) =
            codeMotionAnalysis.merge

          println(fansi.Color.Yellow(s"Final move destinations report...\n"))
          println(
            fansi.Color
              .Green(moveDestinationsReport.summarizeInText.mkString("\n"))
          )

          verifyContent(renamedPath, mergeResultsByPath)(
            tokens(expectedRenamedMergeContent).get
          )

          verifyAbsenceOfContent(originalPath, mergeResultsByPath)
      }
  end simpleCodeMotionAcrossAFileRenameExamples

  @TestFactory
  def forwardingThroughCodeMotionAcrossAFileRenameExamples(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 1,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    val originalPath: FakePath  = "*** ORIGINAL ***"
    val renamedPath: FakePath   = "*** RENAMED ***"
    val forwardedPath: FakePath = "*** FORWARDED ***"

    Trials.api
      .choose(
        (
          "Forwarded pure move preceded by a pure move.",
          ("PrecedingAnchor", "Forwarded"),
          ("PrecedingAnchor", "Forwarded"),
          "PrecedingAnchor Forwarded",
          "PrecedingAnchor Forwarded"
        ),
        (
          "Forwarded pure move succeeded by a pure move.",
          ("SucceedingAnchor", "Forwarded"),
          ("SucceedingAnchor", "Forwarded"),
          "Forwarded SucceedingAnchor",
          "Forwarded SucceedingAnchor"
        ),
        (
          "Forwarded pure move surrounded by pure moves.",
          ("PrecedingAnchor SucceedingAnchor", "Forwarded"),
          ("PrecedingAnchor SucceedingAnchor", "Forwarded"),
          "PrecedingAnchor Forwarded SucceedingAnchor",
          "PrecedingAnchor Forwarded SucceedingAnchor"
        ),
        (
          "Forwarded edit preceded by a pure move.",
          ("PrecedingAnchor", "ForwardedEdited"),
          ("PrecedingAnchor", "ForwardedEdit"),
          "PrecedingAnchor ForwardedEdited",
          "PrecedingAnchor ForwardedEdit"
        ),
        (
          "Forwarded edit succeeded by a pure move.",
          ("SucceedingAnchor", "ForwardedEdited"),
          ("SucceedingAnchor", "ForwardedEdit"),
          "ForwardedEdited SucceedingAnchor",
          "ForwardedEdit SucceedingAnchor"
        ),
        (
          "Forwarded edit surrounded by pure moves.",
          ("PrecedingAnchor SucceedingAnchor", "ForwardedEdited"),
          ("PrecedingAnchor SucceedingAnchor", "ForwardedEdit"),
          "PrecedingAnchor ForwardedEdited SucceedingAnchor",
          "PrecedingAnchor ForwardedEdit SucceedingAnchor"
        ),
        (
          "Forwarded deletion preceded by a pure move.",
          ("PrecedingAnchor", "ForwardedDeleted"),
          ("PrecedingAnchor", ""),
          "PrecedingAnchor ForwardedDeleted",
          "PrecedingAnchor"
        ),
        (
          "Forwarded deletion succeeded by a pure move.",
          ("SucceedingAnchor", "ForwardedDeleted"),
          ("SucceedingAnchor", ""),
          "ForwardedDeleted SucceedingAnchor",
          "SucceedingAnchor"
        ),
        (
          "Forwarded deletion surrounded by pure moves.",
          ("PrecedingAnchor SucceedingAnchor", "ForwardedDeleted"),
          ("PrecedingAnchor SucceedingAnchor", ""),
          "PrecedingAnchor ForwardedDeleted SucceedingAnchor",
          "PrecedingAnchor SucceedingAnchor"
        ),
        (
          "Combined forwarded edit and deletion preceded by a pure move.",
          ("PrecedingAnchor", "ForwardedEdited ForwardedDeleted"),
          ("PrecedingAnchor", "ForwardedEdit"),
          "PrecedingAnchor ForwardedDeleted ForwardedEdited",
          "PrecedingAnchor ForwardedEdit"
        ),
        (
          "Combined forwarded edit and deletion succeeded by a pure move.",
          ("SucceedingAnchor", "ForwardedEdited ForwardedDeleted"),
          ("SucceedingAnchor", "ForwardedEdit"),
          "ForwardedDeleted ForwardedEdited SucceedingAnchor",
          "ForwardedEdit SucceedingAnchor"
        ),
        (
          "Combined forwarded edit and deletion surrounded by pure moves.",
          (
            "PrecedingAnchor SucceedingAnchor",
            "ForwardedEdited ForwardedDeleted"
          ),
          ("PrecedingAnchor SucceedingAnchor", "ForwardedEdit"),
          "PrecedingAnchor ForwardedDeleted ForwardedEdited SucceedingAnchor",
          "PrecedingAnchor ForwardedEdit SucceedingAnchor"
        ),
        (
          "Crazy mixed bag with multiplexed forwarded edit preceded by a pure move.",
          (
            "PrecedingAnchor Deleted EditedOne EditedTwo",
            "ForwardedEditedOne ForwardedEditedTwo"
          ),
          (
            "PrecedingAnchor EditedOne CapturedEdit",
            "ForwardedMultiplexedEdit"
          ),
          "PrecedingAnchor Inserted Deleted ForwardedEditedTwo Edit ForwardedEditedOne EditedTwo",
          "PrecedingAnchor Inserted ForwardedMultiplexedEdit Edit ForwardedMultiplexedEdit CapturedEdit"
        ),
        (
          "Crazy mixed bag with multiplexed forwarded edit succeeded by a pure move.",
          (
            "Deleted EditedOne EditedTwo SucceedingAnchor",
            "ForwardedEditedOne ForwardedEditedTwo"
          ),
          (
            "EditedOne CapturedEdit SucceedingAnchor",
            "ForwardedMultiplexedEdit"
          ),
          "Inserted Deleted ForwardedEditedTwo Edit ForwardedEditedOne EditedTwo SucceedingAnchor",
          "Inserted ForwardedMultiplexedEdit Edit ForwardedMultiplexedEdit CapturedEdit SucceedingAnchor"
        ),
        (
          "Crazy mixed bag with multiplexed forwarded edit surrounded by pure moves.",
          (
            "PrecedingAnchor Deleted EditedOne EditedTwo SucceedingAnchor",
            "ForwardedEditedOne ForwardedEditedTwo"
          ),
          (
            "PrecedingAnchor EditedOne CapturedEdit SucceedingAnchor",
            "ForwardedMultiplexedEdit"
          ),
          "PrecedingAnchor Inserted Deleted ForwardedEditedTwo Edit ForwardedEditedOne EditedTwo SucceedingAnchor",
          "PrecedingAnchor Inserted ForwardedMultiplexedEdit Edit ForwardedMultiplexedEdit CapturedEdit SucceedingAnchor"
        ),
        (
          "Multiple forwarded pure moves landing in migrated edit.",
          ("Edited", "ForwardedMoveOne ForwardedMoveTwo ForwardedMoveThree"),
          ("Edited", "ForwardedMoveOne ForwardedMoveTwo ForwardedMoveThree"),
          "ForwardedMoveThree ForwardedMoveTwo ForwardedMoveOne",
          "ForwardedMoveThree ForwardedMoveTwo ForwardedMoveOne"
        ),
        (
          "Combined forwarded edit landing in migrated edit.",
          (
            "Edited",
            "ForwardedEditedOne ForwardedEditedTwo ForwardedEditedThree"
          ),
          ("Edited", "CombinedForwardedEdit"),
          "ForwardedEditedThree ForwardedEditedTwo ForwardedEditedOne",
          "CombinedForwardedEdit"
        ),
        (
          "Combined forwarded deletions landing in migrated edit.",
          (
            "Edited",
            "ForwardedDeletedOne ForwardedDeletedTwo ForwardedDeletedThree"
          ),
          ("Edited", ""),
          "ForwardedDeletedThree ForwardedDeletedTwo ForwardedDeletedOne",
          ""
        ),
        (
          "Multiple double-forwarded edits landing in migrated edit.",
          (
            "Edited ForwardedEditOne ForwardedEditTwo ForwardedEditThree",
            "ForwardedEditedOne DeletedOne ForwardedEditedTwo DeletedTwo ForwardedEditedThree"
          ),
          (
            "Edited",
            "ForwardedEditOne DeletedOne ForwardedEditTwo DeletedTwo ForwardedEditThree"
          ),
          "ForwardedEditedThree ForwardedEditedTwo ForwardedEditedOne",
          "ForwardedEditThree ForwardedEditTwo ForwardedEditOne"
        )
      )
      .and(Trials.api.booleans)
      .withLimit(100)
      .dynamicTests {
        case (
              (
                label,
                (baseOriginalContent, baseForwardedContent),
                (leftRenamedContent, leftForwardedContent),
                rightOriginalContent,
                expectedRenamedMergeContent
              ),
              mirrorImage
            ) =>
          println(fansi.Color.Yellow(s"*** $label ***"))

          val baseSources = MappedContentSourcesOfTokens(
            contentsByPath = Map(
              originalPath  -> tokens(baseOriginalContent).get,
              forwardedPath -> tokens(baseForwardedContent).get
            ),
            label = "base"
          )

          val (leftSources, rightSources) =
            if mirrorImage then
              (
                MappedContentSourcesOfTokens(
                  contentsByPath = Map(
                    originalPath -> tokens(
                      rightOriginalContent
                    ).get
                  ),
                  label = "left"
                ),
                MappedContentSourcesOfTokens(
                  contentsByPath = Map(
                    renamedPath   -> tokens(leftRenamedContent).get,
                    forwardedPath -> tokens(leftForwardedContent).get
                  ),
                  label = "right"
                )
              )
            else
              (
                MappedContentSourcesOfTokens(
                  contentsByPath = Map(
                    renamedPath   -> tokens(leftRenamedContent).get,
                    forwardedPath -> tokens(leftForwardedContent).get
                  ),
                  label = "left"
                ),
                MappedContentSourcesOfTokens(
                  contentsByPath = Map(
                    originalPath -> tokens(
                      rightOriginalContent
                    ).get
                  ),
                  label = "right"
                )
              )

          val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
            baseSources = baseSources,
            leftSources = leftSources,
            rightSources = rightSources
          )(configuration): @unchecked

          val (mergeResultsByPath, moveDestinationsReport) =
            codeMotionAnalysis.merge

          println(fansi.Color.Yellow(s"Final move destinations report...\n"))
          println(
            fansi.Color
              .Green(moveDestinationsReport.summarizeInText.mkString("\n"))
          )

          verifyContent(renamedPath, mergeResultsByPath)(
            tokens(expectedRenamedMergeContent).get
          )

          verifyAbsenceOfContent(originalPath, mergeResultsByPath)

          verifyAbsenceOfContent(forwardedPath, mergeResultsByPath)
      }
  end forwardingThroughCodeMotionAcrossAFileRenameExamples

  @Test
  def multipleEditForwarding(): Unit =
    val configuration = Configuration(
      minimumMatchSize = 1,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 100
    )

    val finalEditDestinationPath = "*** DELTA ***"

    val hoppingPaths = Seq("*** ALPHA ***", "*** BETA ***", "*** GAMMA ***")

    val paths: Seq[FakePath] = hoppingPaths :+ finalEditDestinationPath

    val leftMoveContents = (1 to 3).map(2 * _)

    val rightMoveContents = leftMoveContents.map(_ - 1)

    val forwardedEdit = -457643

    val filler = (1 to 10).map(100 * _).mkString(" ")

    // For each base path, the first half of the content moves intra-file on the
    // right and inter-file on the left.
    val basePathContents = (rightMoveContents zip leftMoveContents).map {
      case (rightMoveSource, leftMoveSource) =>
        s"$rightMoveSource $filler $leftMoveSource"
    } :+ filler

    // We start with the edit to be forwarded, which bounces the move
    // destinations up by one.
    val leftPathContents =
      (forwardedEdit +: leftMoveContents).map(leading => s"$leading $filler")

    val rightPathContents =
      rightMoveContents.map(trailing => s"$filler $trailing") :+ filler

    val expectedContentForFinalEditDestinationPath = s"$forwardedEdit $filler"

    val baseSources = MappedContentSourcesOfTokens(
      contentsByPath = (paths zip basePathContents.map(tokens(_).get)).toMap,
      label = "base"
    )

    val leftSources = MappedContentSourcesOfTokens(
      contentsByPath = (paths zip leftPathContents.map(tokens(_).get)).toMap,
      label = "left"
    )

    val rightSources = MappedContentSourcesOfTokens(
      contentsByPath = (paths zip rightPathContents.map(tokens(_).get)).toMap,
      label = "right"
    )

    val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
      baseSources = baseSources,
      leftSources = leftSources,
      rightSources = rightSources
    )(configuration): @unchecked

    val (mergeResultsByPath, moveDestinationsReport) =
      codeMotionAnalysis.merge

    println(fansi.Color.Yellow(s"Final move destinations report...\n"))
    println(
      fansi.Color
        .Green(moveDestinationsReport.summarizeInText.mkString("\n"))
    )

    hoppingPaths.foreach(hoppingPath =>
      verifyContent(hoppingPath, mergeResultsByPath)(
        tokens(filler).get
      )
    )

    verifyContent(finalEditDestinationPath, mergeResultsByPath)(
      tokens(expectedContentForFinalEditDestinationPath).get
    )
  end multipleEditForwarding

  @TestFactory
  def issue126BugReproduction(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 1,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseText = "AmbiguousABCAmbiguousDE"

      val leftText = "AmbiguousABCDEAmbiguous"

      val rightText = "EditABCAmbiguousDE"

      val expectedMergeText = "EditABCDEAmbiguous"

      val placeholderPath: FakePath = "*** STUNT DOUBLE ***"

      val tokenRegex = raw"Ambiguous|Edit|A|B|C|D|E".r.anchored

      def stuntDoubleTokens(content: String): Vector[Token] = tokenRegex
        .findAllMatchIn(content)
        .map(_.group(0))
        .map(Token.Significant.apply)
        .toVector

      val baseSources = MappedContentSourcesOfTokens(
        contentsByPath = Map(placeholderPath -> stuntDoubleTokens(baseText)),
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
        baseSources = baseSources,
        leftSources = leftSources,
        rightSources = rightSources
      )(configuration): @unchecked

      val (mergeResultsByPath, _) =
        codeMotionAnalysis.merge

      verifyContent(placeholderPath, mergeResultsByPath)(
        stuntDoubleTokens(expectedMergeText)
      )
    }
  end issue126BugReproduction

  @TestFactory
  def contentIsEmptiedOnOneSide(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 1,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val emptyText = ""

      val baseText = "Um, ah, mumble, mumble, erm..."

      val leftText = "Um,\tah, mumble,\nmumble, erm..."

      val rightText = emptyText

      // NOTE: this is in contrast to the situation in
      // `MainTest.fileIsAbsentOnOneSide`. Here, the emptying of the content on
      // one side is merged in.
      val expectedMergeText = emptyText

      val placeholderPath: FakePath = "*** STUNT DOUBLE ***"

      val baseSources = MappedContentSourcesOfTokens(
        contentsByPath = Map(placeholderPath -> tokens(baseText).get),
        label = "base"
      )
      val leftSources = MappedContentSourcesOfTokens(
        contentsByPath = Map(
          placeholderPath -> tokens(
            if mirrorImage then rightText else leftText
          ).get
        ),
        label = "left"
      )
      val rightSources = MappedContentSourcesOfTokens(
        contentsByPath = Map(
          placeholderPath -> tokens(
            if mirrorImage then leftText else rightText
          ).get
        ),
        label = "right"
      )

      val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
        baseSources = baseSources,
        leftSources = leftSources,
        rightSources = rightSources
      )(configuration): @unchecked

      val (mergeResultsByPath, _) =
        codeMotionAnalysis.merge

      verifyContent(placeholderPath, mergeResultsByPath)(
        tokens(expectedMergeText).get
      )
    }
  end contentIsEmptiedOnOneSide

  @TestFactory
  def fileIsAbsentOnOneSide(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 1,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseText = "Um, ah, mumble, mumble, erm..."

      val leftText = "Um,\tah, mumble,\nmumble, erm..."

      // NOTE: this is in contrast to the situation in
      // `MainTest.contentIsEmptiedOnOneSide`. Here, the implied deletion of the
      // file is ignored for a simple merge; this is motivated by having to
      // emulate Git's treatment of file deletion-versus-modification conflicts,
      // where we want the modified file to stay as is in its entirety. What we
      // see here is a degenerate case of this, where there is no modification
      // at all on the side opposing the file deletion. This feels hokey, but is
      // worked around by `Main.InWorkingDirectory.indexUpdates`. Furthermore,
      // if the loss of the file is due to content moving to one or more other
      // files, then this *is* reflected as a loss of content in the merge; that
      // is tested for elsewhere in this suite.
      val expectedMergeText = leftText

      val placeholderPath: FakePath = "*** STUNT DOUBLE ***"

      val baseSources = MappedContentSourcesOfTokens(
        contentsByPath = Map(placeholderPath -> tokens(baseText).get),
        label = "base"
      )
      val leftSources = MappedContentSourcesOfTokens(
        contentsByPath =
          if mirrorImage then Map.empty
          else Map(placeholderPath -> tokens(leftText).get),
        label = "left"
      )
      val rightSources = MappedContentSourcesOfTokens(
        contentsByPath =
          if mirrorImage then Map(placeholderPath -> tokens(leftText).get)
          else Map.empty,
        label = "right"
      )

      val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
        baseSources = baseSources,
        leftSources = leftSources,
        rightSources = rightSources
      )(configuration): @unchecked

      val (mergeResultsByPath, _) =
        codeMotionAnalysis.merge

      verifyContent(placeholderPath, mergeResultsByPath)(
        tokens(expectedMergeText).get
      )
    }
  end fileIsAbsentOnOneSide

  @TestFactory
  def issue144BugReproduction(): DynamicTests =
    val configuration = Configuration(
      minimumMatchSize = 4,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 0,
      ambiguousMatchesThreshold = 10
    )

    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val storyPath: FakePath       = "Story"
      val filmScriptPath: FakePath  = "Film Script"
      val filmTrailerPath: FakePath = "Film Trailer"

      val baseSources = MappedContentSourcesOfTokens(
        contentsByPath = Map(storyPath -> tokens(baselineStory).get),
        label = "base"
      )
      val leftSources = MappedContentSourcesOfTokens(
        contentsByPath =
          if mirrorImage then
            Map(
              filmScriptPath  -> tokens(filmScript).get,
              filmTrailerPath -> tokens(filmTrailer).get
            )
          else Map(storyPath -> tokens(storyWithSpellingChanged).get),
        label = "left"
      )
      val rightSources = MappedContentSourcesOfTokens(
        contentsByPath =
          if mirrorImage then
            Map(storyPath -> tokens(storyWithSpellingChanged).get)
          else
            Map(
              filmScriptPath  -> tokens(filmScript).get,
              filmTrailerPath -> tokens(filmTrailer).get
            )
        ,
        label = "right"
      )

      val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
        baseSources = baseSources,
        leftSources = leftSources,
        rightSources = rightSources
      )(configuration): @unchecked

      val (mergeResultsByPath, _) =
        codeMotionAnalysis.merge

      verifyAbsenceOfContent(storyPath, mergeResultsByPath)

      verifyContent(filmScriptPath, mergeResultsByPath)(
        tokens(expectedFilmScript).get
      )
      verifyContent(filmTrailerPath, mergeResultsByPath)(
        tokens(expectedFilmTrailer).get
      )
    }
  end issue144BugReproduction

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
      |A stitch in time saves nine (but you aren't going to need it)..
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
    """
      |A bird in hand is worth two in the bush.
      |A stitch in time saves (but you aren't going to need it)..
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

  protected val storyPrologue: String =
    """
      |This is the prologue; it sets the scene for the main plot, and conveys a mood for the entire story.
      |Usually, a prologue is either sad, or has a sense of adversity - this is because nobody wants to read a story that is happy all the way through, and they certainly don’t want things to start off looking promising and then go downhill.
      |The viewers in the US will be incensed to see how I have spelt ‘prologue’ - on more than one level!
      |""".stripMargin

  protected val storyPlot: String =
    """
      |Git merge and Kinetic Merge were issued various challenges…
      |""".stripMargin

  protected val baselineStory: String =
    storyPrologue + "\n\n" + storyPlot

  protected val storyPrologueWithSpellingChanged: String =
    """
      |This is the prolog; it sets the scene for the main plot, and conveys a mood for the entire story.
      |Usually, a prolog is either sad, or has a sense of adversity - this is because nobody wants to read a story that is happy all the way through, and they certainly don’t want things to start off looking promising and then go downhill.
      |The viewers in the US will be delighted to see how I have spelled ‘prolog’ - on more than one level!
      |""".stripMargin

  protected val storyWithSpellingChanged: String =
    storyPrologueWithSpellingChanged + "\n\n" + storyPlot

  protected val filmTrailer: String = storyPrologue

  protected val filmScript: String = storyPrologue + "\n\n" + storyPlot

  protected val expectedFilmScript: String =
    storyPrologueWithSpellingChanged + "\n\n" + storyPlot

  protected val expectedFilmTrailer: String =
    storyPrologueWithSpellingChanged
end ProseExamples
