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

    val expected = stuntDoubleTokens(issue23BugReproductionExpectedMerge)

    val (mergeResultsByPath, _) =
      codeMotionAnalysis.merge

    val mergeResult = mergeResultsByPath(placeholderPath)

    println(fansi.Color.Yellow(s"Checking $placeholderPath...\n"))
    println(fansi.Color.Yellow("Expected..."))
    println(fansi.Color.Green(reconstituteTextFrom(expected)))

    mergeResult match
      case FullyMerged(result) =>
        println(fansi.Color.Yellow("Fully merged result..."))
        println(fansi.Color.Green(reconstituteTextFrom(result)))
        assert(result.corresponds(expected)(Token.equality))
      case MergedWithConflicts(leftResult, rightResult) =>
        println(fansi.Color.Red(s"Left result..."))
        println(fansi.Color.Green(reconstituteTextFrom(leftResult)))
        println(fansi.Color.Red(s"Right result..."))
        println(fansi.Color.Green(reconstituteTextFrom(rightResult)))

        fail("Should have seen a clean merge.")
    end match

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
          propagatedSurroundedInsertionExampleBase,
          propagatedSurroundedInsertionExampleLeft,
          propagatedSurroundedInsertionExampleRight,
          propagatedSurroundedInsertionExampleExpectedMerge
        ),
        (
          propagatedLeadingInsertionExampleBase,
          propagatedLeadingInsertionExampleLeft,
          propagatedLeadingInsertionExampleRight,
          propagatedLeadingInsertionExampleExpectedMerge
        ),
        (
          propagatedTrailingInsertionExampleBase,
          propagatedTrailingInsertionExampleLeft,
          propagatedTrailingInsertionExampleRight,
          propagatedTrailingInsertionExampleExpectedMerge
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

          val expected = stuntDoubleTokens(expectedText)

          val (mergeResultsByPath, _) =
            codeMotionAnalysis.merge

          val mergeResult = mergeResultsByPath(placeholderPath)

          println(fansi.Color.Yellow(s"Checking $placeholderPath...\n"))
          println(fansi.Color.Yellow("Expected..."))
          println(fansi.Color.Green(reconstituteTextFrom(expected)))

          mergeResult match
            case FullyMerged(result) =>
              println(fansi.Color.Yellow("Fully merged result..."))
              println(fansi.Color.Green(reconstituteTextFrom(result)))
              assert(result.corresponds(expected)(Token.equality))
            case MergedWithConflicts(leftResult, rightResult) =>
              println(fansi.Color.Red(s"Left result..."))
              println(fansi.Color.Green(reconstituteTextFrom(leftResult)))
              println(fansi.Color.Red(s"Right result..."))
              println(fansi.Color.Green(reconstituteTextFrom(rightResult)))

              fail("Should have seen a clean merge.")
          end match
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

          val expected = stuntDoubleTokens(expectedText)

          val (mergeResultsByPath, _) =
            codeMotionAnalysis.merge

          val mergeResult = mergeResultsByPath(placeholderPath)

          println(fansi.Color.Yellow(s"Checking $placeholderPath...\n"))
          println(fansi.Color.Yellow("Expected..."))
          println(fansi.Color.Green(reconstituteTextFrom(expected)))

          mergeResult match
            case FullyMerged(result) =>
              println(fansi.Color.Yellow("Fully merged result..."))
              println(fansi.Color.Green(reconstituteTextFrom(result)))
              assert(result.corresponds(expected)(Token.equality))
            case MergedWithConflicts(leftResult, rightResult) =>
              println(fansi.Color.Red(s"Left result..."))
              println(fansi.Color.Green(reconstituteTextFrom(leftResult)))
              println(fansi.Color.Red(s"Right result..."))
              println(fansi.Color.Green(reconstituteTextFrom(rightResult)))

              fail("Should have seen a clean merge.")
          end match
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

    val expected = tokens(codeMotionExampleExpectedMerge).get

    val (mergeResultsByPath, _) =
      codeMotionAnalysis.merge

    val mergeResult = mergeResultsByPath(placeholderPath)

    println(fansi.Color.Yellow(s"Checking $placeholderPath...\n"))
    println(fansi.Color.Yellow("Expected..."))
    println(fansi.Color.Green(reconstituteTextFrom(expected)))

    mergeResult match
      case FullyMerged(result) =>
        println(fansi.Color.Yellow("Fully merged result..."))
        println(fansi.Color.Green(reconstituteTextFrom(result)))
        assert(result.corresponds(expected)(Token.equality))
      case MergedWithConflicts(leftResult, rightResult) =>
        println(fansi.Color.Red(s"Left result..."))
        println(fansi.Color.Green(reconstituteTextFrom(leftResult)))
        println(fansi.Color.Red(s"Right result..."))
        println(fansi.Color.Green(reconstituteTextFrom(rightResult)))

        fail("Should have seen a clean merge.")
    end match

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

    val expectedForOriginal = tokens(
      codeMotionExampleWithSplitOriginalExpectedMerge
    ).get
    val expectedForHivedOff = tokens(
      codeMotionExampleWithSplitHivedOffExpectedMerge
    ).get

    val (mergeResultsByPath, _) =
      codeMotionAnalysis.merge

    println(fansi.Color.Yellow(s"Checking $hivedOffPath...\n"))
    println(fansi.Color.Yellow("Expected..."))
    println(fansi.Color.Green(reconstituteTextFrom(expectedForHivedOff)))

    mergeResultsByPath(hivedOffPath) match
      case FullyMerged(result) =>
        println(fansi.Color.Yellow("Fully merged result..."))
        println(fansi.Color.Green(reconstituteTextFrom(result)))
        assert(result.corresponds(expectedForHivedOff)(Token.equality))
      case MergedWithConflicts(leftResult, rightResult) =>
        println(fansi.Color.Red(s"Left result..."))
        println(fansi.Color.Green(reconstituteTextFrom(leftResult)))
        println(fansi.Color.Red(s"Right result..."))
        println(fansi.Color.Green(reconstituteTextFrom(rightResult)))

        fail("Should have seen a clean merge.")
    end match

    println(fansi.Color.Yellow(s"Checking $originalPath...\n"))
    println(fansi.Color.Yellow("Expected..."))
    println(fansi.Color.Green(reconstituteTextFrom(expectedForOriginal)))

    mergeResultsByPath(originalPath) match
      case FullyMerged(result) =>
        println(fansi.Color.Yellow("Fully merged result..."))
        println(fansi.Color.Green(reconstituteTextFrom(result)))
        assert(result.corresponds(expectedForOriginal)(Token.equality))
      case MergedWithConflicts(leftResult, rightResult) =>
        println(fansi.Color.Red(s"Left result..."))
        println(fansi.Color.Green(reconstituteTextFrom(leftResult)))
        println(fansi.Color.Red(s"Right result..."))
        println(fansi.Color.Green(reconstituteTextFrom(rightResult)))

        fail("Should have seen a clean merge.")
    end match

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

        val mergeResult = mergeResultsByPath(placeholderPath)

        println(fansi.Color.Yellow(s"Checking $placeholderPath...\n"))
        println(fansi.Color.Yellow("Expected..."))
        println(fansi.Color.Green(reconstituteTextFrom(expected)))

        mergeResult match
          case FullyMerged(result) =>
            println(fansi.Color.Yellow("Fully merged result..."))
            println(fansi.Color.Green(reconstituteTextFrom(result)))
            assert(result.sameElements(expected))
          case MergedWithConflicts(leftResult, rightResult) =>
            println(fansi.Color.Red(s"Left result..."))
            println(fansi.Color.Green(reconstituteTextFrom(leftResult)))
            println(fansi.Color.Red(s"Right result..."))
            println(fansi.Color.Green(reconstituteTextFrom(rightResult)))

            fail("Should have seen a clean merge.")
        end match

    }
  end whitespaceOnlyEditing
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

  protected val propagatedSurroundedInsertionExampleBase: String =
    """
      |FishChipsMushyPeasKetchupMuesliToastTeaKippersNoodlesSandwichCakePudding
      |""".stripMargin

  protected val propagatedSurroundedInsertionExampleLeft: String =
    """
      |MuesliToastTeaKippersNoodlesSandwichCakeFishChipsMushyPeasKetchupPudding
      |""".stripMargin

  protected val propagatedSurroundedInsertionExampleRight: String =
    """
      |FishChipsCurrySauceMushyPeasKetchupMuesliToastCoffeeKippersNoodlesSandwichCakePudding
      |""".stripMargin

  protected val propagatedSurroundedInsertionExampleExpectedMerge: String =
    """
      |MuesliToastCoffeeKippersNoodlesSandwichCakeFishChipsCurrySauceMushyPeasKetchupPudding
      |""".stripMargin

  protected val propagatedLeadingInsertionExampleBase: String =
    """
      |FishChipsMushyPeasKetchupMuesliToastTeaKippersNoodlesSandwichCakePudding
      |""".stripMargin

  protected val propagatedLeadingInsertionExampleLeft: String =
    """
      |MuesliToastTeaKippersNoodlesSandwichCakeFishChipsMushyPeasKetchupPudding
      |""".stripMargin

  protected val propagatedLeadingInsertionExampleRight: String =
    """
      |CurrySauceFishChipsMushyPeasKetchupMuesliToastCoffeeKippersNoodlesSandwichCakePudding
      |""".stripMargin

  protected val propagatedLeadingInsertionExampleExpectedMerge: String =
    """
      |MuesliToastCoffeeKippersNoodlesSandwichCakeCurrySauceFishChipsMushyPeasKetchupPudding
      |""".stripMargin

  protected val propagatedTrailingInsertionExampleBase: String =
    """
      |FishChipsMushyPeasKetchupMuesliToastTeaKippersNoodlesSandwichCakePudding
      |""".stripMargin

  protected val propagatedTrailingInsertionExampleLeft: String =
    """
      |FishChipsMushyPeasKetchupSandwichCakePuddingMuesliToastTeaKippersNoodles
      |""".stripMargin

  protected val propagatedTrailingInsertionExampleRight: String =
    """
      |FishChipsMushyPeasKetchupMuesliToastCoffeeKippersNoodlesSandwichCakePuddingFigs
      |""".stripMargin

  protected val propagatedTrailingInsertionExampleExpectedMerge: String =
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
      |    override val showTypes: Boolean    = /* TODO - remove this comment, it's here to force propagation of the edit on the right. */ true
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
      |    override val showTypes: Boolean    = /* TODO - remove this comment, it's here to force propagation of the edit on the right. */ true
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
      |    override val showTypes: Boolean    = false // This edit should propagate.
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
      |    override val showTypes: Boolean    = false // This edit should propagate.
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
      |            Instant deadline = null; // This edit should propagate.
      |
      |            @Override
      |            public boolean moreCasesToDo() {
      |                if (null == deadline /* This edit should propagate. */) {
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
      |    boolean moreCasesToDo(); // This rename should propagate.
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
      |    boolean moreCasesToDo(); // This rename should propagate.
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
      |            Instant deadline = null; // This edit should propagate.
      |
      |            @Override
      |            public boolean moreCasesToDo() {
      |                if (null == deadline /* This edit should propagate. */) {
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
end ProseExamples
