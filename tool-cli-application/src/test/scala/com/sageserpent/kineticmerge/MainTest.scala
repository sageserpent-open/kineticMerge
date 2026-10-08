package com.sageserpent.kineticmerge

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.Main.ApplicationRequest
import com.sageserpent.kineticmerge.MainTest.*
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.ProseExamples
import com.sageserpent.kineticmerge.core.Token.{
  tokens,
  equality as tokenEquality
}
import com.softwaremill.tagging.*
import org.junit.jupiter.api.TestFactory
import os.{Path, RelPath}

object MainTest extends ProseExamples:
  private type ImperativeResource[Payload] = Resource[IO, Payload]

  private val arthur = RelPath("pathPrefix1") / "arthur.txt"

  private val sandra = RelPath("pathPrefix1") / "pathPrefix2" / "sandra.txt"

  private val tyson = RelPath("pathPrefix1") / "pathPrefix2" / "tyson.txt"

  private val casesLimitStrategy =
    RelPath("pathPrefix1") / "CasesLimitStrategy.java"
  private val movedCasesLimitStrategy =
    RelPath("pathPrefix1") / "pathPrefix2" / "CasesLimitStrategy.java"
  private val excisedCasesLimitStrategies =
    RelPath("pathPrefix1") / "CasesLimitStrategies.java"
  private val expectyFlavouredAssert =
    RelPath("pathPrefix1") / "pathPrefix2" / "ExpectyFlavouredAssert.scala"

  private val arthurFirstVariation  = "chap"
  private val arthurSecondVariation = "boy"

  private val tysonResponse       = "Alright marra?"
  private val evilTysonExultation = "Ha, ha, ha, ha, hah!"

  private val optionalSubdirectories: Trials[Option[RelPath]] =
    trialsApi.only("runMergeInHere").map(RelPath.apply).options

  private val baseCasesLimitStrategyContent =
    codeMotionExampleWithSplitOriginalBase
  private val replacementCasesLimitStrategyContent =
    "... and now for something completely different."
  private val editedCasesLimitStrategyContent =
    codeMotionExampleWithSplitOriginalLeft
  private val justTheInterfaceForCasesLimitStrategyContent =
    codeMotionExampleWithSplitOriginalRight
  private val excisedCasesLimitStrategiesContent =
    codeMotionExampleWithSplitHivedOffRight
  private val justTheInterfaceForCasesLimitStrategyExpectedContent =
    codeMotionExampleWithSplitOriginalExpectedMerge
  private val excisedCasesLimitStrategiesExpectedContent =
    codeMotionExampleWithSplitHivedOffExpectedMerge
  private val baseExpectyFlavouredAssertContent   = codeMotionExampleBase
  private val editedExpectyFlavouredAssertContent = codeMotionExampleRight

  private def introducingArthur(paths: Path*): Unit =
    paths.foreach(path =>
      os.write(path / arthur, "Hello, my old mucker!\n", createFolders = true)
    )
  end introducingArthur

  private def arthurContinues(paths: Path*): Unit =
    paths.foreach(path =>
      os.write.append(
        path / arthur,
        s"Pleased to see you, old $arthurSecondVariation.\n"
      )
    )
  end arthurContinues

  private def arthurElaborates(paths: Path*): Unit =
    paths.foreach(path =>
      os.write.append(
        path / arthur,
        s"Pleased to see you, old $arthurFirstVariation.\n"
      )
    )
  end arthurElaborates

  private def arthurCorrectsHimself(paths: Path*): Unit =
    paths.foreach(path =>
      os.write.over(
        path / arthur,
        "Hello, all and sundry!\n"
      )
    )
  end arthurCorrectsHimself

  private def arthurClearsHisThroat(paths: Path*): Unit =
    paths.foreach(path =>
      os.write.append(
        path / arthur,
        "\n"
      )
    )
  end arthurClearsHisThroat

  private def exeuntArthur(paths: Path*): Unit =
    paths.foreach(path => os.remove(path / arthur))
  end exeuntArthur

  private def arthurExcusesHimself(paths: Path*): Unit =
    paths.foreach(path => os.remove(path / arthur))
  end arthurExcusesHimself

  private def arthurDeniesHavingSaidAnything(paths: Path*): Unit =
    paths.foreach(path => os.write.over(path / arthur, ""))
  end arthurDeniesHavingSaidAnything

  private def enterTysonStageLeft(paths: Path*): Unit =
    paths.foreach(path =>
      os.write(path / tyson, s"$tysonResponse\n", createFolders = true)
    )
  end enterTysonStageLeft

  private def evilTysonMakesDramaticEntranceExulting(paths: Path*): Unit =
    paths.foreach(path =>
      os.write(path / tyson, s"$evilTysonExultation\n", createFolders = true)
    )
  end evilTysonMakesDramaticEntranceExulting

  private def sandraHeadsOffHome(paths: Path*): Unit =
    paths.foreach(path => os.remove(path / sandra))
  end sandraHeadsOffHome

  private def sandraStopsByBriefly(paths: Path*): Unit =
    paths.foreach(path =>
      os.write(
        path / sandra,
        "Hiya - just gan yam now...\n",
        createFolders = true
      )
    )
  end sandraStopsByBriefly

  private def arthurSaidConflictingThings(path: Path): Unit =
    val arthurSaid = os.read(path / arthur)

    assert(
      arthurSaid.contains(arthurFirstVariation) && arthurSaid
        .contains(arthurSecondVariation)
    )
  end arthurSaidConflictingThings

  private def arthurTakesOnAPseudonym(paths: Path*): Unit =
    paths.foreach { path =>
      os.move(
        path / arthur,
        path / movedCasesLimitStrategy,
        createFolders = true
      )
    }
  end arthurTakesOnAPseudonym

  private def tysonSaidConflictingThings(path: Path): Unit =
    val tysonSaid = os.read(path / tyson)

    assert(
      tysonSaid.contains(tysonResponse) && tysonSaid
        .contains(evilTysonExultation)
    )
  end tysonSaidConflictingThings

  private def introducingCasesLimitStrategy(paths: Path*): Unit =
    paths.foreach(path =>
      os.write(
        path / casesLimitStrategy,
        baseCasesLimitStrategyContent,
        createFolders = true
      )
    )
  end introducingCasesLimitStrategy

  private def introducingInterfaceOnlyCasesLimitStrategy(
      paths: Path*
  ): Unit =
    paths.foreach(path =>
      os.write(
        path / casesLimitStrategy,
        justTheInterfaceForCasesLimitStrategyExpectedContent,
        createFolders = true
      )
    )
  end introducingInterfaceOnlyCasesLimitStrategy

  private def introducingCasesLimitStrategies(paths: Path*): Unit =
    paths.foreach(path =>
      os.write(
        path / excisedCasesLimitStrategies,
        excisedCasesLimitStrategiesExpectedContent,
        createFolders = true
      )
    )
  end introducingCasesLimitStrategies

  private def editingInterfaceOnlyCasesLimitStrategy(paths: Path*): Unit =
    paths.foreach(path =>
      os.write.over(
        path / casesLimitStrategy,
        justTheInterfaceForCasesLimitStrategyContent,
        createFolders = true
      )
    )
  end editingInterfaceOnlyCasesLimitStrategy

  private def editingCasesLimitStrategies(paths: Path*): Unit =
    paths.foreach(path =>
      os.write.over(
        path / excisedCasesLimitStrategies,
        excisedCasesLimitStrategiesContent,
        createFolders = true
      )
    )
  end editingCasesLimitStrategies

  private def editingCasesLimitStrategy(paths: Path*): Unit =
    paths.foreach(path =>
      os.write.over(
        path / casesLimitStrategy,
        editedCasesLimitStrategyContent,
        createFolders = true
      )
    )
  end editingCasesLimitStrategy

  private def removingCasesLimitStrategy(paths: Path*): Unit =
    paths.foreach(path => os.remove(path / casesLimitStrategy))
  end removingCasesLimitStrategy

  private def emptyingCasesLimitStrategy(paths: Path*): Unit =
    paths.foreach(path => os.write.over(path / casesLimitStrategy, ""))
  end emptyingCasesLimitStrategy

  private def splittingCasesLimitStrategy(paths: Path*): Unit =
    paths.foreach { path =>
      os.write.over(
        path / casesLimitStrategy,
        justTheInterfaceForCasesLimitStrategyContent,
        createFolders = true
      )
      os.write(
        path / excisedCasesLimitStrategies,
        excisedCasesLimitStrategiesContent,
        createFolders = true
      )
    }
  end splittingCasesLimitStrategy

  private def condensingCasesLimitStrategy(paths: Path*): Unit =
    paths.foreach { path =>
      os.write.over(
        path / casesLimitStrategy,
        editedCasesLimitStrategyContent,
        createFolders = true
      )
      os.remove(path / excisedCasesLimitStrategies)
    }
  end condensingCasesLimitStrategy

  private def moveCasesLimitStrategy(paths: Path*): Unit =
    paths.foreach { path =>
      os.move(
        path / casesLimitStrategy,
        path / movedCasesLimitStrategy,
        createFolders = true
      )
    }
  end moveCasesLimitStrategy

  private def reintroducingCasesLimitStrategy(paths: Path*): Unit =
    paths.foreach(path =>
      os.write(
        path / casesLimitStrategy,
        replacementCasesLimitStrategyContent,
        createFolders = true
      )
    )
  end reintroducingCasesLimitStrategy

  private def arthurBecomesAnExpertOnCasesLimitStrategy(paths: Path*): Unit =
    paths.foreach(path =>
      os.write.append(
        path / arthur,
        baseCasesLimitStrategyContent
      )
    )
  end arthurBecomesAnExpertOnCasesLimitStrategy

  private def introducingExpectyFlavouredAssert(paths: Path*): Unit =
    paths.foreach(path =>
      os.write(
        path / expectyFlavouredAssert,
        baseExpectyFlavouredAssertContent,
        createFolders = true
      )
    )
  end introducingExpectyFlavouredAssert

  private def editingExpectyFlavouredAssert(paths: Path*): Unit =
    paths.foreach(path =>
      os.write.over(
        path / expectyFlavouredAssert,
        editedExpectyFlavouredAssertContent,
        createFolders = true
      )
    )
  end editingExpectyFlavouredAssert

  private def swapTheTwoFiles(paths: Path*): Unit =
    paths.foreach { path =>
      os.write.over(
        path / casesLimitStrategy,
        baseExpectyFlavouredAssertContent,
        createFolders = true
      )
      os.write.over(
        path / expectyFlavouredAssert,
        baseCasesLimitStrategyContent,
        createFolders = true
      )
    }
  end swapTheTwoFiles

  private def threeSideDirectories(): ImperativeResource[(Path, Path, Path)] =
    for
      baseDirectory <- Resource.make(IO {
        os.temp.dir(prefix = "base")
      })(temporaryDirectory => IO { os.remove.all.apply(temporaryDirectory) })
      leftDirectory <- Resource.make(IO {
        os.temp.dir(prefix = "left")
      })(temporaryDirectory => IO { os.remove.all.apply(temporaryDirectory) })
      rightDirectory <- Resource.make(IO {
        os.temp.dir(prefix = "right")
      })(temporaryDirectory => IO { os.remove.all.apply(temporaryDirectory) })
    yield (baseDirectory, leftDirectory, rightDirectory)
    end for
  end threeSideDirectories

  private def contentMatches(expected: String)(actual: String) =
    tokens(actual).get.corresponds(
      tokens(
        expected
      ).get
    )(tokenEquality)

  private def mergeWrapper(
      optionalSubdirectory: Option[RelPath],
      baseDirectory: Path,
      leftDirectory: Path,
      rightDirectory: Path,
      minimumAmbiguousMatchSize: Int
  ): (Path, Path, Path) =
    val workingDirectory =
      optionalSubdirectory.fold(ifEmpty = baseDirectory)(baseDirectory / _)

    val exitCode = Main.mergeSides(
      ApplicationRequest.default.copy(
        mergeSideDirectories =
          Seq(baseDirectory, leftDirectory, rightDirectory),
        quiet = false,
        minimumAmbiguousMatchSize = minimumAmbiguousMatchSize
      )
    )(workingDirectory = workingDirectory)

    assert(
      2 > exitCode
    ) // Either a successful merge or a conflict should be the outcome for these tests.

    (baseDirectory, leftDirectory, rightDirectory)
  end mergeWrapper

  private def verifyCleanMerge(
      baseDirectory: Path,
      leftDirectory: Path,
      rightDirectory: Path
  ): Unit =
    val baseContents  = contentsByRelativePathOf(baseDirectory)
    val leftContents  = contentsByRelativePathOf(leftDirectory)
    val rightContents = contentsByRelativePathOf(rightDirectory)

    assert(baseContents == leftContents && baseContents == rightContents)
  end verifyCleanMerge

  private def verifyConflictedMerge(
      baseDirectory: Path,
      leftDirectory: Path,
      rightDirectory: Path
  ): Unit =
    val baseContents  = contentsByRelativePathOf(baseDirectory)
    val leftContents  = contentsByRelativePathOf(leftDirectory)
    val rightContents = contentsByRelativePathOf(rightDirectory)

    val commonToAllThreeSides =
      baseContents intersect leftContents intersect rightContents

    val baseDifferences  = baseContents.diff(commonToAllThreeSides)
    val leftDifferences  = leftContents.diff(commonToAllThreeSides)
    val rightDifferences = rightContents.diff(commonToAllThreeSides)

    assert(
      baseDifferences.nonEmpty || leftDifferences.nonEmpty || rightDifferences.nonEmpty
    )

    // The differences should be across all three sides when they occur...
    assert(
      (baseDifferences intersect leftDifferences).isEmpty &&
        (baseDifferences intersect rightDifferences).isEmpty &&
        (leftDifferences intersect rightDifferences).isEmpty
    )
  end verifyConflictedMerge

  private def contentsByRelativePathOf(directory: Path) =
    os
      .walk(directory)
      .filter(os.isFile)
      .map(path => path.relativeTo(directory) -> os.read(path))
end MainTest

class MainTest:
  @TestFactory
  def trivialMerge(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(14)
      .dynamicTests {
        case (
              optionalSubdirectory,
              ourBranchIsBehindTheirs
            ) =>
          threeSideDirectories()
            .use { case (baseDirectory, leftDirectory, rightDirectory) =>
              IO {
                optionalSubdirectory.foreach { subdirectory =>
                  os.makeDir.all(baseDirectory / subdirectory)
                  os.makeDir.all(leftDirectory / subdirectory)
                  os.makeDir.all(rightDirectory / subdirectory)
                }

                introducingArthur(baseDirectory, leftDirectory, rightDirectory)

                if ourBranchIsBehindTheirs then
                  arthurContinues(rightDirectory)
                else
                  arthurContinues(leftDirectory)
                end if

                mergeWrapper(
                  optionalSubdirectory,
                  baseDirectory,
                  leftDirectory,
                  rightDirectory,
                  minimumAmbiguousMatchSize = 0
                )

                verifyCleanMerge(baseDirectory, leftDirectory, rightDirectory)
              }
            }
            .unsafeRunSync()
      }
  end trivialMerge

  @TestFactory
  def cleanMergeBringingInANewFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingArthur(baseDirectory, leftDirectory, rightDirectory)

              val (mainDir, newFileDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              enterTysonStageLeft(newFileDir)
              arthurContinues(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 0
              )

              verifyCleanMerge(baseDirectory, leftDirectory, rightDirectory)
            }
          }
          .unsafeRunSync()
      }
  end cleanMergeBringingInANewFile

  @TestFactory
  def cleanMergeDeletingAFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingArthur(baseDirectory, leftDirectory, rightDirectory)

              val (mainDir, deletedFileDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              exeuntArthur(deletedFileDir)
              enterTysonStageLeft(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 0
              )

              verifyCleanMerge(baseDirectory, leftDirectory, rightDirectory)
            }
          }
          .unsafeRunSync()
      }
  end cleanMergeDeletingAFile

  @TestFactory
  def cleanMergeOfAFileAddedInBothBranches(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              sandraStopsByBriefly(baseDirectory, leftDirectory, rightDirectory)

              val (mainDir, benignTwinDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              introducingArthur(benignTwinDir)
              arthurContinues(benignTwinDir)

              enterTysonStageLeft(mainDir)
              introducingArthur(mainDir)
              sandraHeadsOffHome(mainDir)
              arthurClearsHisThroat(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 0
              )

              verifyCleanMerge(baseDirectory, leftDirectory, rightDirectory)
            }
          }
          .unsafeRunSync()
      }
  end cleanMergeOfAFileAddedInBothBranches

  @TestFactory
  def conflictingAdditionOfTheSameFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(4)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingArthur(baseDirectory, leftDirectory, rightDirectory)
              sandraStopsByBriefly(baseDirectory, leftDirectory, rightDirectory)

              val (mainDir, evilTwinDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              evilTysonMakesDramaticEntranceExulting(evilTwinDir)

              sandraHeadsOffHome(mainDir)
              enterTysonStageLeft(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 0
              )

              verifyConflictedMerge(baseDirectory, leftDirectory, rightDirectory)
            }
          }
          .unsafeRunSync()
      }
  end conflictingAdditionOfTheSameFile

  @TestFactory
  def conflictingInsertModificationAndDeletionOfTheSameFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(4)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingArthur(baseDirectory, leftDirectory, rightDirectory)
              sandraStopsByBriefly(baseDirectory, leftDirectory, rightDirectory)

              val (mainDir, deletedFileDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              enterTysonStageLeft(deletedFileDir)
              exeuntArthur(deletedFileDir)

              sandraHeadsOffHome(mainDir)
              arthurContinues(mainDir)

              val arthurOnTheRecord = os.read(mainDir / arthur)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 0
              )

              verifyConflictedMerge(baseDirectory, leftDirectory, rightDirectory)

              assert(
                contentMatches(expected = arthurOnTheRecord)(
                  os.read(mainDir / arthur)
                )
              )
            }
          }
          .unsafeRunSync()
      }
  end conflictingInsertModificationAndDeletionOfTheSameFile

  @TestFactory
  def conflictingEditModificationAndDeletionOfTheSameFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(4)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingArthur(baseDirectory, leftDirectory, rightDirectory)
              sandraStopsByBriefly(baseDirectory, leftDirectory, rightDirectory)

              val (mainDir, deletedFileDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              enterTysonStageLeft(deletedFileDir)
              exeuntArthur(deletedFileDir)

              sandraHeadsOffHome(mainDir)
              arthurBecomesAnExpertOnCasesLimitStrategy(mainDir)

              val arthurOnTheRecord = os.read(mainDir / arthur)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 0
              )

              verifyConflictedMerge(baseDirectory, leftDirectory, rightDirectory)

              assert(
                contentMatches(expected = arthurOnTheRecord)(
                  os.read(mainDir / arthur)
                )
              )
            }
          }
          .unsafeRunSync()
      }
  end conflictingEditModificationAndDeletionOfTheSameFile

  @TestFactory
  def conflictingContentClearanceModificationAndDeletionOfTheSameFile()
      : DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(4)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingArthur(baseDirectory, leftDirectory, rightDirectory)
              sandraStopsByBriefly(baseDirectory, leftDirectory, rightDirectory)

              val (mainDir, deletedFileDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              enterTysonStageLeft(deletedFileDir)
              exeuntArthur(deletedFileDir)

              sandraHeadsOffHome(mainDir)
              arthurDeniesHavingSaidAnything(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 0
              )

              verifyConflictedMerge(baseDirectory, leftDirectory, rightDirectory)
            }
          }
          .unsafeRunSync()
      }
  end conflictingContentClearanceModificationAndDeletionOfTheSameFile

  @TestFactory
  def conflictingModificationOfTheSameFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(4)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingArthur(baseDirectory, leftDirectory, rightDirectory)
              sandraStopsByBriefly(baseDirectory, leftDirectory, rightDirectory)

              val (mainDir, concurrentlyModifiedDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              enterTysonStageLeft(concurrentlyModifiedDir)
              arthurElaborates(concurrentlyModifiedDir)

              sandraHeadsOffHome(mainDir)
              arthurContinues(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 0
              )

              verifyConflictedMerge(baseDirectory, leftDirectory, rightDirectory)
            }
          }
          .unsafeRunSync()
      }
  end conflictingModificationOfTheSameFile

  @TestFactory
  def cleanMergeOfAFileDeletedInBothBranches(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingArthur(baseDirectory, leftDirectory, rightDirectory)
              sandraStopsByBriefly(baseDirectory, leftDirectory, rightDirectory)

              val (mainDir, concurrentlyDeletedDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              enterTysonStageLeft(concurrentlyDeletedDir)
              exeuntArthur(concurrentlyDeletedDir)

              sandraHeadsOffHome(mainDir)
              arthurContinues(mainDir)
              arthurExcusesHimself(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 0
              )

              verifyCleanMerge(baseDirectory, leftDirectory, rightDirectory)
            }
          }
          .unsafeRunSync()
      }
  end cleanMergeOfAFileDeletedInBothBranches

  @TestFactory
  def cleanMergeOfAFileModifiedInBothBranches(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingArthur(baseDirectory, leftDirectory, rightDirectory)
              sandraStopsByBriefly(baseDirectory, leftDirectory, rightDirectory)

              val (mainDir, concurrentlyModifiedDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              enterTysonStageLeft(concurrentlyModifiedDir)
              arthurCorrectsHimself(concurrentlyModifiedDir)

              sandraHeadsOffHome(mainDir)
              arthurContinues(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 0
              )

              verifyCleanMerge(baseDirectory, leftDirectory, rightDirectory)
            }
          }
          .unsafeRunSync()
      }
  end cleanMergeOfAFileModifiedInBothBranches

  @TestFactory
  def anEditAndADeletionPropagatingThroughAFileMove(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingCasesLimitStrategy(
                baseDirectory,
                leftDirectory,
                rightDirectory
              )

              val (mainDir, movedFileDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              moveCasesLimitStrategy(movedFileDir)
              editingCasesLimitStrategy(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 5
              )

              verifyCleanMerge(baseDirectory, leftDirectory, rightDirectory)

              assert(
                contentMatches(expected = editedCasesLimitStrategyContent)(
                  os.read(leftDirectory / movedCasesLimitStrategy)
                )
              )
            }
          }
          .unsafeRunSync()
      }
  end anEditAndADeletionPropagatingThroughAFileMove

  @TestFactory
  def anEditAndADeletionPropagatingThroughAFileSplit(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(20)
      .dynamicTests {
        case (
              optionalSubdirectory,
              flipBranches,
              loseOriginalFileInSplit
            ) =>
          threeSideDirectories()
            .use { case (baseDirectory, leftDirectory, rightDirectory) =>
              IO {
                optionalSubdirectory.foreach { subdirectory =>
                  os.makeDir.all(baseDirectory / subdirectory)
                  os.makeDir.all(leftDirectory / subdirectory)
                  os.makeDir.all(rightDirectory / subdirectory)
                }

                introducingCasesLimitStrategy(
                  baseDirectory,
                  leftDirectory,
                  rightDirectory
                )

                val (mainDir, splitFileDir) =
                  if flipBranches then (rightDirectory, leftDirectory)
                  else (leftDirectory, rightDirectory)

                splittingCasesLimitStrategy(splitFileDir)

                if loseOriginalFileInSplit then
                  moveCasesLimitStrategy(splitFileDir)
                end if

                editingCasesLimitStrategy(mainDir)

                mergeWrapper(
                  optionalSubdirectory,
                  baseDirectory,
                  leftDirectory,
                  rightDirectory,
                  minimumAmbiguousMatchSize = 5
                )

                verifyCleanMerge(baseDirectory, leftDirectory, rightDirectory)

                assert(
                  contentMatches(
                    expected =
                      justTheInterfaceForCasesLimitStrategyExpectedContent
                  )(
                    os.read(
                      leftDirectory / (if loseOriginalFileInSplit then
                                         movedCasesLimitStrategy
                                       else casesLimitStrategy)
                    )
                  )
                )

                assert(
                  contentMatches(expected =
                    excisedCasesLimitStrategiesExpectedContent
                  )(
                    os.read(leftDirectory / excisedCasesLimitStrategies)
                  )
                )
              }
            }
            .unsafeRunSync()
      }
  end anEditAndADeletionPropagatingThroughAFileSplit

  @TestFactory
  def anEditAndADeletionPropagatingThroughAFileCondensation(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(20)
      .dynamicTests {
        case (
              optionalSubdirectory,
              flipBranches,
              loseBothOriginalFilesInJoin
            ) =>
          threeSideDirectories()
            .use { case (baseDirectory, leftDirectory, rightDirectory) =>
              IO {
                optionalSubdirectory.foreach { subdirectory =>
                  os.makeDir.all(baseDirectory / subdirectory)
                  os.makeDir.all(leftDirectory / subdirectory)
                  os.makeDir.all(rightDirectory / subdirectory)
                }

                introducingInterfaceOnlyCasesLimitStrategy(
                  baseDirectory,
                  leftDirectory,
                  rightDirectory
                )
                introducingCasesLimitStrategies(
                  baseDirectory,
                  leftDirectory,
                  rightDirectory
                )

                val (mainDir, condensedFilesDir) =
                  if flipBranches then (rightDirectory, leftDirectory)
                  else (leftDirectory, rightDirectory)

                condensingCasesLimitStrategy(condensedFilesDir)

                if loseBothOriginalFilesInJoin then
                  moveCasesLimitStrategy(condensedFilesDir)
                end if

                editingInterfaceOnlyCasesLimitStrategy(mainDir)
                editingCasesLimitStrategies(mainDir)

                mergeWrapper(
                  optionalSubdirectory,
                  baseDirectory,
                  leftDirectory,
                  rightDirectory,
                  minimumAmbiguousMatchSize = 5
                )

                verifyCleanMerge(baseDirectory, leftDirectory, rightDirectory)

                assert(
                  contentMatches(expected = baseCasesLimitStrategyContent)(
                    os.read(
                      leftDirectory / (if loseBothOriginalFilesInJoin
                                       then movedCasesLimitStrategy
                                       else casesLimitStrategy)
                    )
                  )
                )
              }
            }
            .unsafeRunSync()
      }
  end anEditAndADeletionPropagatingThroughAFileCondensation

  @TestFactory
  def twoFilesSwappingAroundWithModificationOfOne(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingCasesLimitStrategy(
                baseDirectory,
                leftDirectory,
                rightDirectory
              )
              introducingExpectyFlavouredAssert(
                baseDirectory,
                leftDirectory,
                rightDirectory
              )

              val (mainDir, swappedFilesDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              swapTheTwoFiles(swappedFilesDir)
              editingExpectyFlavouredAssert(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 0
              )

              verifyCleanMerge(baseDirectory, leftDirectory, rightDirectory)

              assert(
                contentMatches(expected = editedExpectyFlavouredAssertContent)(
                  os.read(leftDirectory / casesLimitStrategy)
                )
              )

              assert(
                contentMatches(expected = baseCasesLimitStrategyContent)(
                  os.read(leftDirectory / expectyFlavouredAssert)
                )
              )
            }
          }
          .unsafeRunSync()
      }
  end twoFilesSwappingAroundWithModificationOfOne

  @TestFactory
  def twoFilesSwappingAroundWithModificationsToBoth(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingCasesLimitStrategy(
                baseDirectory,
                leftDirectory,
                rightDirectory
              )
              introducingExpectyFlavouredAssert(
                baseDirectory,
                leftDirectory,
                rightDirectory
              )

              val (mainDir, swappedFilesDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              swapTheTwoFiles(swappedFilesDir)
              editingCasesLimitStrategy(mainDir)
              editingExpectyFlavouredAssert(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 5
              )

              verifyCleanMerge(baseDirectory, leftDirectory, rightDirectory)

              assert(
                contentMatches(expected = editedExpectyFlavouredAssertContent)(
                  os.read(leftDirectory / casesLimitStrategy)
                )
              )
              assert(
                contentMatches(expected = editedCasesLimitStrategyContent)(
                  os.read(leftDirectory / expectyFlavouredAssert)
                )
              )
            }
          }
          .unsafeRunSync()
      }
  end twoFilesSwappingAroundWithModificationsToBoth

  @TestFactory
  def contentClearancePropagatingThroughAFileMove(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches, noCommit) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingCasesLimitStrategy(
                baseDirectory,
                leftDirectory,
                rightDirectory
              )

              val (mainDir, movedFileDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              moveCasesLimitStrategy(movedFileDir)
              emptyingCasesLimitStrategy(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 5
              )

              verifyCleanMerge(baseDirectory, leftDirectory, rightDirectory)

              assert(
                0 == os.size(leftDirectory / movedCasesLimitStrategy)
              )
              assert(
                !os.exists(leftDirectory / casesLimitStrategy)
              )
            }
          }
          .unsafeRunSync()
      }
  end contentClearancePropagatingThroughAFileMove

  @TestFactory
  def conflictingDeletionAndFileMoveOfTheSameFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingCasesLimitStrategy(
                baseDirectory,
                leftDirectory,
                rightDirectory
              )

              val (mainDir, movedFileDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              moveCasesLimitStrategy(movedFileDir)
              removingCasesLimitStrategy(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 5
              )

              verifyConflictedMerge(baseDirectory, leftDirectory, rightDirectory)

              assert(
                contentMatches(expected = baseCasesLimitStrategyContent)(
                  os.read(movedFileDir / movedCasesLimitStrategy)
                )
              )
            }
          }
          .unsafeRunSync()
      }
  end conflictingDeletionAndFileMoveOfTheSameFile

  @TestFactory
  def conflictingDeletionAndEditedFileMoveOfTheSameFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingCasesLimitStrategy(
                baseDirectory,
                leftDirectory,
                rightDirectory
              )

              val (mainDir, movedFileDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              editingCasesLimitStrategy(movedFileDir)
              moveCasesLimitStrategy(movedFileDir)

              removingCasesLimitStrategy(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 5
              )

              verifyConflictedMerge(baseDirectory, leftDirectory, rightDirectory)

              assert(
                contentMatches(expected = editedCasesLimitStrategyContent)(
                  os.read(movedFileDir / movedCasesLimitStrategy)
                )
              )
            }
          }
          .unsafeRunSync()
      }
  end conflictingDeletionAndEditedFileMoveOfTheSameFile

  @TestFactory
  def cleanMergeOfDeletionAndFileCondensationOfTheSameFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingCasesLimitStrategy(
                baseDirectory,
                leftDirectory,
                rightDirectory
              )
              introducingArthur(baseDirectory, leftDirectory, rightDirectory)

              val (mainDir, condensedFileDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              removingCasesLimitStrategy(condensedFileDir)
              arthurBecomesAnExpertOnCasesLimitStrategy(condensedFileDir)

              sandraStopsByBriefly(mainDir)
              removingCasesLimitStrategy(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 5
              )

              verifyCleanMerge(baseDirectory, leftDirectory, rightDirectory)
            }
          }
          .unsafeRunSync()
      }
  end cleanMergeOfDeletionAndFileCondensationOfTheSameFile

  @TestFactory
  def conflictingDeletionAndReplacementWithFileMoveOfTheSameFile()
      : DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingCasesLimitStrategy(
                baseDirectory,
                leftDirectory,
                rightDirectory
              )

              val (mainDir, movedFileDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              moveCasesLimitStrategy(movedFileDir)
              reintroducingCasesLimitStrategy(movedFileDir)

              removingCasesLimitStrategy(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 5
              )

              verifyConflictedMerge(baseDirectory, leftDirectory, rightDirectory)

              assert(
                contentMatches(expected = replacementCasesLimitStrategyContent)(
                  os.read(movedFileDir / casesLimitStrategy)
                )
              )

              assert(
                contentMatches(expected = baseCasesLimitStrategyContent)(
                  os.read(movedFileDir / movedCasesLimitStrategy)
                )
              )
            }
          }
          .unsafeRunSync()
      }
  end conflictingDeletionAndReplacementWithFileMoveOfTheSameFile

  @TestFactory
  def conflictingDeletionAndReplacementWithEditedFileMoveOfTheSameFile()
      : DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingCasesLimitStrategy(
                baseDirectory,
                leftDirectory,
                rightDirectory
              )

              val (mainDir, movedFileDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              editingCasesLimitStrategy(movedFileDir)
              moveCasesLimitStrategy(movedFileDir)
              reintroducingCasesLimitStrategy(movedFileDir)

              removingCasesLimitStrategy(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 5
              )

              verifyConflictedMerge(baseDirectory, leftDirectory, rightDirectory)

              assert(
                contentMatches(expected = replacementCasesLimitStrategyContent)(
                  os.read(movedFileDir / casesLimitStrategy)
                )
              )

              assert(
                contentMatches(expected = editedCasesLimitStrategyContent)(
                  os.read(movedFileDir / movedCasesLimitStrategy)
                )
              )
            }
          }
          .unsafeRunSync()
      }
  end conflictingDeletionAndReplacementWithEditedFileMoveOfTheSameFile

  @TestFactory
  def conflictingConvergingFileMovesFromDifferentFiles(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        threeSideDirectories()
          .use { case (baseDirectory, leftDirectory, rightDirectory) =>
            IO {
              optionalSubdirectory.foreach { subdirectory =>
                os.makeDir.all(baseDirectory / subdirectory)
                os.makeDir.all(leftDirectory / subdirectory)
                os.makeDir.all(rightDirectory / subdirectory)
              }

              introducingCasesLimitStrategy(
                baseDirectory,
                leftDirectory,
                rightDirectory
              )
              introducingArthur(
                baseDirectory,
                leftDirectory,
                rightDirectory
              )

              val (mainDir, casesLimitStrategyMovesDir) =
                if flipBranches then (rightDirectory, leftDirectory)
                else (leftDirectory, rightDirectory)

              moveCasesLimitStrategy(casesLimitStrategyMovesDir)

              arthurTakesOnAPseudonym(mainDir)

              mergeWrapper(
                optionalSubdirectory,
                baseDirectory,
                leftDirectory,
                rightDirectory,
                minimumAmbiguousMatchSize = 5
              )

              verifyConflictedMerge(baseDirectory, leftDirectory, rightDirectory)
            }
          }
          .unsafeRunSync()
      }
  end conflictingConvergingFileMovesFromDifferentFiles

end MainTest
