package com.sageserpent.kineticmerge

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.Main.{ApplicationRequest, Tags}
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

  private val masterBranch = "master"

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
  private val arthurIsMarkedWithConflictingUpdateAndDeletionInTheIndex =
    pathIsMarkedWithConflictingUpdateAndDeletionInTheIndex(arthur)

  private def introducingArthur(path: Path): Unit =
    os.write(path / arthur, "Hello, my old mucker!\n", createFolders = true)
    println(os.proc("git", "add", arthur).call(path).out.text())
    println(
      os.proc("git", "commit", "-m", "'Introducing Arthur.'")
        .call(path)
        .out
        .text()
    )
  end introducingArthur

  private def arthurContinues(path: Path): Unit =
    os.write.append(
      path / arthur,
      s"Pleased to see you, old $arthurSecondVariation.\n"
    )
    println(
      os.proc("git", "commit", "-am", "'Arthur continues...'")
        .call(path)
        .out
        .text()
    )
  end arthurContinues

  private def arthurElaborates(path: Path): Unit =
    os.write.append(
      path / arthur,
      s"Pleased to see you, old $arthurFirstVariation.\n"
    )
    println(
      os.proc("git", "commit", "-am", "'Arthur elaborates.'")
        .call(path)
        .out
        .text()
    )
  end arthurElaborates

  private def arthurCorrectsHimself(path: Path): Unit =
    os.write.over(
      path / arthur,
      "Hello, all and sundry!\n"
    )
    println(
      os.proc("git", "commit", "-am", "'Arthur corrects himself.'")
        .call(path)
        .out
        .text()
    )
  end arthurCorrectsHimself

  private def arthurClearsHisThroat(path: Path): Unit =
    os.write.append(
      path / arthur,
      "\n"
    )
    println(
      os.proc("git", "commit", "-am", "'Arthur clears his throat.'")
        .call(path)
        .out
        .text()
    )
  end arthurClearsHisThroat

  private def exeuntArthur(path: Path): Unit =
    println(os.proc("git", "rm", arthur).call(path).out.text())
    println(
      os.proc(s"git", "commit", "-m", "'Exeunt Arthur.'").call(path).out.text()
    )
  end exeuntArthur

  private def arthurExcusesHimself(path: Path): Unit =
    println(os.proc("git", "rm", arthur).call(path).out.text())
    println(
      os.proc("git", "commit", "-m", "'Arthur excuses himself.'")
        .call(path)
        .out
        .text()
    )
  end arthurExcusesHimself

  private def arthurDeniesHavingSaidAnything(path: Path): Unit =
    os.write.over(path / arthur, "")
    println(
      os.proc("git", "commit", "-am", "'Arthur denies having said anything.'")
        .call(path)
        .out
        .text()
    )
  end arthurDeniesHavingSaidAnything

  private def enterTysonStageLeft(path: Path): Unit =
    os.write(path / tyson, s"$tysonResponse\n", createFolders = true)
    println(os.proc("git", "add", tyson).call(path).out.text())
    println(
      os.proc("git", "commit", "-m", "'Tyson responds.'").call(path).out.text()
    )
  end enterTysonStageLeft

  private def evilTysonMakesDramaticEntranceExulting(path: Path): Unit =
    os.write(path / tyson, s"$evilTysonExultation\n", createFolders = true)
    println(os.proc("git", "add", tyson).call(path).out.text())
    println(
      os.proc("git", "commit", "-m", "'Evil Tyson exults.'")
        .call(path)
        .out
        .text()
    )
  end evilTysonMakesDramaticEntranceExulting

  private def sandraHeadsOffHome(path: Path): Unit =
    println(os.proc("git", "rm", sandra).call(path).out.text())
    println(
      os.proc("git", "commit", "-m", "'Sandra heads off home.'")
        .call(path)
        .out
        .text()
    )
  end sandraHeadsOffHome

  private def sandraStopsByBriefly(path: Path): Unit =
    os.write(
      path / sandra,
      "Hiya - just gan yam now...\n",
      createFolders = true
    )
    println(os.proc("git", "add", sandra).call(path).out.text())
    println(
      os.proc("git", "commit", "-m", "'Sandra stops by briefly...'")
        .call(path)
        .out
        .text()
    )
  end sandraStopsByBriefly

  private def noUpdatesInIndexForArthur(status: String): Unit =
    assert(!status.contains(arthur))

  private def arthurIsMarkedWithConflictingUpdatesInTheIndex(
      status: String
  ): Unit =
    assert(
      s"UU\\s+$arthur".r.findFirstIn(status).isDefined
    )

  private def arthurSaidConflictingThings(path: Path): Unit =
    val arthurSaid = os.read(path / arthur)

    assert(
      arthurSaid.contains(arthurFirstVariation) && arthurSaid
        .contains(arthurSecondVariation)
    )
  end arthurSaidConflictingThings

  private def arthurTakesOnAPseudonym(path: Path): Unit =
    os.move(
      path / arthur,
      path / movedCasesLimitStrategy,
      createFolders = true
    )

    println(os.proc("git", "rm", arthur).call(path).out.text())
    println(
      os.proc("git", "add", movedCasesLimitStrategy).call(path).out.text()
    )
    println(
      os.proc("git", "commit", "-m", "'Moving `arthur`.'")
        .call(path)
        .out
        .text()
    )
  end arthurTakesOnAPseudonym

  private def tysonSaidConflictingThings(path: Path): Unit =
    val tysonSaid = os.read(path / tyson)

    assert(
      tysonSaid.contains(tysonResponse) && tysonSaid
        .contains(evilTysonExultation)
    )
  end tysonSaidConflictingThings

  private def pathIsMarkedWithConflictingUpdateAndDeletionInTheIndex(
      path: RelPath
  )(flipBranches: Boolean, status: String): Unit =
    assert(
      s"${if flipBranches then "DU" else "UD"}\\s+$path".r
        .findFirstIn(status)
        .isDefined
    )

  private def pathIsMarkedWithConflictingDeletionAndRenameInTheIndex(
      path: RelPath
  )(flipBranches: Boolean, status: String): Unit =
    assert(
      s"${if flipBranches then "AU" else "UA"}\\s+$path".r
        .findFirstIn(status)
        .isDefined
    )

  private def pathIsMarkedWithConflictingAdditionAndAdditionInTheIndex(
      path: RelPath
  )(status: String): Unit =
    assert(
      s"AA\\s+$path".r
        .findFirstIn(status)
        .isDefined
    )

  private def noUpdatesInIndexForTyson(status: String): Unit =
    assert(!status.contains(tyson))

  private def tysonIsMarkedAsAddedInTheIndex(status: String): Unit =
    assert(s"A\\s+$tyson.*".r.findFirstIn(status).isDefined)

  private def tysonIsMarkedWithConflictingAdditionsInTheIndex(
      status: String
  ): Unit =
    assert(s"AA\\s+$tyson".r.findFirstIn(status).isDefined)

  private def noUpdatesInIndexForSandra(status: String): Unit =
    assert(!status.contains(sandra))

  private def sandraIsMarkedAsDeletedInTheIndex(status: String): Unit =
    assert(s"D\\s+$sandra".r.findFirstIn(status).isDefined)

  private def introducingCasesLimitStrategy(path: Path): Unit =
    os.write(
      path / casesLimitStrategy,
      baseCasesLimitStrategyContent,
      createFolders = true
    )
    println(os.proc("git", "add", casesLimitStrategy).call(path).out.text())
    println(
      os.proc("git", "commit", "-m", "'Introducing `CasesLimitStrategy`.'")
        .call(path)
        .out
        .text()
    )
  end introducingCasesLimitStrategy

  private def introducingInterfaceOnlyCasesLimitStrategy(path: Path): Unit =
    os.write(
      path / casesLimitStrategy,
      justTheInterfaceForCasesLimitStrategyExpectedContent,
      createFolders = true
    )
    println(os.proc("git", "add", casesLimitStrategy).call(path).out.text())
    println(
      os.proc("git", "commit", "-m", "'Introducing `CasesLimitStrategy`.'")
        .call(path)
        .out
        .text()
    )
  end introducingInterfaceOnlyCasesLimitStrategy

  private def introducingCasesLimitStrategies(path: Path): Unit =
    os.write(
      path / excisedCasesLimitStrategies,
      excisedCasesLimitStrategiesExpectedContent,
      createFolders = true
    )
    println(
      os.proc("git", "add", excisedCasesLimitStrategies).call(path).out.text()
    )
    println(
      os.proc("git", "commit", "-m", "'Introducing `CasesLimitStrategies`.'")
        .call(path)
        .out
        .text()
    )
  end introducingCasesLimitStrategies

  private def editingInterfaceOnlyCasesLimitStrategy(path: Path): Unit =
    os.write.over(
      path / casesLimitStrategy,
      justTheInterfaceForCasesLimitStrategyContent,
      createFolders = true
    )
    println(
      os.proc("git", "commit", "-am", "'Editing `CasesLimitStrategy`.'")
        .call(path)
        .out
        .text()
    )
  end editingInterfaceOnlyCasesLimitStrategy

  private def editingCasesLimitStrategies(path: Path): Unit =
    os.write.over(
      path / excisedCasesLimitStrategies,
      excisedCasesLimitStrategiesContent,
      createFolders = true
    )
    println(
      os.proc("git", "commit", "-am", "'Editing `CasesLimitStrategies`.'")
        .call(path)
        .out
        .text()
    )
  end editingCasesLimitStrategies

  private def editingCasesLimitStrategy(path: Path): Unit =
    os.write.over(
      path / casesLimitStrategy,
      editedCasesLimitStrategyContent,
      createFolders = true
    )
    println(
      os.proc("git", "commit", "-am", "'Editing `CasesLimitStrategy`.'")
        .call(path)
        .out
        .text()
    )
  end editingCasesLimitStrategy

  private def removingCasesLimitStrategy(path: Path): Unit =
    os.remove(path / casesLimitStrategy)
    println(os.proc("git", "rm", casesLimitStrategy).call(path).out.text())
    println(
      os.proc(s"git", "commit", "-m", "'Removing `CasesLimitStrategy`.'")
        .call(path)
        .out
        .text()
    )
  end removingCasesLimitStrategy

  private def splittingCasesLimitStrategy(path: Path): Unit =
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
    println(
      os.proc("git", "add", excisedCasesLimitStrategies).call(path).out.text()
    )
    println(
      os.proc(
        "git",
        "commit",
        "-am",
        "'Excising `CasesLimitStrategies` from `CasesLimitStrategy`.'"
      ).call(path)
        .out
        .text()
    )
  end splittingCasesLimitStrategy

  private def condensingCasesLimitStrategy(path: Path): Unit =
    os.write.over(
      path / casesLimitStrategy,
      editedCasesLimitStrategyContent,
      createFolders = true
    )
    os.remove(
      path / excisedCasesLimitStrategies
    )
    println(
      os.proc("git", "rm", excisedCasesLimitStrategies).call(path).out.text()
    )
    println(
      os.proc(
        "git",
        "commit",
        "-am",
        "'Condensing `CasesLimitStrategies` in with `CasesLimitStrategy`.'"
      ).call(path)
        .out
        .text()
    )
  end condensingCasesLimitStrategy

  private def moveCasesLimitStrategy(path: Path): Unit =
    os.move(
      path / casesLimitStrategy,
      path / movedCasesLimitStrategy,
      createFolders = true
    )

    println(os.proc("git", "rm", casesLimitStrategy).call(path).out.text())
    println(
      os.proc("git", "add", movedCasesLimitStrategy).call(path).out.text()
    )
    println(
      os.proc("git", "commit", "-m", "'Moving `CasesLimitStrategy`.'")
        .call(path)
        .out
        .text()
    )
  end moveCasesLimitStrategy

  private def reintroducingCasesLimitStrategy(path: Path): Unit =
    os.write(
      path / casesLimitStrategy,
      replacementCasesLimitStrategyContent,
      createFolders = true
    )
    println(os.proc("git", "add", casesLimitStrategy).call(path).out.text())
    println(
      os.proc("git", "commit", "-m", "'Reintroducing `CasesLimitStrategy`.'")
        .call(path)
        .out
        .text()
    )
  end reintroducingCasesLimitStrategy

  private def arthurBecomesAnExpertOnCasesLimitStrategy(path: Path): Unit =
    os.write.append(
      path / arthur,
      baseCasesLimitStrategyContent
    )
    println(
      os.proc("git", "commit", "-am", "'Arthur declaims on software.'")
        .call(path)
        .out
        .text()
    )
  end arthurBecomesAnExpertOnCasesLimitStrategy

  private def introducingExpectyFlavouredAssert(path: Path): Unit =
    os.write(
      path / expectyFlavouredAssert,
      baseExpectyFlavouredAssertContent,
      createFolders = true
    )
    println(os.proc("git", "add", expectyFlavouredAssert).call(path).out.text())
    println(
      os.proc("git", "commit", "-m", "'Introducing `ExpectyFlavouredAssert`.'")
        .call(path)
        .out
        .text()
    )
  end introducingExpectyFlavouredAssert

  private def editingExpectyFlavouredAssert(path: Path): Unit =
    os.write.over(
      path / expectyFlavouredAssert,
      editedExpectyFlavouredAssertContent,
      createFolders = true
    )
    println(
      os.proc("git", "commit", "-am", "'Editing `ExpectyFlavouredAssert`.'")
        .call(path)
        .out
        .text()
    )
  end editingExpectyFlavouredAssert

  private def swapTheTwoFiles(path: Path): Unit =
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
    println(
      os.proc(
        "git",
        "commit",
        "-am",
        "'Swapping the contents of `CasesLimitStrategy` and `ExpectyFlavouredAssert`.'"
      ).call(path)
        .out
        .text()
    )
  end swapTheTwoFiles

  private def verifyTrivialMergeMovesToTheMostAdvancedCommitWithACleanIndex(
      path: Path
  )(
      commitOfAdvancedBranch: String,
      ourBranch: String,
      exitCode: Int @@ Main.Tags.ExitCode
  ): Unit =
    assert(exitCode == 0)

    val branchName = currentBranch(path)

    assert(branchName == ourBranch)

    val postMergeCommitOfAdvancedBranch =
      currentCommit(path)

    assert(postMergeCommitOfAdvancedBranch == commitOfAdvancedBranch)

    assert(currentStatus(path).isEmpty)
  end verifyTrivialMergeMovesToTheMostAdvancedCommitWithACleanIndex

  private def verifyMergeMakesANewCommitWithACleanIndex(path: Path)(
      commitOfOneBranch: String,
      commitOfTheOtherBranch: String,
      ourBranch: String,
      exitCode: Int @@ Main.Tags.ExitCode
  ): Unit =
    assert(exitCode == 0)

    val branchName = currentBranch(path)

    assert(branchName == ourBranch)

    val (postMergeCommit, parents) = currentMergeCommit(path)

    assert(parents.size == 2)

    assert(postMergeCommit != commitOfOneBranch)
    assert(postMergeCommit != commitOfTheOtherBranch)

    val commitOfOneBranchIsAncestor =
      os.proc(
        "git",
        "merge-base",
        "--is-ancestor",
        commitOfOneBranch,
        postMergeCommit
      ).call(path, check = false)
        .exitCode == 0

    assert(commitOfOneBranchIsAncestor)

    val commitOfTheOtherBranchIsAncestor =
      os.proc(
        "git",
        "merge-base",
        "--is-ancestor",
        commitOfTheOtherBranch,
        postMergeCommit
      ).call(path, check = false)
        .exitCode == 0

    assert(commitOfTheOtherBranchIsAncestor)

    assert(currentStatus(path).isEmpty)
  end verifyMergeMakesANewCommitWithACleanIndex

  private def currentMergeCommit(path: Path): (String, Seq[String]) =
    os
      .proc(s"git", "log", "-1", "--format=tformat:%H %P")
      .call(path)
      .out
      .text()
      .strip
      .split("\\s+") match
      case Array(postMergeCommit, parents*) => postMergeCommit -> parents
    : @unchecked

  private def currentStatus(path: Path) =
    os.proc(s"git", "status", "--short").call(path).out.text().strip

  private def verifyATrivialNoFastForwardNoChangesMergeDoesNotMakeACommit(
      path: Path
  )(
      commitOfAdvancedBranch: String,
      ourBranch: String,
      exitCode: Int @@ Main.Tags.ExitCode
  ): Unit =
    assert(exitCode == 0)

    val branchName = currentBranch(path)

    assert(branchName == ourBranch)

    val postMergeCommit =
      currentCommit(path)

    assert(
      postMergeCommit == commitOfAdvancedBranch
    )

    assert(!os.exists(mergeHeadPath(path)))

    val status = os.proc("git", "status", "--short").call(path).out.text().strip

    assert(status.isEmpty)
  end verifyATrivialNoFastForwardNoChangesMergeDoesNotMakeACommit

  private def currentCommit(path: Path) =
    os.proc("git", "log", "-1", "--format=tformat:%H")
      .call(path)
      .out
      .text()
      .strip

  private def currentBranch(path: Path) =
    os.proc("git", "branch", "--show-current").call(path).out.text().strip()

  private def mergeHeadPath(path: Path) =
    path / ".git" / "MERGE_HEAD"

  private def verifyATrivialNoFastForwardNoCommitMergeDoesNotMakeACommit(
      path: Path
  )(
      commitOfAdvancedBranch: String,
      commitOfRetardedBranch: String,
      ourBranch: String,
      exitCode: Int @@ Main.Tags.ExitCode
  ): Unit =
    assert(exitCode == 1)

    val branchName = currentBranch(path)

    assert(branchName == ourBranch)

    val postMergeCommit =
      currentCommit(path)

    assert(
      postMergeCommit == commitOfRetardedBranch
    )

    assert(
      mergeHead(path) == commitOfAdvancedBranch
    )

    assert(currentStatus(path).nonEmpty)
  end verifyATrivialNoFastForwardNoCommitMergeDoesNotMakeACommit

  private def mergeHead(path: Path) =
    os.read(mergeHeadPath(path)).strip()

  private def verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
      path: Path
  )(
      flipBranches: Boolean,
      commitOfNonMasterFileBranch: String,
      commitOfMasterBranch: String,
      ourBranch: String,
      exitCode: Int @@ Main.Tags.ExitCode
  ): String =
    assert(exitCode == 1)

    val branchName = currentBranch(path)

    assert(branchName == ourBranch)

    val postMergeCommit = currentCommit(path)

    assert(
      postMergeCommit == (if flipBranches then commitOfNonMasterFileBranch
                          else commitOfMasterBranch)
    )

    assert(
      mergeHead(path) == (if flipBranches then commitOfMasterBranch
                          else commitOfNonMasterFileBranch)
    )

    assert(currentStatus(path).nonEmpty)

    currentStatus(path)
  end verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex

  private def gitRepository(): ImperativeResource[Path] =
    for
      temporaryDirectory <- Resource.make(IO {
        os.temp.dir(prefix = "toyGitRepository")
      })(temporaryDirectory => IO { os.remove.all.apply(temporaryDirectory) })
      _ <- Resource.eval(IO {
        os.proc("git", "init").call(temporaryDirectory).out.text()
      })
      _ <- Resource.eval(IO {
        os.proc("git", "config", "user.name", "MainTest")
          .call(temporaryDirectory)
          .out
          .text()
      })
      _ <- Resource.eval(IO {
        os.proc("git", "config", "user.email", "non-existent@dev.null")
          .call(temporaryDirectory)
          .out
          .text()
      })
      _ <- Resource.eval(IO {
        makeNewBranch(temporaryDirectory)(masterBranch)
      })
    yield temporaryDirectory
    end for
  end gitRepository

  private def makeNewBranch(path: Path)(evilTwinBranch: String): Unit =
    println(
      os.proc("git", "checkout", "-b", evilTwinBranch)
        .call(path)
        .out
        .text()
    )

  private def checkoutBranch(path: Path)(evilTwinBranch: String): Unit =
    println(
      os.proc("git", "checkout", evilTwinBranch)
        .call(path)
        .out
        .text()
    )

  private def contentMatches(expected: String)(actual: String) =
    tokens(actual).get.corresponds(
      tokens(
        expected
      ).get
    )(tokenEquality)
end MainTest

class MainTest:
  @TestFactory
  def trivialMerge(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans and trialsApi.booleans)
      .withLimit(14)
      .dynamicTests {
        case (
              optionalSubdirectory,
              ourBranchIsBehindTheirs,
              noFastForward,
              noCommit
            ) =>
          gitRepository()
            .use(path =>
              IO {
                optionalSubdirectory
                  .foreach(subdirectory => os.makeDir(path / subdirectory))

                introducingArthur(path)

                val commitOfMasterBranch = currentCommit(path)

                val advancedBranch = "advancedBranch"

                makeNewBranch(path)(advancedBranch)

                arthurContinues(path)

                val commitOfAdvancedBranch = currentCommit(path)

                if ourBranchIsBehindTheirs then
                  checkoutBranch(path)(masterBranch)
                end if

                val (ourBranch, theirBranch) =
                  if ourBranchIsBehindTheirs then masterBranch -> advancedBranch
                  else advancedBranch                          -> masterBranch

                val exitCode = Main.mergeTheirBranch(
                  ApplicationRequest.default.copy(
                    theirBranchHead =
                      theirBranch.taggedWith[Tags.CommitOrBranchName],
                    noCommit = noCommit,
                    noFastForward = noFastForward,
                    minimumAmbiguousMatchSize = 0
                  )
                )(workingDirectory =
                  optionalSubdirectory.fold(ifEmpty = path)(path / _)
                )

                if noFastForward then
                  if !ourBranchIsBehindTheirs then
                    verifyATrivialNoFastForwardNoChangesMergeDoesNotMakeACommit(
                      path
                    )(
                      commitOfAdvancedBranch,
                      ourBranch,
                      exitCode
                    )
                  else if noCommit then
                    verifyATrivialNoFastForwardNoCommitMergeDoesNotMakeACommit(
                      path
                    )(
                      commitOfAdvancedBranch,
                      commitOfMasterBranch,
                      ourBranch,
                      exitCode
                    )

                    println(
                      os.proc("git", "commit", "-m", "'Completing merge.'")
                        .call(path)
                        .out
                        .text()
                    )

                    verifyMergeMakesANewCommitWithACleanIndex(path)(
                      commitOfMasterBranch,
                      commitOfAdvancedBranch,
                      ourBranch,
                      exitCode =
                        0.taggedWith[Tags.ExitCode] // Placeholder as Kinetic Merge hasn't actually done the merge.
                    )
                  else
                    verifyMergeMakesANewCommitWithACleanIndex(path)(
                      commitOfMasterBranch,
                      commitOfAdvancedBranch,
                      ourBranch,
                      exitCode
                    )
                else
                  verifyTrivialMergeMovesToTheMostAdvancedCommitWithACleanIndex(
                    path
                  )(
                    commitOfAdvancedBranch,
                    ourBranch,
                    exitCode
                  )
                end if
              }
            )
            .unsafeRunSync()
      }
  end trivialMerge

  @TestFactory
  def cleanMergeBringingInANewFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches, noCommit) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingArthur(path)

              val newFileBranch = "newFileBranch"

              makeNewBranch(path)(newFileBranch)

              enterTysonStageLeft(path)

              val commitOfNewFileBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              arthurContinues(path)

              val commitOfMasterBranch = currentCommit(path)

              if flipBranches then checkoutBranch(path)(newFileBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then newFileBranch -> masterBranch
                else masterBranch                  -> newFileBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit,
                  minimumAmbiguousMatchSize = 0
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              if noCommit then
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfNewFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              else
                verifyMergeMakesANewCommitWithACleanIndex(path)(
                  commitOfNewFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              end if
            }
          )
          .unsafeRunSync()
      }
  end cleanMergeBringingInANewFile

  @TestFactory
  def cleanMergeDeletingAFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches, noCommit) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingArthur(path)

              val deletedFileBranch = "deletedFileBranch"

              makeNewBranch(path)(deletedFileBranch)

              exeuntArthur(path)

              val commitOfDeletedFileBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              enterTysonStageLeft(path)

              val commitOfMasterBranch = currentCommit(path)

              if flipBranches then checkoutBranch(path)(deletedFileBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then deletedFileBranch -> masterBranch
                else masterBranch                      -> deletedFileBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit,
                  minimumAmbiguousMatchSize = 0
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              if noCommit then
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfDeletedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              else
                verifyMergeMakesANewCommitWithACleanIndex(path)(
                  commitOfDeletedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              end if
            }
          )
          .unsafeRunSync()
      }
  end cleanMergeDeletingAFile

  @TestFactory
  def cleanMergeOfAFileAddedInBothBranches(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches, noCommit) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              sandraStopsByBriefly(path)

              val benignTwinBranch = "benignTwin"

              makeNewBranch(path)(benignTwinBranch)

              introducingArthur(path)

              arthurContinues(path)

              val commitOfBenignTwinBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              // This is purely to prevent the following call to
              // `introducingArthur` from making a *duplicate commit* of the one
              // already done on the master branch. Otherwise, if Git infers a
              // duplicate commit, then the common ancestor will include the
              // Arthur file; thus we will not be testing file addition, rather
              // *modification*.
              enterTysonStageLeft(path)

              introducingArthur(path)

              sandraHeadsOffHome(path)

              arthurClearsHisThroat(path)

              val commitOfMasterBranch = currentCommit(path).strip

              if flipBranches then checkoutBranch(path)(benignTwinBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then benignTwinBranch -> masterBranch
                else masterBranch                     -> benignTwinBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit,
                  minimumAmbiguousMatchSize = 0
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              if noCommit then
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfBenignTwinBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              else
                verifyMergeMakesANewCommitWithACleanIndex(path)(
                  commitOfBenignTwinBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              end if
            }
          )
          .unsafeRunSync()
      }
  end cleanMergeOfAFileAddedInBothBranches

  @TestFactory
  def conflictingAdditionOfTheSameFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(4)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingArthur(path)

              sandraStopsByBriefly(path)

              val evilTwinBranch = "evilTwin"

              makeNewBranch(path)(evilTwinBranch)

              evilTysonMakesDramaticEntranceExulting(path)

              val commitOfEvilTwinBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              sandraHeadsOffHome(path)

              enterTysonStageLeft(path)

              val commitOfMasterBranch = currentCommit(path).strip

              if flipBranches then checkoutBranch(path)(evilTwinBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then evilTwinBranch -> masterBranch
                else masterBranch                   -> evilTwinBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  minimumAmbiguousMatchSize = 0
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              val status =
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfEvilTwinBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )

              noUpdatesInIndexForArthur(status)

              tysonIsMarkedWithConflictingAdditionsInTheIndex(status)

              tysonSaidConflictingThings(path)

              if flipBranches then sandraIsMarkedAsDeletedInTheIndex(status)
              else noUpdatesInIndexForSandra(status)
              end if
            }
          )
          .unsafeRunSync()
      }
  end conflictingAdditionOfTheSameFile

  @TestFactory
  def conflictingInsertModificationAndDeletionOfTheSameFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(4)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingArthur(path)

              sandraStopsByBriefly(path)

              val deletedFileBranch = "deletedFileBranch"

              makeNewBranch(path)(deletedFileBranch)

              enterTysonStageLeft(path)

              exeuntArthur(path)

              val commitOfDeletedFileBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              sandraHeadsOffHome(path)

              arthurContinues(path)

              val commitOfMasterBranch = currentCommit(path)

              val arthurOnTheRecord = os.read(path / arthur)

              if flipBranches then checkoutBranch(path)(deletedFileBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then deletedFileBranch -> masterBranch
                else masterBranch                      -> deletedFileBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  minimumAmbiguousMatchSize = 0
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              val status =
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfDeletedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )

              arthurIsMarkedWithConflictingUpdateAndDeletionInTheIndex(
                flipBranches,
                status
              )

              assert(
                contentMatches(expected = arthurOnTheRecord)(
                  os.read(path / arthur)
                )
              )

              if flipBranches then
                sandraIsMarkedAsDeletedInTheIndex(status)
                noUpdatesInIndexForTyson(status)
              else
                noUpdatesInIndexForSandra(status)
                tysonIsMarkedAsAddedInTheIndex(status)
              end if
            }
          )
          .unsafeRunSync()
      }
  end conflictingInsertModificationAndDeletionOfTheSameFile

  @TestFactory
  def conflictingEditModificationAndDeletionOfTheSameFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(4)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingArthur(path)

              sandraStopsByBriefly(path)

              val deletedFileBranch = "deletedFileBranch"

              makeNewBranch(path)(deletedFileBranch)

              enterTysonStageLeft(path)

              exeuntArthur(path)

              val commitOfDeletedFileBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              sandraHeadsOffHome(path)

              // NOTE: this keeps the *original* content; we need to be sure
              // that isn't lost in the merge.
              arthurBecomesAnExpertOnCasesLimitStrategy(path)

              val commitOfMasterBranch = currentCommit(path)

              val arthurOnTheRecord = os.read(path / arthur)

              if flipBranches then checkoutBranch(path)(deletedFileBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then deletedFileBranch -> masterBranch
                else masterBranch                      -> deletedFileBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  minimumAmbiguousMatchSize = 0
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              val status =
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfDeletedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )

              arthurIsMarkedWithConflictingUpdateAndDeletionInTheIndex(
                flipBranches,
                status
              )

              assert(
                contentMatches(expected = arthurOnTheRecord)(
                  os.read(path / arthur)
                )
              )

              if flipBranches then
                sandraIsMarkedAsDeletedInTheIndex(status)
                noUpdatesInIndexForTyson(status)
              else
                noUpdatesInIndexForSandra(status)
                tysonIsMarkedAsAddedInTheIndex(status)
              end if
            }
          )
          .unsafeRunSync()
      }
  end conflictingEditModificationAndDeletionOfTheSameFile

  @TestFactory
  def conflictingContentClearanceModificationAndDeletionOfTheSameFile()
      : DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(4)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingArthur(path)

              sandraStopsByBriefly(path)

              val deletedFileBranch = "deletedFileBranch"

              makeNewBranch(path)(deletedFileBranch)

              enterTysonStageLeft(path)

              exeuntArthur(path)

              val commitOfDeletedFileBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              sandraHeadsOffHome(path)

              arthurDeniesHavingSaidAnything(path)

              val commitOfMasterBranch = currentCommit(path)

              if flipBranches then checkoutBranch(path)(deletedFileBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then deletedFileBranch -> masterBranch
                else masterBranch                      -> deletedFileBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  minimumAmbiguousMatchSize = 0
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              val status =
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfDeletedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )

              arthurIsMarkedWithConflictingUpdateAndDeletionInTheIndex(
                flipBranches,
                status
              )

              assert(0 == os.size(path / arthur))

              if flipBranches then
                sandraIsMarkedAsDeletedInTheIndex(status)
                noUpdatesInIndexForTyson(status)
              else
                noUpdatesInIndexForSandra(status)
                tysonIsMarkedAsAddedInTheIndex(status)
              end if
            }
          )
          .unsafeRunSync()
      }
  end conflictingContentClearanceModificationAndDeletionOfTheSameFile

  @TestFactory
  def conflictingModificationOfTheSameFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans)
      .withLimit(4)
      .dynamicTests { case (optionalSubdirectory, flipBranches) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingArthur(path)

              sandraStopsByBriefly(path)

              val concurrentlyModifiedFileBranch =
                "concurrentlyModifiedFileBranch"

              makeNewBranch(path)(concurrentlyModifiedFileBranch)

              enterTysonStageLeft(path)

              arthurElaborates(path)

              val commitOfConcurrentlyModifiedFileBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              sandraHeadsOffHome(path)

              arthurContinues(path)

              val commitOfMasterBranch = currentCommit(path)

              if flipBranches then
                checkoutBranch(path)(concurrentlyModifiedFileBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then
                  concurrentlyModifiedFileBranch -> masterBranch
                else masterBranch -> concurrentlyModifiedFileBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  minimumAmbiguousMatchSize = 0
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              val status =
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfConcurrentlyModifiedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )

              arthurIsMarkedWithConflictingUpdatesInTheIndex(status)

              arthurSaidConflictingThings(path)

              if flipBranches then
                sandraIsMarkedAsDeletedInTheIndex(status)
                noUpdatesInIndexForTyson(status)
              else
                noUpdatesInIndexForSandra(status)
                tysonIsMarkedAsAddedInTheIndex(status)
              end if
            }
          )
          .unsafeRunSync()
      }
  end conflictingModificationOfTheSameFile

  @TestFactory
  def cleanMergeOfAFileDeletedInBothBranches(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches, noCommit) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingArthur(path)

              sandraStopsByBriefly(path)

              val concurrentlyDeletedFileBranch =
                "concurrentlyDeletedFileBranch"

              makeNewBranch(path)(concurrentlyDeletedFileBranch)

              enterTysonStageLeft(path)

              exeuntArthur(path)

              val commitOfConcurrentlyDeletedFileBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              sandraHeadsOffHome(path)

              arthurContinues(path)

              arthurExcusesHimself(path)

              val commitOfMasterBranch = currentCommit(path)

              if flipBranches then
                checkoutBranch(path)(concurrentlyDeletedFileBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then
                  concurrentlyDeletedFileBranch -> masterBranch
                else masterBranch               -> concurrentlyDeletedFileBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit,
                  minimumAmbiguousMatchSize = 0
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              if noCommit then
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfConcurrentlyDeletedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              else
                verifyMergeMakesANewCommitWithACleanIndex(path)(
                  commitOfConcurrentlyDeletedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              end if

            }
          )
          .unsafeRunSync()
      }
  end cleanMergeOfAFileDeletedInBothBranches

  @TestFactory
  def cleanMergeOfAFileModifiedInBothBranches(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches, noCommit) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingArthur(path)

              sandraStopsByBriefly(path)

              val concurrentlyModifiedFileBranch =
                "concurrentlyModifiedFileBranch"

              makeNewBranch(path)(concurrentlyModifiedFileBranch)

              enterTysonStageLeft(path)

              arthurCorrectsHimself(path)

              val commitOfConcurrentlyModifiedFileBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              sandraHeadsOffHome(path)

              arthurContinues(path)

              val commitOfMasterBranch = currentCommit(path)

              if flipBranches then
                checkoutBranch(path)(concurrentlyModifiedFileBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then
                  concurrentlyModifiedFileBranch -> masterBranch
                else masterBranch -> concurrentlyModifiedFileBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit,
                  minimumAmbiguousMatchSize = 0
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              if noCommit then
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfConcurrentlyModifiedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              else
                verifyMergeMakesANewCommitWithACleanIndex(path)(
                  commitOfConcurrentlyModifiedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              end if
            }
          )
          .unsafeRunSync()
      }
  end cleanMergeOfAFileModifiedInBothBranches

  @TestFactory
  def anEditAndADeletionPropagatingThroughAFileMove(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches, noCommit) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingCasesLimitStrategy(path)

              val movedFileBranch = "movedFileBranch"

              makeNewBranch(path)(movedFileBranch)

              moveCasesLimitStrategy(path)

              val commitOfMovedFileBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              editingCasesLimitStrategy(path)

              val commitOfMasterBranch = currentCommit(path)

              if flipBranches then checkoutBranch(path)(movedFileBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then movedFileBranch -> masterBranch
                else masterBranch                    -> movedFileBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit,
                  minimumAmbiguousMatchSize = 5
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              if noCommit then
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfMovedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              else
                verifyMergeMakesANewCommitWithACleanIndex(path)(
                  commitOfMovedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              end if

              assert(
                contentMatches(expected = editedCasesLimitStrategyContent)(
                  os.read(path / movedCasesLimitStrategy)
                )
              )
              assert(!os.exists(path / casesLimitStrategy))
            }
          )
          .unsafeRunSync()
      }
  end anEditAndADeletionPropagatingThroughAFileMove

  @TestFactory
  def anEditAndADeletionPropagatingThroughAFileSplit(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans and trialsApi.booleans)
      .withLimit(20)
      .dynamicTests {
        case (
              optionalSubdirectory,
              flipBranches,
              noCommit,
              loseOriginalFileInSplit
            ) =>
          gitRepository()
            .use(path =>
              IO {
                optionalSubdirectory
                  .foreach(subdirectory => os.makeDir(path / subdirectory))

                introducingCasesLimitStrategy(path)

                val splitFileBranch = "splitFileBranch"

                makeNewBranch(path)(splitFileBranch)

                splittingCasesLimitStrategy(path)

                if loseOriginalFileInSplit then moveCasesLimitStrategy(path)
                end if

                val commitOfSplitFileBranch = currentCommit(path)

                checkoutBranch(path)(masterBranch)

                editingCasesLimitStrategy(path)

                val commitOfMasterBranch = currentCommit(path)

                if flipBranches then checkoutBranch(path)(splitFileBranch)
                end if

                val (ourBranch, theirBranch) =
                  if flipBranches then splitFileBranch -> masterBranch
                  else masterBranch                    -> splitFileBranch

                val exitCode = Main.mergeTheirBranch(
                  ApplicationRequest.default.copy(
                    theirBranchHead =
                      theirBranch.taggedWith[Tags.CommitOrBranchName],
                    noCommit = noCommit,
                    minimumAmbiguousMatchSize = 5
                  )
                )(workingDirectory =
                  optionalSubdirectory.fold(ifEmpty = path)(path / _)
                )

                if noCommit then
                  verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                    path
                  )(
                    flipBranches,
                    commitOfSplitFileBranch,
                    commitOfMasterBranch,
                    ourBranch,
                    exitCode
                  )
                else
                  verifyMergeMakesANewCommitWithACleanIndex(path)(
                    commitOfSplitFileBranch,
                    commitOfMasterBranch,
                    ourBranch,
                    exitCode
                  )
                end if

                assert(
                  contentMatches(
                    expected =
                      justTheInterfaceForCasesLimitStrategyExpectedContent
                  )(
                    os.read(
                      path / (if loseOriginalFileInSplit then
                                movedCasesLimitStrategy
                              else casesLimitStrategy)
                    )
                  )
                )
                assert(
                  contentMatches(expected =
                    excisedCasesLimitStrategiesExpectedContent
                  )(
                    os.read(path / excisedCasesLimitStrategies)
                  )
                )
              }
            )
            .unsafeRunSync()
      }
  end anEditAndADeletionPropagatingThroughAFileSplit

  @TestFactory
  def anEditAndADeletionPropagatingThroughAFileCondensation(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans and trialsApi.booleans)
      .withLimit(20)
      .dynamicTests {
        case (
              optionalSubdirectory,
              flipBranches,
              noCommit,
              loseBothOriginalFilesInJoin
            ) =>
          gitRepository()
            .use(path =>
              IO {
                optionalSubdirectory
                  .foreach(subdirectory => os.makeDir(path / subdirectory))

                // What follows is
                // `anEditAndADeletionPropagatingThroughAFileSplit` in
                // reverse...

                introducingInterfaceOnlyCasesLimitStrategy(path)
                introducingCasesLimitStrategies(path)

                val condensedFilesBranch = "condensedFilesBranch"

                makeNewBranch(path)(condensedFilesBranch)

                condensingCasesLimitStrategy(path)

                if loseBothOriginalFilesInJoin then moveCasesLimitStrategy(path)
                end if

                val commitOfCondensedFilesBranch = currentCommit(path)

                checkoutBranch(path)(masterBranch)

                editingInterfaceOnlyCasesLimitStrategy(path)
                editingCasesLimitStrategies(path)

                val commitOfMasterBranch = currentCommit(path)

                if flipBranches then checkoutBranch(path)(condensedFilesBranch)
                end if

                val (ourBranch, theirBranch) =
                  if flipBranches then condensedFilesBranch -> masterBranch
                  else masterBranch -> condensedFilesBranch

                val exitCode = Main.mergeTheirBranch(
                  ApplicationRequest.default.copy(
                    theirBranchHead =
                      theirBranch.taggedWith[Tags.CommitOrBranchName],
                    noCommit = noCommit,
                    minimumAmbiguousMatchSize = 5
                  )
                )(workingDirectory =
                  optionalSubdirectory.fold(ifEmpty = path)(path / _)
                )

                if noCommit then
                  verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                    path
                  )(
                    flipBranches,
                    commitOfCondensedFilesBranch,
                    commitOfMasterBranch,
                    ourBranch,
                    exitCode
                  )
                else
                  verifyMergeMakesANewCommitWithACleanIndex(path)(
                    commitOfCondensedFilesBranch,
                    commitOfMasterBranch,
                    ourBranch,
                    exitCode
                  )
                end if

                assert(
                  contentMatches(expected = baseCasesLimitStrategyContent)(
                    os.read(
                      path / (if loseBothOriginalFilesInJoin then
                                movedCasesLimitStrategy
                              else casesLimitStrategy)
                    )
                  )
                )
              }
            )
            .unsafeRunSync()
      }
  end anEditAndADeletionPropagatingThroughAFileCondensation

  @TestFactory
  def twoFilesSwappingAroundWithModificationOfOne(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches, noCommit) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingCasesLimitStrategy(path)
              introducingExpectyFlavouredAssert(path)

              val swappedFilesBranch = "swappedFileBranch"

              makeNewBranch(path)(swappedFilesBranch)

              swapTheTwoFiles(path)

              val commitOfSwappedFilesBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              editingExpectyFlavouredAssert(path)

              val commitOfMasterBranch = currentCommit(path)

              if flipBranches then checkoutBranch(path)(swappedFilesBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then swappedFilesBranch -> masterBranch
                else masterBranch                       -> swappedFilesBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit,
                  minimumAmbiguousMatchSize = 0
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              if noCommit then
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfSwappedFilesBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              else
                verifyMergeMakesANewCommitWithACleanIndex(path)(
                  commitOfSwappedFilesBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              end if

              assert(
                contentMatches(expected = editedExpectyFlavouredAssertContent)(
                  os.read(path / casesLimitStrategy)
                )
              )
              assert(
                contentMatches(expected = baseCasesLimitStrategyContent)(
                  os.read(path / expectyFlavouredAssert)
                )
              )
            }
          )
          .unsafeRunSync()
      }
  end twoFilesSwappingAroundWithModificationOfOne

  @TestFactory
  def twoFilesSwappingAroundWithModificationsToBoth(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches, noCommit) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingCasesLimitStrategy(path)
              introducingExpectyFlavouredAssert(path)

              val swappedFilesBranch = "swappedFileBranch"

              makeNewBranch(path)(swappedFilesBranch)

              swapTheTwoFiles(path)

              val commitOfSwappedFilesBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              editingCasesLimitStrategy(path)
              editingExpectyFlavouredAssert(path)

              val commitOfMasterBranch = currentCommit(path)

              if flipBranches then checkoutBranch(path)(swappedFilesBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then swappedFilesBranch -> masterBranch
                else masterBranch                       -> swappedFilesBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit,
                  minimumAmbiguousMatchSize = 5
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              if noCommit then
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfSwappedFilesBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              else
                verifyMergeMakesANewCommitWithACleanIndex(path)(
                  commitOfSwappedFilesBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              end if

              assert(
                contentMatches(expected = editedExpectyFlavouredAssertContent)(
                  os.read(path / casesLimitStrategy)
                )
              )
              assert(
                contentMatches(expected = editedCasesLimitStrategyContent)(
                  os.read(path / expectyFlavouredAssert)
                )
              )
            }
          )
          .unsafeRunSync()
      }
  end twoFilesSwappingAroundWithModificationsToBoth

  @TestFactory
  def issue48BugReproduction(): DynamicTests =
    (trialsApi.booleans and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (flipBranches, noCommit, emptyRenamedFileInBase) =>
        gitRepository()
          .use(path =>
            IO {
              val originalFilename = "aFile.txt"
              val renamedFilename  = "theRenamedFile.txt"

              {
                os.write(
                  path / originalFilename,
                  """
                    |This is the first line,
                    |followed by the second.
                    |
                    |Can you see where this is going?
                    |Need a hint?
                    |THE END.
                    |""".stripMargin
                )
                println(
                  os.proc("git", "add", originalFilename).call(path).out.text()
                )

                if emptyRenamedFileInBase then
                  os.write(path / renamedFilename, "")
                  println(
                    os.proc("git", "add", renamedFilename).call(path).out.text()
                  )
                end if

                println(
                  os.proc(
                    "git",
                    "commit",
                    "-m",
                    s"'Introducing `$originalFilename`${
                        if emptyRenamedFileInBase
                        then s" (and an empty `$renamedFilename`)"
                        else ""
                      }.'"
                  ).call(path)
                    .out
                    .text()
                )
              }

              val movedFileBranch = "renamedFileBranch"

              makeNewBranch(path)(movedFileBranch)

              {
                os.remove(
                  path / originalFilename
                )
                val renamedFileContent =
                  """
                    |This is the first line,
                    |followed by the second.
                    |
                    |Can you see where this is going?
                    |Need a hint? No, good - you're a quick study.
                    |THE END.
                    |""".stripMargin

                if emptyRenamedFileInBase then
                  os.write.over(
                    path / renamedFilename,
                    renamedFileContent
                  )
                else
                  os.write(
                    path / renamedFilename,
                    renamedFileContent
                  )
                end if

                println(
                  os.proc("git", "rm", originalFilename).call(path).out.text()
                )
                println(
                  os.proc("git", "add", renamedFilename).call(path).out.text()
                )
                println(
                  os.proc(
                    "git",
                    "commit",
                    "-m",
                    s"'Renaming `$originalFilename` to ${
                        if emptyRenamedFileInBase then "existing " else ""
                      }`$renamedFilename` with an edit.'"
                  ).call(path)
                    .out
                    .text()
                )
              }

              checkoutBranch(path)(masterBranch)

              {
                os.write.over(
                  path / originalFilename,
                  """
                    |This is the obligatory zeroth line.
                    |Can you see where this is going?
                    |Need a hint?
                    |This was the first line,
                    |followed by the second.
                    |
                    |THE END.
                    |""".stripMargin,
                  createFolders = true
                )
                println(
                  os.proc(
                    "git",
                    "commit",
                    "-am",
                    s"'Editing `$originalFilename`.'"
                  ).call(path)
                    .out
                    .text()
                )
              }

              if flipBranches then checkoutBranch(path)(movedFileBranch)
              end if

              val theirBranch =
                if flipBranches then masterBranch
                else movedFileBranch

              val _ = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit,
                  minimumMatchSize = 3
                )
              )(workingDirectory = path)

              assert(os.exists(path / renamedFilename))
              assert(!os.exists(path / originalFilename))
            }
          )
          .unsafeRunSync()
      }
  end issue48BugReproduction

  @TestFactory
  def conflictingDeletionAndFileMoveOfTheSameFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches, noCommit) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingCasesLimitStrategy(path)

              val movedFileBranch = "movedFileBranch"

              makeNewBranch(path)(movedFileBranch)

              moveCasesLimitStrategy(path)

              val commitOfMovedFileBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              removingCasesLimitStrategy(path)

              val commitOfMasterBranch = currentCommit(path)

              if flipBranches then checkoutBranch(path)(movedFileBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then movedFileBranch -> masterBranch
                else masterBranch                    -> movedFileBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit,
                  minimumAmbiguousMatchSize = 5
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              val status =
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfMovedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )

              pathIsMarkedWithConflictingDeletionAndRenameInTheIndex(
                movedCasesLimitStrategy
              )(flipBranches, status)

              assert(
                contentMatches(expected = baseCasesLimitStrategyContent)(
                  os.read(path / movedCasesLimitStrategy)
                )
              )
              assert(!os.exists(path / casesLimitStrategy))
            }
          )
          .unsafeRunSync()
      }
  end conflictingDeletionAndFileMoveOfTheSameFile

  @TestFactory
  def conflictingDeletionAndEditedFileMoveOfTheSameFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches, noCommit) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingCasesLimitStrategy(path)

              val movedFileBranch = "movedFileBranch"

              makeNewBranch(path)(movedFileBranch)

              editingCasesLimitStrategy(path)

              moveCasesLimitStrategy(path)

              val commitOfMovedFileBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              removingCasesLimitStrategy(path)

              val commitOfMasterBranch = currentCommit(path)

              if flipBranches then checkoutBranch(path)(movedFileBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then movedFileBranch -> masterBranch
                else masterBranch                    -> movedFileBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit,
                  minimumAmbiguousMatchSize = 5
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              val status =
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfMovedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )

              pathIsMarkedWithConflictingDeletionAndRenameInTheIndex(
                movedCasesLimitStrategy
              )(flipBranches, status)

              assert(
                contentMatches(expected = editedCasesLimitStrategyContent)(
                  os.read(path / movedCasesLimitStrategy)
                )
              )
              assert(!os.exists(path / casesLimitStrategy))
            }
          )
          .unsafeRunSync()
      }
  end conflictingDeletionAndEditedFileMoveOfTheSameFile

  @TestFactory
  def cleanMergeOfDeletionAndFileCondensationOfTheSameFile(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches, noCommit) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingCasesLimitStrategy(path)

              introducingArthur(path)

              val condensedFileBranch = "condensedFileBranch"

              makeNewBranch(path)(condensedFileBranch)

              removingCasesLimitStrategy(path)

              arthurBecomesAnExpertOnCasesLimitStrategy(path)

              val commitOfCondensedFileBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              sandraStopsByBriefly(path)

              removingCasesLimitStrategy(path)

              val commitOfMasterBranch = currentCommit(path)

              if flipBranches then checkoutBranch(path)(condensedFileBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then condensedFileBranch -> masterBranch
                else masterBranch                        -> condensedFileBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit,
                  minimumAmbiguousMatchSize = 5
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              if noCommit then
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfCondensedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              else
                verifyMergeMakesANewCommitWithACleanIndex(path)(
                  commitOfCondensedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )
              end if

              assert(!os.exists(path / casesLimitStrategy))
            }
          )
          .unsafeRunSync()
      }
  end cleanMergeOfDeletionAndFileCondensationOfTheSameFile

  @TestFactory
  def conflictingDeletionAndReplacementWithFileMoveOfTheSameFile()
      : DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches, noCommit) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingCasesLimitStrategy(path)

              val movedFileBranch = "movedFileBranch"

              makeNewBranch(path)(movedFileBranch)

              moveCasesLimitStrategy(path)

              reintroducingCasesLimitStrategy(path)

              val commitOfMovedFileBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              removingCasesLimitStrategy(path)

              val commitOfMasterBranch = currentCommit(path)

              if flipBranches then checkoutBranch(path)(movedFileBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then movedFileBranch -> masterBranch
                else masterBranch                    -> movedFileBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit,
                  minimumAmbiguousMatchSize = 5
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              val status =
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfMovedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )

              pathIsMarkedWithConflictingUpdateAndDeletionInTheIndex(
                casesLimitStrategy
              )(!flipBranches, status)

              assert(
                contentMatches(expected = replacementCasesLimitStrategyContent)(
                  os.read(path / casesLimitStrategy)
                )
              )

              assert(
                contentMatches(expected = baseCasesLimitStrategyContent)(
                  os.read(path / movedCasesLimitStrategy)
                )
              )
            }
          )
          .unsafeRunSync()
      }
  end conflictingDeletionAndReplacementWithFileMoveOfTheSameFile

  @TestFactory
  def conflictingDeletionAndReplacementWithEditedFileMoveOfTheSameFile()
      : DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches, noCommit) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingCasesLimitStrategy(path)

              val movedFileBranch = "movedFileBranch"

              makeNewBranch(path)(movedFileBranch)

              editingCasesLimitStrategy(path)

              moveCasesLimitStrategy(path)

              reintroducingCasesLimitStrategy(path)

              val commitOfMovedFileBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              removingCasesLimitStrategy(path)

              val commitOfMasterBranch = currentCommit(path)

              if flipBranches then checkoutBranch(path)(movedFileBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then movedFileBranch -> masterBranch
                else masterBranch                    -> movedFileBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit,
                  minimumAmbiguousMatchSize = 5
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              val status =
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfMovedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )

              pathIsMarkedWithConflictingUpdateAndDeletionInTheIndex(
                casesLimitStrategy
              )(!flipBranches, status)

              assert(
                contentMatches(expected = replacementCasesLimitStrategyContent)(
                  os.read(path / casesLimitStrategy)
                )
              )

              assert(
                contentMatches(expected = editedCasesLimitStrategyContent)(
                  os.read(path / movedCasesLimitStrategy)
                )
              )
            }
          )
          .unsafeRunSync()
      }
  end conflictingDeletionAndReplacementWithEditedFileMoveOfTheSameFile

  @TestFactory
  def conflictingConvergingFileMovesFromDifferentFiles(): DynamicTests =
    (optionalSubdirectories and trialsApi.booleans and trialsApi.booleans)
      .withLimit(10)
      .dynamicTests { case (optionalSubdirectory, flipBranches, noCommit) =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory
                .foreach(subdirectory => os.makeDir(path / subdirectory))

              introducingCasesLimitStrategy(path)

              introducingArthur(path)

              val casesLimitStrategyMovesBranch =
                "casesLimitStrategyMovesBranch"

              makeNewBranch(path)(casesLimitStrategyMovesBranch)

              moveCasesLimitStrategy(path)

              val commitOfMovedFileBranch = currentCommit(path)

              checkoutBranch(path)(masterBranch)

              arthurTakesOnAPseudonym(path)

              val commitOfMasterBranch = currentCommit(path)

              if flipBranches then
                checkoutBranch(path)(casesLimitStrategyMovesBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then
                  casesLimitStrategyMovesBranch -> masterBranch
                else masterBranch               -> casesLimitStrategyMovesBranch

              val exitCode = Main.mergeTheirBranch(
                ApplicationRequest.default.copy(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit,
                  minimumAmbiguousMatchSize = 5
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path / _)
              )

              val status =
                verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
                  path
                )(
                  flipBranches,
                  commitOfMovedFileBranch,
                  commitOfMasterBranch,
                  ourBranch,
                  exitCode
                )

              pathIsMarkedWithConflictingAdditionAndAdditionInTheIndex(
                movedCasesLimitStrategy
              )(status)
            }
          )
          .unsafeRunSync()
      }
  end conflictingConvergingFileMovesFromDifferentFiles

end MainTest
