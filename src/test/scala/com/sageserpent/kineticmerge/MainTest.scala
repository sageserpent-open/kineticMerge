package com.sageserpent.kineticmerge

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.{DynamicTests, *}
import com.sageserpent.kineticmerge.Main.{CommandLineArguments, Tags}
import com.sageserpent.kineticmerge.MainTest.*
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.softwaremill.tagging.*
import org.junit.jupiter.api.{BeforeEach, DynamicTest, Test, TestFactory}
import os.{Path, RelPath}

object MainTest:
  private type ImperativeResource[Payload] = Resource[IO, Payload]

  private val masterBranch = "master"

  private val arthur = RelPath("pathPrefix1") / "arthur.txt"

  private val sandra = RelPath("pathPrefix1") / "pathPrefix2" / "sandra.txt"

  private val tyson = RelPath("pathPrefix1") / "pathPrefix2" / "tyson.txt"

  private val arthurFirstVariation  = "chap"
  private val arthurSecondVariation = "boy"

  private val tysonResponse       = "Alright marra?"
  private val evilTysonExultation = "Ha, ha, ha, ha, hah!"

  private val optionalSubdirectories: Trials[Option[RelPath]] =
    trialsApi.only("runMergeInHere").map(RelPath.apply).options

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

  private def tysonSaidConflictingThings(path: Path): Unit =
    val tysonSaid = os.read(path / tyson)

    assert(
      tysonSaid.contains(tysonResponse) && tysonSaid
        .contains(evilTysonExultation)
    )
  end tysonSaidConflictingThings

  private def arthurIsMarkedWithConflictingDeletionAndUpdateInTheIndex(
      flipBranches: Boolean,
      status: String
  ): Unit =
    assert(
      s"${if flipBranches then "DU" else "UD"}\\s+$arthur".r
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

  private def currentStatus(path: Path) =
    os.proc(s"git", "status", "--short").call(path).out.text().strip

  private def currentBranch(path: Path) =
    os.proc("git", "branch", "--show-current").call(path).out.text().strip()

  private def currentCommit(path: Path) =
    os.proc("git", "log", "-1", "--format=tformat:%H")
      .call(path)
      .out
      .text()
      .strip

  private def mergeHead(path: Path) =
    os.read(mergeHeadPath(path)).strip()

  private def mergeHeadPath(path: Path) =
    path / ".git" / "MERGE_HEAD"

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
                  CommandLineArguments(
                    theirBranchHead =
                      theirBranch.taggedWith[Tags.CommitOrBranchName],
                    noCommit = noCommit,
                    noFastForward = noFastForward
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
                CommandLineArguments(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit
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
                CommandLineArguments(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit
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
                CommandLineArguments(theirBranchHead =
                  theirBranch.taggedWith[Tags.CommitOrBranchName]
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
  def conflictingModificationAndDeletionOfTheSameFile(): DynamicTests =
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

              if flipBranches then checkoutBranch(path)(deletedFileBranch)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then deletedFileBranch -> masterBranch
                else masterBranch                      -> deletedFileBranch

              val exitCode = Main.mergeTheirBranch(
                CommandLineArguments(theirBranchHead =
                  theirBranch.taggedWith[Tags.CommitOrBranchName]
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

              arthurIsMarkedWithConflictingDeletionAndUpdateInTheIndex(
                flipBranches,
                status
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
  end conflictingModificationAndDeletionOfTheSameFile

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
                CommandLineArguments(theirBranchHead =
                  theirBranch.taggedWith[Tags.CommitOrBranchName]
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
                CommandLineArguments(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit
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
                CommandLineArguments(
                  theirBranchHead =
                    theirBranch.taggedWith[Tags.CommitOrBranchName],
                  noCommit = noCommit
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
end MainTest
