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

import java.io.{ByteArrayInputStream, File, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.util.Comparator
import scala.language.postfixOps
import scala.sys.process.Process

object MainTest:
  private type ImperativeResource[Payload] = Resource[IO, Payload]

  private val masterBranch = "master"

  private val arthur = "arthur.txt"

  private val sandra = "sandra.txt"

  private val tyson = "tyson.txt"

  private val arthurFirstVariation  = "chap"
  private val arthurSecondVariation = "boy"

  private val tysonResponse       = "Alright marra?"
  private val evilTysonExultation = "Ha, ha, ha, ha, hah!"

  private val optionalSubdirectories: Trials[Option[Path]] =
    trialsApi.only("runMergeInHere").map(Path.of(_)).options

  private def introducingArthur(path: Path)(using
      ProcessBuilderFromCommandString
  ): Unit =
    Files.writeString(path.resolve(arthur), "Hello, my old mucker!\n")
    println(s"git add $arthur" !!)
    println(s"git commit -m 'Introducing Arthur.'" !!)
  end introducingArthur

  private def arthurContinues(path: Path)(using
      ProcessBuilderFromCommandString
  ): Unit =
    Files.writeString(
      path.resolve(arthur),
      s"Pleased to see you, old $arthurSecondVariation.\n",
      StandardOpenOption.APPEND
    )
    println(s"git commit -am 'Arthur continues...'" !!)
  end arthurContinues

  private def arthurElaborates(path: Path)(using
      ProcessBuilderFromCommandString
  ): Unit =
    Files.writeString(
      path.resolve(arthur),
      s"Pleased to see you, old $arthurFirstVariation.\n",
      StandardOpenOption.APPEND
    )
    println(s"git commit -am 'Arthur elaborates.'" !!)
  end arthurElaborates

  private def arthurCorrectHimself(path: Path)(using
      ProcessBuilderFromCommandString
  ): Unit =
    Files.writeString(
      path.resolve(arthur),
      "Hello, all and sundry!\n",
      StandardOpenOption.TRUNCATE_EXISTING
    )
    println(s"git commit -am 'Arthur corrects himself.'" !!)
  end arthurCorrectHimself

  private def exeuntArthur()(using ProcessBuilderFromCommandString): Unit =
    println(s"git rm $arthur" !!)
    println(s"git commit -m 'Exeunt Arthur.'" !!)
  end exeuntArthur

  private def arthurExcusesHimself()(using
      ProcessBuilderFromCommandString
  ): Unit =
    println(s"git rm $arthur" !!)
    println(s"git commit -m 'Arthur excuses himself.'" !!)
  end arthurExcusesHimself

  private def enterTysonStageLeft(path: Path)(using
      ProcessBuilderFromCommandString
  ): Unit =
    Files.writeString(path.resolve(tyson), s"$tysonResponse\n")
    println(s"git add $tyson" !!)
    println(s"git commit -m 'Tyson responds.'" !!)
  end enterTysonStageLeft

  private def evilTysonMakesDramaticEntranceExulting(path: Path)(using
      ProcessBuilderFromCommandString
  ): Unit =
    Files.writeString(path.resolve(tyson), s"$evilTysonExultation\n")
    println(s"git add $tyson" !!)
    println(s"git commit -m 'Evil Tyson exults.'" !!)
  end evilTysonMakesDramaticEntranceExulting

  private def sandraHeadsOffHome()(using
      ProcessBuilderFromCommandString
  ): Unit =
    println(s"git rm $sandra" !!)
    println(s"git commit -m 'Sandra heads off home.'" !!)
  end sandraHeadsOffHome

  private def sandraStopsByBriefly(path: Path)(using
      ProcessBuilderFromCommandString
  ): Unit =
    Files.writeString(
      path.resolve(sandra),
      "Hiya - just gan yam now...\n"
    )
    println(s"git add $sandra" !!)
    println(s"git commit -m 'Sandra stops by briefly...'" !!)
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
    val arthurSaid = Files.readString(path.resolve(arthur))

    assert(
      arthurSaid.contains(arthurFirstVariation) && arthurSaid
        .contains(arthurSecondVariation)
    )
  end arthurSaidConflictingThings

  private def tysonSaidConflictingThings(path: Path): Unit =
    val tysonSaid = Files.readString(path.resolve(tyson))

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
      commitOfAdvancedBranch: String,
      ourBranch: String,
      exitCode: Int @@ Main.Tags.ExitCode
  )(using ProcessBuilderFromCommandString): Unit =
    assert(exitCode == 0)

    val branchName = ("git branch --show-current" !!).strip()

    assert(branchName == ourBranch)

    val postMergeCommitOfAdvancedBranch =
      (s"git log -1 --format=tformat:%H" !!).strip

    assert(postMergeCommitOfAdvancedBranch == commitOfAdvancedBranch)

    val status = (s"git status --short" !!).strip

    assert(status.isEmpty)
  end verifyTrivialMergeMovesToTheMostAdvancedCommitWithACleanIndex

  private def verifyMergeMakesANewCommitWithACleanIndex(
      commitOfOneBranch: String,
      commitOfTheOtherBranch: String,
      ourBranch: String,
      exitCode: Int @@ Main.Tags.ExitCode
  )(using ProcessBuilderFromCommandString): Unit =
    assert(exitCode == 0)

    val branchName = ("git branch --show-current" !!).strip()

    assert(branchName == ourBranch)

    val Array(postMergeCommit, parents*) =
      (s"git log -1 --format='tformat:%H %P'" !!).strip
      .split("\\s+"): @unchecked

    assert(parents.size == 2)

    assert(postMergeCommit != commitOfOneBranch)
    assert(postMergeCommit != commitOfTheOtherBranch)

    val commitOfOneBranchIsAncestor =
      (s"git merge-base --is-ancestor $commitOfOneBranch $postMergeCommit" !) == 0

    assert(commitOfOneBranchIsAncestor)

    val commitOfTheOtherBranchIsAncestor =
      (s"git merge-base --is-ancestor $commitOfTheOtherBranch $postMergeCommit" !) == 0

    assert(commitOfTheOtherBranchIsAncestor)

    val status = (s"git status --short" !!).strip

    assert(status.isEmpty)
  end verifyMergeMakesANewCommitWithACleanIndex

  private def verifyATrivialNoFastForwardNoChangesMergeDoesNotMakeACommit(
      path: Path
  )(
      commitOfAdvancedBranch: String,
      ourBranch: String,
      exitCode: Int @@ Main.Tags.ExitCode
  )(using ProcessBuilderFromCommandString): Unit =
    assert(exitCode == 0)

    val branchName = ("git branch --show-current" !!).strip()

    assert(branchName == ourBranch)

    val postMergeCommit =
      (s"git log -1 --format=tformat:%H" !!).strip

    assert(
      postMergeCommit == commitOfAdvancedBranch
    )

    assert(!Files.exists(mergeHeadPath(path)))

    val status = (s"git status --short" !!).strip

    assert(status.isEmpty)
  end verifyATrivialNoFastForwardNoChangesMergeDoesNotMakeACommit

  private def verifyATrivialNoFastForwardNoCommitMergeDoesNotMakeACommit(
      path: Path
  )(
      commitOfAdvancedBranch: String,
      commitOfRetardedBranch: String,
      ourBranch: String,
      exitCode: Int @@ Main.Tags.ExitCode
  )(using ProcessBuilderFromCommandString): Unit =
    assert(exitCode == 1)

    val branchName = ("git branch --show-current" !!).strip()

    assert(branchName == ourBranch)

    val postMergeCommit =
      (s"git log -1 --format=tformat:%H" !!).strip

    assert(
      postMergeCommit == commitOfRetardedBranch
    )

    assert(
      mergeHead(path) == commitOfAdvancedBranch
    )

    val status = (s"git status --short" !!).strip

    assert(status.nonEmpty)
  end verifyATrivialNoFastForwardNoCommitMergeDoesNotMakeACommit

  private def mergeHead(path: Path) =
    Files.readString(mergeHeadPath(path)).strip()

  private def mergeHeadPath(path: Path) =
    path.resolve(".git").resolve("MERGE_HEAD")

  private def verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex(
      path: Path
  )(
      flipBranches: Boolean,
      commitOfNonMasterFileBranch: String,
      commitOfMasterBranch: String,
      ourBranch: String,
      exitCode: Int @@ Main.Tags.ExitCode
  )(using ProcessBuilderFromCommandString): String =
    assert(exitCode == 1)

    val branchName = ("git branch --show-current" !!).strip()

    assert(branchName == ourBranch)

    val postMergeCommit =
      (s"git log -1 --format=tformat:%H" !!).strip

    assert(
      postMergeCommit == (if flipBranches then commitOfNonMasterFileBranch
                          else commitOfMasterBranch)
    )

    assert(
      mergeHead(path) == (if flipBranches then commitOfMasterBranch
                          else commitOfNonMasterFileBranch)
    )

    val status = (s"git status --short" !!).strip

    assert(status.nonEmpty)

    status
  end verifyAConflictedOrNoCommitMergeDoesNotMakeACommitAndLeavesADirtyIndex

  private def gitRepository(): ImperativeResource[Path] =
    for
      temporaryDirectory <- Resource.make(IO {
        Files.createTempDirectory("toyGitRepository")
      })(temporaryDirectory =>
        IO {
          Files
            .walkFileTree(
              temporaryDirectory,
              new FileVisitor[Path]:
                override def preVisitDirectory(
                    dir: Path,
                    attrs: BasicFileAttributes
                ): FileVisitResult = FileVisitResult.CONTINUE

                override def visitFile(
                    path: Path,
                    attrs: BasicFileAttributes
                ): FileVisitResult =
                  Files.delete(path)
                  FileVisitResult.CONTINUE
                end visitFile

                override def visitFileFailed(
                    file: Path,
                    exc: IOException
                ): FileVisitResult = FileVisitResult.CONTINUE

                override def postVisitDirectory(
                    path: Path,
                    exc: IOException
                ): FileVisitResult =
                  Files.delete(path)
                  FileVisitResult.CONTINUE
                end postVisitDirectory
            )
        }
      )

      given ProcessBuilderFromCommandString =
        processBuilderFromCommandStringUsing(
          temporaryDirectory
        )

      _ <- Resource.eval(IO { "git init" !! })
      _ <- Resource.eval(IO { s"git checkout -b $masterBranch" !! })
    yield temporaryDirectory
    end for
  end gitRepository

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
                optionalSubdirectory.foreach(subdirectory =>
                  Files.createDirectory(path.resolve(subdirectory))
                )

                given ProcessBuilderFromCommandString =
                  processBuilderFromCommandStringUsing(path)

                introducingArthur(path)

                val commitOfMasterBranch =
                  (s"git log -1 --format=tformat:%H" !!).strip

                val advancedBranch = "advancedBranch"

                println(s"git checkout -b $advancedBranch" !!)

                arthurContinues(path)

                val commitOfAdvancedBranch =
                  (s"git log -1 --format=tformat:%H" !!).strip

                if ourBranchIsBehindTheirs then
                  println(s"git checkout $masterBranch" !!)
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
                  optionalSubdirectory.fold(ifEmpty = path)(path.resolve)
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

                    println(s"git commit -m 'Completing merge.'" !!)

                    verifyMergeMakesANewCommitWithACleanIndex(
                      commitOfMasterBranch,
                      commitOfAdvancedBranch,
                      ourBranch,
                      exitCode =
                        0.taggedWith[Tags.ExitCode] // Placeholder as Kinetic Merge hasn't actually done the merge.
                    )
                  else
                    verifyMergeMakesANewCommitWithACleanIndex(
                      commitOfMasterBranch,
                      commitOfAdvancedBranch,
                      ourBranch,
                      exitCode
                    )
                else
                  verifyTrivialMergeMovesToTheMostAdvancedCommitWithACleanIndex(
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
              optionalSubdirectory.foreach(subdirectory =>
                Files.createDirectory(path.resolve(subdirectory))
              )

              given ProcessBuilderFromCommandString =
                processBuilderFromCommandStringUsing(path)

              introducingArthur(path)

              val newFileBranch = "newFileBranch"

              println(s"git checkout -b $newFileBranch" !!)

              enterTysonStageLeft(path)

              val commitOfNewFileBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              println(s"git checkout $masterBranch" !!)

              arthurContinues(path)

              val commitOfMasterBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              if flipBranches then println(s"git checkout $newFileBranch" !!)
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
                optionalSubdirectory.fold(ifEmpty = path)(path.resolve)
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
                verifyMergeMakesANewCommitWithACleanIndex(
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
              optionalSubdirectory.foreach(subdirectory =>
                Files.createDirectory(path.resolve(subdirectory))
              )

              given ProcessBuilderFromCommandString =
                processBuilderFromCommandStringUsing(path)

              introducingArthur(path)

              val deletedFileBranch = "deletedFileBranch"

              println(s"git checkout -b $deletedFileBranch" !!)

              exeuntArthur()

              val commitOfDeletedFileBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              println(s"git checkout $masterBranch" !!)

              enterTysonStageLeft(path)

              val commitOfMasterBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              if flipBranches then
                println(s"git checkout $deletedFileBranch" !!)
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
                optionalSubdirectory.fold(ifEmpty = path)(path.resolve)
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
                verifyMergeMakesANewCommitWithACleanIndex(
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
              optionalSubdirectory.foreach(subdirectory =>
                Files.createDirectory(path.resolve(subdirectory))
              )

              given ProcessBuilderFromCommandString =
                processBuilderFromCommandStringUsing(path)

              introducingArthur(path)

              sandraStopsByBriefly(path)

              val evilTwinBranch = "evilTwin"

              println(s"git checkout -b $evilTwinBranch" !!)

              evilTysonMakesDramaticEntranceExulting(path)

              val commitOfEvilTwinBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              println(s"git checkout $masterBranch" !!)

              sandraHeadsOffHome()

              enterTysonStageLeft(path)

              val commitOfMasterBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              if flipBranches then println(s"git checkout $evilTwinBranch" !!)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then evilTwinBranch -> masterBranch
                else masterBranch                   -> evilTwinBranch

              val exitCode = Main.mergeTheirBranch(
                CommandLineArguments(theirBranchHead =
                  theirBranch.taggedWith[Tags.CommitOrBranchName]
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path.resolve)
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
              optionalSubdirectory.foreach(subdirectory =>
                Files.createDirectory(path.resolve(subdirectory))
              )

              given ProcessBuilderFromCommandString =
                processBuilderFromCommandStringUsing(path)

              introducingArthur(path)

              sandraStopsByBriefly(path)

              val deletedFileBranch = "deletedFileBranch"

              println(s"git checkout -b $deletedFileBranch" !!)

              enterTysonStageLeft(path)

              exeuntArthur()

              val commitOfDeletedFileBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              println(s"git checkout $masterBranch" !!)

              sandraHeadsOffHome()

              arthurContinues(path)

              val commitOfMasterBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              if flipBranches then
                println(s"git checkout $deletedFileBranch" !!)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then deletedFileBranch -> masterBranch
                else masterBranch                      -> deletedFileBranch

              val exitCode = Main.mergeTheirBranch(
                CommandLineArguments(theirBranchHead =
                  theirBranch.taggedWith[Tags.CommitOrBranchName]
                )
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path.resolve)
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
              optionalSubdirectory.foreach(subdirectory =>
                Files.createDirectory(path.resolve(subdirectory))
              )

              given ProcessBuilderFromCommandString =
                processBuilderFromCommandStringUsing(path)

              introducingArthur(path)

              sandraStopsByBriefly(path)

              val concurrentlyModifiedFileBranch =
                "concurrentlyModifiedFileBranch"

              println(s"git checkout -b $concurrentlyModifiedFileBranch" !!)

              enterTysonStageLeft(path)

              arthurElaborates(path)

              val commitOfConcurrentlyModifiedFileBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              println(s"git checkout $masterBranch" !!)

              sandraHeadsOffHome()

              arthurContinues(path)

              val commitOfMasterBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              if flipBranches then
                println(s"git checkout $concurrentlyModifiedFileBranch" !!)
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
                optionalSubdirectory.fold(ifEmpty = path)(path.resolve)
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
              optionalSubdirectory.foreach(subdirectory =>
                Files.createDirectory(path.resolve(subdirectory))
              )

              given ProcessBuilderFromCommandString =
                processBuilderFromCommandStringUsing(path)

              introducingArthur(path)

              sandraStopsByBriefly(path)

              val concurrentlyDeletedFileBranch =
                "concurrentlyDeletedFileBranch"

              println(s"git checkout -b $concurrentlyDeletedFileBranch" !!)

              enterTysonStageLeft(path)

              exeuntArthur()

              val commitOfConcurrentlyDeletedFileBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              println(s"git checkout $masterBranch" !!)

              sandraHeadsOffHome()

              arthurContinues(path)

              arthurExcusesHimself()

              val commitOfMasterBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              if flipBranches then
                println(
                  s"git checkout $concurrentlyDeletedFileBranch" !!
                )
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
                optionalSubdirectory.fold(ifEmpty = path)(path.resolve)
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
                verifyMergeMakesANewCommitWithACleanIndex(
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
              optionalSubdirectory.foreach(subdirectory =>
                Files.createDirectory(path.resolve(subdirectory))
              )

              given ProcessBuilderFromCommandString =
                processBuilderFromCommandStringUsing(path)

              introducingArthur(path)

              sandraStopsByBriefly(path)

              val concurrentlyModifiedFileBranch =
                "concurrentlyModifiedFileBranch"

              println(s"git checkout -b $concurrentlyModifiedFileBranch" !!)

              enterTysonStageLeft(path)

              arthurCorrectHimself(path)

              val commitOfConcurrentlyModifiedFileBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              println(s"git checkout $masterBranch" !!)

              sandraHeadsOffHome()

              arthurContinues(path)

              val commitOfMasterBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              if flipBranches then
                println(s"git checkout $concurrentlyModifiedFileBranch" !!)
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
                optionalSubdirectory.fold(ifEmpty = path)(path.resolve)
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
                verifyMergeMakesANewCommitWithACleanIndex(
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
