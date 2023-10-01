package com.sageserpent.kineticmerge

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.{DynamicTests, *}
import com.sageserpent.kineticmerge.MainTest.{gitRepository, masterBranch, optionalSubdirectories}
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

  private val optionalSubdirectories: Trials[Option[Path]] =
    trialsApi.only("runMergeInHere").map(Path.of(_)).options

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
  def noOperationMerge(): DynamicTests =
    optionalSubdirectories
      .withLimit(2)
      .dynamicTests(optionalSubdirectory =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory.foreach(subdirectory =>
                Files.createDirectory(path.resolve(subdirectory))
              )

              given ProcessBuilderFromCommandString =
                processBuilderFromCommandStringUsing(path)

              val arthur = "arthur.txt"
              Files.writeString(path.resolve(arthur), "Hello, my old mucker!\n")
              println(s"git add $arthur" !!)
              println(s"git commit -m 'First commit.'" !!)

              val advancedBranch = "advancedBranch"

              println(s"git checkout -b $advancedBranch" !!)

              Files.writeString(
                path.resolve(arthur),
                "Pleased to see you, old boy.\n",
                StandardOpenOption.APPEND
              )
              println(s"git commit -am 'Second commit.'" !!)

              val commitOfAdvancedBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              val exitCode = Main.mergeTheirBranch(
                masterBranch.taggedWith[Main.Tags.CommitOrBranchName]
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path.resolve)
              )

              assert(exitCode == 0)

              val branchName = ("git branch --show-current" !!).strip()

              assert(branchName == advancedBranch)

              val postMergeCommitOfAdvancedBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              assert(postMergeCommitOfAdvancedBranch == commitOfAdvancedBranch)

              val status = (s"git status --short" !!).strip

              assert(status.isEmpty)
            }
          )
          .unsafeRunSync()
      )
  end noOperationMerge

  @TestFactory
  def fastForwardMerge(): DynamicTests =
    optionalSubdirectories
      .withLimit(2)
      .dynamicTests(optionalSubdirectory =>
        gitRepository()
          .use(path =>
            IO {
              optionalSubdirectory.foreach(subdirectory =>
                Files.createDirectory(path.resolve(subdirectory))
              )

              given ProcessBuilderFromCommandString =
                processBuilderFromCommandStringUsing(path)

              val arthur = "arthur.txt"
              Files.writeString(path.resolve(arthur), "Hello, my old mucker!\n")
              println(s"git add $arthur" !!)
              println(s"git commit -m 'First commit.'" !!)

              val advancedBranch = "advancedBranch"

              println(s"git checkout -b $advancedBranch" !!)

              Files.writeString(
                path.resolve(arthur),
                "Pleased to see you, old boy.\n",
                StandardOpenOption.APPEND
              )
              println(s"git commit -am 'Second commit.'" !!)

              val commitOfAdvancedBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              println(s"git checkout $masterBranch" !!)

              val exitCode = Main.mergeTheirBranch(
                advancedBranch.taggedWith[Main.Tags.CommitOrBranchName]
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path.resolve)
              )

              assert(exitCode == 0)

              val branchName = ("git branch --show-current" !!).strip()

              assert(branchName == masterBranch)

              val postMergeCommitOfMasterBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              assert(postMergeCommitOfMasterBranch == commitOfAdvancedBranch)

              val status = (s"git status --short" !!).strip

              assert(status.isEmpty)
            }
          )
          .unsafeRunSync()
      )
  end fastForwardMerge

  @TestFactory
  def cleanMergeBringingInANewFile(): DynamicTests =
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

              val arthur = "arthur.txt"
              Files.writeString(path.resolve(arthur), "Hello, my old mucker!\n")
              println(s"git add $arthur" !!)
              println(s"git commit -m 'Introducing Arthur.'" !!)

              val newFileBranch = "newFileBranch"

              println(s"git checkout -b $newFileBranch" !!)

              val tyson = "tyson.txt"
              Files.writeString(path.resolve(tyson), "Alright marra!\n")
              println(s"git add $tyson" !!)
              println(s"git commit -m 'Tyson responds.'" !!)

              val commitOfNewFileBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              println(s"git checkout $masterBranch" !!)

              Files.writeString(
                path.resolve(arthur),
                "Pleased to see you, old boy.\n",
                StandardOpenOption.APPEND
              )
              println(s"git commit -am 'Arthur continues...'" !!)

              val commitOfMasterBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              if flipBranches then println(s"git checkout $newFileBranch" !!)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then newFileBranch -> masterBranch
                else masterBranch                  -> newFileBranch

              val exitCode = Main.mergeTheirBranch(
                theirBranch.taggedWith[Main.Tags.CommitOrBranchName]
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path.resolve)
              )

              assert(exitCode == 0)

              val branchName = ("git branch --show-current" !!).strip()

              assert(branchName == ourBranch)

              val postMergeCommit =
                (s"git log -1 --format=tformat:%H" !!).strip

              assert(postMergeCommit != commitOfMasterBranch)
              assert(postMergeCommit != commitOfNewFileBranch)

              val commitOfMasterBranchIsAncestor =
                (s"git merge-base --is-ancestor $commitOfMasterBranch $postMergeCommit" !) == 0

              assert(commitOfMasterBranchIsAncestor)

              val commitOfNewFileBranchIsAncestor =
                (s"git merge-base --is-ancestor $commitOfNewFileBranch $postMergeCommit" !) == 0

              assert(commitOfNewFileBranchIsAncestor)

              val status = (s"git status --short" !!).strip

              assert(status.isEmpty)
            }
          )
          .unsafeRunSync()
      }
  end cleanMergeBringingInANewFile

  @TestFactory
  def cleanMergeDeletingAFile(): DynamicTests =
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

              val arthur = "arthur.txt"
              Files.writeString(path.resolve(arthur), "Hello, my old mucker!\n")
              println(s"git add $arthur" !!)
              println(s"git commit -m 'Introducing Arthur.'" !!)

              val deletedFileBranch = "deletedFileBranch"

              println(s"git checkout -b $deletedFileBranch" !!)

              println(s"git rm $arthur" !!)
              println(s"git commit -m 'Exeunt Arthur.'" !!)

              val commitOfDeletedFileBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              println(s"git checkout $masterBranch" !!)

              val tyson = "tyson.txt"
              Files.writeString(path.resolve(tyson), "Alright marra!\n")
              println(s"git add $tyson" !!)
              println(s"git commit -m 'Tyson responds.'" !!)

              val commitOfMasterBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              if flipBranches then
                println(s"git checkout $deletedFileBranch" !!)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then deletedFileBranch -> masterBranch
                else masterBranch                      -> deletedFileBranch

              val exitCode = Main.mergeTheirBranch(
                theirBranch.taggedWith[Main.Tags.CommitOrBranchName]
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path.resolve)
              )

              assert(exitCode == 0)

              val branchName = ("git branch --show-current" !!).strip()

              assert(branchName == ourBranch)

              val postMergeCommit =
                (s"git log -1 --format=tformat:%H" !!).strip

              assert(postMergeCommit != commitOfMasterBranch)
              assert(postMergeCommit != commitOfDeletedFileBranch)

              val commitOfMasterBranchIsAncestor =
                (s"git merge-base --is-ancestor $commitOfMasterBranch $postMergeCommit" !) == 0

              assert(commitOfMasterBranchIsAncestor)

              val commitOfNewFileBranchIsAncestor =
                (s"git merge-base --is-ancestor $commitOfDeletedFileBranch $postMergeCommit" !) == 0

              assert(commitOfNewFileBranchIsAncestor)

              val status = (s"git status --short" !!).strip

              assert(status.isEmpty)
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

              val arthur = "arthur.txt"
              Files.writeString(path.resolve(arthur), "Hello, my old mucker!\n")
              println(s"git add $arthur" !!)
              println(s"git commit -m 'Introducing Arthur.'" !!)

              val evilTwinBranch = "evilTwin"

              println(s"git checkout -b $evilTwinBranch" !!)

              val tyson = "tyson.txt"
              Files.writeString(path.resolve(tyson), "Ha, ha, ha, ha, hah!\n")
              println(s"git add $tyson" !!)
              println(s"git commit -m 'Evil Tyson exults.'" !!)

              val commitOfEvilTwinBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              println(s"git checkout $masterBranch" !!)

              Files.writeString(path.resolve(tyson), "Alright marra!\n")
              println(s"git add $tyson" !!)
              println(s"git commit -m 'Good Tyson responds.'" !!)

              val commitOfMasterBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              if flipBranches then println(s"git checkout $evilTwinBranch" !!)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then evilTwinBranch -> masterBranch
                else masterBranch                   -> evilTwinBranch

              val exitCode = Main.mergeTheirBranch(
                theirBranch.taggedWith[Main.Tags.CommitOrBranchName]
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path.resolve)
              )

              assert(exitCode == 1)

              val branchName = ("git branch --show-current" !!).strip()

              assert(branchName == ourBranch)

              val postMergeCommit =
                (s"git log -1 --format=tformat:%H" !!).strip

              assert(
                postMergeCommit == (if flipBranches then commitOfEvilTwinBranch
                                    else commitOfMasterBranch)
              )

              val status = (s"git status --short" !!).strip

              assert(s"AA\\s+$tyson".r.findFirstIn(status).isDefined)
              assert(!status.contains(arthur))
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

              val arthur = "arthur.txt"
              Files.writeString(path.resolve(arthur), "Hello, my old mucker!\n")
              println(s"git add $arthur" !!)
              println(s"git commit -m 'Introducing Arthur.'" !!)

              val deletedFileBranch = "deletedFileBranch"

              println(s"git checkout -b $deletedFileBranch" !!)

              val tyson = "tyson.txt"
              Files.writeString(path.resolve(tyson), "Alright marra!\n")
              println(s"git add $tyson" !!)
              println(s"git commit -m 'Tyson responds.'" !!)

              println(s"git rm $arthur" !!)
              println(s"git commit -m 'Exeunt Arthur.'" !!)

              val commitOfDeletedFileBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              println(s"git checkout $masterBranch" !!)

              Files.writeString(
                path.resolve(arthur),
                "Pleased to see you, old boy.\n",
                StandardOpenOption.APPEND
              )
              println(s"git commit -am 'Arthur continues...'" !!)

              val commitOfMasterBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              if flipBranches then
                println(s"git checkout $deletedFileBranch" !!)
              end if

              val (ourBranch, theirBranch) =
                if flipBranches then deletedFileBranch -> masterBranch
                else masterBranch                      -> deletedFileBranch

              val exitCode = Main.mergeTheirBranch(
                theirBranch.taggedWith[Main.Tags.CommitOrBranchName]
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path.resolve)
              )

              assert(exitCode == 1)

              val branchName = ("git branch --show-current" !!).strip()

              assert(branchName == ourBranch)

              val postMergeCommit =
                (s"git log -1 --format=tformat:%H" !!).strip

              assert(
                postMergeCommit == (if flipBranches then
                                      commitOfDeletedFileBranch
                                    else commitOfMasterBranch)
              )

              val status = (s"git status --short" !!).strip

              assert(
                s"${if flipBranches then "DU" else "UD"}\\s+$arthur".r
                  .findFirstIn(status)
                  .isDefined
              )
              if flipBranches then assert(!status.contains(tyson))
              else assert(s"A\\s+$tyson.*".r.findFirstIn(status).isDefined)
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

              val arthur = "arthur.txt"
              Files.writeString(path.resolve(arthur), "Hello, my old mucker!\n")
              println(s"git add $arthur" !!)
              println(s"git commit -m 'Introducing Arthur.'" !!)

              val concurrentlyModifiedFileBranch =
                "concurrentlyModifiedFileBranch"

              println(s"git checkout -b $concurrentlyModifiedFileBranch" !!)

              val tyson = "tyson.txt"
              Files.writeString(path.resolve(tyson), "Alright marra!\n")
              println(s"git add $tyson" !!)
              println(s"git commit -m 'Tyson responds.'" !!)

              Files.writeString(
                path.resolve(arthur),
                "Pleased to see you, old chap.\n",
                StandardOpenOption.APPEND
              )
              println(s"git commit -am 'Arthur elaborates.'" !!)

              val commitOfConcurrentlyModifiedFileBranch =
                (s"git log -1 --format=tformat:%H" !!).strip

              println(s"git checkout $masterBranch" !!)

              Files.writeString(
                path.resolve(arthur),
                "Pleased to see you, old boy.\n",
                StandardOpenOption.APPEND
              )
              println(s"git commit -am 'Arthur continues...'" !!)

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
                theirBranch.taggedWith[Main.Tags.CommitOrBranchName]
              )(workingDirectory =
                optionalSubdirectory.fold(ifEmpty = path)(path.resolve)
              )

              assert(exitCode == 1)

              val branchName = ("git branch --show-current" !!).strip()

              assert(branchName == ourBranch)

              val postMergeCommit =
                (s"git log -1 --format=tformat:%H" !!).strip

              assert(
                postMergeCommit == (if flipBranches then
                                      commitOfConcurrentlyModifiedFileBranch
                                    else commitOfMasterBranch)
              )

              val status = (s"git status --short" !!).strip

              assert(
                s"UU\\s+$arthur".r.findFirstIn(status).isDefined
              )
              if flipBranches then assert(!status.contains(tyson))
              else assert(s"A\\s+$tyson.*".r.findFirstIn(status).isDefined)
              end if
            }
          )
          .unsafeRunSync()
      }
  end conflictingModificationOfTheSameFile
end MainTest
