package com.sageserpent.kineticmerge

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import com.sageserpent.kineticmerge.Main.ProcessBuilderFromCommandString
import com.sageserpent.kineticmerge.MainTest.{gitRepository, masterBranch, processFromCommandString}
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.softwaremill.tagging.*
import org.junit.jupiter.api.{BeforeEach, Test}

import java.io.{ByteArrayInputStream, File, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.util.Comparator
import scala.language.postfixOps
import scala.sys.process.{Process, stringToProcess}

object MainTest:
  private type ImperativeResource[Payload] = Resource[IO, Payload]

  private val masterBranch = "master"

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

      given ProcessBuilderFromCommandString = processFromCommandString(
        temporaryDirectory
      )

      _ <- Resource.eval(IO { "git init" !! })
      _ <- Resource.eval(IO { s"git checkout -b $masterBranch" !! })
    yield temporaryDirectory
    end for
  end gitRepository

  private def processFromCommandString(
      path: Path
  ): ProcessBuilderFromCommandString =
    (command: String) => Process(command, Some(path.toFile))
  end processFromCommandString

end MainTest

class MainTest:
  @Test
  def noOperationMerge(): Unit =
    gitRepository()
      .use(path =>
        IO {
          given ProcessBuilderFromCommandString =
            processFromCommandString(path)

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

          val commitOfAdvancedBranch = s"git log -1 --format=tformat:%H" !!

          val exitCode = Main.mergeTheirBranch(
            masterBranch.taggedWith[Main.Tags.CommitOrBranchName]
          )

          assert(exitCode == 0)

          val postMergeCommitOfAdvancedBranch =
            s"git log -1 --format=tformat:%H" !!

          assert(postMergeCommitOfAdvancedBranch == commitOfAdvancedBranch)
        }
      )
      .unsafeRunSync()
  end noOperationMerge

  @Test
  def fastForwardMerge(): Unit =
    gitRepository()
      .use(path =>
        IO {
          given ProcessBuilderFromCommandString = processFromCommandString(path)

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

          val commitOfAdvancedBranch = s"git log -1 --format=tformat:%H" !!

          println(s"git checkout $masterBranch" !!)

          val exitCode = Main.mergeTheirBranch(
            advancedBranch.taggedWith[Main.Tags.CommitOrBranchName]
          )

          assert(exitCode == 0)

          val commitOfMasterBranch = s"git log -1 --format=tformat:%H" !!

          assert(commitOfMasterBranch == commitOfAdvancedBranch)
        }
      )
      .unsafeRunSync()
  end fastForwardMerge
end MainTest
