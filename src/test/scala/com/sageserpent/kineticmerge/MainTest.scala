package com.sageserpent.kineticmerge

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import com.sageserpent.kineticmerge.MainTest.gitRepository
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import org.junit.jupiter.api.{BeforeEach, Test}

import java.io.{ByteArrayInputStream, File, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, FileVisitor, Files, Path}
import java.util.Comparator
import scala.language.postfixOps
import scala.sys.process.*

object MainTest:
  private type ImperativeResource[Payload] = Resource[IO, Payload]

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

      _ <- Resource.eval(IO { s"git init $temporaryDirectory" !! })
      _ <- Resource.eval(IO {
        Process(s"git checkout -b master", Some(temporaryDirectory.toFile)) !!
      })
    yield temporaryDirectory
  end gitRepository
end MainTest

class MainTest:
  @Test
  def tryOutTheResource(): Unit =
    gitRepository()
      .use(path =>
        IO {
          println(Process(s"pwd", Some(path.toFile)) !!)
          println(Process(s"git status", Some(path.toFile)) !!)
          Files
            .writeString(path.resolve("arthur.txt"), "Hello, my old mucker!\n")
          println(Process(s"git add arthur.txt", Some(path.toFile)) !!)
          println(Process(s"git diff --cached", Some(path.toFile)) !!)
          println(Process(s"git commit -m 'Messing'", Some(path.toFile)) !!)
          println(Process(s"git log", Some(path.toFile)) !!)
        }
      )
      .unsafeRunSync()
end MainTest
