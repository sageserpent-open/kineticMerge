package com.sageserpent.kineticmerge

import cats.syntax.traverse.toTraverseOps

import java.nio.file.Path
import scala.language.postfixOps
import scala.sys.process.*
import scala.util.Try

object Main:
  private type BlobId = String

  private type CommitIdOrBranchName = String
  private val whitespaceRun = "\\s+"

  private val ourBranchHead = "HEAD"

  extension [Payload](fallible: Try[Payload])
    private def labelExceptionWith(label: String): Either[String, Payload] =
      fallible.toEither.left.map(_ => label)
  end extension

  def main(args: Array[String]): Unit =
    // NOTE: the use of Git below is based on spike work on MacOS - the version
    // of Git shipped tends to be a *long* way behind the latest release, so the
    // latest and greatest versions of commands are not always available. At
    // time of writing, Mac OS Ventura 13.5.2 ships Git 2.24.3, contrast with
    // Git 2.42.0 being the latest stable release.

    args match
      case Array(theirBranchHead: CommitIdOrBranchName) =>
        val workflow = for
          _ <- Try { "git --version" !! }
            .labelExceptionWith(label = "Git is not available.")

          workingTree <- Try {
            "git rev-parse --show-toplevel" !!
          }.labelExceptionWith(label =
            "The current working directory is not part of a Git working tree."
          )

          _ <- Try { s"git diff-index --exit-code $ourBranchHead" !! }
            .labelExceptionWith(label =
              "There are uncommitted changes prior to commencing the merge."
            )

          workingTreePath <- Try { Path.of(workingTree) }
            .labelExceptionWith(label =
              s"Unexpected error: working tree reported by Git: $workingTree is not a valid path."
            )

          bestAncestorCommit <- Try {
            s"git merge-base $ourBranchHead $theirBranchHead" !!
          }.labelExceptionWith(label =
            s"$theirBranchHead is not a valid branch or commit."
          )

          ourChanges <- Try {
            s"git diff --no-renames --name-status $bestAncestorCommit $ourBranchHead".lazyLines.toList
          }.labelExceptionWith(label =
            s"Can't determine changes made on our branch since ancestor commit $bestAncestorCommit"
          ).flatMap(_.traverse(pathChangeFor(ourBranchHead)))

          theirChanges <- Try {
            s"git diff --no-renames --name-status $bestAncestorCommit $theirBranchHead".lazyLines.toList
          }.labelExceptionWith(label =
            s"Can't determine changes made on their branch $theirBranchHead since ancestor commit $bestAncestorCommit"
          ).flatMap(_.traverse(pathChangeFor(theirBranchHead)))
        yield (ourChanges, theirChanges)

        workflow.fold(
          exception =>
            println(exception)
            System.exit(1)
          ,
          stuff =>
            println(stuff)
            0
        )
      case Array() =>
        Console.err.println("No branch or commit id provided to merge from.")
        System.exit(2)
      case _ =>
        Console.err.println(
          s"Expected a single branch or commit id, but got multiple entries shown below:\n${args.mkString("\n")}"
        )
        System.exit(3)

  private def pathChangeFor(
      commitIdOrBranchName: CommitIdOrBranchName
  )(line: String): Either[String, (Path, Changed)] =
    Try {
      line.split(whitespaceRun) match
        case Array("M", changedFile) =>
          val path = Path.of(changedFile)
          path -> blobAndContentFor(commitIdOrBranchName)(path).map(
            Changed.Modified.apply.tupled
          )
        case Array("A", changedFile) =>
          val path = Path.of(changedFile)
          path -> blobAndContentFor(commitIdOrBranchName)(path).map(
            Changed.Added.apply.tupled
          )
        case Array("D", path) =>
          Path.of(path) -> Right(Changed.Deleted)
    }.labelExceptionWith(label =
      s"Unexpected error - can't parse changes reported by Git: $line"
    ).flatMap { case (path, changed) => changed.map(path -> _) }

  private def blobAndContentFor(commitIdOrBranchName: CommitIdOrBranchName)(
      path: Path
  ): Either[String, (BlobId, String)] =
    Try {
      val line = s"git ls-tree $commitIdOrBranchName $path" !!

      line.split(whitespaceRun) match
        case Array(_, _, blobId, _) =>
          val content = s"git cat-file blob $blobId" !!

          blobId -> content
      end match
    }.labelExceptionWith(label =
      s"Unexpected error - can't determine blob id for path: $path in commit or branch: $commitIdOrBranchName."
    )
  end blobAndContentFor

  private enum Changed:
    case Modified(blobId: BlobId, content: String)
    case Added(blobId: BlobId, content: String)
    case Deleted
  end Changed

end Main
