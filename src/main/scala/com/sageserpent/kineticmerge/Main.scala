package com.sageserpent.kineticmerge

import cats.syntax.traverse.toTraverseOps

import java.nio.file.Path
import scala.language.postfixOps
import scala.sys.process.*
import scala.util.Try

object Main:
  private type Mode = String

  private type BlobId = String

  private type Content = String

  private type CommitIdOrBranchName = String

  private type Label = String

  private val whitespaceRun = "\\s+"

  private val ourBranchHead: CommitIdOrBranchName = "HEAD"

  private val fakeModeForDeletion: Mode = "0"

  private val fakeBlobIdForDeletion: BlobId =
    "0000000000000000000000000000000000000000"

  private val hereDocumentEof = "EndOfInput"

  extension [Payload](fallible: Try[Payload])
    private def labelExceptionWith(label: Label): Either[Label, Payload] =
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

          _ <- Try {
            s"git rev-parse $theirBranchHead" !!
          }.labelExceptionWith(label =
            s"Can't determine a best ancestor commit between $ourBranchHead and $theirBranchHead."
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
            .map(_.toMap)

          theirChanges <- Try {
            s"git diff --no-renames --name-status $bestAncestorCommit $theirBranchHead".lazyLines.toList
          }.labelExceptionWith(label =
            s"Can't determine changes made on their branch $theirBranchHead since ancestor commit $bestAncestorCommit"
          ).flatMap(_.traverse(pathChangeFor(theirBranchHead)))
            .map(_.toMap)

          // NOTE: changes that belong only to our branch don't need to be
          // handled explicitly - they are already in the merge by default,
          // because we build the merge commit index from the point of view of
          // our branch.
          overallChangesInvolvingTheirs = theirChanges.foldLeft(
            List.empty[(Path, (Change, Option[Change]))]
          ) { case (partialResult, (path, theirChange)) =>
            (path, (theirChange, ourChanges.get(path))) :: partialResult
          }

          indexUpdates <- overallChangesInvolvingTheirs.traverse {
            case (path, (Change.Addition(mode, blobId, _), None)) =>
              Try {
                s"git update-index --add --cacheinfo $mode,$blobId,$path" !!
              }.labelExceptionWith(
                s"Unexpected error: could not update index for their added file: $path"
              )
            case (path, (Change.Deletion, None)) =>
              Try {
                s"git update-index --index-info << $hereDocumentEof\n$fakeModeForDeletion $fakeBlobIdForDeletion\t$path\n$hereDocumentEof" !!
              }.labelExceptionWith(
                s"Unexpected error: could not update index for their deleted file: $path"
              )
          }
        yield ???

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
  )(line: String): Either[Label, (Path, Change)] =
    Try {
      line.split(whitespaceRun) match
        case Array("M", changedFile) =>
          val path = Path.of(changedFile)
          path -> blobAndContentFor(commitIdOrBranchName)(path).map(
            Change.Modification.apply.tupled
          )
        case Array("A", changedFile) =>
          val path = Path.of(changedFile)
          path -> blobAndContentFor(commitIdOrBranchName)(path).map(
            Change.Addition.apply.tupled
          )
        case Array("D", path) =>
          Path.of(path) -> Right(Change.Deletion)
    }.labelExceptionWith(label =
      s"Unexpected error - can't parse changes reported by Git: $line"
    ).flatMap { case (path, changed) => changed.map(path -> _) }

  private def blobAndContentFor(commitIdOrBranchName: CommitIdOrBranchName)(
      path: Path
  ): Either[Label, (Mode, BlobId, Content)] =
    Try {
      val line = s"git ls-tree $commitIdOrBranchName $path" !!

      line.split(whitespaceRun) match
        case Array(mode, _, blobId, _) =>
          val content = s"git cat-file blob $blobId" !!

          (mode, blobId, content)
      end match
    }.labelExceptionWith(label =
      s"Unexpected error - can't determine blob id for path: $path in commit or branch: $commitIdOrBranchName."
    )
  end blobAndContentFor

  private enum Change:
    case Modification(mode: Mode, blobId: BlobId, content: Content)
    case Addition(mode: Mode, blobId: BlobId, content: Content)
    case Deletion
  end Change

end Main
