package com.sageserpent.kineticmerge

import cats.syntax.traverse.toTraverseOps
import com.sageserpent.kineticmerge.Main.BlobId

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
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
            case (
                  path,
                  (
                    Change.Addition(_, _, _),
                    Some(Change.Deletion | Change.Modification(_, _, _))
                  )
                ) =>
              Left(
                s"Unexpected error: file: $path has been added on our branch $ourBranchHead and either deleted or modified on their branch $theirBranchHead."
              )

            case (
                  path,
                  (
                    Change.Deletion | Change.Modification(_, _, _),
                    Some(Change.Addition(_, _, _))
                  )
                ) =>
              Left(
                s"Unexpected error: file: $path has been either deleted or modified on our branch $ourBranchHead and added on their branch $theirBranchHead."
              )

            case (
                  path,
                  (Change.Modification(mode, blobId, _), Some(Change.Deletion))
                ) =>
              recordDeletionInIndex(ourBranchHead, path)
              recordConflictModificationInIndex(stageIndex = 3)(
                theirBranchHead,
                path,
                mode,
                blobId
              )

            case (
                  path,
                  (Change.Deletion, Some(Change.Modification(mode, blobId, _)))
                ) =>
              recordDeletionInIndex(theirBranchHead, path)
              recordConflictModificationInIndex(stageIndex = 2)(
                ourBranchHead,
                path,
                mode,
                blobId
              )

            case (path, (Change.Modification(mode, blobId, _), None)) =>
              recordModificationInIndex(theirBranchHead, path, mode, blobId)

            case (path, (Change.Addition(mode, blobId, _), None)) =>
              recordAdditionInIndex(theirBranchHead, path, mode, blobId)

            case (path, (Change.Deletion, None)) =>
              recordDeletionInIndex(theirBranchHead, path)

            case (
                  path,
                  (
                    Change.Addition(theirMode, theirBlobId, theirContent),
                    Some(Change.Addition(ourMode, ourBlobId, ourContent))
                  )
                ) =>
              // TODO - perform merge and update the index depending on whether
              // the merge was clean or conflicted.
              ???

            case (
                  path,
                  (
                    Change.Modification(theirMode, theirBlobId, theirContent),
                    Some(Change.Modification(ourMode, ourBlobId, ourContent))
                  )
                ) =>
              // TODO - perform merge and update the index depending on whether
              // the merge was clean or conflicted.
              ???

            case (_, (Change.Deletion, Some(Change.Deletion))) =>
              // We already have the deletion in our branch, so no need to
              // update the index. We do yield a result so that there is still a
              // merge commit if this is the only change, though - that should
              // *not* be a fast-forward merge.
              Right(())
          }

          _ <- Try {
            "git restore :/" !!
          } labelExceptionWith ("Unexpected error: could not synchronize the working directory tree to the Git index after merging.")
        yield indexUpdates

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

  private def recordConflictModificationInIndex(stageIndex: Int)(
      commitIdOrBranchName: CommitIdOrBranchName,
      path: Path,
      mode: Mode,
      blobId: BlobId
  ): Either[Label, String] =
    Try {
      (s"git update-index --index-info" #< {
        new ByteArrayInputStream(
          s"$mode $blobId $stageIndex\t$path"
            .getBytes(StandardCharsets.UTF_8)
        )
      }) !!
    }.labelExceptionWith(
      s"Unexpected error: could not update index for file: $path modified on $commitIdOrBranchName."
    )
  end recordConflictModificationInIndex

  private def recordModificationInIndex(
      commitIdOrBranchName: CommitIdOrBranchName,
      path: Path,
      mode: Mode,
      blobId: BlobId
  ): Either[Label, String] =
    Try {
      (s"git update-index --index-info" #< {
        new ByteArrayInputStream(
          s"$mode $blobId\t$path"
            .getBytes(StandardCharsets.UTF_8)
        )
      }) !!
    }.labelExceptionWith(
      s"Unexpected error: could not update index for file: $path modified on $commitIdOrBranchName."
    )

  private def recordAdditionInIndex(
      commitIdOrBranchName: CommitIdOrBranchName,
      path: Path,
      mode: Mode,
      blobId: BlobId
  ): Either[Label, String] =
    Try {
      s"git update-index --add --cacheinfo $mode,$blobId,$path" !!
    }.labelExceptionWith(
      s"Unexpected error: could not update index for file: $path added on: $commitIdOrBranchName."
    )

  private def recordDeletionInIndex(
      commitIdOrBranchName: CommitIdOrBranchName,
      path: Path
  ): Either[Label, String] =
    Try {
      (s"git update-index --index-info" #< {
        new ByteArrayInputStream(
          s"$fakeModeForDeletion $fakeBlobIdForDeletion\t$path"
            .getBytes(StandardCharsets.UTF_8)
        )
      }) !!
    }.labelExceptionWith(
      s"Unexpected error: could not update index for file: $path deleted on $commitIdOrBranchName."
    )

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
