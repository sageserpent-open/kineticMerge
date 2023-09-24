package com.sageserpent.kineticmerge

import cats.syntax.traverse.toTraverseOps
import com.sageserpent.kineticmerge.Main.BlobId
import com.sageserpent.kineticmerge.core.merge.Result
import com.sageserpent.kineticmerge.core.{Token, mergeTokens}

import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.{CopyOption, Files, Path, StandardCopyOption}
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

          indexUpdates <- indexUpdates(
            bestAncestorCommit,
            ourBranchHead,
            theirBranchHead
          )(overallChangesInvolvingTheirs)
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

  private def indexUpdates(
      bestAncestorCommit: CommitIdOrBranchName,
      ourBranchHead: CommitIdOrBranchName,
      theirBranchHead: CommitIdOrBranchName
  )(
      overallChangesInvolvingTheirs: List[(Path, (Change, Option[Change]))]
  ): Either[Label, List[Any]] =
    overallChangesInvolvingTheirs.traverse {
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
            (
              Change.Modification(mode, theirBlobId, _),
              Some(Change.Deletion)
            )
          ) =>
        for
          (_, bestAncestorCommitBlobId, _) <- blobAndContentFor(
            bestAncestorCommit
          )(path)
          _ <- recordDeletionInIndex(path)
          _ <- recordConflictModificationInIndex(stageIndex = 1)(
            bestAncestorCommit,
            path,
            mode,
            bestAncestorCommitBlobId
          )
          - <- recordConflictModificationInIndex(stageIndex = 3)(
            theirBranchHead,
            path,
            mode,
            theirBlobId
          )
          _ <-
            // Git's merge updates the working directory tree with *their*
            // modified file which wouldn't have been present on our
            // branch prior to the merge. So that's what we do too.
            Try {
              s"git cat-file blob $theirBlobId" #> path.toFile !!
            }
              .labelExceptionWith(label =
                s"Unexpected error: could not update working directory tree with conflicted merge file: $path"
              )
        yield ()

      case (
            path,
            (
              Change.Deletion,
              Some(Change.Modification(mode, ourBlobId, _))
            )
          ) =>
        for
          (_, bestAncestorCommitBlobId, _) <- blobAndContentFor(
            bestAncestorCommit
          )(path)
          - <- recordDeletionInIndex(path)
          - <- recordConflictModificationInIndex(stageIndex = 1)(
            bestAncestorCommit,
            path,
            mode,
            bestAncestorCommitBlobId
          )
          - <- recordConflictModificationInIndex(stageIndex = 2)(
            ourBranchHead,
            path,
            mode,
            ourBlobId
          )
        // The modified file would have been present on our branch; given
        // that we started with a clean working directory tree, we just
        // leave it there to match what Git merge does.
        yield ()

      case (path, (Change.Modification(mode, blobId, _), None)) =>
        for
          _ <- recordModificationInIndex(path, mode, blobId)
          - <- Try {
            s"git cat-file blob $blobId" #> path.toFile !!
          }
            .labelExceptionWith(label =
              s"Unexpected error: could not update working directory tree with modified file: $path."
            )
        yield ()

      case (path, (Change.Addition(mode, blobId, _), None)) =>
        for
          _ <- recordAdditionInIndex(path, mode, blobId)
          - <- Try {
            s"git cat-file blob $blobId" #> path.toFile !!
          }
            .labelExceptionWith(label =
              s"Unexpected error: could not update working directory tree with added file: $path."
            )
        yield ()

      case (path, (Change.Deletion, None)) =>
        for
          _ <- recordDeletionInIndex(path)
          _ <- Try {
            s"rm -rf $path" !!
          }
            .labelExceptionWith(label =
              s"Unexpected error: could not update working directory tree by deleting file: $path."
            )
        yield ()

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
        for
          (
            bestAncestorCommitMode,
            bestAncestorCommitBlobId,
            bestAncestorCommitContent
          ) <-
            blobAndContentFor(bestAncestorCommit)(path)

          mergedFileMode <-
            if bestAncestorCommitMode == ourMode then Right(theirMode)
            else if bestAncestorCommitMode == theirMode then Right(ourMode)
            else if ourMode == theirMode then Right(ourMode)
            else
              Left(
                s"Conflicting file modes for file: $path; on base ancestor commit: $bestAncestorCommitMode, on our branch head: $ourMode and on their branch head: $theirMode."
              )

          bestAncestorTokens <- Try {
            Token.tokens(bestAncestorCommitContent).get
          }.labelExceptionWith(label =
            s"Failed to tokenize file: $path on best ancestor commit: $bestAncestorCommit."
          )

          ourAncestorTokens <- Try { Token.tokens(ourContent).get }
            .labelExceptionWith(label =
              s"Failed to tokenize file: $path on our branch head: $ourBranchHead."
            )

          theirAncestorTokens <- Try { Token.tokens(theirContent).get }
            .labelExceptionWith(label =
              s"Failed to tokenize file: $path on their branch head: $theirBranchHead."
            )

          mergeResult <- mergeTokens(
            base = bestAncestorTokens,
            left = ourAncestorTokens,
            right = theirAncestorTokens
          ).left.map(_.toString)

          _ <- mergeResult match
            case Result.FullyMerged(elements) =>
              val mergedContent = elements.map(_.text).mkString
              for
                mergedBlobId <- storeBlobFor(path, mergedContent)
                _ <- recordModificationInIndex(
                  path,
                  mergedFileMode,
                  mergedBlobId
                )
                - <- Try {
                  s"git cat-file blob $mergedBlobId" #> path.toFile !!
                }
                  .labelExceptionWith(label =
                    s"Unexpected error: could not update working directory tree with merged file: $path."
                  )
              yield ()
              end for

            case Result.MergedWithConflicts(leftElements, rightElements) =>
              val leftContent  = leftElements.map(_.text).mkString
              val rightContent = rightElements.map(_.text).mkString

              for
                baseTemporaryFile <- temporaryFile(suffix = ".base")
                - <- Try {
                  Files.write(
                    baseTemporaryFile.toPath,
                    bestAncestorCommitContent.getBytes(StandardCharsets.UTF_8)
                  )
                }
                  .labelExceptionWith(label =
                    s"Unexpected error: could not write to temporary file: $baseTemporaryFile."
                  )

                leftTemporaryFile <- temporaryFile(suffix = ".left")
                - <- Try {
                  Files.write(
                    leftTemporaryFile.toPath,
                    leftContent.getBytes(StandardCharsets.UTF_8)
                  )
                }
                  .labelExceptionWith(label =
                    s"Unexpected error: could not write to temporary file: $leftTemporaryFile."
                  )

                rightTemporaryFile <- temporaryFile(suffix = ".right")
                - <- Try {
                  Files.write(
                    rightTemporaryFile.toPath,
                    rightContent.getBytes(StandardCharsets.UTF_8)
                  )
                }
                  .labelExceptionWith(label =
                    s"Unexpected error: could not write to temporary file: $rightTemporaryFile."
                  )

                _ <-
                  val exitCode =
                    s"git merge-file -L $ourBranchHead -L $bestAncestorCommit -L $theirBranchHead $leftTemporaryFile $baseTemporaryFile $rightTemporaryFile" !

                  if 0 <= exitCode then Right(())
                  else
                    Left(
                      s"Unexpected error: could not generate conflicted file contents on behalf of: $path in temporary file: $leftTemporaryFile"
                    )
                  end if
                _ <- Try {
                  Files.copy(
                    leftTemporaryFile.toPath,
                    path,
                    StandardCopyOption.REPLACE_EXISTING
                  )
                }.labelExceptionWith(label =
                  s"Unexpected error: could not copy results of conflicted merge in: $leftTemporaryFile to working directory tree file: $path."
                )

                leftBlob  <- storeBlobFor(path, leftContent)
                rightBlob <- storeBlobFor(path, rightContent)
                _ <- recordConflictModificationInIndex(stageIndex = 1)(
                  bestAncestorCommit,
                  path,
                  bestAncestorCommitMode,
                  bestAncestorCommitBlobId
                )
                _ <- recordConflictModificationInIndex(stageIndex = 2)(
                  ourBranchHead,
                  path,
                  ourMode,
                  leftBlob
                )
                _ <- recordConflictModificationInIndex(stageIndex = 1)(
                  theirBranchHead,
                  path,
                  theirMode,
                  rightBlob
                )
              yield ()
              end for
        yield ()

      case (_, (Change.Deletion, Some(Change.Deletion))) =>
        // We already have the deletion in our branch, so no need to
        // update the index. We do yield a result so that there is still a
        // merge commit if this is the only change, though - this should
        // *not* be a fast-forward merge.
        Right(())
    }

  private def temporaryFile(suffix: Content): Either[Label, File] =
    for
      baseTemporaryFile <- Try {
        Files.createTempFile("kinetic-merge-", suffix).toFile
      }.labelExceptionWith(
        s"Unexpected error: could not create temporary file."
      )
      _ <- Try { baseTemporaryFile.deleteOnExit() }.labelExceptionWith(label =
        s"Unexpected error: could not register temporary file: $baseTemporaryFile for deletion on exit."
      )
    yield baseTemporaryFile

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
      s"Unexpected error: could not update conflict stage #$stageIndex index for modified file: $path from $commitIdOrBranchName."
    )
  end recordConflictModificationInIndex

  private def recordModificationInIndex(
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
      s"Unexpected error: could not update index for modified file: $path."
    )

  private def recordAdditionInIndex(
      path: Path,
      mode: Mode,
      blobId: BlobId
  ): Either[Label, String] =
    Try {
      s"git update-index --add --cacheinfo $mode,$blobId,$path" !!
    }.labelExceptionWith(
      s"Unexpected error: could not update index for added file: $path."
    )

  private def recordDeletionInIndex(
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
      s"Unexpected error: could not update index for deleted file: $path."
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
      end match
    }.labelExceptionWith(label =
      s"Unexpected error - can't parse changes reported by Git: $line."
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

  private def storeBlobFor(
      path: Path,
      content: Content
  ): Either[String, BlobId] =
    Try {
      val line = (s"git hash-object -t blob -w --stdin" #< {
        new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8))
      }) !!

      line.split(whitespaceRun) match
        case Array(blobId) => blobId
      end match
    }.labelExceptionWith(label =
      s"Unexpected error - could not create a blob for file: $path."
    )

  private enum Change:
    case Modification(mode: Mode, blobId: BlobId, content: Content)
    case Addition(mode: Mode, blobId: BlobId, content: Content)
    case Deletion
  end Change

end Main
