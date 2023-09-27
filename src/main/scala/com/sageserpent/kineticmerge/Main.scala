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

  private type ExitCode = Int

  private type Workflow[Payload] = Either[Label, Payload]

  private val whitespaceRun = "\\s+"

  private val fakeModeForDeletion: Mode = "0"

  private val fakeBlobIdForDeletion: BlobId =
    "0000000000000000000000000000000000000000"

  private val successfulMerge: ExitCode      = 0
  private val conflictedMerge: ExitCode      = 1
  private val theirBranchIsMissing: ExitCode = 2
  private val tooManyArguments: ExitCode     = 3
  private val unexpectedError: ExitCode      = 4

  extension [Payload](fallible: Try[Payload])
    private def labelExceptionWith(label: Label): Workflow[Payload] =
      fallible.toEither.left.map(_ => label)
  end extension

  def main(args: Array[String]): Unit =
    // NOTE: the use of Git below is based on spike work on MacOS - the version
    // of Git shipped tends to be a *long* way behind the latest release, so the
    // latest and greatest versions of commands are not always available. At
    // time of writing, Mac OS Ventura 13.5.2 ships Git 2.24.3, contrast with
    // Git 2.42.0 being the latest stable release.

    val exitCode = args match
      case Array(theirBranchHead: CommitIdOrBranchName) =>
        val workflow = for
          _ <- Try { "git --version" !! }
            .labelExceptionWith(label = "Git is not available.")

          workingTree <- Try {
            ("git rev-parse --show-toplevel" !!).strip()
          }.labelExceptionWith(label =
            "The current working directory is not part of a Git working tree."
          )

          workingTreePath <- Try { Path.of(workingTree) }
            .labelExceptionWith(label =
              s"Unexpected error: working tree reported by Git: $workingTree is not a valid path."
            )

          ourBranchHead: CommitIdOrBranchName <- Try {
            val branchName = ("git branch --show-current" !!).strip()

            if branchName.nonEmpty then branchName
            else
              // Handle a detached commit.
              ("git rev-parse HEAD" !!).strip()
            end if
          }.labelExceptionWith(label =
            s"Can't determine a branch name or commit id for our branch head."
          )

          oursAlreadyContainsTheirs <- firstBranchIsContainedBySecond(
            theirBranchHead,
            ourBranchHead
          )

          theirsAlreadyContainsOurs <- firstBranchIsContainedBySecond(
            ourBranchHead,
            theirBranchHead
          )

          exitCode <-
            if oursAlreadyContainsTheirs then
              // Nothing to do, our branch has all their commits already.
              Right(successfulMerge)
            else if theirsAlreadyContainsOurs then
              // Fast-forward our branch to their head commit.
              Try { s"git reset --hard $theirBranchHead" !!; successfulMerge }
                .labelExceptionWith(label =
                  s"Unexpected error: could not fast-forward our branch $ourBranchHead to their branch: $theirBranchHead"
                )
            else // Perform a real merge...
              for
                theirCommitId <- Try {
                  s"git rev-parse $theirBranchHead" !!
                }.labelExceptionWith(label =
                  s"$theirBranchHead is not a valid branch or commit."
                )

                _ <- Try { s"git diff-index --exit-code $ourBranchHead" !! }
                  .labelExceptionWith(label =
                    "There are uncommitted changes prior to commencing the merge."
                  )

                bestAncestorCommitId <- Try {
                  (s"git merge-base $ourBranchHead $theirBranchHead" !!).strip()
                }.labelExceptionWith(label =
                  s"Can't determine a best ancestor commit between $ourBranchHead and $theirBranchHead."
                )

                ourChanges <- Try {
                  s"git diff --no-renames --name-status $bestAncestorCommitId $ourBranchHead".lazyLines.toList
                }.labelExceptionWith(label =
                  s"Can't determine changes made on our branch since ancestor commit $bestAncestorCommitId"
                ).flatMap(_.traverse(pathChangeFor(ourBranchHead)))
                  .map(_.toMap)

                theirChanges <- Try {
                  s"git diff --no-renames --name-status $bestAncestorCommitId $theirBranchHead".lazyLines.toList
                }.labelExceptionWith(label =
                  s"Can't determine changes made on their branch $theirBranchHead since ancestor commit $bestAncestorCommitId"
                ).flatMap(_.traverse(pathChangeFor(theirBranchHead)))
                  .map(_.toMap)

                // NOTE: changes that belong only to our branch don't need to be
                // handled explicitly - they are already in the merge by
                // default, because we build the merge commit index from the
                // point of view of our branch.
                overallChangesInvolvingTheirs = theirChanges.foldLeft(
                  List.empty[(Path, (Change, Option[Change]))]
                ) { case (partialResult, (path, theirChange)) =>
                  (path, (theirChange, ourChanges.get(path))) :: partialResult
                }

                exitCode <-
                  mergeWithRollback(
                    theirBranchHead,
                    ourBranchHead,
                    theirCommitId,
                    bestAncestorCommitId,
                    overallChangesInvolvingTheirs
                  )
              yield exitCode
        yield exitCode

        workflow.fold(
          label =>
            println(label)
            unexpectedError
          ,
          identity
        )
      case Array() =>
        Console.err.println("No branch or commit id provided to merge from.")
        theirBranchIsMissing
      case _ =>
        Console.err.println(
          s"Expected a single branch or commit id, but got multiple entries shown below:\n${args.mkString("\n")}"
        )
        tooManyArguments

    System.exit(exitCode)
  end main

  private def firstBranchIsContainedBySecond(
      firstBranchHead: Content,
      secondBranchHead: CommitIdOrBranchName
  ): Workflow[Boolean] =
    Try {
      s"git merge-base --is-ancestor $firstBranchHead $secondBranchHead" !
    }
      .labelExceptionWith(label =
        s"Unexpected error: could not determine whether $firstBranchHead is an ancestor of $secondBranchHead."
      )
      .map(0 == _)

  private def mergeWithRollback(
      theirBranchHead: Content,
      ourBranchHead: CommitIdOrBranchName,
      theirCommitId: String,
      bestAncestorCommitId: Content,
      overallChangesInvolvingTheirs: List[(Path, (Change, Option[Change]))]
  ): Workflow[ExitCode] =
    val workflow =
      for
        indexUpdates <- indexUpdates(
          bestAncestorCommitId,
          ourBranchHead,
          theirBranchHead
        )(overallChangesInvolvingTheirs)

        goodForAMergeCommit = indexUpdates.forall {
          case IndexState.OneEntry           => true
          case IndexState.ConflictingEntries => false
        }

        exitCodeWhenThereAreNoUnexpectedErrors <-
          if goodForAMergeCommit then
            for
              treeId <- Try {
                (s"git write-tree" !!).strip()
              }
                .labelExceptionWith(label =
                  s"Unexpected error: could not write a tree object from the index."
                )
              commitId <- Try {
                val message =
                  s"Merge from: $theirBranchHead into: $ourBranchHead."

                (s"git commit-tree -p $ourBranchHead -p $theirBranchHead -m '$message' $treeId" !!).strip()
              }.labelExceptionWith(label =
                s"Unexpected error: could not create a commit from tree object: $treeId"
              )
              _ <- Try {
                s"git reset --hard $commitId" !!
              }
                .labelExceptionWith(label =
                  s"Unexpected error: could not advance $ourBranchHead to commit: $commitId."
                )
            yield successfulMerge
          else
            for
              gitDir <- Try {
                ("git rev-parse --git-dir" !!).strip()
              }
                .labelExceptionWith(label =
                  "Could not determine location of `GIT_DIR`."
                )
              gitDirPath <- Try {
                Path.of(gitDir)
              }
                .labelExceptionWith(label =
                  s"Unexpected error: `GIT_DIR` reported by Git: $gitDir is not a valid path."
                )
              _ <- Try {
                s"echo $theirCommitId" #> gitDirPath
                  .resolve("MERGE_HEAD")
                  .toFile !!
              }.labelExceptionWith(label =
                s"Unexpected error: could not write `MERGE_HEAD` to reference $theirBranchHead."
              )
            yield conflictedMerge
          end if
      yield exitCodeWhenThereAreNoUnexpectedErrors

    // NASTY HACK: hokey cleanup, need to think about the best approach...
    workflow.left.map(label =>
      try "git reset --hard" !!
      catch
        case exception =>
          println(s"Failed to rollback changes after unexpected error.")
      end try

      label
    )
  end mergeWithRollback

  private def indexUpdates(
      bestAncestorCommitId: CommitIdOrBranchName,
      ourBranchHead: CommitIdOrBranchName,
      theirBranchHead: CommitIdOrBranchName
  )(
      overallChangesInvolvingTheirs: List[(Path, (Change, Option[Change]))]
  ): Workflow[List[IndexState]] =
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
          (_, bestAncestorCommitIdBlobId, _) <- blobAndContentFor(
            bestAncestorCommitId
          )(path)
          _ <- recordDeletionInIndex(path)
          _ <- recordConflictModificationInIndex(stageIndex = 1)(
            bestAncestorCommitId,
            path,
            mode,
            bestAncestorCommitIdBlobId
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
        yield IndexState.ConflictingEntries

      case (
            path,
            (
              Change.Deletion,
              Some(Change.Modification(mode, ourBlobId, _))
            )
          ) =>
        for
          (_, bestAncestorCommitIdBlobId, _) <- blobAndContentFor(
            bestAncestorCommitId
          )(path)
          - <- recordDeletionInIndex(path)
          - <- recordConflictModificationInIndex(stageIndex = 1)(
            bestAncestorCommitId,
            path,
            mode,
            bestAncestorCommitIdBlobId
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
        yield IndexState.ConflictingEntries

      case (path, (Change.Modification(mode, blobId, _), None)) =>
        for
          _ <- recordModificationInIndex(path, mode, blobId)
          - <- Try {
            s"git cat-file blob $blobId" #> path.toFile !!
          }
            .labelExceptionWith(label =
              s"Unexpected error: could not update working directory tree with modified file: $path."
            )
        yield IndexState.OneEntry

      case (path, (Change.Addition(mode, blobId, _), None)) =>
        for
          _ <- recordAdditionInIndex(path, mode, blobId)
          - <- Try {
            s"git cat-file blob $blobId" #> path.toFile !!
          }
            .labelExceptionWith(label =
              s"Unexpected error: could not update working directory tree with added file: $path."
            )
        yield IndexState.OneEntry

      case (path, (Change.Deletion, None)) =>
        for
          _ <- recordDeletionInIndex(path)
          _ <- Try {
            s"rm -rf $path" !!
          }
            .labelExceptionWith(label =
              s"Unexpected error: could not update working directory tree by deleting file: $path."
            )
        yield IndexState.OneEntry

      case (
            path,
            (
              Change.Addition(theirMode, theirBlobId, theirContent),
              Some(Change.Addition(ourMode, ourBlobId, ourContent))
            )
          ) =>
        indexStateForTwoWayMerge(
          ourBranchHead,
          theirBranchHead
        )(
          path,
          theirMode,
          theirContent,
          ourMode,
          ourContent
        )

      case (
            path,
            (
              Change.Modification(theirMode, theirBlobId, theirContent),
              Some(Change.Modification(ourMode, ourBlobId, ourContent))
            )
          ) =>
        indexStateForThreeWayMerge(
          bestAncestorCommitId,
          ourBranchHead,
          theirBranchHead
        )(
          path,
          theirMode,
          theirContent,
          ourMode,
          ourContent
        )

      case (_, (Change.Deletion, Some(Change.Deletion))) =>
        // We already have the deletion in our branch, so no need to
        // update the index. We do yield a result so that there is still a
        // merge commit if this is the only change, though - this should
        // *not* be a fast-forward merge.
        Right(IndexState.OneEntry)
    }

  private def indexStateForTwoWayMerge(
      ourBranchHead: CommitIdOrBranchName,
      theirBranchHead: CommitIdOrBranchName
  )(
      path: Path,
      theirMode: Mode,
      theirContent: Content,
      ourMode: Mode,
      ourContent: Content
  ) =
    for
      mergedFileMode <-
        if ourMode == theirMode then Right(ourMode)
        else
          Left(
            s"Conflicting file modes for file: $path; on our branch head: $ourMode and on their branch head: $theirMode."
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
        base = Vector.empty,
        left = ourAncestorTokens,
        right = theirAncestorTokens
      ).left.map(_.toString)

      indexState <- mergeResult match
        case Result.FullyMerged(elements) =>
          indexStateForCleanMerge(path, mergedFileMode, elements)

        case Result.MergedWithConflicts(leftElements, rightElements) =>
          val leftContent  = leftElements.map(_.text).mkString
          val rightContent = rightElements.map(_.text).mkString

          for
            fakeBaseTemporaryFile <- temporaryFile(
              suffix = ".base",
              content = ""
            )

            leftTemporaryFile <- temporaryFile(
              suffix = ".left",
              content = leftContent
            )

            rightTemporaryFile <- temporaryFile(
              suffix = ".right",
              content = rightContent
            )

            _ <-
              val noPriorContentName = "no prior content"

              val exitCode =
                s"git merge-file -L $ourBranchHead -L '$noPriorContentName' -L $theirBranchHead $leftTemporaryFile $fakeBaseTemporaryFile $rightTemporaryFile" !

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
            _         <- recordDeletionInIndex(path)
            _ <- recordConflictModificationInIndex(stageIndex = 2)(
              ourBranchHead,
              path,
              ourMode,
              leftBlob
            )
            _ <- recordConflictModificationInIndex(stageIndex = 3)(
              theirBranchHead,
              path,
              theirMode,
              rightBlob
            )
          yield IndexState.ConflictingEntries
          end for
    yield indexState

  private def indexStateForThreeWayMerge(
      bestAncestorCommitId: CommitIdOrBranchName,
      ourBranchHead: CommitIdOrBranchName,
      theirBranchHead: CommitIdOrBranchName
  )(
      path: Path,
      theirMode: Mode,
      theirContent: Content,
      ourMode: Mode,
      ourContent: Content
  ) =
    for
      (
        bestAncestorCommitIdMode,
        bestAncestorCommitIdBlobId,
        bestAncestorCommitIdContent
      ) <- blobAndContentFor(bestAncestorCommitId)(path)

      mergedFileMode <-
        if bestAncestorCommitIdMode == ourMode then Right(theirMode)
        else if bestAncestorCommitIdMode == theirMode then Right(ourMode)
        else if ourMode == theirMode then Right(ourMode)
        else
          Left(
            s"Conflicting file modes for file: $path; on base ancestor commit: $bestAncestorCommitIdMode, on our branch head: $ourMode and on their branch head: $theirMode."
          )

      bestAncestorTokens <- Try {
        Token.tokens(bestAncestorCommitIdContent).get
      }.labelExceptionWith(label =
        s"Failed to tokenize file: $path on best ancestor commit: $bestAncestorCommitId."
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

      indexState <- mergeResult match
        case Result.FullyMerged(elements) =>
          indexStateForCleanMerge(path, mergedFileMode, elements)

        case Result.MergedWithConflicts(leftElements, rightElements) =>
          val leftContent  = leftElements.map(_.text).mkString
          val rightContent = rightElements.map(_.text).mkString

          for
            baseTemporaryFile <- temporaryFile(
              suffix = ".base",
              content = bestAncestorCommitIdContent
            )

            leftTemporaryFile <- temporaryFile(
              suffix = ".left",
              content = leftContent
            )

            rightTemporaryFile <- temporaryFile(
              suffix = ".right",
              content = rightContent
            )

            _ <-
              val exitCode =
                s"git merge-file -L $ourBranchHead -L $bestAncestorCommitId -L $theirBranchHead $leftTemporaryFile $baseTemporaryFile $rightTemporaryFile" !

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
            _         <- recordDeletionInIndex(path)
            _ <- recordConflictModificationInIndex(stageIndex = 1)(
              bestAncestorCommitId,
              path,
              bestAncestorCommitIdMode,
              bestAncestorCommitIdBlobId
            )
            _ <- recordConflictModificationInIndex(stageIndex = 2)(
              ourBranchHead,
              path,
              ourMode,
              leftBlob
            )
            _ <- recordConflictModificationInIndex(stageIndex = 3)(
              theirBranchHead,
              path,
              theirMode,
              rightBlob
            )
          yield IndexState.ConflictingEntries
          end for
    yield indexState

  private def indexStateForCleanMerge(
      path: Path,
      mergedFileMode: Mode,
      elements: IndexedSeq[Token]
  ) =
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
    yield IndexState.OneEntry
    end for
  end indexStateForCleanMerge

  private def recordModificationInIndex(
      path: Path,
      mode: Mode,
      blobId: BlobId
  ): Workflow[Unit] =
    Try {
      val _ = (s"git update-index --index-info" #< {
        new ByteArrayInputStream(
          s"$mode $blobId\t$path"
            .getBytes(StandardCharsets.UTF_8)
        )
      }) !!
    }.labelExceptionWith(
      s"Unexpected error: could not update index for modified file: $path."
    )

  private def storeBlobFor(
      path: Path,
      content: Content
  ): Workflow[BlobId] =
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

  private def temporaryFile(
      suffix: String,
      content: Content
  ): Workflow[File] =
    for
      temporaryFile <- Try {
        Files.createTempFile("kinetic-merge-", ".base").toFile
      }.labelExceptionWith(
        s"Unexpected error: could not create temporary file."
      )
      _ <- Try {
        temporaryFile.deleteOnExit()
      }.labelExceptionWith(label =
        s"Unexpected error: could not register temporary file: $temporaryFile for deletion on exit."
      )
      - <- Try {
        Files.write(
          temporaryFile.toPath,
          content.getBytes(StandardCharsets.UTF_8)
        )
      }.labelExceptionWith(label =
        s"Unexpected error: could not write to temporary file: $temporaryFile."
      )
    yield temporaryFile

  private def recordConflictModificationInIndex(stageIndex: Int)(
      commitIdOrBranchName: CommitIdOrBranchName,
      path: Path,
      mode: Mode,
      blobId: BlobId
  ): Workflow[Unit] =
    Try {
      val _ = (s"git update-index --index-info" #< {
        new ByteArrayInputStream(
          s"$mode $blobId $stageIndex\t$path"
            .getBytes(StandardCharsets.UTF_8)
        )
      }) !!
    }.labelExceptionWith(
      s"Unexpected error: could not update conflict stage #$stageIndex index for modified file: $path from $commitIdOrBranchName."
    )
  end recordConflictModificationInIndex

  private def recordAdditionInIndex(
      path: Path,
      mode: Mode,
      blobId: BlobId
  ): Workflow[Unit] =
    Try {
      val _ = s"git update-index --add --cacheinfo $mode,$blobId,$path" !!
    }.labelExceptionWith(
      s"Unexpected error: could not update index for added file: $path."
    )

  private def recordDeletionInIndex(
      path: Path
  ): Workflow[Unit] =
    Try {
      val _ = (s"git update-index --index-info" #< {
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
  )(line: String): Workflow[(Path, Change)] =
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
  ): Workflow[(Mode, BlobId, Content)] =
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

  private enum IndexState:
    case OneEntry
    case ConflictingEntries
  end IndexState

end Main
