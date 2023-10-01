package com.sageserpent.kineticmerge

import cats.data.{EitherT, Writer}
import cats.syntax.traverse.toTraverseOps
import com.sageserpent.kineticmerge.core.merge.Result
import com.sageserpent.kineticmerge.core.{Token, mergeTokens}
import com.softwaremill.tagging.*
import fansi.Str

import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.{CopyOption, Files, Path, StandardCopyOption}
import scala.language.postfixOps
import scala.sys.process.{Process, ProcessBuilder, stringToProcess}
import scala.util.Try

type ProcessBuilderFromCommandString = Conversion[String, ProcessBuilder]
def processBuilderFromCommandStringUsing(
    path: Path
): ProcessBuilderFromCommandString =
  (command: String) => Process(command, Some(path.toFile))
end processBuilderFromCommandStringUsing

object Main:
  private type WorkflowLog                = List[String]
  private type WorkflowLogWriter[Payload] = Writer[WorkflowLog, Payload]
  private type Workflow[Payload] =
    EitherT[WorkflowLogWriter, String @@ Tags.ErrorMessage, Payload]

  private val whitespaceRun = "\\s+"

  private val fakeModeForDeletion: String @@ Tags.Mode =
    "0".taggedWith[Tags.Mode]
  private val fakeBlobIdForDeletion: String @@ Tags.BlobId =
    "0000000000000000000000000000000000000000".taggedWith[Tags.BlobId]

  private val successfulMerge: Int @@ Tags.ExitCode =
    0.taggedWith[Tags.ExitCode]
  private val conflictedMerge: Int @@ Tags.ExitCode =
    1.taggedWith[Tags.ExitCode]
  private val theirBranchIsMissing: Int @@ Tags.ExitCode =
    2.taggedWith[Tags.ExitCode]
  private val tooManyArguments: Int @@ Tags.ExitCode =
    3.taggedWith[Tags.ExitCode]
  private val error: Int @@ Tags.ExitCode = 4.taggedWith[Tags.ExitCode]

  private val bestCommonAncestorStageIndex: Int @@ Tags.StageIndex =
    1.taggedWith[Tags.StageIndex]
  private val ourStageIndex: Int @@ Tags.StageIndex =
    2.taggedWith[Tags.StageIndex]
  private val theirStageIndex: Int @@ Tags.StageIndex =
    3.taggedWith[Tags.StageIndex]

  // Assume this can never fail, so no need to handle exceptions. Famous last
  // words...
  private val defaultWorkingDirectory = Path.of(".")

  def main(args: Array[String]): Unit =
    // NOTE: the use of Git below is based on spike work on MacOS - the version
    // of Git shipped tends to be a *long* way behind the latest release, so the
    // latest and greatest versions of commands are not always available. At
    // time of writing, Mac OS Ventura 13.5.2 ships Git 2.24.3, contrast with
    // Git 2.42.0 being the latest stable release.

    val exitCode = args match
      case Array(singleArgument) =>
        mergeTheirBranch(
          singleArgument.taggedWith[Tags.CommitOrBranchName]
        )(defaultWorkingDirectory)
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

  def mergeTheirBranch(
      theirBranchHead: String @@ Main.Tags.CommitOrBranchName
  )(workingDirectory: Path): Int @@ Main.Tags.ExitCode =
    // This is passed down the call chain to be used in all places where a
    // command is run using `ProcessBuilder` methods such as `!!`, `lines`. The
    // default runs in the current working directory, but tests can change this
    // to their own temporary Git repository.
    given ProcessBuilderFromCommandString =
      processBuilderFromCommandStringUsing(workingDirectory)

    val workflow = for
      _ <- Try {
        "git --version" !!
      }
        .labelExceptionWith(errorMessage = "Git is not available.")

      _ <- Try {
        ("git rev-parse --show-toplevel" !!).strip()
      }.labelExceptionWith(errorMessage =
        "The current working directory is not part of a Git working tree."
      )

      ourBranchHead <- Try {
        val branchName = ("git branch --show-current" !!).strip()
        .taggedWith[Tags.CommitOrBranchName]

        if branchName.nonEmpty then branchName
        else
          // Handle a detached commit.
          ("git rev-parse HEAD" !!).strip()
          .taggedWith[Tags.CommitOrBranchName]
        end if
      }.labelExceptionWith(errorMessage =
        s"Could not determine a branch name or commit id for our branch head."
      )

      theirCommitId <- Try {
        s"git rev-parse $theirBranchHead" !!
      }.labelExceptionWith(errorMessage =
        s"Ref ${underline(theirBranchHead)} is not a valid branch or commit."
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
          right(successfulMerge)
            .logOperation(
              s"Nothing to do - our branch ${underline(ourBranchHead)} already contains ${underline(theirBranchHead)}."
            )
        else if theirsAlreadyContainsOurs then
          // Fast-forward our branch to their head commit.
          Try {
            s"git reset --hard $theirBranchHead" !! : Unit
            successfulMerge
          }
            .labelExceptionWith(errorMessage =
              s"Unexpected error: could not fast-forward our branch ${underline(ourBranchHead)} to their branch ${underline(theirBranchHead)}."
            )
            .logOperation(
              s"Fast forward our branch ${underline(ourBranchHead)} to their branch ${underline(theirBranchHead)}."
            )
        else // Perform a real merge...
          for
            _ <- Try {
              s"git diff-index --exit-code $ourBranchHead" !!
            }
              .labelExceptionWith(errorMessage =
                "There are uncommitted changes prior to commencing the merge."
              )

            bestAncestorCommitId <- Try {
              (s"git merge-base $ourBranchHead $theirBranchHead" !!).strip()
              .taggedWith[Tags.CommitOrBranchName]
            }.labelExceptionWith(errorMessage =
              s"Could not determine a best ancestor commit between our branch ${underline(ourBranchHead)} and their branch ${underline(theirBranchHead)}."
            )

            ourChanges <- Try {
              s"git diff --no-renames --name-status $bestAncestorCommitId $ourBranchHead".lazyLines.toList
            }.labelExceptionWith(errorMessage =
              s"Could not determine changes made on our branch ${underline(ourBranchHead)} since ancestor commit ${underline(bestAncestorCommitId)}."
            ).flatMap(_.traverse(pathChangeFor(ourBranchHead)))
              .map(_.toMap)

            theirChanges <- Try {
              s"git diff --no-renames --name-status $bestAncestorCommitId $theirBranchHead".lazyLines.toList
            }.labelExceptionWith(errorMessage =
              s"Could not determine changes made on their branch ${underline(theirBranchHead)} since ancestor commit ${underline(bestAncestorCommitId)}."
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
                workingDirectory,
                theirBranchHead,
                ourBranchHead,
                theirCommitId,
                bestAncestorCommitId,
                overallChangesInvolvingTheirs
              )
          yield exitCode
    yield exitCode

    val (log, exitCode) = workflow
      .foldF(
        errorMessage =>
          Writer
            .value[WorkflowLog, Int @@ Tags.ExitCode](error)
            .tell(List(errorMessage)),
        Writer.value[WorkflowLog, Int @@ Tags.ExitCode]
      )
      .run

    log.foreach(println)

    exitCode
  end mergeTheirBranch

  extension [Payload](fallible: Try[Payload])
    private def labelExceptionWith(errorMessage: String): Workflow[Payload] =
      EitherT
        .fromEither[WorkflowLogWriter](fallible.toEither)
        .leftMap(_ => errorMessage.taggedWith[Tags.ErrorMessage])
  end extension

  extension [Payload](workflow: Workflow[Payload])
    private def logOperation(message: String): Workflow[Payload] =
      workflow.semiflatTap(_ => Writer.tell(List(message)))
  end extension

  private def firstBranchIsContainedBySecond(
      firstBranchHead: String @@ Tags.CommitOrBranchName,
      secondBranchHead: String @@ Tags.CommitOrBranchName
  )(using ProcessBuilderFromCommandString): Workflow[Boolean] =
    Try {
      s"git merge-base --is-ancestor $firstBranchHead $secondBranchHead" !
    }
      .labelExceptionWith(errorMessage =
        s"Unexpected error: could not determine whether branch ${underline(firstBranchHead)} is an ancestor of branch ${underline(secondBranchHead)}."
      )
      .map(0 == _)

  private def mergeWithRollback(
      workingDirectory: Path,
      theirBranchHead: String @@ Tags.CommitOrBranchName,
      ourBranchHead: String @@ Tags.CommitOrBranchName,
      theirCommitId: String,
      bestAncestorCommitId: String @@ Tags.CommitOrBranchName,
      overallChangesInvolvingTheirs: List[(Path, (Change, Option[Change]))]
  )(using ProcessBuilderFromCommandString): Workflow[Int @@ Tags.ExitCode] =
    val workflow =
      for
        indexUpdates <- indexUpdates(
          workingDirectory,
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
                .labelExceptionWith(errorMessage =
                  s"Unexpected error: could not write a tree object from the index."
                )
              commitId <- Try {
                val message =
                  s"Merge from ${underline(theirBranchHead)} into ${underline(ourBranchHead)}."

                (s"git commit-tree -p $ourBranchHead -p $theirBranchHead -m '$message' $treeId" !!).strip()
              }.labelExceptionWith(errorMessage =
                s"Unexpected error: could not create a commit from tree object ${underline(treeId)}"
              )
              _ <- Try {
                s"git reset --soft $commitId" !!
              }
                .labelExceptionWith(errorMessage =
                  s"Unexpected error: could not advance branch ${underline(ourBranchHead)} to commit ${underline(commitId)}."
                )
                .logOperation(
                  s"Successful merge, made a new commit ${underline(commitId)}"
                )
            yield successfulMerge
          else
            for
              gitDir <- Try {
                // TODO - write a test that exposes the need for an absolute git
                // as opposed to a merely relative git.
                ("git rev-parse --absolute-git-dir" !!).strip()
              }
                .labelExceptionWith(errorMessage =
                  "Could not determine location of `GIT_DIR`."
                )
              gitDirPath <- Try {
                Path.of(gitDir)
              }
                .labelExceptionWith(errorMessage =
                  s"Unexpected error: `GIT_DIR` reported by Git ${underline(gitDir)} is not a valid path."
                )
              _ <- Try {
                s"echo $theirCommitId" #> gitDirPath
                  .resolve("MERGE_HEAD")
                  .toFile !!
              }.labelExceptionWith(errorMessage =
                s"Unexpected error: could not write `MERGE_HEAD` to reference their branch ${underline(theirBranchHead)}."
              ).logOperation(
                "Merge conflicts found, handing over for manual resolution..."
              )
            yield conflictedMerge
          end if
      yield exitCodeWhenThereAreNoUnexpectedErrors

    // NASTY HACK: hokey cleanup, need to think about the best approach...
    workflow.leftMap(label =>
      try "git reset --hard" !!
      catch
        case exception =>
          println(s"Failed to rollback changes after unexpected error.")
      end try

      label
    )
  end mergeWithRollback

  private def indexUpdates(
      workingDirectory: Path,
      bestAncestorCommitId: String @@ Tags.CommitOrBranchName,
      ourBranchHead: String @@ Tags.CommitOrBranchName,
      theirBranchHead: String @@ Tags.CommitOrBranchName
  )(
      overallChangesInvolvingTheirs: List[(Path, (Change, Option[Change]))]
  )(using ProcessBuilderFromCommandString): Workflow[List[IndexState]] =
    overallChangesInvolvingTheirs.traverse {
      case (
            path,
            (
              Change.Addition(_, _, _),
              Some(Change.Deletion | Change.Modification(_, _, _))
            )
          ) =>
        left(
          s"Unexpected error: file ${underline(path)} has been added on our branch ${underline(ourBranchHead)} and either deleted or modified on their branch ${underline(theirBranchHead)}."
        )

      case (
            path,
            (
              Change.Deletion | Change.Modification(_, _, _),
              Some(Change.Addition(_, _, _))
            )
          ) =>
        left(
          s"Unexpected error: file ${underline(path)} has been either deleted or modified on our branch ${underline(ourBranchHead)} and added on their branch ${underline(theirBranchHead)}."
        )

      case (
            path,
            (
              Change.Modification(mode, theirBlobId, _),
              Some(Change.Deletion)
            )
          ) =>
        (for
          (_, bestAncestorCommitIdBlobId, _) <- blobAndContentFor(
            bestAncestorCommitId
          )(path)
          _ <- recordDeletionInIndex(path)
          _ <- recordConflictModificationInIndex(stageIndex =
            bestCommonAncestorStageIndex
          )(
            bestAncestorCommitId,
            path,
            mode,
            bestAncestorCommitIdBlobId
          )
          - <- recordConflictModificationInIndex(stageIndex = theirStageIndex)(
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
              s"git cat-file blob $theirBlobId" #> workingDirectory
                .resolve(path)
                .toFile !!
            }
              .labelExceptionWith(errorMessage =
                s"Unexpected error: could not update working directory tree with conflicted merge file ${underline(path)}"
              )
        yield IndexState.ConflictingEntries).logOperation(
          s"Conflict - file ${underline(path)} was deleted on our branch ${underline(ourBranchHead)} and modified on their branch ${underline(theirBranchHead)}."
        )

      case (
            path,
            (
              Change.Deletion,
              Some(Change.Modification(mode, ourBlobId, _))
            )
          ) =>
        (for
          (_, bestAncestorCommitIdBlobId, _) <- blobAndContentFor(
            bestAncestorCommitId
          )(path)
          - <- recordDeletionInIndex(path)
          - <- recordConflictModificationInIndex(stageIndex =
            bestCommonAncestorStageIndex
          )(
            bestAncestorCommitId,
            path,
            mode,
            bestAncestorCommitIdBlobId
          )
          - <- recordConflictModificationInIndex(stageIndex = ourStageIndex)(
            ourBranchHead,
            path,
            mode,
            ourBlobId
          )
        // The modified file would have been present on our branch; given
        // that we started with a clean working directory tree, we just
        // leave it there to match what Git merge does.
        yield IndexState.ConflictingEntries).logOperation(
          s"Conflict - file ${underline(path)} was modified on our branch ${underline(ourBranchHead)} and deleted on their branch ${underline(theirBranchHead)}."
        )

      case (path, (Change.Modification(mode, blobId, _), None)) =>
        for
          _ <- recordModificationInIndex(path, mode, blobId)
          - <- Try {
            s"git cat-file blob $blobId" #> workingDirectory
              .resolve(path)
              .toFile !!
          }
            .labelExceptionWith(errorMessage =
              s"Unexpected error: could not update working directory tree with modified file ${underline(path)}."
            )
        yield IndexState.OneEntry

      case (path, (Change.Addition(mode, blobId, _), None)) =>
        for
          _ <- recordAdditionInIndex(path, mode, blobId)
          - <- Try {
            s"git cat-file blob $blobId" #> workingDirectory
              .resolve(path)
              .toFile !!
          }
            .labelExceptionWith(errorMessage =
              s"Unexpected error: could not update working directory tree with added file ${underline(path)}."
            )
        yield IndexState.OneEntry

      case (path, (Change.Deletion, None)) =>
        for
          _ <- recordDeletionInIndex(path)
          _ <- Try {
            s"rm -rf $path" !!
          }
            .labelExceptionWith(errorMessage =
              s"Unexpected error: could not update working directory tree by deleting file ${underline(path)}."
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
          workingDirectory,
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
          workingDirectory,
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

      case (path, (Change.Deletion, Some(Change.Deletion))) =>
        // We already have the deletion in our branch, so no need to
        // update the index. We do yield a result so that there is still a
        // merge commit if this is the only change, though - this should
        // *not* be a fast-forward merge.
        right(IndexState.OneEntry)
          .logOperation(
            s"Coincidental deletion of file ${underline(path)} on our branch ${underline(ourBranchHead)} and on their branch ${underline(theirBranchHead)}."
          )
    }

  private def underline(anything: Any): Str =
    fansi.Underlined.On(anything.toString)

  private def left[Payload](errorMessage: String): Workflow[Payload] =
    EitherT.leftT[WorkflowLogWriter, Payload](
      errorMessage.taggedWith[Tags.ErrorMessage]
    )

  private def right[Payload](payload: Payload): Workflow[Payload] =
    EitherT.rightT[WorkflowLogWriter, String @@ Tags.ErrorMessage](payload)

  private def indexStateForTwoWayMerge(
      workingDirectory: Path,
      ourBranchHead: String @@ Tags.CommitOrBranchName,
      theirBranchHead: String @@ Tags.CommitOrBranchName
  )(
      path: Path,
      theirMode: String @@ Tags.Mode,
      theirContent: String @@ Tags.Content,
      ourMode: String @@ Tags.Mode,
      ourContent: String @@ Tags.Content
  )(using ProcessBuilderFromCommandString): Workflow[IndexState] =
    for
      mergedFileMode <-
        if ourMode == theirMode then right(ourMode)
        else
          left(
            s"Conflicting file modes for file ${underline(path)}; on our branch head ${underline(ourMode)} and on their branch head ${underline(theirMode)}."
          )

      ourAncestorTokens <- Try { Token.tokens(ourContent).get }
        .labelExceptionWith(errorMessage =
          s"Failed to tokenize file ${underline(path)} on our branch head ${underline(ourBranchHead)}."
        )

      theirAncestorTokens <- Try { Token.tokens(theirContent).get }
        .labelExceptionWith(errorMessage =
          s"Failed to tokenize file ${underline(path)} on their branch head ${underline(theirBranchHead)}."
        )

      mergeResult <- EitherT
        .fromEither[WorkflowLogWriter](
          mergeTokens(
            base = Vector.empty,
            left = ourAncestorTokens,
            right = theirAncestorTokens
          )
        )
        .leftMap(_.toString.taggedWith[Tags.ErrorMessage])

      indexState <- mergeResult match
        case Result.FullyMerged(elements) =>
          indexStateForCleanMerge(workingDirectory)(
            path,
            mergedFileMode,
            elements
          )

        case Result.MergedWithConflicts(leftElements, rightElements) =>
          val leftContent =
            leftElements.map(_.text).mkString.taggedWith[Tags.Content]
          val rightContent =
            rightElements.map(_.text).mkString.taggedWith[Tags.Content]

          (for
            fakeBaseTemporaryFile <- temporaryFile(
              suffix = ".base",
              content = "".taggedWith[Tags.Content]
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

              if 0 <= exitCode then right(())
              else
                left(
                  s"Unexpected error: could not generate conflicted file contents on behalf of ${underline(path)} in temporary file ${underline(leftTemporaryFile)}"
                )
              end if
            _ <- Try {
              Files.copy(
                leftTemporaryFile.toPath,
                path,
                StandardCopyOption.REPLACE_EXISTING
              )
            }.labelExceptionWith(errorMessage =
              s"Unexpected error: could not copy results of conflicted merge in ${underline(leftTemporaryFile)} to working directory tree file ${underline(path)}."
            )

            leftBlob  <- storeBlobFor(path, leftContent)
            rightBlob <- storeBlobFor(path, rightContent)
            _         <- recordDeletionInIndex(path)
            _ <- recordConflictModificationInIndex(stageIndex = ourStageIndex)(
              ourBranchHead,
              path,
              ourMode,
              leftBlob
            )
            _ <- recordConflictModificationInIndex(stageIndex =
              theirStageIndex
            )(
              theirBranchHead,
              path,
              theirMode,
              rightBlob
            )
          yield IndexState.ConflictingEntries).logOperation(
            s"Conflict - file ${underline(path)} was added on our branch ${underline(ourBranchHead)} and added on their branch ${underline(theirBranchHead)}."
          )
    yield indexState

  private def indexStateForThreeWayMerge(
      workingDirectory: Path,
      bestAncestorCommitId: String @@ Tags.CommitOrBranchName,
      ourBranchHead: String @@ Tags.CommitOrBranchName,
      theirBranchHead: String @@ Tags.CommitOrBranchName
  )(
      path: Path,
      theirMode: String @@ Tags.Mode,
      theirContent: String @@ Tags.Content,
      ourMode: String @@ Tags.Mode,
      ourContent: String @@ Tags.Content
  )(using ProcessBuilderFromCommandString): Workflow[IndexState] =
    for
      (
        bestAncestorCommitIdMode,
        bestAncestorCommitIdBlobId,
        bestAncestorCommitIdContent
      ) <- blobAndContentFor(bestAncestorCommitId)(path)

      mergedFileMode <-
        if bestAncestorCommitIdMode == ourMode then right(theirMode)
        else if bestAncestorCommitIdMode == theirMode then right(ourMode)
        else if ourMode == theirMode then right(ourMode)
        else
          left(
            s"Conflicting file modes for file ${underline(path)}; on base ancestor commit ${underline(bestAncestorCommitIdMode)}, on our branch head ${underline(ourMode)} and on their branch head ${underline(theirMode)}."
          )

      bestAncestorTokens <- Try {
        Token.tokens(bestAncestorCommitIdContent).get
      }.labelExceptionWith(errorMessage =
        s"Failed to tokenize file ${underline(path)} on best ancestor commit ${underline(bestAncestorCommitId)}."
      )

      ourAncestorTokens <- Try { Token.tokens(ourContent).get }
        .labelExceptionWith(errorMessage =
          s"Failed to tokenize file ${underline(path)} on our branch head ${underline(ourBranchHead)}."
        )

      theirAncestorTokens <- Try { Token.tokens(theirContent).get }
        .labelExceptionWith(errorMessage =
          s"Failed to tokenize file ${underline(path)} on their branch head ${underline(theirBranchHead)}."
        )

      mergeResult <- EitherT
        .fromEither[WorkflowLogWriter](
          mergeTokens(
            base = bestAncestorTokens,
            left = ourAncestorTokens,
            right = theirAncestorTokens
          )
        )
        .leftMap(_.toString.taggedWith[Tags.ErrorMessage])

      indexState <- mergeResult match
        case Result.FullyMerged(elements) =>
          indexStateForCleanMerge(workingDirectory)(
            path,
            mergedFileMode,
            elements
          )

        case Result.MergedWithConflicts(leftElements, rightElements) =>
          val leftContent =
            leftElements.map(_.text).mkString.taggedWith[Tags.Content]
          val rightContent =
            rightElements.map(_.text).mkString.taggedWith[Tags.Content]

          (for
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

              if 0 <= exitCode then right(())
              else
                left(
                  s"Unexpected error: could not generate conflicted file contents on behalf of ${underline(path)} in temporary file ${underline(leftTemporaryFile)}"
                )
              end if
            _ <- Try {
              Files.copy(
                leftTemporaryFile.toPath,
                path,
                StandardCopyOption.REPLACE_EXISTING
              )
            }.labelExceptionWith(errorMessage =
              s"Unexpected error: could not copy results of conflicted merge in ${underline(leftTemporaryFile)} to working directory tree file ${underline(path)}."
            )

            leftBlob  <- storeBlobFor(path, leftContent)
            rightBlob <- storeBlobFor(path, rightContent)
            _         <- recordDeletionInIndex(path)
            _ <- recordConflictModificationInIndex(stageIndex =
              bestCommonAncestorStageIndex
            )(
              bestAncestorCommitId,
              path,
              bestAncestorCommitIdMode,
              bestAncestorCommitIdBlobId
            )
            _ <- recordConflictModificationInIndex(stageIndex = ourStageIndex)(
              ourBranchHead,
              path,
              ourMode,
              leftBlob
            )
            _ <- recordConflictModificationInIndex(stageIndex =
              theirStageIndex
            )(
              theirBranchHead,
              path,
              theirMode,
              rightBlob
            )
          yield IndexState.ConflictingEntries).logOperation(
            s"Conflict - file ${underline(path)} was modified on our branch ${underline(ourBranchHead)} and modified on their branch ${underline(theirBranchHead)}."
          )
    yield indexState

  private def indexStateForCleanMerge(workingDirectory: Path)(
      path: Path,
      mergedFileMode: String @@ Tags.Mode,
      elements: IndexedSeq[Token]
  )(using ProcessBuilderFromCommandString): Workflow[IndexState] =
    val mergedContent =
      elements.map(_.text).mkString.taggedWith[Tags.Content]
    for
      mergedBlobId <- storeBlobFor(path, mergedContent)
      _ <- recordModificationInIndex(
        path,
        mergedFileMode,
        mergedBlobId
      )
      - <- Try {
        s"git cat-file blob $mergedBlobId" #> workingDirectory
          .resolve(path)
          .toFile !!
      }
        .labelExceptionWith(errorMessage =
          s"Unexpected error: could not update working directory tree with merged file ${underline(path)}."
        )
    yield IndexState.OneEntry
    end for
  end indexStateForCleanMerge

  private def recordModificationInIndex(
      path: Path,
      mode: String @@ Tags.Mode,
      blobId: String @@ Tags.BlobId
  )(using ProcessBuilderFromCommandString): Workflow[Unit] =
    Try {
      val _ = (s"git update-index --index-info" #< {
        new ByteArrayInputStream(
          s"$mode $blobId\t$path"
            .getBytes(StandardCharsets.UTF_8)
        )
      }) !!
    }.labelExceptionWith(
      s"Unexpected error: could not update index for modified file ${underline(path)}."
    )

  private def storeBlobFor(
      path: Path,
      content: String @@ Tags.Content
  )(using ProcessBuilderFromCommandString): Workflow[String @@ Tags.BlobId] =
    Try {
      val line = (s"git hash-object -t blob -w --stdin" #< {
        new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8))
      }) !!

      line.split(whitespaceRun) match
        case Array(blobId) => blobId.taggedWith[Tags.BlobId]
      end match
    }.labelExceptionWith(errorMessage =
      s"Unexpected error - could not create a blob for file ${underline(path)}."
    )

  private def temporaryFile(
      suffix: String,
      content: String @@ Tags.Content
  ): Workflow[File] =
    for
      temporaryFile <- Try {
        Files.createTempFile("kinetic-merge-", ".base").toFile
      }.labelExceptionWith(
        s"Unexpected error: could not create temporary file."
      )
      _ <- Try {
        temporaryFile.deleteOnExit()
      }.labelExceptionWith(errorMessage =
        s"Unexpected error: could not register temporary file ${underline(temporaryFile)} for deletion on exit."
      )
      - <- Try {
        Files.write(
          temporaryFile.toPath,
          content.getBytes(StandardCharsets.UTF_8)
        )
      }.labelExceptionWith(errorMessage =
        s"Unexpected error: could not write to temporary file ${underline(temporaryFile)}."
      )
    yield temporaryFile

  private def recordConflictModificationInIndex(
      stageIndex: Int @@ Tags.StageIndex
  )(
      commitIdOrBranchName: String @@ Tags.CommitOrBranchName,
      path: Path,
      mode: String @@ Tags.Mode,
      blobId: String @@ Tags.BlobId
  )(using ProcessBuilderFromCommandString): Workflow[Unit] =
    Try {
      val _ = (s"git update-index --index-info" #< {
        new ByteArrayInputStream(
          s"$mode $blobId $stageIndex\t$path"
            .getBytes(StandardCharsets.UTF_8)
        )
      }) !!
    }.labelExceptionWith(
      s"Unexpected error: could not update conflict stage #${underline(stageIndex)} index for modified file ${underline(path)} from commit or branch ${underline(commitIdOrBranchName)}."
    )
  end recordConflictModificationInIndex

  private def recordAdditionInIndex(
      path: Path,
      mode: String @@ Tags.Mode,
      blobId: String @@ Tags.BlobId
  )(using ProcessBuilderFromCommandString): Workflow[Unit] =
    Try {
      val _ = s"git update-index --add --cacheinfo $mode,$blobId,$path" !!
    }.labelExceptionWith(
      s"Unexpected error: could not update index for added file ${underline(path)}."
    )

  private def recordDeletionInIndex(
      path: Path
  )(using ProcessBuilderFromCommandString): Workflow[Unit] =
    Try {
      val _ = (s"git update-index --index-info" #< {
        new ByteArrayInputStream(
          s"$fakeModeForDeletion $fakeBlobIdForDeletion\t$path"
            .getBytes(StandardCharsets.UTF_8)
        )
      }) !!
    }.labelExceptionWith(
      s"Unexpected error: could not update index for deleted file ${underline(path)}."
    )

  private def pathChangeFor(
      commitIdOrBranchName: String @@ Tags.CommitOrBranchName
  )(
      line: String
  )(using ProcessBuilderFromCommandString): Workflow[(Path, Change)] =
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
          Path.of(path) -> right(Change.Deletion)
      end match
    }.labelExceptionWith(errorMessage =
      s"Unexpected error - can't parse changes reported by Git ${underline(line)}."
    ).flatMap { case (path, changed) => changed.map(path -> _) }

  private def blobAndContentFor(
      commitIdOrBranchName: String @@ Tags.CommitOrBranchName
  )(
      path: Path
  )(using ProcessBuilderFromCommandString): Workflow[
    (String @@ Tags.Mode, String @@ Tags.BlobId, String @@ Tags.Content)
  ] =
    Try {
      val line = s"git ls-tree $commitIdOrBranchName $path" !!

      line.split(whitespaceRun) match
        case Array(mode, _, blobId, _) =>
          val content = s"git cat-file blob $blobId" !!

          (
            mode.taggedWith[Tags.Mode],
            blobId.taggedWith[Tags.BlobId],
            content.taggedWith[Tags.Content]
          )
      end match
    }.labelExceptionWith(errorMessage =
      s"Unexpected error - can't determine blob id for path ${underline(path)} in commit or branch ${underline(commitIdOrBranchName)}."
    )
  end blobAndContentFor

  object Tags:
    trait Mode
    trait BlobId
    trait Content
    trait CommitOrBranchName
    trait ErrorMessage
    trait ExitCode
    trait StageIndex
  end Tags

  private enum Change:
    case Modification(
        mode: String @@ Tags.Mode,
        blobId: String @@ Tags.BlobId,
        content: String @@ Tags.Content
    )
    case Addition(
        mode: String @@ Tags.Mode,
        blobId: String @@ Tags.BlobId,
        content: String @@ Tags.Content
    )
    case Deletion
  end Change

  private enum IndexState:
    case OneEntry
    case ConflictingEntries
  end IndexState

end Main
