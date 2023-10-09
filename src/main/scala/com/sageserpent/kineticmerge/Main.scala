package com.sageserpent.kineticmerge

import cats.data.{EitherT, WriterT}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.traverse.toTraverseOps
import com.sageserpent.kineticmerge.Main.Tags
import com.sageserpent.kineticmerge.core.merge.Result
import com.sageserpent.kineticmerge.core.{Token, mergeTokens}
import com.softwaremill.tagging.*
import fansi.Str
import os.{CommandResult, Path, RelPath}
import scopt.OParser

import scala.io.Source

object Main:
  // NOTE: the use of Git below is based on spike work on MacOS - the version of
  // Git shipped tends to be a *long* way behind the latest release, so the
  // latest and greatest versions of commands are not always available. At time
  // of writing, Mac OS Ventura 13.5.2 ships Git 2.24.3, contrast with Git
  // 2.42.0 being the latest stable release.
  private type ErrorOrOperationMessage    = Either[String, String]
  private type WorkflowLog                = List[ErrorOrOperationMessage]
  private type WorkflowLogWriter[Payload] = WriterT[IO, WorkflowLog, Payload]
  private type Workflow[Payload] =
    EitherT[WorkflowLogWriter, String @@ Tags.ErrorMessage, Payload]

  private val whitespaceRun = "\\s+"

  private val noBranchProvided: String @@ Tags.CommitOrBranchName =
    "".taggedWith[Tags.CommitOrBranchName]

  private val fakeModeForDeletion: String @@ Tags.Mode =
    "0".taggedWith[Tags.Mode]
  private val fakeBlobIdForDeletion: String @@ Tags.BlobId =
    "0000000000000000000000000000000000000000".taggedWith[Tags.BlobId]

  private val successfulMerge: Int @@ Tags.ExitCode =
    0.taggedWith[Tags.ExitCode]
  private val conflictedMerge: Int @@ Tags.ExitCode =
    1.taggedWith[Tags.ExitCode]
  private val incorrectCommandLine: Int @@ Tags.ExitCode =
    2.taggedWith[Tags.ExitCode]
  private val error: Int @@ Tags.ExitCode = 3.taggedWith[Tags.ExitCode]

  private val bestCommonAncestorStageIndex: Int @@ Tags.StageIndex =
    1.taggedWith[Tags.StageIndex]
  private val ourStageIndex: Int @@ Tags.StageIndex =
    2.taggedWith[Tags.StageIndex]
  private val theirStageIndex: Int @@ Tags.StageIndex =
    3.taggedWith[Tags.StageIndex]

  def main(args: Array[String]): Unit =
    val parser =
      val builder = OParser.builder[CommandLineArguments]
      import builder.*

      val kineticMergeVersion = Source
        .fromResource("version.txt")
        .getLines()
        .nextOption()
        .getOrElse("Not a packaged build.")

      OParser.sequence(
        programName("kinetic-merge"),
        head("kinetic-merge", s"$kineticMergeVersion"),
        help(name = "help").text("Output this summary."),
        version(name = "version").text("Show the version of this command."),
        opt[Unit](name = "no-commit")
          .action((noCommit, commandLineArguments) =>
            commandLineArguments.copy(noCommit = true)
          )
          .text(
            "Do not commit a successful merge - leave merged changes staged in the index for review."
          ),
        opt[Unit](name = "no-ff")
          .action((noFastForward, commandLineArguments) =>
            commandLineArguments.copy(noFastForward = true)
          )
          .text("Prevent fast-forward merge - make a merge commit instead."),
        arg[String](name = "<their branch to merge into ours>")
          .action((theirBranch, commandLineArguments) =>
            commandLineArguments.copy(theirBranchHead =
              theirBranch.taggedWith[Tags.CommitOrBranchName]
            )
          )
          .required()
          .maxOccurs(1),
        note(
          "Utility to merge another Git branch's changes ('their branch') into the active Git branch in the current working directory ('our branch')."
        ),
        note(
          s"Exits with code $successfulMerge on completed successful merge."
        ),
        note(
          s"Exits with code $conflictedMerge on completed successful merge."
        ),
        note(
          s"Exits with code $incorrectCommandLine if command line is incorrect."
        ),
        note(
          s"Exits with code $error if Git porcelain or the filesystem experiences an error; any changes are rolled back."
        )
      )
    end parser

    val exitCode = OParser
      .parse(
        parser,
        args,
        CommandLineArguments(theirBranchHead = noBranchProvided)
      )
      .fold(ifEmpty = incorrectCommandLine)(
        mergeTheirBranch(_)(os.pwd)
      )

    System.exit(exitCode)
  end main

  def mergeTheirBranch(
      commandLineArguments: CommandLineArguments
  )(workingDirectory: Path): Int @@ Main.Tags.ExitCode =
    import commandLineArguments.*

    val workflow = for
      _ <- IO {
        os.proc("git", "--version").call(workingDirectory)
      }
        .labelExceptionWith(errorMessage = "Git is not available.")

      topLevel <- IO {
        os.proc("git", "rev-parse", "--show-toplevel")
          .call(workingDirectory)
          .out
          .text()
          .strip()
      }.labelExceptionWith(errorMessage =
        "The current working directory is not part of a Git working tree."
      )

      topLevelWorkingDirectory <- IO { Path(topLevel) }
        .labelExceptionWith(errorMessage =
          s"Unexpected error: top level of Git repository ${underline(topLevel)} is not a valid path."
        )

      inTopLevelWorkingDirectory = InWorkingDirectory(
        topLevelWorkingDirectory
      )

      ourBranchHead <- inTopLevelWorkingDirectory.ourBranchHead()

      theirCommitId <- inTopLevelWorkingDirectory.theirCommitId(theirBranchHead)

      oursAlreadyContainsTheirs <- inTopLevelWorkingDirectory
        .firstBranchIsContainedBySecond(
          theirBranchHead,
          ourBranchHead
        )

      theirsAlreadyContainsOurs <- inTopLevelWorkingDirectory
        .firstBranchIsContainedBySecond(
          ourBranchHead,
          theirBranchHead
        )

      exitCode <-
        if oursAlreadyContainsTheirs
        then
          // Nothing to do, our branch has all their commits already.
          right(successfulMerge)
            .logOperation(
              s"Nothing to do - our branch ${underline(ourBranchHead)} already contains ${underline(theirBranchHead)}."
            )
        else if theirsAlreadyContainsOurs && !noFastForward
        then
          inTopLevelWorkingDirectory.fastForwardToTheirs(
            ourBranchHead,
            theirBranchHead
          )
        else // Perform a real merge...
          for
            _ <- inTopLevelWorkingDirectory.confirmThereAreNoUncommittedChanges(
              ourBranchHead
            )

            bestAncestorCommitId <- inTopLevelWorkingDirectory
              .bestAncestorCommitId(ourBranchHead, theirBranchHead)

            ourChanges <- inTopLevelWorkingDirectory.changes(
              inTopLevelWorkingDirectory,
              ourBranchHead,
              bestAncestorCommitId,
              possessive = "our"
            )

            theirChanges <- inTopLevelWorkingDirectory.changes(
              inTopLevelWorkingDirectory,
              theirBranchHead,
              bestAncestorCommitId,
              possessive = "their"
            )

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
              inTopLevelWorkingDirectory.mergeWithRollback(
                theirBranchHead,
                ourBranchHead,
                theirCommitId,
                bestAncestorCommitId,
                overallChangesInvolvingTheirs,
                noCommit,
                noFastForward
              )
          yield exitCode
    yield exitCode

    val (log, exitCode) = workflow
      .foldF(
        errorMessage =>
          for _ <- WriterT.tell(List(Left(errorMessage)))
          yield error,
        WriterT.value
      )
      .run
      .unsafeRunSync()

    log.foreach {
      case Left(errorMessage)      => Console.err.println(errorMessage)
      case Right(operationMessage) => Console.println(operationMessage)
    }

    exitCode
  end mergeTheirBranch

  extension [Payload](fallible: IO[Payload])
    private def labelExceptionWith(errorMessage: String): Workflow[Payload] =
      EitherT
        .liftAttemptK[WorkflowLogWriter, Throwable]
        .apply(WriterT.liftF(fallible))
        .leftMap(_ => errorMessage.taggedWith[Tags.ErrorMessage])
  end extension

  extension [Payload](workflow: Workflow[Payload])
    private def logOperation(message: String): Workflow[Payload] =
      workflow.semiflatTap(_ => WriterT.tell(List(Right(message))))
  end extension

  private def underline(anything: Any): Str =
    fansi.Underlined.On(anything.toString)

  private def right[Payload](payload: Payload): Workflow[Payload] =
    EitherT.rightT[WorkflowLogWriter, String @@ Tags.ErrorMessage](payload)

  private def left[Payload](errorMessage: String): Workflow[Payload] =
    EitherT.leftT[WorkflowLogWriter, Payload](
      errorMessage.taggedWith[Tags.ErrorMessage]
    )

  private def temporaryFile(
      suffix: String,
      content: String @@ Tags.Content
  ): Workflow[Path] =
    for temporaryFile <- IO {
        os.temp(
          contents = content,
          prefix = "kinetic-merge-",
          suffix = ".base",
          deleteOnExit = true
        )
      }.labelExceptionWith(
        s"Unexpected error: could not create temporary file."
      )
    yield temporaryFile

  case class CommandLineArguments(
      theirBranchHead: String @@ Main.Tags.CommitOrBranchName,
      noCommit: Boolean = false,
      noFastForward: Boolean = false
  )

  private case class InWorkingDirectory(workingDirectory: Path):
    def ourBranchHead(): Workflow[String @@ Main.Tags.CommitOrBranchName] =
      IO {
        val branchName = os
          .proc("git", "branch", "--show-current")
          .call(workingDirectory)
          .out
          .text()
          .strip()
          .taggedWith[Tags.CommitOrBranchName]

        if branchName.nonEmpty then branchName
        else
          // Handle a detached commit.
          os.proc("git", "rev-parse", "HEAD")
            .call(workingDirectory)
            .out
            .text()
            .strip()
            .taggedWith[Tags.CommitOrBranchName]
        end if
      }.labelExceptionWith(errorMessage =
        s"Could not determine a branch name or commit id for our branch head."
      )

    def theirCommitId(
        theirBranchHead: String @@ Main.Tags.CommitOrBranchName
    ): Workflow[String @@ Tags.CommitOrBranchName] =
      IO {
        os.proc("git", "rev-parse", theirBranchHead)
          .call(workingDirectory)
          .out
          .text()
          .taggedWith[Tags.CommitOrBranchName]
      }.labelExceptionWith(errorMessage =
        s"Ref ${underline(theirBranchHead)} is not a valid branch or commit."
      )

    def firstBranchIsContainedBySecond(
        firstBranchHead: String @@ Tags.CommitOrBranchName,
        secondBranchHead: String @@ Tags.CommitOrBranchName
    ): Workflow[Boolean] =
      IO {
        os.proc(
          "git",
          "merge-base",
          "--is-ancestor",
          firstBranchHead,
          secondBranchHead
        ).call(workingDirectory, check = false)
          .exitCode
      }.labelExceptionWith(errorMessage =
        s"Unexpected error: could not determine whether branch ${underline(firstBranchHead)} is an ancestor of branch ${underline(secondBranchHead)}."
      ).map(0 == _)

    def fastForwardToTheirs(
        ourBranchHead: String @@ Main.Tags.CommitOrBranchName,
        theirBranchHead: String @@ Main.Tags.CommitOrBranchName
    ): Workflow[Int @@ Main.Tags.ExitCode] =
      IO {
        os.proc("git", "reset", "--hard", theirBranchHead)
          .call(workingDirectory): Unit
        successfulMerge
      }
        .labelExceptionWith(errorMessage =
          s"Unexpected error: could not fast-forward our branch ${underline(ourBranchHead)} to their branch ${underline(theirBranchHead)}."
        )
        .logOperation(
          s"Fast forward our branch ${underline(ourBranchHead)} to their branch ${underline(theirBranchHead)}."
        )

    def confirmThereAreNoUncommittedChanges(
        ourBranchHead: String @@ Main.Tags.CommitOrBranchName
    ): Workflow[Unit] =
      IO {
        val _ = os
          .proc("git", "diff-index", "--exit-code", ourBranchHead)
          .call(workingDirectory)
      }
        .labelExceptionWith(errorMessage =
          "There are uncommitted changes prior to commencing the merge."
        )

    def bestAncestorCommitId(
        ourBranchHead: String @@ Main.Tags.CommitOrBranchName,
        theirBranchHead: String @@ Main.Tags.CommitOrBranchName
    ): Workflow[String @@ Main.Tags.CommitOrBranchName] =
      IO {
        os.proc("git", "merge-base", ourBranchHead, theirBranchHead)
          .call(workingDirectory)
          .out
          .text()
          .strip()
          .taggedWith[Tags.CommitOrBranchName]
      }.labelExceptionWith(errorMessage =
        s"Could not determine a best ancestor commit between our branch ${underline(ourBranchHead)} and their branch ${underline(theirBranchHead)}."
      )

    def changes(
        inTopLevelWorkingDirectory: InWorkingDirectory,
        branchOrCommit: String @@ Main.Tags.CommitOrBranchName,
        bestAncestorCommitId: String @@ Main.Tags.CommitOrBranchName,
        possessive: String
    ): Workflow[Map[Path, Change]] =
      IO {
        os.proc(
          "git",
          "diff",
          "--no-renames",
          "--name-status",
          bestAncestorCommitId,
          branchOrCommit
        ).call(workingDirectory)
          .out
          .lines()
      }.labelExceptionWith(errorMessage =
        s"Could not determine changes made on $possessive branch ${underline(branchOrCommit)} since ancestor commit ${underline(bestAncestorCommitId)}."
      ).flatMap(
        _.traverse(
          inTopLevelWorkingDirectory.pathChangeFor(branchOrCommit)
        )
      ).map(_.toMap)

    def pathChangeFor(commitIdOrBranchName: String @@ Tags.CommitOrBranchName)(
        line: String
    ): Workflow[(Path, Change)] =
      IO {
        line.split(whitespaceRun) match
          case Array("M", changedFile) =>
            val path = workingDirectory / changedFile
            path -> blobAndContentFor(commitIdOrBranchName)(
              path
            ).map(
              Change.Modification.apply.tupled
            )
          case Array("A", changedFile) =>
            val path = workingDirectory / RelPath(changedFile)
            path -> blobAndContentFor(commitIdOrBranchName)(
              path
            ).map(
              Change.Addition.apply.tupled
            )
          case Array("D", path) =>
            workingDirectory / path -> right(Change.Deletion)
        end match
      }.labelExceptionWith(errorMessage =
        s"Unexpected error - can't parse changes reported by Git ${underline(line)}."
      ).flatMap { case (path, changed) => changed.map(path -> _) }

    private def blobAndContentFor(
        commitIdOrBranchName: String @@ Tags.CommitOrBranchName
    )(
        path: Path
    ): Workflow[
      (String @@ Tags.Mode, String @@ Tags.BlobId, String @@ Tags.Content)
    ] =
      IO {
        val line = os
          .proc("git", "ls-tree", commitIdOrBranchName, path)
          .call(workingDirectory)
          .out
          .text()

        line.split(whitespaceRun) match
          case Array(mode, _, blobId, _) =>
            val content = os
              .proc("git", "cat-file", "blob", blobId)
              .call(workingDirectory)
              .out
              .text()

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

    def mergeWithRollback(
        theirBranchHead: String @@ Tags.CommitOrBranchName,
        ourBranchHead: String @@ Tags.CommitOrBranchName,
        theirCommitId: String @@ Tags.CommitOrBranchName,
        bestAncestorCommitId: String @@ Tags.CommitOrBranchName,
        overallChangesInvolvingTheirs: List[(Path, (Change, Option[Change]))],
        noCommit: Boolean,
        noFastForward: Boolean
    ): Workflow[Int @@ Tags.ExitCode] =
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
            val commitMessage =
              // No underlining here, please...
              s"Merge from $theirBranchHead into $ourBranchHead."

            if goodForAMergeCommit && !noCommit then
              for
                treeId <- IO {
                  os.proc("git", "write-tree")
                    .call(workingDirectory)
                    .out
                    .text()
                    .strip()
                }
                  .labelExceptionWith(errorMessage =
                    s"Unexpected error: could not write a tree object from the index."
                  )
                commitId <- IO {
                  os.proc(
                    "git",
                    "commit-tree",
                    "-p",
                    ourBranchHead,
                    "-p",
                    theirBranchHead,
                    "-m",
                    s"'$commitMessage'",
                    treeId
                  ).call(workingDirectory)
                    .out
                    .text()
                    .strip()
                }.labelExceptionWith(errorMessage =
                  s"Unexpected error: could not create a commit from tree object ${underline(treeId)}"
                )
                _ <- IO {
                  os.proc("git", "reset", "--soft", commitId)
                    .call(workingDirectory)
                    .out
                    .text()
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
                gitDir <- IO {
                  os.proc("git", "rev-parse", "--absolute-git-dir")
                    .call(workingDirectory)
                    .out
                    .text()
                    .strip()
                }
                  .labelExceptionWith(errorMessage =
                    "Could not determine location of `GIT_DIR`."
                  )
                gitDirPath <- IO {
                  Path(gitDir)
                }
                  .labelExceptionWith(errorMessage =
                    s"Unexpected error: `GIT_DIR` reported by Git ${underline(gitDir)} is not a valid path."
                  )
                _ <- IO {
                  os.write.over(gitDirPath / "MERGE_HEAD", theirCommitId)
                }.labelExceptionWith(errorMessage =
                  s"Unexpected error: could not write `MERGE_HEAD` to reference their branch ${underline(theirBranchHead)}."
                )
                mergeMode = if noFastForward then "no-ff" else ""
                _ <- IO {
                  os.write.over(gitDirPath / "MERGE_MODE", mergeMode)
                }.labelExceptionWith(errorMessage =
                  s"Unexpected error: could not write `MERGE_MODE` to propagate the merge mode ${underline(mergeMode)}."
                )
                _ <- IO {
                  os.write.over(gitDirPath / "MERGE_MSG", commitMessage)
                }.labelExceptionWith(errorMessage =
                  s"Unexpected error: could not write `MERGE_MSG` to prepare the commit message ${underline(commitMessage)}."
                ).logOperation(
                  if goodForAMergeCommit then
                    "Successful merge, leaving merged changes in the index for review..."
                  else
                    "Merge conflicts found, handing over for manual resolution..."
                )
              yield conflictedMerge
            end if
        yield exitCodeWhenThereAreNoUnexpectedErrors

      // NASTY HACK: hokey cleanup, need to think about the best approach...
      workflow.leftMap(label =>
        try os.proc("git", "reset", "--hard").call(workingDirectory)
        catch
          case exception =>
            println(s"Failed to rollback changes after unexpected error.")
        end try

        label
      )
    end mergeWithRollback

    private def indexUpdates(
        bestAncestorCommitId: String @@ Tags.CommitOrBranchName,
        ourBranchHead: String @@ Tags.CommitOrBranchName,
        theirBranchHead: String @@ Tags.CommitOrBranchName
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
            _ <- recordConflictModificationInIndex(
              stageIndex = bestCommonAncestorStageIndex
            )(
              bestAncestorCommitId,
              path,
              mode,
              bestAncestorCommitIdBlobId
            )
            - <- recordConflictModificationInIndex(
              stageIndex = theirStageIndex
            )(
              theirBranchHead,
              path,
              mode,
              theirBlobId
            )
            _ <-
              // Git's merge updates the working directory tree with *their*
              // modified file which wouldn't have been present on our
              // branch prior to the merge. So that's what we do too.
              IO {
                os.proc("git", "cat-file", "blob", theirBlobId)
                  .call(workingDirectory, stdout = path)
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
            - <- recordConflictModificationInIndex(
              stageIndex = bestCommonAncestorStageIndex
            )(
              bestAncestorCommitId,
              path,
              mode,
              bestAncestorCommitIdBlobId
            )
            - <- recordConflictModificationInIndex(
              stageIndex = ourStageIndex
            )(
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
            _ <- recordModificationInIndex(
              path,
              mode,
              blobId
            )
            - <- IO {
              os.proc("git", "cat-file", "blob", blobId)
                .call(workingDirectory, stdout = path)
            }
              .labelExceptionWith(errorMessage =
                s"Unexpected error: could not update working directory tree with modified file ${underline(path)}."
              )
          yield IndexState.OneEntry

        case (path, (Change.Addition(mode, blobId, _), None)) =>
          for
            _ <- recordAdditionInIndex(path, mode, blobId)
            - <- IO {
              os.write.over(
                path,
                os.proc("git", "cat-file", "blob", blobId)
                  .spawn(workingDirectory)
                  .stdout,
                createFolders = true
              )
            }
              .labelExceptionWith(errorMessage =
                s"Unexpected error: could not update working directory tree with added file ${underline(path)}."
              )
          yield IndexState.OneEntry

        case (path, (Change.Deletion, None)) =>
          for
            _ <- recordDeletionInIndex(path)
            _ <- IO {
              os.remove(path)
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

    private def indexStateForTwoWayMerge(
        ourBranchHead: String @@ Tags.CommitOrBranchName,
        theirBranchHead: String @@ Tags.CommitOrBranchName
    )(
        path: Path,
        theirMode: String @@ Tags.Mode,
        theirContent: String @@ Tags.Content,
        ourMode: String @@ Tags.Mode,
        ourContent: String @@ Tags.Content
    ): Workflow[IndexState] =
      for
        mergedFileMode <-
          if ourMode == theirMode then right(ourMode)
          else
            left(
              s"Conflicting file modes for file ${underline(path)}; on our branch head ${underline(ourMode)} and on their branch head ${underline(theirMode)}."
            )

        ourAncestorTokens <- IO {
          Token.tokens(ourContent).get
        }
          .labelExceptionWith(errorMessage =
            s"Failed to tokenize file ${underline(path)} on our branch head ${underline(ourBranchHead)}."
          )

        theirAncestorTokens <- IO {
          Token.tokens(theirContent).get
        }
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
            indexStateForCleanMerge(
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
                  os.proc(
                    "git",
                    "merge-file",
                    "-L",
                    ourBranchHead,
                    "-L",
                    s"'$noPriorContentName'",
                    "-L",
                    theirBranchHead,
                    leftTemporaryFile,
                    fakeBaseTemporaryFile,
                    rightTemporaryFile
                  ).call(workingDirectory, check = false)
                    .exitCode

                if 0 <= exitCode then right(())
                else
                  left(
                    s"Unexpected error: could not generate conflicted file contents on behalf of ${underline(path)} in temporary file ${underline(leftTemporaryFile)}"
                  )
                end if
              _ <- IO {
                os.copy.over(leftTemporaryFile, path)
              }.labelExceptionWith(errorMessage =
                s"Unexpected error: could not copy results of conflicted merge in ${underline(leftTemporaryFile)} to working directory tree file ${underline(path)}."
              )

              leftBlob  <- storeBlobFor(path, leftContent)
              rightBlob <- storeBlobFor(path, rightContent)
              _         <- recordDeletionInIndex(path)
              _ <- recordConflictModificationInIndex(
                stageIndex = ourStageIndex
              )(
                ourBranchHead,
                path,
                ourMode,
                leftBlob
              )
              _ <- recordConflictModificationInIndex(
                stageIndex = theirStageIndex
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

    private def indexStateForCleanMerge(
        path: Path,
        mergedFileMode: String @@ Tags.Mode,
        elements: IndexedSeq[Token]
    ): Workflow[IndexState] =
      val mergedContent =
        elements.map(_.text).mkString.taggedWith[Tags.Content]
      for
        mergedBlobId <- storeBlobFor(path, mergedContent)
        _ <- recordModificationInIndex(
          path,
          mergedFileMode,
          mergedBlobId
        )
        - <- IO {
          os.proc("git", "cat-file", "blob", mergedBlobId)
            .call(workingDirectory, stdout = path)
        }
          .labelExceptionWith(errorMessage =
            s"Unexpected error: could not update working directory tree with merged file ${underline(path)}."
          )
      yield IndexState.OneEntry
      end for
    end indexStateForCleanMerge

    private def indexStateForThreeWayMerge(
        bestAncestorCommitId: String @@ Tags.CommitOrBranchName,
        ourBranchHead: String @@ Tags.CommitOrBranchName,
        theirBranchHead: String @@ Tags.CommitOrBranchName
    )(
        path: Path,
        theirMode: String @@ Tags.Mode,
        theirContent: String @@ Tags.Content,
        ourMode: String @@ Tags.Mode,
        ourContent: String @@ Tags.Content
    ): Workflow[IndexState] =
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

        bestAncestorTokens <- IO {
          Token.tokens(bestAncestorCommitIdContent).get
        }.labelExceptionWith(errorMessage =
          s"Failed to tokenize file ${underline(path)} on best ancestor commit ${underline(bestAncestorCommitId)}."
        )

        ourAncestorTokens <- IO {
          Token.tokens(ourContent).get
        }
          .labelExceptionWith(errorMessage =
            s"Failed to tokenize file ${underline(path)} on our branch head ${underline(ourBranchHead)}."
          )

        theirAncestorTokens <- IO {
          Token.tokens(theirContent).get
        }
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
            indexStateForCleanMerge(
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
                  os.proc(
                    "git",
                    "merge-file",
                    "-L",
                    ourBranchHead,
                    "-L",
                    bestAncestorCommitId,
                    "-L",
                    theirBranchHead,
                    leftTemporaryFile,
                    baseTemporaryFile,
                    rightTemporaryFile
                  ).call(workingDirectory, check = false)
                    .exitCode

                if 0 <= exitCode then right(())
                else
                  left(
                    s"Unexpected error: could not generate conflicted file contents on behalf of ${underline(path)} in temporary file ${underline(leftTemporaryFile)}"
                  )
                end if
              _ <- IO {
                os.copy.over(leftTemporaryFile, path)
              }.labelExceptionWith(errorMessage =
                s"Unexpected error: could not copy results of conflicted merge in ${underline(leftTemporaryFile)} to working directory tree file ${underline(path)}."
              )

              leftBlob  <- storeBlobFor(path, leftContent)
              rightBlob <- storeBlobFor(path, rightContent)
              _         <- recordDeletionInIndex(path)
              _ <- recordConflictModificationInIndex(
                stageIndex = bestCommonAncestorStageIndex
              )(
                bestAncestorCommitId,
                path,
                bestAncestorCommitIdMode,
                bestAncestorCommitIdBlobId
              )
              _ <- recordConflictModificationInIndex(
                stageIndex = ourStageIndex
              )(
                ourBranchHead,
                path,
                ourMode,
                leftBlob
              )
              _ <- recordConflictModificationInIndex(
                stageIndex = theirStageIndex
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

    private def recordAdditionInIndex(
        path: Path,
        mode: String @@ Tags.Mode,
        blobId: String @@ Tags.BlobId
    ): Workflow[Unit] =
      IO {
        val _ = os
          .proc(
            "git",
            "update-index",
            "--add",
            "--cacheinfo",
            mode,
            blobId,
            path relativeTo workingDirectory
          )
          .call(workingDirectory)
      }.labelExceptionWith(
        s"Unexpected error: could not update index for added file ${underline(path)}."
      )

    private def recordDeletionInIndex(
        path: Path
    ): Workflow[Unit] =
      IO {
        val _ = os
          .proc("git", "update-index", "--index-info")
          .call(
            workingDirectory,
            stdin =
              s"$fakeModeForDeletion $fakeBlobIdForDeletion\t${path relativeTo workingDirectory}"
          )
      }.labelExceptionWith(
        s"Unexpected error: could not update index for deleted file ${underline(path)}."
      )

    private def recordModificationInIndex(
        path: Path,
        mode: String @@ Tags.Mode,
        blobId: String @@ Tags.BlobId
    ): Workflow[Unit] =
      IO {
        val _ = os
          .proc("git", "update-index", "--index-info")
          .call(
            workingDirectory,
            stdin = s"$mode $blobId\t${path relativeTo workingDirectory}"
          )
      }.labelExceptionWith(
        s"Unexpected error: could not update index for modified file ${underline(path)}."
      )

    private def recordConflictModificationInIndex(
        stageIndex: Int @@ Tags.StageIndex
    )(
        commitIdOrBranchName: String @@ Tags.CommitOrBranchName,
        path: Path,
        mode: String @@ Tags.Mode,
        blobId: String @@ Tags.BlobId
    ): Workflow[Unit] =
      IO {
        val _ = os
          .proc("git", "update-index", "--index-info")
          .call(
            workingDirectory,
            stdin =
              s"$mode $blobId $stageIndex\t${path relativeTo workingDirectory}"
          )
      }.labelExceptionWith(
        s"Unexpected error: could not update conflict stage #${underline(stageIndex)} index for modified file ${underline(path)} from commit or branch ${underline(commitIdOrBranchName)}."
      )
    end recordConflictModificationInIndex

    private def storeBlobFor(
        path: Path,
        content: String @@ Tags.Content
    ): Workflow[String @@ Tags.BlobId] =
      IO {
        val line = os
          .proc("git", "hash-object", "-t", "blob", "-w", "--stdin")
          .call(workingDirectory, stdin = content)
          .out
          .text()

        line.split(whitespaceRun) match
          case Array(blobId) => blobId.taggedWith[Tags.BlobId]
        end match
      }.labelExceptionWith(errorMessage =
        s"Unexpected error - could not create a blob for file ${underline(path)}."
      )
  end InWorkingDirectory

  object Tags:
    trait Mode
    trait BlobId
    trait Content
    trait CommitOrBranchName
    trait ErrorMessage
    trait ExitCode
    trait StageIndex
  end Tags

  enum Change:
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
