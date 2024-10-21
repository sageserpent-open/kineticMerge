package com.sageserpent.kineticmerge

import cats.data.{EitherT, WriterT}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.foldable.toFoldableOps
import cats.syntax.traverse.toTraverseOps
import cats.{Eq, Order}
import com.google.common.hash.{Funnel, HashFunction, Hashing}
import com.sageserpent.kineticmerge.Main.MergeInput.*
import com.sageserpent.kineticmerge.core.*
import com.sageserpent.kineticmerge.core.CodeMotionAnalysis.Configuration
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisExtension.*
import com.sageserpent.kineticmerge.core.Token.tokens
import com.softwaremill.tagging.*
import com.typesafe.scalalogging.StrictLogging
import fansi.Str
import os.{CommandResult, Path, RelPath}
import scopt.{DefaultOEffectSetup, OParser}

import scala.annotation.varargs
import scala.collection.BuildFrom
import scala.collection.decorators.mapDecorator
import scala.io.Source
import scala.util.Try

object Main extends StrictLogging:
  // NOTE: the use of Git below is based on spike work on MacOS - the version of
  // Git shipped tends to be a *long* way behind the latest release, so the
  // latest and greatest versions of commands are not always available. At time
  // of writing, Mac OS Ventura 13.5.2 ships Git 2.24.3, contrast with Git
  // 2.42.0 being the latest stable release.
  private type ErrorOrOperationMessage =
    Either[String @@ Tags.ErrorMessage, String]
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

  // NOTE: allow a degree of overlap between the alternate groups, this avoids
  // doing any downstream disambiguation between a percentage and either an
  // implied or an explicit fraction in the range [0, 1].
  private val matchThresholdRegex = raw"(\d{1,3})%|(\d+)|([01]\.\d+)".r.anchored

  /** Entry point for the application. In contrast with [[apply]], it will pass
    * an exit code via [[System.exit]] and thus force a shutdown of the
    * application.
    * @param commandLineArguments
    *   Command line arguments as an array, as per Scala's entry point
    *   specification.
    */
  def main(commandLineArguments: Array[String]): Unit =
    System.exit(
      apply(progressRecording = ConsoleProgressRecording, commandLineArguments*)
    )
  end main

  /** @param progressRecording
    * @param commandLineArguments
    *   Command line arguments as varargs.
    * @return
    *   The exit code as a plain integer, suitable for consumption by both Scala
    *   and Java client code.
    */
  @varargs
  def apply(
      progressRecording: ProgressRecording,
      commandLineArguments: String*
  ): Int =
    val logbackRootLevelLoggingJavaPropertyName = "logback-root-level"

    val parser =
      val builder = OParser.builder[ApplicationRequest]
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
        opt[Int](name = "minimum-match-size")
          .validate(minimumMatchSize =>
            if 0 < minimumMatchSize then success
            else failure(s"Minimum match size must be positive.")
          )
          .action((minimumMatchSize, commandLineArguments) =>
            commandLineArguments
              .copy(minimumMatchSize = minimumMatchSize)
          )
          .text("Minimum number of tokens for a match to be considered."),
        opt[String](name = "match-threshold")
          .validate(matchThreshold =>
            if matchThresholdRegex.matches(matchThreshold) then success
            else
              failure(
                s"Match threshold ${underline(matchThreshold)} must be a percentage, or digits to the right of the decimal point of a non-negative fraction less than one, or a fraction at least zero and at most one."
              )
          )
          .action { (matchThreshold, commandLineArguments) =>
            val thresholdSizeFractionForMatching = matchThreshold match
              case matchThresholdRegex(
                    percentage,
                    impliedFraction,
                    explicitFraction
                  ) =>
                (
                  Option(percentage),
                  Option(impliedFraction),
                  Option(explicitFraction)
                ).match
                  case (Some(percentage), None, None) =>
                    // Parse as an integer first.
                    percentage.toInt.toDouble / 100
                  case (None, Some(impliedFraction), None) =>
                    val numberOfDigits = impliedFraction.size
                    // Push all the digits to the right of the decimal point.
                    impliedFraction.toInt * Math.pow(10, -numberOfDigits)
                  case (None, None, Some(explicitFraction)) =>
                    explicitFraction.toDouble

            commandLineArguments.copy(thresholdSizeFractionForMatching =
              thresholdSizeFractionForMatching
            )
          }
          .text(
            "Minimum fraction of a containing file's size for a section of text to qualify for matching."
          ),
        opt[Int](name = "minimum-ambiguous-match-size")
          .validate(minimumAmbiguousMatchSize =>
            if 0 <= minimumAmbiguousMatchSize then success
            else failure(s"Minimum match size must be zero or positive.")
          )
          .action((minimumAmbiguousMatchSize, commandLineArguments) =>
            commandLineArguments
              .copy(minimumAmbiguousMatchSize = minimumAmbiguousMatchSize)
          )
          .text(
            "Minimum number of tokens for an ambiguous match to be considered."
          ),
        arg[String](name = "<their branch to merge into ours>")
          .action((theirBranch, commandLineArguments) =>
            commandLineArguments.copy(theirBranchHead =
              theirBranch.taggedWith[Tags.CommitOrBranchName]
            )
          )
          .required()
          .maxOccurs(1),
        checkConfig(commandLineArguments =>
          if 0 > commandLineArguments.thresholdSizeFractionForMatching || 1 < commandLineArguments.thresholdSizeFractionForMatching
          then
            failure(
              s"Match threshold fraction ${underline(commandLineArguments.thresholdSizeFractionForMatching)} should be at least zero and at most one."
            )
          else success
        ),
        note(
          "Utility to merge another Git branch's changes ('their branch') into the active Git branch in the current working directory ('our branch')."
        ),
        note(
          s"Exits with code $successfulMerge on completed successful merge."
        ),
        note(
          s"Exits with code $conflictedMerge on completed merge with conflicts."
        ),
        note(
          s"Exits with code $incorrectCommandLine if command line is incorrect."
        ),
        note(
          s"Exits with code $error if Git porcelain or the filesystem experiences an error; any changes are rolled back."
        ),
        note(
          s"Logging is via Logback and is disabled by default - set the root logging level via the Java system property: ${underline(logbackRootLevelLoggingJavaPropertyName)}."
        )
      )
    end parser

    case class EarlyTermination(exitCode: Int @@ Tags.ExitCode)
        extends RuntimeException

    val applicationRequest: Try[Option[ApplicationRequest]] = Try {
      OParser
        .parse(
          parser,
          commandLineArguments,
          ApplicationRequest(),
          new DefaultOEffectSetup:
            // Don't terminate the application, let execution return back to the
            // caller via a glorified long-jump.
            override def terminate(exitState: Either[String, Unit]): Unit =
              throw EarlyTermination(exitState match
                case Left(_)  => 1.taggedWith[Tags.ExitCode]
                case Right(_) => 0.taggedWith[Tags.ExitCode]
              )
        )
    }

    applicationRequest.fold(
      {
        case EarlyTermination(exitCode) => exitCode
        case exception: Exception =>
          exception.printStackTrace()
          error
      },
      _.fold(ifEmpty = incorrectCommandLine)(
        mergeTheirBranch(_)(workingDirectory = os.pwd, progressRecording)
      )
    )
  end apply

  def mergeTheirBranch(applicationRequest: ApplicationRequest)(
      workingDirectory: Path,
      progressRecording: ProgressRecording = NoProgressRecording
  ): Int @@ Main.Tags.ExitCode =
    import applicationRequest.*

    val configuration = Configuration(
      minimumMatchSize,
      thresholdSizeFractionForMatching,
      minimumAmbiguousMatchSize,
      progressRecording = progressRecording
    )

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

      inTopLevelWorkingDirectory = InWorkingDirectory(topLevelWorkingDirectory)

      ourBranchHead <- inTopLevelWorkingDirectory.ourBranchHead()

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

            mergeInputs <- inTopLevelWorkingDirectory.mergeInputsOf(
              bestAncestorCommitId,
              ourBranchHead,
              theirBranchHead
            )(ourChanges, theirChanges)

            exitCode <-
              inTopLevelWorkingDirectory.mergeWithRollback(
                ourBranchHead,
                theirBranchHead,
                bestAncestorCommitId,
                mergeInputs,
                noCommit,
                noFastForward,
                configuration
              )
          yield exitCode
          end for
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

  private def right[Payload](payload: Payload): Workflow[Payload] =
    EitherT.rightT[WorkflowLogWriter, String @@ Tags.ErrorMessage](payload)

  extension [Payload](fallible: IO[Payload])
    private def labelExceptionWith(errorMessage: String): Workflow[Payload] =
      EitherT
        .liftAttemptK[WorkflowLogWriter, Throwable]
        .apply(WriterT.liftF(fallible))
        .leftMap(exception =>
          // TODO: something pure, functional and wholesome that could be seen
          // at high church...
          logger.error(exception.getMessage)
          exception.printStackTrace()
          errorMessage.taggedWith[Tags.ErrorMessage]
        )
  end extension

  extension [Payload](workflow: Workflow[Payload])
    private def logOperation(message: String): Workflow[Payload] =
      workflow.semiflatTap(_ => WriterT.tell(List(Right(message))))
  end extension

  private def underline(anything: Any): Str =
    fansi.Underlined.On(anything.toString)

  /** @param commandLineArguments
    *   Command line arguments as varargs.
    * @return
    *   The exit code as a plain integer, suitable for consumption by both Scala
    *   and Java client code.
    */
  @varargs
  def apply(commandLineArguments: String*): Int = apply(
    progressRecording = NoProgressRecording,
    commandLineArguments = commandLineArguments*
  )

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

  case class ApplicationRequest(
      theirBranchHead: String @@ Main.Tags.CommitOrBranchName =
        noBranchProvided,
      noCommit: Boolean = false,
      noFastForward: Boolean = false,
      minimumMatchSize: Int =
        // Don't allow monograph matches - these would bombard the merge with
        // useless all-sides matches that create a *lot* of overhead. In
        // practice, avoiding small window sizes above one leads to a much
        // better merge as well.
        4,
      thresholdSizeFractionForMatching: Double = 0,
      minimumAmbiguousMatchSize: Int = 10
  )

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

  enum MergeInput:
    case JustOurModification(
        ourModification: Change.Modification,
        bestAncestorCommitIdContent: String @@ Tags.Content
    )
    case JustTheirModification(
        theirModification: Change.Modification,
        bestAncestorCommitIdContent: String @@ Tags.Content
    )
    case JustOurAddition(ourAddition: Change.Addition)
    case JustTheirAddition(theirAddition: Change.Addition)
    case JustOurDeletion(bestAncestorCommitIdContent: String @@ Tags.Content)
    case JustTheirDeletion(bestAncestorCommitIdContent: String @@ Tags.Content)
    case OurModificationAndTheirDeletion(
        ourModification: Change.Modification,
        bestAncestorCommitIdMode: String @@ Tags.Mode,
        bestAncestorCommitIdBlobId: String @@ Tags.BlobId,
        bestAncestorCommitIdContent: String @@ Tags.Content
    )
    case TheirModificationAndOurDeletion(
        theirModification: Change.Modification,
        bestAncestorCommitIdMode: String @@ Tags.Mode,
        bestAncestorCommitIdBlobId: String @@ Tags.BlobId,
        bestAncestorCommitIdContent: String @@ Tags.Content
    )
    case BothContributeAnAddition(
        ourAddition: Change.Addition,
        theirAddition: Change.Addition,
        mergedFileMode: String @@ Tags.Mode
    )
    case BothContributeAModification(
        ourModification: Change.Modification,
        theirModification: Change.Modification,
        bestAncestorCommitIdMode: String @@ Tags.Mode,
        bestAncestorCommitIdBlobId: String @@ Tags.BlobId,
        bestAncestorCommitIdContent: String @@ Tags.Content,
        mergedFileMode: String @@ Tags.Mode
    )
    case BothContributeADeletion(
        bestAncestorCommitIdContent: String @@ Tags.Content
    )
  end MergeInput

  private case class InWorkingDirectory(
      workingDirectory: Path
  ):
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
            val path = workingDirectory / RelPath(changedFile)
            path -> blobAndContentFor(commitIdOrBranchName)(
              path
            ).map(
              Change.Modification.apply.tupled
            )
          case Array("A", addedFile) =>
            val path = workingDirectory / RelPath(addedFile)
            path -> blobAndContentFor(commitIdOrBranchName)(
              path
            ).map(
              Change.Addition.apply.tupled
            )
          case Array("D", deletedFile) =>
            val path = RelPath(deletedFile)
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

    def mergeInputsOf(
        bestAncestorCommitId: String @@ Tags.CommitOrBranchName,
        ourBranchHead: String @@ Tags.CommitOrBranchName,
        theirBranchHead: String @@ Tags.CommitOrBranchName
    )(
        ourChanges: Map[Path, Change],
        theirChanges: Map[Path, Change]
    ): Workflow[List[(Path, MergeInput)]] =

      val outerJoin = ourChanges.mergeByKey(theirChanges)

      outerJoin.toList
        .traverse {
          case (
                path,
                (
                  Some(_: Change.Addition),
                  Some(Change.Deletion | _: Change.Modification)
                )
              ) =>
            left(
              s"Unexpected error: file ${underline(path)} has been added on our branch ${underline(ourBranchHead)} and either deleted or modified on their branch ${underline(theirBranchHead)}."
            )

          case (
                path,
                (
                  Some(Change.Deletion | _: Change.Modification),
                  Some(_: Change.Addition)
                )
              ) =>
            left(
              s"Unexpected error: file ${underline(path)} has been either deleted or modified on our branch ${underline(ourBranchHead)} and added on their branch ${underline(theirBranchHead)}."
            )

          case (
                path,
                (Some(ourModification: Change.Modification), None)
              ) =>
            for (
                _,
                _,
                bestAncestorCommitIdContent
              ) <- blobAndContentFor(bestAncestorCommitId)(path)
            yield path -> JustOurModification(
              ourModification,
              bestAncestorCommitIdContent
            )

          case (
                path,
                (None, Some(theirModification: Change.Modification))
              ) =>
            for (
                _,
                _,
                bestAncestorCommitIdContent
              ) <- blobAndContentFor(bestAncestorCommitId)(path)
            yield path -> JustTheirModification(
              theirModification,
              bestAncestorCommitIdContent
            )

          case (
                path,
                (Some(ourAddition: Change.Addition), None)
              ) =>
            right(path -> JustOurAddition(ourAddition))

          case (
                path,
                (None, Some(theirAddition: Change.Addition))
              ) =>
            right(path -> JustTheirAddition(theirAddition))

          case (
                path,
                (Some(Change.Deletion), None)
              ) =>
            for (
                _,
                _,
                bestAncestorCommitIdContent
              ) <- blobAndContentFor(bestAncestorCommitId)(path)
            yield path -> JustOurDeletion(bestAncestorCommitIdContent)

          case (
                path,
                (None, Some(Change.Deletion))
              ) =>
            for (
                _,
                _,
                bestAncestorCommitIdContent
              ) <- blobAndContentFor(bestAncestorCommitId)(path)
            yield path -> JustTheirDeletion(bestAncestorCommitIdContent)

          case (
                path,
                (
                  Some(ourModification: Change.Modification),
                  Some(Change.Deletion)
                )
              ) =>
            for (
                bestAncestorCommitIdMode,
                bestAncestorCommitIdBlobId,
                bestAncestorCommitIdContent
              ) <- blobAndContentFor(bestAncestorCommitId)(path)
            yield path -> OurModificationAndTheirDeletion(
              ourModification,
              bestAncestorCommitIdMode,
              bestAncestorCommitIdBlobId,
              bestAncestorCommitIdContent
            )

          case (
                path,
                (
                  Some(Change.Deletion),
                  Some(theirModification: Change.Modification)
                )
              ) =>
            for (
                bestAncestorCommitIdMode,
                bestAncestorCommitIdBlobId,
                bestAncestorCommitIdContent
              ) <- blobAndContentFor(bestAncestorCommitId)(path)
            yield path -> TheirModificationAndOurDeletion(
              theirModification,
              bestAncestorCommitIdMode,
              bestAncestorCommitIdBlobId,
              bestAncestorCommitIdContent
            )

          case (
                path,
                (
                  Some(ourAddition: Change.Addition),
                  Some(theirAddition: Change.Addition)
                )
              ) =>
            for mergedFileMode <-
                if ourAddition.mode == theirAddition.mode then
                  right(ourAddition.mode)
                else
                  left(
                    s"Conflicting file modes for file ${underline(path)}; on our branch head ${underline(ourAddition.mode)} and on their branch head ${underline(theirAddition.mode)}."
                  )
            yield path -> BothContributeAnAddition(
              ourAddition,
              theirAddition,
              mergedFileMode
            )

          case (
                path,
                (
                  Some(ourModification: Change.Modification),
                  Some(theirModification: Change.Modification)
                )
              ) =>
            for
              (
                bestAncestorCommitIdMode,
                bestAncestorCommitIdBlobId,
                bestAncestorCommitIdContent
              ) <- blobAndContentFor(bestAncestorCommitId)(path)
              mergedFileMode <-
                if bestAncestorCommitIdMode == ourModification.mode then
                  right(theirModification.mode)
                else if bestAncestorCommitIdMode == theirModification.mode then
                  right(ourModification.mode)
                else if ourModification.mode == theirModification.mode then
                  right(ourModification.mode)
                else
                  left(
                    s"Conflicting file modes for file ${underline(path)}; on best ancestor commit ${underline(bestAncestorCommitIdMode)}, on our branch head ${underline(ourModification.mode)} and on their branch head ${underline(theirModification.mode)}."
                  )
            yield path -> BothContributeAModification(
              ourModification,
              theirModification,
              bestAncestorCommitIdMode,
              bestAncestorCommitIdBlobId,
              bestAncestorCommitIdContent,
              mergedFileMode
            )

          case (
                path,
                (
                  Some(Change.Deletion),
                  Some(Change.Deletion)
                )
              ) =>
            for (
                _,
                _,
                bestAncestorCommitIdContent
              ) <- blobAndContentFor(bestAncestorCommitId)(path)
            yield path -> BothContributeADeletion(bestAncestorCommitIdContent)
        }
    end mergeInputsOf

    def mergeWithRollback(
        ourBranchHead: String @@ Main.Tags.CommitOrBranchName,
        theirBranchHead: String @@ Main.Tags.CommitOrBranchName,
        bestAncestorCommitId: String @@ Main.Tags.CommitOrBranchName,
        mergeInputs: List[(Path, MergeInput)],
        noCommit: Boolean,
        noFastForward: Boolean,
        configuration: Configuration
    ): Workflow[Int @@ Tags.ExitCode] =
      val workflow =
        for
          indexUpdates <- indexUpdates(
            bestAncestorCommitId,
            ourBranchHead,
            theirBranchHead,
            configuration
          )(mergeInputs)

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
                _ <- right(()).logOperation(
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
                theirCommitId <- theirCommitId(theirBranchHead)
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
                )
                _ <- right(()).logOperation(
                  if goodForAMergeCommit then
                    "Successful merge, leaving merged changes in the index for review..."
                  else
                    "Merge conflicts found, handing over for manual resolution..."
                )
              yield conflictedMerge
            end if
        yield exitCodeWhenThereAreNoUnexpectedErrors

      val workflowWithWorkaround =
        for
          payload <- workflow
          _ <- IO {
            // Do this to work around the issue mentioned here:
            // https://stackoverflow.com/questions/51146392/cannot-git-merge-abort-until-git-status,
            // this has been observed when `git merge-file` successfully writes
            // a non-conflicted file after Kinetic Merge has reported a conflict
            // for that file.
            os.proc("git", "status")
              .call(workingDirectory)
          }.labelExceptionWith(errorMessage =
            s"Unexpected error: could not check the status of the working tree."
          )
        yield payload

      // NASTY HACK: hokey cleanup, need to think about the best approach...
      workflowWithWorkaround.leftMap(label =>
        try os.proc("git", "reset", "--hard").call(workingDirectory)
        catch
          case exception =>
            println(s"Failed to rollback changes after unexpected error.")
        end try

        label
      )
    end mergeWithRollback

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

    private def indexUpdates(
        bestAncestorCommitId: String @@ Tags.CommitOrBranchName,
        ourBranchHead: String @@ Tags.CommitOrBranchName,
        theirBranchHead: String @@ Tags.CommitOrBranchName,
        configuration: Configuration
    )(
        mergeInputs: List[(Path, MergeInput)]
    ): Workflow[List[IndexState]] =
      given Eq[Token]     = Token.equality
      given Order[Token]  = Token.comparison
      given Funnel[Token] = Token.funnel
      given HashFunction  = Hashing.murmur3_32_fixed()

      for
        (baseContentsByPath, leftContentsByPath, rightContentsByPath) <-
          mergeInputs.foldM(
            (
              Map.empty[Path, IndexedSeq[Token]],
              Map.empty[Path, IndexedSeq[Token]],
              Map.empty[Path, IndexedSeq[Token]]
            )
          ) {
            case (
                  (baseContentsByPath, leftContentsByPath, rightContentsByPath),
                  (path, mergeInput)
                ) =>
              mergeInput match
                case JustOurModification(
                      ourModification,
                      bestAncestorCommitIdContent
                    ) =>
                  val unchangedContent = tokens(bestAncestorCommitIdContent).get

                  right(
                    (
                      baseContentsByPath + (path -> unchangedContent),
                      leftContentsByPath + (path -> tokens(
                        ourModification.content
                      ).get),
                      rightContentsByPath + (path -> unchangedContent)
                    )
                  )

                case JustTheirModification(
                      theirModification,
                      bestAncestorCommitIdContent
                    ) =>
                  val unchangedContent = tokens(bestAncestorCommitIdContent).get

                  right(
                    (
                      baseContentsByPath + (path -> unchangedContent),
                      leftContentsByPath + (path -> unchangedContent),
                      rightContentsByPath + (path -> tokens(
                        theirModification.content
                      ).get)
                    )
                  )

                case JustOurAddition(ourAddition) =>
                  right(
                    (
                      baseContentsByPath,
                      leftContentsByPath + (path -> tokens(
                        ourAddition.content
                      ).get),
                      rightContentsByPath
                    )
                  )

                case JustTheirAddition(theirAddition) =>
                  right(
                    (
                      baseContentsByPath,
                      leftContentsByPath,
                      rightContentsByPath + (path -> tokens(
                        theirAddition.content
                      ).get)
                    )
                  )

                case JustOurDeletion(bestAncestorCommitIdContent) =>
                  val unchangedContent = tokens(bestAncestorCommitIdContent).get

                  right(
                    (
                      baseContentsByPath + (path -> unchangedContent),
                      leftContentsByPath,
                      rightContentsByPath + (path -> unchangedContent)
                    )
                  )

                case JustTheirDeletion(bestAncestorCommitIdContent) =>
                  val unchangedContent = tokens(bestAncestorCommitIdContent).get

                  right(
                    (
                      baseContentsByPath + (path -> unchangedContent),
                      leftContentsByPath + (path -> unchangedContent),
                      rightContentsByPath
                    )
                  )

                case OurModificationAndTheirDeletion(
                      ourModification,
                      _,
                      _,
                      bestAncestorCommitIdContent
                    ) =>
                  right(
                    (
                      baseContentsByPath + (path -> tokens(
                        bestAncestorCommitIdContent
                      ).get),
                      leftContentsByPath + (path -> tokens(
                        ourModification.content
                      ).get),
                      rightContentsByPath
                    )
                  )

                case TheirModificationAndOurDeletion(
                      theirModification,
                      _,
                      _,
                      bestAncestorCommitIdContent
                    ) =>
                  right(
                    (
                      baseContentsByPath + (path -> tokens(
                        bestAncestorCommitIdContent
                      ).get),
                      leftContentsByPath,
                      rightContentsByPath + (path -> tokens(
                        theirModification.content
                      ).get)
                    )
                  )

                case BothContributeAnAddition(
                      ourAddition,
                      theirAddition,
                      _
                    ) =>
                  right(
                    (
                      baseContentsByPath,
                      leftContentsByPath + (path -> tokens(
                        ourAddition.content
                      ).get),
                      rightContentsByPath + (path -> tokens(
                        theirAddition.content
                      ).get)
                    )
                  )

                case BothContributeAModification(
                      ourModification,
                      theirModification,
                      _,
                      _,
                      bestAncestorCommitIdContent,
                      _
                    ) =>
                  right(
                    (
                      baseContentsByPath + (path -> tokens(
                        bestAncestorCommitIdContent
                      ).get),
                      leftContentsByPath + (path -> tokens(
                        ourModification.content
                      ).get),
                      rightContentsByPath + (path -> tokens(
                        theirModification.content
                      ).get)
                    )
                  )

                case BothContributeADeletion(bestAncestorCommitIdContent) =>
                  right(
                    (
                      baseContentsByPath + (path -> tokens(
                        bestAncestorCommitIdContent
                      ).get),
                      leftContentsByPath,
                      rightContentsByPath
                    )
                  )
          }

        codeMotionAnalysis: CodeMotionAnalysis[Path, Token] <- EitherT
          .fromEither[WorkflowLogWriter] {

            CodeMotionAnalysis.of(
              base = MappedContentSourcesOfTokens(
                baseContentsByPath,
                label = s"BASE: ${bestAncestorCommitId.take(8)}"
              ),
              left = MappedContentSourcesOfTokens(
                leftContentsByPath,
                label = s"OURS: $ourBranchHead"
              ),
              right = MappedContentSourcesOfTokens(
                rightContentsByPath,
                label = s"THEIRS: $theirBranchHead"
              )
            )(configuration)
          }
          .leftMap(_.toString.taggedWith[Tags.ErrorMessage])

        (mergeResultsByPath, moveDestinationsReport) = codeMotionAnalysis.merge

        _ <- moveDestinationsReport.summarizeInText.foldLeft(right(()))(
          _ logOperation _
        )

        indexStates: List[Option[IndexState]] <-
          mergeInputs.traverse { case (path, mergeInput) =>
            mergeInput match
              case JustOurModification(ourModification, _) =>
                val FullyMerged(tokens) = mergeResultsByPath(path): @unchecked

                val mergedFileContent = reconstituteTextFrom(tokens)

                val ourModificationWasTweakedByTheMerge =
                  mergedFileContent != ourModification.content

                if ourModificationWasTweakedByTheMerge then
                  for
                    blobId <- storeBlobFor(path, mergedFileContent)
                    _ <- restoreFileFromBlobId(
                      path,
                      blobId
                    )
                    _ <- recordModificationInIndex(
                      path,
                      ourModification.mode,
                      blobId
                    )
                  yield Some(IndexState.OneEntry)
                else right(None)
                end if

              case JustTheirModification(theirModification, _) =>
                val FullyMerged(tokens) = mergeResultsByPath(path): @unchecked

                val mergedFileContent = reconstituteTextFrom(tokens)

                val theirModificationWasTweakedByTheMerge =
                  mergedFileContent != theirModification.content

                if theirModificationWasTweakedByTheMerge then
                  for
                    blobId <- storeBlobFor(path, mergedFileContent)
                    _ <- restoreFileFromBlobId(
                      path,
                      blobId
                    )
                    _ <- recordModificationInIndex(
                      path,
                      theirModification.mode,
                      blobId
                    )
                  yield Some(IndexState.OneEntry)
                else
                  for
                    _ <- restoreFileFromBlobId(
                      path,
                      theirModification.blobId
                    )
                    _ <- recordModificationInIndex(
                      path,
                      theirModification.mode,
                      theirModification.blobId
                    )
                  yield Some(IndexState.OneEntry)
                end if

              case JustOurAddition(ourAddition) =>
                val FullyMerged(tokens) = mergeResultsByPath(path): @unchecked

                val mergedFileContent = reconstituteTextFrom(tokens)

                val ourAdditionWasTweakedByTheMerge =
                  mergedFileContent != ourAddition.content

                if ourAdditionWasTweakedByTheMerge then
                  for
                    blobId <- storeBlobFor(path, mergedFileContent)
                    _ <- restoreFileFromBlobId(
                      path,
                      blobId
                    )
                    _ <- recordModificationInIndex(
                      path,
                      ourAddition.mode,
                      blobId
                    )
                  yield Some(IndexState.OneEntry)
                else right(None)
                end if

              case JustTheirAddition(theirAddition) =>
                val FullyMerged(tokens) = mergeResultsByPath(path): @unchecked

                val mergedFileContent = reconstituteTextFrom(tokens)

                val theirAdditionWasTweakedByTheMerge =
                  mergedFileContent != theirAddition.content

                if theirAdditionWasTweakedByTheMerge then
                  for
                    blobId <- storeBlobFor(path, mergedFileContent)
                    _ <- restoreFileFromBlobId(
                      path,
                      blobId
                    )
                    _ <- recordAdditionInIndex(
                      path,
                      theirAddition.mode,
                      blobId
                    )
                  yield Some(IndexState.OneEntry)
                else
                  for
                    _ <- restoreFileFromBlobId(
                      path,
                      theirAddition.blobId
                    )
                    _ <- recordAdditionInIndex(
                      path,
                      theirAddition.mode,
                      theirAddition.blobId
                    )
                  yield Some(IndexState.OneEntry)
                end if

              case JustOurDeletion(_) =>
                right(None)

              case JustTheirDeletion(_) =>
                for
                  _ <- recordDeletionInIndex(path)
                  _ <- deleteFile(path)
                yield Some(IndexState.OneEntry)

              case OurModificationAndTheirDeletion(
                    ourModification,
                    bestAncestorCommitIdMode,
                    bestAncestorCommitIdBlobId,
                    _
                  ) =>
                val tokens = mergeResultsByPath(path) match
                  case FullyMerged(mergedTokens)               => mergedTokens
                  case MergedWithConflicts(ourMergedTokens, _) =>
                    // We don't care about their view of the merge - their side
                    // simply deleted the whole file, so it contributes nothing
                    // interesting to the merge; the only point of the merge
                    // here was to pick up propagated edits / deletions and to
                    // note move destinations.
                    ourMergedTokens

                val mergedFileContent = reconstituteTextFrom(tokens)
                val ourModificationWasTweakedByTheMerge =
                  mergedFileContent != ourModification.content

                for
                  - <- recordDeletionInIndex(path)
                  - <- recordConflictModificationInIndex(
                    stageIndex = bestCommonAncestorStageIndex
                  )(
                    bestAncestorCommitId,
                    path,
                    bestAncestorCommitIdMode,
                    bestAncestorCommitIdBlobId
                  )
                  indexState <-
                    if ourModificationWasTweakedByTheMerge then
                      if mergedFileContent.nonEmpty then
                        for
                          blobId <- storeBlobFor(path, mergedFileContent)
                          _ <- restoreFileFromBlobId(
                            path,
                            blobId
                          )
                          _ <- recordConflictModificationInIndex(
                            stageIndex = ourStageIndex
                          )(
                            ourBranchHead,
                            path,
                            ourModification.mode,
                            blobId
                          ).logOperation(
                            s"Conflict - file ${underline(path)} was modified on our branch ${underline(ourBranchHead)} and deleted on their branch ${underline(theirBranchHead)}."
                          )
                        yield IndexState.ConflictingEntries
                      else
                        for
                          _ <- recordDeletionInIndex(path)
                          _ <- deleteFile(path)
                        yield IndexState.OneEntry
                    else
                      // The modified file would have been present on our
                      // branch; given that we started with a clean working
                      // directory tree, we just leave it there to match what
                      // Git merge does.
                      for _ <- recordConflictModificationInIndex(
                          stageIndex = ourStageIndex
                        )(
                          ourBranchHead,
                          path,
                          ourModification.mode,
                          ourModification.blobId
                        ).logOperation(
                          s"Conflict - file ${underline(path)} was modified on our branch ${underline(ourBranchHead)} and deleted on their branch ${underline(theirBranchHead)}."
                        )
                      yield IndexState.ConflictingEntries
                yield Some(indexState)
                end for

              case TheirModificationAndOurDeletion(
                    theirModification,
                    bestAncestorCommitIdMode,
                    bestAncestorCommitIdBlobId,
                    _
                  ) =>
                val tokens = mergeResultsByPath(path) match
                  case FullyMerged(mergedTokens)                 => mergedTokens
                  case MergedWithConflicts(_, theirMergedTokens) =>
                    // We don't care about our view of the merge - our side
                    // simply deleted the whole file, so it contributes nothing
                    // interesting to the merge; the only point of the merge
                    // here was to pick up propagated edits / deletions and to
                    // note move destinations.
                    theirMergedTokens

                val mergedFileContent = reconstituteTextFrom(tokens)
                val theirModificationWasTweakedByTheMerge =
                  mergedFileContent != theirModification.content

                for
                  _ <- recordDeletionInIndex(path)
                  _ <- recordConflictModificationInIndex(
                    stageIndex = bestCommonAncestorStageIndex
                  )(
                    bestAncestorCommitId,
                    path,
                    bestAncestorCommitIdMode,
                    bestAncestorCommitIdBlobId
                  )
                  indexState <-
                    // Git's merge updates the working directory tree with
                    // *their* modified file which wouldn't have been present on
                    // our branch prior to the merge. So that's what we do too.
                    if theirModificationWasTweakedByTheMerge then
                      if mergedFileContent.nonEmpty then
                        for
                          blobId <- storeBlobFor(path, mergedFileContent)
                          _ <- restoreFileFromBlobId(
                            path,
                            blobId
                          )
                          _ <- recordConflictModificationInIndex(
                            stageIndex = theirStageIndex
                          )(
                            theirBranchHead,
                            path,
                            theirModification.mode,
                            blobId
                          ).logOperation(
                            s"Conflict - file ${underline(path)} was deleted on our branch ${underline(ourBranchHead)} and modified on their branch ${underline(theirBranchHead)}."
                          )
                        yield IndexState.ConflictingEntries
                      else
                        for
                          _ <- recordDeletionInIndex(path)
                          _ <- deleteFile(path)
                        yield IndexState.OneEntry
                    else
                      for
                        _ <- restoreFileFromBlobId(
                          path,
                          theirModification.blobId
                        )
                        _ <- recordConflictModificationInIndex(
                          stageIndex = theirStageIndex
                        )(
                          theirBranchHead,
                          path,
                          theirModification.mode,
                          theirModification.blobId
                        ).logOperation(
                          s"Conflict - file ${underline(path)} was deleted on our branch ${underline(ourBranchHead)} and modified on their branch ${underline(theirBranchHead)}."
                        )
                      yield IndexState.ConflictingEntries
                yield Some(indexState)
                end for

              case BothContributeAnAddition(_, _, mergedFileMode) =>
                mergeResultsByPath(path) match
                  case FullyMerged(tokens) =>
                    val mergedFileContent = reconstituteTextFrom(tokens)

                    for
                      blobId <- storeBlobFor(path, mergedFileContent)
                      _ <- restoreFileFromBlobId(
                        path,
                        blobId
                      )
                      _ <- recordModificationInIndex(
                        path,
                        mergedFileMode,
                        blobId
                      )
                    yield Some(IndexState.OneEntry)
                    end for

                  case MergedWithConflicts(leftTokens, rightTokens) =>
                    val leftContent  = reconstituteTextFrom(leftTokens)
                    val rightContent = reconstituteTextFrom(rightTokens)

                    for
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

                      lastMinuteResolution <-
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

                        if 0 <= exitCode then right(0 == exitCode)
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
                        mergedFileMode,
                        leftBlob
                      )
                      _ <- recordConflictModificationInIndex(
                        stageIndex = theirStageIndex
                      )(
                        theirBranchHead,
                        path,
                        mergedFileMode,
                        rightBlob
                      ).logOperation(
                        s"Conflict - file ${underline(path)} was added on our branch ${underline(
                            ourBranchHead
                          )} and added on their branch ${underline(theirBranchHead)}${lastMinuteResolutionNotes(lastMinuteResolution)}."
                      )
                    yield Some(IndexState.ConflictingEntries)
                    end for

              case BothContributeAModification(
                    _,
                    _,
                    bestAncestorCommitIdMode,
                    bestAncestorCommitIdBlobId,
                    bestAncestorCommitIdContent,
                    mergedFileMode
                  ) =>
                mergeResultsByPath(path) match
                  case FullyMerged(tokens) =>
                    val mergedFileContent = reconstituteTextFrom(tokens)

                    for
                      blobId <- storeBlobFor(path, mergedFileContent)
                      _ <- restoreFileFromBlobId(
                        path,
                        blobId
                      )
                      _ <- recordModificationInIndex(
                        path,
                        mergedFileMode,
                        blobId
                      )
                    yield Some(IndexState.OneEntry)
                    end for

                  case MergedWithConflicts(leftTokens, rightTokens) =>
                    val leftContent  = reconstituteTextFrom(leftTokens)
                    val rightContent = reconstituteTextFrom(rightTokens)

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

                      lastMinuteResolution <-
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
                            baseTemporaryFile,
                            rightTemporaryFile
                          ).call(workingDirectory, check = false)
                            .exitCode

                        if 0 <= exitCode then right(0 == exitCode)
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
                        mergedFileMode,
                        leftBlob
                      )
                      _ <- recordConflictModificationInIndex(
                        stageIndex = theirStageIndex
                      )(
                        theirBranchHead,
                        path,
                        mergedFileMode,
                        rightBlob
                      ).logOperation(
                        s"Conflict - file ${underline(path)} was modified on our branch ${underline(
                            ourBranchHead
                          )} and modified on their branch ${underline(theirBranchHead)}${lastMinuteResolutionNotes(lastMinuteResolution)}."
                      )
                    yield Some(IndexState.ConflictingEntries)
                    end for

              case BothContributeADeletion(_) =>
                // We already have the deletion in our branch, so no need to
                // update the index. We do yield a result so that there is still
                // a merge commit if this is the only change, though - this
                // should *not* be a fast-forward merge.
                right(Some(IndexState.OneEntry))
                  .logOperation(
                    s"Coincidental deletion of file ${underline(path)} on our branch ${underline(ourBranchHead)} and on their branch ${underline(theirBranchHead)}."
                  )
          }
      yield indexStates.flatten
      end for
    end indexUpdates

    private def deleteFile(path: Path): Workflow[Unit] = IO {
      os.remove(path): Unit
    }.labelExceptionWith(errorMessage =
      s"Unexpected error: could not update working directory tree by deleting file ${underline(path)}."
    )

    private def restoreFileFromBlobId(
        path: Path,
        blobId: String @@ Main.Tags.BlobId
    ) =
      IO {
        os.write.over(
          path,
          os.proc(
            "git",
            "cat-file",
            "blob",
            blobId
          ).spawn(workingDirectory)
            .stdout,
          createFolders = true
        )
      }.labelExceptionWith(errorMessage =
        s"Unexpected error: could not update working directory tree with file ${underline(path)}."
      )

    private def reconstituteTextFrom(
        tokens: IndexedSeq[Token]
    ): String @@ Main.Tags.Content =
      tokens.map(_.text).mkString.taggedWith[Tags.Content]

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

    private def lastMinuteResolutionNotes(lastMinuteResolution: Boolean) =
      if lastMinuteResolution then
        ", but was resolved trivially when creating the conflicted file - leaving it marked as unresolved for now"
      else ""

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
    end recordAdditionInIndex
  end InWorkingDirectory

  private enum IndexState:
    case OneEntry
    case ConflictingEntries
  end IndexState

  object Tags:
    trait Mode
    trait BlobId
    trait Content
    trait CommitOrBranchName
    trait ErrorMessage
    trait ExitCode
    trait StageIndex
  end Tags

end Main
