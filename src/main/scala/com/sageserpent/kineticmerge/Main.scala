package com.sageserpent.kineticmerge

import cats.Order
import cats.data.{EitherT, WriterT}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.foldable.toFoldableOps
import cats.syntax.traverse.toTraverseOps
import com.google.common.hash.{Funnel, HashFunction, Hashing}
import com.sageserpent.kineticmerge.Main.MergeInput.*
import com.sageserpent.kineticmerge.core.*
import com.sageserpent.kineticmerge.core.CodeMotionAnalysis.Configuration
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisExtension.*
import com.sageserpent.kineticmerge.core.Token.tokens
import com.softwaremill.tagging.*
import com.typesafe.scalalogging.StrictLogging
import fansi.Str
import os.{Path, RelPath}
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
  private type Workflow[Payload]          =
    EitherT[WorkflowLogWriter, String @@ Tags.ErrorMessage, Payload]
  private val whitespaceRun                                       = "\\s+"
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
            "Do not commit a successful merge - leave merged changes staged in the index for review. Off by default."
          ),
        opt[Unit](name = "no-ff")
          .action((noFastForward, commandLineArguments) =>
            commandLineArguments.copy(noFastForward = true)
          )
          .text(
            "Prevent fast-forward merge - make a merge commit instead. Off by default."
          ),
        opt[Int](name = "minimum-match-size")
          .validate(minimumMatchSize =>
            if 0 < minimumMatchSize then success
            else failure(s"Minimum match size must be positive.")
          )
          .action((minimumMatchSize, commandLineArguments) =>
            commandLineArguments
              .copy(minimumMatchSize = minimumMatchSize)
          )
          .text(
            s"Minimum number of tokens for a match to be considered. Default of ${ApplicationRequest.default.minimumMatchSize}."
          ),
        opt[String](name = "match-threshold")
          .validate(matchThreshold =>
            if matchThresholdRegex.matches(matchThreshold) then success
            else
              failure(
                s"Match threshold ${underline(matchThreshold)} must be a percentage, or digits to the right of the decimal point of a non-negative fraction less than one, or a non-negative fraction at most one."
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
            s"Minimum fraction of a containing file's size for a section of text to qualify for matching; zero implying no restriction. Default of ${ApplicationRequest.default.thresholdSizeFractionForMatching}."
          ),
        opt[Int](name = "minimum-ambiguous-match-size")
          .validate(minimumAmbiguousMatchSize =>
            if 0 <= minimumAmbiguousMatchSize then success
            else
              failure(
                "Minimum ambiguous match size must be zero or positive."
              )
          )
          .action((minimumAmbiguousMatchSize, commandLineArguments) =>
            commandLineArguments
              .copy(minimumAmbiguousMatchSize = minimumAmbiguousMatchSize)
          )
          .text(
            s"Minimum number of tokens for an ambiguous match to be considered. Default of ${ApplicationRequest.default.minimumAmbiguousMatchSize}."
          ),
        opt[Int](name = "ambiguous-matches-threshold")
          .validate(ambiguousMatchesThreshold =>
            if 0 < ambiguousMatchesThreshold then success
            else failure(s"Ambiguous matches threshold must be positive.")
          )
          .action((ambiguousMatchesThreshold, commandLineArguments) =>
            commandLineArguments
              .copy(ambiguousMatchesThreshold = ambiguousMatchesThreshold)
          )
          .text(
            s"Maximum number of matches of the same kind that can refer to the same matched content. Default of ${ApplicationRequest.default.ambiguousMatchesThreshold}."
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

    val applicationRequest: Try[Option[ApplicationRequest]] = Try {
      OParser
        .parse(
          parser,
          commandLineArguments,
          ApplicationRequest.default,
          new DefaultOEffectSetup:
            // Don't terminate the application, let execution return back to the
            // caller via a glorified long-jump.
            override def terminate(exitState: Either[String, Unit]): Unit =
              throw EarlyTermination(exitState match
                case Left(_)  => 1.taggedWith[Tags.ExitCode]
                case Right(_) => 0.taggedWith[Tags.ExitCode])
        )
    }

    applicationRequest.fold(
      {
        case EarlyTermination(exitCode) => exitCode
        case exception: Exception       =>
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
      ambiguousMatchesThreshold,
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

      _ <- inTopLevelWorkingDirectory.theirCommitId(theirBranchHead)

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
              ourBranchHead,
              bestAncestorCommitId,
              possessive = "our"
            )

            theirChanges <- inTopLevelWorkingDirectory.changes(
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
                bestAncestorCommitId,
                ourBranchHead,
                theirBranchHead,
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
      theirBranchHead: String @@ Main.Tags.CommitOrBranchName,
      noCommit: Boolean,
      noFastForward: Boolean,
      minimumMatchSize: Int,
      thresholdSizeFractionForMatching: Double,
      minimumAmbiguousMatchSize: Int,
      ambiguousMatchesThreshold: Int
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
        bestAncestorCommitIdMode: String @@ Tags.Mode,
        bestAncestorCommitIdBlobId: String @@ Tags.BlobId,
        bestAncestorCommitIdContent: String @@ Tags.Content
    )
    case JustTheirModification(
        theirModification: Change.Modification,
        bestAncestorCommitIdMode: String @@ Tags.Mode,
        bestAncestorCommitIdBlobId: String @@ Tags.BlobId,
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

  private case class EarlyTermination(exitCode: Int @@ Tags.ExitCode)
      extends RuntimeException

  private case class InWorkingDirectory(
      workingDirectory: Path
  ):
    private val numberOfDigitsForShortFormOfCommitId = 8

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
        _.traverse(pathChangeFor(branchOrCommit))
      ).map(_.toMap)

    private def pathChangeFor(
        commitIdOrBranchName: String @@ Tags.CommitOrBranchName
    )(
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
                bestAncestorCommitIdMode,
                bestAncestorCommitIdBlobId,
                bestAncestorCommitIdContent
              ) <- blobAndContentFor(bestAncestorCommitId)(path)
            yield path -> JustOurModification(
              ourModification,
              bestAncestorCommitIdMode,
              bestAncestorCommitIdBlobId,
              bestAncestorCommitIdContent
            )

          case (
                path,
                (None, Some(theirModification: Change.Modification))
              ) =>
            for (
                bestAncestorCommitIdMode,
                bestAncestorCommitIdBlobId,
                bestAncestorCommitIdContent
              ) <- blobAndContentFor(bestAncestorCommitId)(path)
            yield path -> JustTheirModification(
              theirModification,
              bestAncestorCommitIdMode,
              bestAncestorCommitIdBlobId,
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
              )              <- blobAndContentFor(bestAncestorCommitId)(path)
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
        bestAncestorCommitId: String @@ Tags.CommitOrBranchName,
        ourBranchHead: String @@ Tags.CommitOrBranchName,
        theirBranchHead: String @@ Tags.CommitOrBranchName,
        mergeInputs: List[(Path, MergeInput)],
        noCommit: Boolean,
        noFastForward: Boolean,
        configuration: Configuration
    ): Workflow[Int @@ Tags.ExitCode] =
      val workflow =
        for
          goodForAMergeCommit <- indexUpdates(
            bestAncestorCommitId,
            ourBranchHead,
            theirBranchHead,
            configuration
          )(mergeInputs)

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
                _             <- IO {
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
          _       <- IO {
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
        os.proc(
          "git",
          "rev-parse",
          "--verify",
          "--end-of-options",
          theirBranchHead
        ).call(workingDirectory)
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
    ): Workflow[Boolean] =
      given Order[Token]  = Token.comparison
      given Funnel[Token] = Token.funnel
      given HashFunction  = Hashing.murmur3_32_fixed()

      val (
        baseContentsByPath,
        leftContentsByPath,
        rightContentsByPath,
        newPathsOnLeftOrRight
      ) =
        mergeInputs.foldLeft(
          (
            Map.empty[Path, IndexedSeq[Token]],
            Map.empty[Path, IndexedSeq[Token]],
            Map.empty[Path, IndexedSeq[Token]],
            Set.empty[Path]
          )
        ) {
          case (
                (
                  baseContentsByPath,
                  leftContentsByPath,
                  rightContentsByPath,
                  newPathsOnLeftOrRight
                ),
                (path, mergeInput)
              ) =>
            mergeInput match
              case JustOurModification(
                    ourModification,
                    _,
                    _,
                    bestAncestorCommitIdContent
                  ) =>
                val unchangedContent = tokens(bestAncestorCommitIdContent).get

                (
                  baseContentsByPath + (path -> unchangedContent),
                  leftContentsByPath + (path -> tokens(
                    ourModification.content
                  ).get),
                  rightContentsByPath + (path -> unchangedContent),
                  newPathsOnLeftOrRight
                )

              case JustTheirModification(
                    theirModification,
                    _,
                    _,
                    bestAncestorCommitIdContent
                  ) =>
                val unchangedContent = tokens(bestAncestorCommitIdContent).get

                (
                  baseContentsByPath + (path  -> unchangedContent),
                  leftContentsByPath + (path  -> unchangedContent),
                  rightContentsByPath + (path -> tokens(
                    theirModification.content
                  ).get),
                  newPathsOnLeftOrRight
                )

              case JustOurAddition(ourAddition) =>
                (
                  baseContentsByPath,
                  leftContentsByPath + (path -> tokens(
                    ourAddition.content
                  ).get),
                  rightContentsByPath,
                  newPathsOnLeftOrRight + path
                )

              case JustTheirAddition(theirAddition) =>
                (
                  baseContentsByPath,
                  leftContentsByPath,
                  rightContentsByPath + (path -> tokens(
                    theirAddition.content
                  ).get),
                  newPathsOnLeftOrRight + path
                )

              case JustOurDeletion(bestAncestorCommitIdContent) =>
                val unchangedContent = tokens(bestAncestorCommitIdContent).get

                (
                  baseContentsByPath + (path -> unchangedContent),
                  leftContentsByPath,
                  rightContentsByPath + (path -> unchangedContent),
                  newPathsOnLeftOrRight
                )

              case JustTheirDeletion(bestAncestorCommitIdContent) =>
                val unchangedContent = tokens(bestAncestorCommitIdContent).get

                (
                  baseContentsByPath + (path -> unchangedContent),
                  leftContentsByPath + (path -> unchangedContent),
                  rightContentsByPath,
                  newPathsOnLeftOrRight
                )

              case OurModificationAndTheirDeletion(
                    ourModification,
                    _,
                    _,
                    bestAncestorCommitIdContent
                  ) =>
                (
                  baseContentsByPath + (path -> tokens(
                    bestAncestorCommitIdContent
                  ).get),
                  leftContentsByPath + (path -> tokens(
                    ourModification.content
                  ).get),
                  rightContentsByPath,
                  newPathsOnLeftOrRight
                )

              case TheirModificationAndOurDeletion(
                    theirModification,
                    _,
                    _,
                    bestAncestorCommitIdContent
                  ) =>
                (
                  baseContentsByPath + (path -> tokens(
                    bestAncestorCommitIdContent
                  ).get),
                  leftContentsByPath,
                  rightContentsByPath + (path -> tokens(
                    theirModification.content
                  ).get),
                  newPathsOnLeftOrRight
                )

              case BothContributeAnAddition(
                    ourAddition,
                    theirAddition,
                    _
                  ) =>
                (
                  baseContentsByPath,
                  leftContentsByPath + (path -> tokens(
                    ourAddition.content
                  ).get),
                  rightContentsByPath + (path -> tokens(
                    theirAddition.content
                  ).get),
                  newPathsOnLeftOrRight + path
                )

              case BothContributeAModification(
                    ourModification,
                    theirModification,
                    _,
                    _,
                    bestAncestorCommitIdContent,
                    _
                  ) =>
                (
                  baseContentsByPath + (path -> tokens(
                    bestAncestorCommitIdContent
                  ).get),
                  leftContentsByPath + (path -> tokens(
                    ourModification.content
                  ).get),
                  rightContentsByPath + (path -> tokens(
                    theirModification.content
                  ).get),
                  newPathsOnLeftOrRight
                )

              case BothContributeADeletion(bestAncestorCommitIdContent) =>
                (
                  baseContentsByPath + (path -> tokens(
                    bestAncestorCommitIdContent
                  ).get),
                  leftContentsByPath,
                  rightContentsByPath,
                  newPathsOnLeftOrRight
                )
        }

      val baseSources = MappedContentSourcesOfTokens(
        baseContentsByPath,
        label =
          s"BASE: ${bestAncestorCommitId.take(numberOfDigitsForShortFormOfCommitId)}"
      )

      val leftSources = MappedContentSourcesOfTokens(
        leftContentsByPath,
        label = s"OURS: $ourBranchHead"
      )

      val rightSources = MappedContentSourcesOfTokens(
        rightContentsByPath,
        label = s"THEIRS: $theirBranchHead"
      )

      case class AccumulatedMergeState(
          goodForAMergeCommit: Boolean,
          deletedPathsByLeftRenamePath: Map[Path, Path],
          deletedPathsByRightRenamePath: Map[Path, Path],
          conflictingDeletedPathsByLeftRenamePath: Map[Path, Path],
          conflictingDeletedPathsByRightRenamePath: Map[Path, Path],
          conflictingAdditionPathsAndTheirLastMinuteResolutions: Map[
            Path,
            Boolean
          ]
      ):
        // NOTE: no need to yield an updated `AccumulatedMergeState` with
        // `goodForAMergeCommit` as false - this is done upstream already.
        def reportConflictingAdditionsTakingRenamesIntoAccount: Workflow[Unit] =
          conflictingAdditionPathsAndTheirLastMinuteResolutions.toSeq
            .traverse_ { case (path, lastMinuteResolution) =>
              (
                deletedPathsByLeftRenamePath
                  .get(path)
                  .orElse(conflictingDeletedPathsByLeftRenamePath.get(path)),
                deletedPathsByRightRenamePath
                  .get(path)
                  .orElse(conflictingDeletedPathsByRightRenamePath.get(path))
              ) match
                case (None, None) =>
                  right(()).logOperation(
                    s"Conflict - file ${underline(path)} was added on our branch ${underline(ourBranchHead)} and added on their branch ${underline(theirBranchHead)}${lastMinuteResolutionNotes(lastMinuteResolution)}."
                  )
                case (Some(originalPathRenamedOnTheLeft), None) =>
                  right(()).logOperation(
                    s"Conflict - file ${underline(path)} is a rename on our branch ${underline(ourBranchHead)} of ${underline(originalPathRenamedOnTheLeft)} and added on their branch ${underline(theirBranchHead)}${lastMinuteResolutionNotes(lastMinuteResolution)}."
                  )
                case (None, Some(originalPathRenamedOnTheRight)) =>
                  right(()).logOperation(
                    s"Conflict - file ${underline(path)} was added on our branch ${underline(ourBranchHead)} and is a rename on their branch ${underline(theirBranchHead)} of ${underline(originalPathRenamedOnTheRight)}."
                  )
                case (
                      Some(originalPathRenamedOnTheLeft),
                      Some(originalPathRenamedOnTheRight)
                    ) =>
                  right(()).logOperation(
                    s"Conflict - file ${underline(path)} is a rename on our branch ${underline(ourBranchHead)} of ${underline(originalPathRenamedOnTheLeft)} and is a rename on their branch ${underline(theirBranchHead)} of ${underline(originalPathRenamedOnTheRight)}."
                  )
            }

        def reportLeftRenamesConflictingWithRightDeletions
            : Workflow[AccumulatedMergeState] =
          conflictingDeletedPathsByLeftRenamePath.toSeq
            .foldM(this) {
              case (partialResult, (leftRenamedPath, conflictingDeletedPath)) =>
                for
                  _                 <- recordDeletionInIndex(leftRenamedPath)
                  (mode, blobId, _) <- blobAndContentFor(ourBranchHead)(
                    leftRenamedPath
                  )
                  _ <- recordConflictModificationInIndex(ourStageIndex)(
                    ourBranchHead,
                    leftRenamedPath,
                    mode,
                    blobId
                  ).logOperation(
                    s"Conflict - file ${underline(conflictingDeletedPath)} was renamed on our branch ${underline(ourBranchHead)} to ${underline(leftRenamedPath)} and deleted on their branch ${underline(theirBranchHead)}."
                  )
                yield partialResult.copy(goodForAMergeCommit = false)
            }

        def reportLeftDeletionsConflictingWithRightRenames
            : Workflow[AccumulatedMergeState] =
          conflictingDeletedPathsByRightRenamePath.toSeq
            .foldM(this) {
              case (
                    partialResult,
                    (rightRenamedPath, conflictingDeletedPath)
                  ) =>
                for
                  _                 <- recordDeletionInIndex(rightRenamedPath)
                  (mode, blobId, _) <- blobAndContentFor(theirBranchHead)(
                    rightRenamedPath
                  )
                  _ <- recordConflictModificationInIndex(theirStageIndex)(
                    theirBranchHead,
                    rightRenamedPath,
                    mode,
                    blobId
                  ).logOperation(
                    s"Conflict - file ${underline(conflictingDeletedPath)} was deleted on our branch ${underline(ourBranchHead)} and renamed on their branch ${underline(theirBranchHead)} to ${underline(rightRenamedPath)}."
                  )
                yield partialResult.copy(goodForAMergeCommit = false)
            }
      end AccumulatedMergeState

      object AccumulatedMergeState:
        def initial: AccumulatedMergeState = AccumulatedMergeState(
          goodForAMergeCommit = true,
          deletedPathsByLeftRenamePath = Map.empty,
          deletedPathsByRightRenamePath = Map.empty,
          conflictingDeletedPathsByLeftRenamePath = Map.empty,
          conflictingDeletedPathsByRightRenamePath = Map.empty,
          conflictingAdditionPathsAndTheirLastMinuteResolutions = Map.empty
        )
      end AccumulatedMergeState

      case class FileRenamingReport(
          description: String,
          leftRenamePaths: Set[Path],
          rightRenamePaths: Set[Path]
      )

      def fileRenamingReportUsing(
          codeMotionAnalysis: CodeMotionAnalysis[Path, Token],
          moveDestinationsReport: MoveDestinationsReport[Section[Token]]
      )(path: Path): Option[FileRenamingReport] =
        val baseSections = codeMotionAnalysis.base(path).sections

        val (leftRenamePaths, baseSectionsMovingLeftToNewFiles) =
          baseSections
            .flatMap(baseSection =>
              moveDestinationsReport.moveDestinationsBySources
                .get(baseSection)
                .map(
                  _.allOnTheLeft
                    .map(leftSources.pathFor)
                    .intersect(newPathsOnLeftOrRight)
                    .map(_ -> baseSection)
                )
            )
            .flatten
            .unzip match
            case (paths, sections) => paths.toSet -> sections.toSet

        val (rightRenamePaths, baseSectionsMovingRightToNewFiles) =
          baseSections
            .flatMap(baseSection =>
              moveDestinationsReport.moveDestinationsBySources
                .get(baseSection)
                .map(
                  _.allOnTheRight
                    .map(rightSources.pathFor)
                    .intersect(newPathsOnLeftOrRight)
                    .map(_ -> baseSection)
                )
            )
            .flatten
            .unzip match
            case (paths, sections) => paths.toSet -> sections.toSet

        val baseSectionsThatHaveMovedToNewFiles =
          baseSectionsMovingLeftToNewFiles union baseSectionsMovingRightToNewFiles

        val totalContentSize = baseSections.map(_.size).sum

        val movedContentSize =
          baseSectionsThatHaveMovedToNewFiles.map(_.size).sum

        val enoughContentHasMovedToConsiderAsRenaming =
          baseSectionsThatHaveMovedToNewFiles.nonEmpty && 2 * movedContentSize >= totalContentSize

        Option.when(enoughContentHasMovedToConsiderAsRenaming) {
          val leftRenamingDetail = Option.unless(
            leftRenamePaths.isEmpty
          )(
            s"on our branch ${underline(ourBranchHead)} " ++ (if 1 < leftRenamePaths.size
                                                              then
                                                                s"into files ${leftRenamePaths.map(underline).mkString(", ")}"
                                                              else
                                                                s"to file ${underline(leftRenamePaths.head)}")
          )

          val rightRenamingDetail = Option.unless(
            rightRenamePaths.isEmpty
          )(
            s"on their branch ${underline(theirBranchHead)} " ++ (if 1 < rightRenamePaths.size
                                                                  then
                                                                    s"into files ${rightRenamePaths.map(underline).mkString(", ")}"
                                                                  else
                                                                    s"to file ${underline(rightRenamePaths.head)}")
          )

          val description =
            (leftRenamingDetail, rightRenamingDetail) match
              case (Some(leftDetailPayload), None) =>
                s"File ${underline(path)} was renamed $leftDetailPayload."
              case (None, Some(rightDetailPayload)) =>
                s"File ${underline(path)} was renamed $rightDetailPayload."
              case (
                    Some(leftDetailPayload),
                    Some(rightDetailPayload)
                  ) =>
                s"File ${underline(path)} was renamed $leftDetailPayload and $rightDetailPayload."
            end match
          end description

          FileRenamingReport(
            description,
            leftRenamePaths,
            rightRenamePaths
          )
        }
      end fileRenamingReportUsing

      def lastMinuteResolution(
          path: Path,
          baseFile: Path,
          leftFile: Path,
          rightFile: Path,
          baseLabel: String,
          leftLabel: String,
          rightLabel: String
      ) =
        val exitCode =
          os.proc(
            "git",
            "merge-file",
            "--diff3",
            "-L",
            leftLabel,
            "-L",
            baseLabel,
            "-L",
            rightLabel,
            leftFile,
            baseFile,
            rightFile
          ).call(workingDirectory, check = false)
            .exitCode

        if 0 <= exitCode then right(0 == exitCode)
        else
          left(
            s"Unexpected error: could not generate conflicted file contents on behalf of ${underline(path)} in temporary file ${underline(leftFile)}"
          )
        end if
      end lastMinuteResolution

      for
        codeMotionAnalysis: CodeMotionAnalysis[Path, Token] <- EitherT
          .fromEither[WorkflowLogWriter] {
            CodeMotionAnalysis.of(baseSources, leftSources, rightSources)(
              configuration
            )
          }
          .leftMap(_.toString.taggedWith[Tags.ErrorMessage])

        (mergeResultsByPath, moveDestinationsReport) = codeMotionAnalysis.merge

        _ <- moveDestinationsReport.summarizeInText.foldLeft(right(()))(
          _ logOperation _
        )

        fileRenamingReport = fileRenamingReportUsing(
          codeMotionAnalysis,
          moveDestinationsReport
        )

        accumulatedMergeState <- mergeInputs.foldM(
          AccumulatedMergeState.initial
        ) { case (partialResult, (path, mergeInput)) =>
          def recordConflictedMergeOfAddedFile(
              partialResult: AccumulatedMergeState,
              path: Path,
              mode: String @@ Tags.Mode,
              leftContent: String @@ Tags.Content,
              rightContent: String @@ Tags.Content
          ) =
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

              lastMinuteResolution <- lastMinuteResolution(
                path,
                fakeBaseTemporaryFile,
                leftTemporaryFile,
                rightTemporaryFile,
                baseLabel = bestAncestorCommitId,
                leftLabel = ourBranchHead,
                rightLabel = theirBranchHead
              )
              _ <- IO {
                os.copy.over(leftTemporaryFile, path)
              }.labelExceptionWith(errorMessage =
                s"Unexpected error: could not copy results of conflicted merge in ${underline(leftTemporaryFile)} to working directory tree file ${underline(path)}."
              )

              leftBlob  <- storeBlobFor(path, leftContent)
              rightBlob <- storeBlobFor(path, rightContent)
              _         <- recordDeletionInIndex(path)
              _         <- recordConflictModificationInIndex(
                stageIndex = ourStageIndex
              )(
                ourBranchHead,
                path,
                mode,
                leftBlob
              )
              _ <- recordConflictModificationInIndex(
                stageIndex = theirStageIndex
              )(
                theirBranchHead,
                path,
                mode,
                rightBlob
              )
            yield partialResult.copy(
              goodForAMergeCommit = false,
              conflictingAdditionPathsAndTheirLastMinuteResolutions =
                partialResult.conflictingAdditionPathsAndTheirLastMinuteResolutions + (path -> lastMinuteResolution)
            )
            end for
          end recordConflictedMergeOfAddedFile

          def recordConflictedMergeOfModifiedFile(
              partialResult: AccumulatedMergeState,
              path: Path,
              bestAncestorCommitIdMode: String @@ Tags.Mode,
              mode: String @@ Tags.Mode,
              baseContent: String @@ Tags.Content,
              leftContent: String @@ Tags.Content,
              rightContent: String @@ Tags.Content
          ) =
            for
              baseTemporaryFile <- temporaryFile(
                suffix = ".base",
                content = baseContent
              )

              leftTemporaryFile <- temporaryFile(
                suffix = ".left",
                content = leftContent
              )

              rightTemporaryFile <- temporaryFile(
                suffix = ".right",
                content = rightContent
              )

              lastMinuteResolution <- lastMinuteResolution(
                path,
                baseTemporaryFile,
                leftTemporaryFile,
                rightTemporaryFile,
                baseLabel = bestAncestorCommitId,
                leftLabel = ourBranchHead,
                rightLabel = theirBranchHead
              )
              _ <- IO {
                os.copy.over(leftTemporaryFile, path)
              }.labelExceptionWith(errorMessage =
                s"Unexpected error: could not copy results of conflicted merge in ${underline(leftTemporaryFile)} to working directory tree file ${underline(path)}."
              )

              baseBlob  <- storeBlobFor(path, baseContent)
              leftBlob  <- storeBlobFor(path, leftContent)
              rightBlob <- storeBlobFor(path, rightContent)
              _         <- recordDeletionInIndex(path)
              _         <- recordConflictModificationInIndex(
                stageIndex = bestCommonAncestorStageIndex
              )(
                bestAncestorCommitId,
                path,
                bestAncestorCommitIdMode,
                baseBlob
              )
              _ <- recordConflictModificationInIndex(
                stageIndex = ourStageIndex
              )(
                ourBranchHead,
                path,
                mode,
                leftBlob
              )
              _ <- recordConflictModificationInIndex(
                stageIndex = theirStageIndex
              )(
                theirBranchHead,
                path,
                mode,
                rightBlob
              ).logOperation(
                s"Conflict - file ${underline(path)} was modified on our branch ${underline(
                    ourBranchHead
                  )} and modified on their branch ${underline(theirBranchHead)}${lastMinuteResolutionNotes(lastMinuteResolution)}."
              )
            yield partialResult.copy(goodForAMergeCommit = false)
            end for
          end recordConflictedMergeOfModifiedFile

          def recordCleanMergeOfFile(
              partialResult: AccumulatedMergeState,
              path: Path,
              mergedFileContent: String @@ Tags.Content,
              mode: String @@ Tags.Mode
          ) =
            for
              blobId <- storeBlobFor(path, mergedFileContent)
              _      <- restoreFileFromBlobId(
                path,
                blobId
              )
              _ <- recordModificationInIndex(
                path,
                mode,
                blobId
              )
            yield partialResult

          def bringInFileContentFromTheirBranch(
              partialResult: AccumulatedMergeState,
              path: Path,
              mode: String @@ Tags.Mode,
              blobId: String @@ Tags.BlobId
          ) =
            for
              _ <- restoreFileFromBlobId(
                path,
                blobId
              )
              _ <- recordModificationInIndex(
                path,
                mode,
                blobId
              )
            yield partialResult

          def captureRenamesOfPathDeletedOnJustOneSide =
            fileRenamingReport(path)
              .fold(ifEmpty = right(partialResult)) {
                case FileRenamingReport(
                      description,
                      leftRenamePaths,
                      rightRenamePaths
                    ) =>
                  right(
                    partialResult.copy(
                      deletedPathsByLeftRenamePath =
                        partialResult.deletedPathsByLeftRenamePath ++ leftRenamePaths
                          .map(_ -> path),
                      deletedPathsByRightRenamePath =
                        partialResult.deletedPathsByRightRenamePath ++ rightRenamePaths
                          .map(_ -> path)
                    )
                  ).logOperation(description)
              }

          mergeInput match
            case JustOurModification(
                  ourModification,
                  bestAncestorCommitIdMode,
                  bestAncestorCommitIdBlobId,
                  bestAncestorCommitIdContent
                ) =>
              mergeResultsByPath(path) match
                case FullyMerged(tokens) =>
                  val mergedFileContent = reconstituteTextFrom(tokens)

                  val ourModificationWasTweakedByTheMerge =
                    mergedFileContent != ourModification.content

                  if ourModificationWasTweakedByTheMerge then
                    recordCleanMergeOfFile(
                      partialResult,
                      path,
                      mergedFileContent,
                      ourModification.mode
                    )
                  else right(partialResult)
                  end if

                case MergedWithConflicts(baseTokens, leftTokens, rightTokens) =>
                  val baseContent  = reconstituteTextFrom(baseTokens)
                  val leftContent  = reconstituteTextFrom(leftTokens)
                  val rightContent = reconstituteTextFrom(rightTokens)

                  recordConflictedMergeOfModifiedFile(
                    partialResult,
                    path,
                    bestAncestorCommitIdMode,
                    ourModification.mode,
                    baseContent,
                    leftContent,
                    rightContent
                  )

            case JustTheirModification(
                  theirModification,
                  bestAncestorCommitIdMode,
                  bestAncestorCommitIdBlobId,
                  bestAncestorCommitIdContent
                ) =>
              mergeResultsByPath(path) match
                case FullyMerged(tokens) =>
                  val mergedFileContent = reconstituteTextFrom(tokens)

                  val theirModificationWasTweakedByTheMerge =
                    mergedFileContent != theirModification.content

                  if theirModificationWasTweakedByTheMerge then
                    recordCleanMergeOfFile(
                      partialResult,
                      path,
                      mergedFileContent,
                      theirModification.mode
                    )
                  else
                    bringInFileContentFromTheirBranch(
                      partialResult,
                      path,
                      theirModification.mode,
                      theirModification.blobId
                    )
                  end if

                case MergedWithConflicts(baseTokens, leftTokens, rightTokens) =>
                  val baseContent  = reconstituteTextFrom(baseTokens)
                  val leftContent  = reconstituteTextFrom(leftTokens)
                  val rightContent = reconstituteTextFrom(rightTokens)

                  recordConflictedMergeOfModifiedFile(
                    partialResult,
                    path,
                    bestAncestorCommitIdMode,
                    theirModification.mode,
                    baseContent,
                    leftContent,
                    rightContent
                  )

            case JustOurAddition(ourAddition) =>
              mergeResultsByPath(path) match
                case FullyMerged(tokens) =>
                  val mergedFileContent = reconstituteTextFrom(tokens)

                  val ourAdditionWasTweakedByTheMerge =
                    mergedFileContent != ourAddition.content

                  if ourAdditionWasTweakedByTheMerge then
                    recordCleanMergeOfFile(
                      partialResult,
                      path,
                      mergedFileContent,
                      ourAddition.mode
                    )
                  else right(partialResult)
                  end if

                case MergedWithConflicts(baseTokens, leftTokens, rightTokens) =>
                  val leftContent  = reconstituteTextFrom(leftTokens)
                  val rightContent = reconstituteTextFrom(rightTokens)

                  if baseTokens.nonEmpty then
                    val baseContent = reconstituteTextFrom(baseTokens)

                    recordConflictedMergeOfModifiedFile(
                      partialResult,
                      path,
                      ourAddition.mode,
                      ourAddition.mode,
                      baseContent,
                      leftContent,
                      rightContent
                    )
                  else
                    recordConflictedMergeOfAddedFile(
                      partialResult,
                      path,
                      ourAddition.mode,
                      leftContent,
                      rightContent
                    )
                  end if

            case JustTheirAddition(theirAddition) =>
              mergeResultsByPath(path) match
                case FullyMerged(tokens) =>
                  val mergedFileContent = reconstituteTextFrom(tokens)

                  val theirAdditionWasTweakedByTheMerge =
                    mergedFileContent != theirAddition.content

                  if theirAdditionWasTweakedByTheMerge then
                    recordCleanMergeOfFile(
                      partialResult,
                      path,
                      mergedFileContent,
                      theirAddition.mode
                    )
                  else
                    bringInFileContentFromTheirBranch(
                      partialResult,
                      path,
                      theirAddition.mode,
                      theirAddition.blobId
                    )
                  end if

                case MergedWithConflicts(baseTokens, leftTokens, rightTokens) =>
                  val leftContent  = reconstituteTextFrom(leftTokens)
                  val rightContent = reconstituteTextFrom(rightTokens)

                  if baseTokens.nonEmpty then
                    val baseContent = reconstituteTextFrom(baseTokens)

                    recordConflictedMergeOfModifiedFile(
                      partialResult,
                      path,
                      theirAddition.mode,
                      theirAddition.mode,
                      baseContent,
                      leftContent,
                      rightContent
                    )
                  else
                    recordConflictedMergeOfAddedFile(
                      partialResult,
                      path,
                      theirAddition.mode,
                      leftContent,
                      rightContent
                    )
                  end if

            case JustOurDeletion(_) =>
              // NOTE: we don't consult `mergeResultsByPath` because we know the
              // outcome already. This is important, because deletion of an
              // entire file on just one side is treated as a special case by
              // `CodeMotionAnalysisExtension.mergeResultsByPath` and does not
              // necessarily remove the content.
              captureRenamesOfPathDeletedOnJustOneSide

            case JustTheirDeletion(_) =>
              // NOTE: we don't consult `mergeResultsByPath` because we know the
              // outcome already. This is important, because deletion of an
              // entire file on just one side is treated as a special case by
              // `CodeMotionAnalysisExtension.mergeResultsByPath` and does not
              // necessarily remove the content.
              for
                _                      <- recordDeletionInIndex(path)
                _                      <- deleteFile(path)
                decoratedPartialResult <-
                  captureRenamesOfPathDeletedOnJustOneSide
              yield decoratedPartialResult

            case OurModificationAndTheirDeletion(
                  ourModification,
                  bestAncestorCommitIdMode,
                  bestAncestorCommitIdBlobId,
                  _
                ) =>
              val tokens = mergeResultsByPath(path) match
                case FullyMerged(mergedTokens)                  => mergedTokens
                case MergedWithConflicts(_, ourMergedTokens, _) =>
                  // We don't care about their view of the merge - their
                  // side simply deleted the whole file, so it contributes
                  // nothing interesting to the merge; the only point of the
                  // merge here was to pick up propagated edits / deletions
                  // and to note move destinations.
                  // TODO: is this even necessary? How would there be merge
                  // conflicts?
                  ourMergedTokens

              val mergedFileContent = reconstituteTextFrom(tokens)
              val ourModificationWasTweakedByTheMerge =
                mergedFileContent != ourModification.content

              val prelude =
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
                yield ()

              if ourModificationWasTweakedByTheMerge then
                if mergedFileContent.nonEmpty then
                  for
                    _      <- prelude
                    blobId <- storeBlobFor(path, mergedFileContent)
                    _      <- restoreFileFromBlobId(
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
                  yield partialResult.copy(goodForAMergeCommit = false)
                else
                  for
                    _                      <- recordDeletionInIndex(path)
                    _                      <- deleteFile(path)
                    decoratedPartialResult <-
                      captureRenamesOfPathDeletedOnJustOneSide
                  yield decoratedPartialResult
              else
                // The modified file would have been present on our branch;
                // given that we started with a clean working directory
                // tree, we just leave it there to match what Git merge
                // does.
                for
                  _ <- prelude
                  _ <- recordConflictModificationInIndex(
                    stageIndex = ourStageIndex
                  )(
                    ourBranchHead,
                    path,
                    ourModification.mode,
                    ourModification.blobId
                  ).logOperation(
                    s"Conflict - file ${underline(path)} was modified on our branch ${underline(ourBranchHead)} and deleted on their branch ${underline(theirBranchHead)}."
                  )
                yield partialResult.copy(goodForAMergeCommit = false)
              end if

            case TheirModificationAndOurDeletion(
                  theirModification,
                  bestAncestorCommitIdMode,
                  bestAncestorCommitIdBlobId,
                  _
                ) =>
              val tokens = mergeResultsByPath(path) match
                case FullyMerged(mergedTokens) => mergedTokens
                case MergedWithConflicts(_, _, theirMergedTokens) =>
                  // We don't care about our view of the merge - our side
                  // simply deleted the whole file, so it contributes
                  // nothing interesting to the merge; the only point of the
                  // merge here was to pick up propagated edits / deletions
                  // and to note move destinations.
                  theirMergedTokens

              val mergedFileContent = reconstituteTextFrom(tokens)
              val theirModificationWasTweakedByTheMerge =
                mergedFileContent != theirModification.content

              val prelude =
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
                yield ()

              // Git's merge updates the working directory tree with *their*
              // modified file which wouldn't have been present on our
              // branch prior to the merge. So that's what we do too.
              if theirModificationWasTweakedByTheMerge then
                if mergedFileContent.nonEmpty then
                  for
                    _      <- prelude
                    blobId <- storeBlobFor(path, mergedFileContent)
                    _      <- restoreFileFromBlobId(
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
                  yield partialResult.copy(goodForAMergeCommit = false)
                else
                  for
                    _                      <- recordDeletionInIndex(path)
                    decoratedPartialResult <-
                      captureRenamesOfPathDeletedOnJustOneSide
                  yield decoratedPartialResult
              else
                for
                  _ <- prelude
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
                yield partialResult.copy(goodForAMergeCommit = false)
              end if

            case BothContributeAnAddition(_, _, mergedFileMode) =>
              mergeResultsByPath(path) match
                case FullyMerged(tokens) =>
                  val mergedFileContent = reconstituteTextFrom(tokens)

                  recordCleanMergeOfFile(
                    partialResult,
                    path,
                    mergedFileContent,
                    mergedFileMode
                  )

                case MergedWithConflicts(baseTokens, leftTokens, rightTokens) =>
                  val leftContent  = reconstituteTextFrom(leftTokens)
                  val rightContent = reconstituteTextFrom(rightTokens)

                  if baseTokens.nonEmpty then
                    val baseContent = reconstituteTextFrom(baseTokens)

                    recordConflictedMergeOfModifiedFile(
                      partialResult,
                      path,
                      mergedFileMode,
                      mergedFileMode,
                      baseContent,
                      leftContent,
                      rightContent
                    )
                  else
                    recordConflictedMergeOfAddedFile(
                      partialResult,
                      path,
                      mergedFileMode,
                      leftContent,
                      rightContent
                    )
                  end if

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

                  recordCleanMergeOfFile(
                    partialResult,
                    path,
                    mergedFileContent,
                    mergedFileMode
                  )

                case MergedWithConflicts(baseTokens, leftTokens, rightTokens) =>
                  val baseContent  = reconstituteTextFrom(baseTokens)
                  val leftContent  = reconstituteTextFrom(leftTokens)
                  val rightContent = reconstituteTextFrom(rightTokens)

                  recordConflictedMergeOfModifiedFile(
                    partialResult,
                    path,
                    bestAncestorCommitIdMode,
                    mergedFileMode,
                    baseContent,
                    leftContent,
                    rightContent
                  )

            case BothContributeADeletion(_) =>
              // We already have the deletion in our branch, so no need
              // to update the index on behalf of this path, whatever happens...
              fileRenamingReport(path).fold(ifEmpty =
                right(partialResult).logOperation(
                  s"Coincidental deletion of file ${underline(path)} on our branch ${underline(ourBranchHead)} and on their branch ${underline(theirBranchHead)}."
                )
              ) {
                case FileRenamingReport(
                      description,
                      leftRenamePaths,
                      rightRenamePaths
                    ) =>
                  val isARenameVersusDeletionConflict =
                    assume(
                      leftRenamePaths.nonEmpty || rightRenamePaths.nonEmpty
                    )
                    // If all the moved content from `path` going into new files
                    // ends up on just one side, then this is a conflict because
                    // it implies an isolated deletion on the other side.
                    leftRenamePaths.isEmpty || rightRenamePaths.isEmpty
                  end isARenameVersusDeletionConflict

                  right(
                    if isARenameVersusDeletionConflict then
                      partialResult.copy(
                        conflictingDeletedPathsByLeftRenamePath =
                          partialResult.conflictingDeletedPathsByLeftRenamePath ++ leftRenamePaths
                            .map(_ -> path),
                        conflictingDeletedPathsByRightRenamePath =
                          partialResult.conflictingDeletedPathsByRightRenamePath ++ rightRenamePaths
                            .map(_ -> path)
                      )
                    else partialResult
                  ).logOperation(description)
              }
          end match
        }

        _ <-
          accumulatedMergeState.reportConflictingAdditionsTakingRenamesIntoAccount

        withRenameVersusDeletionConflicts <-
          accumulatedMergeState.reportLeftRenamesConflictingWithRightDeletions
            .flatMap(_.reportLeftDeletionsConflictingWithRightRenames)
      yield withRenameVersusDeletionConflicts.goodForAMergeCommit
      end for
    end indexUpdates

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
        tokens: Seq[Token]
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

  object ApplicationRequest:
    val default: ApplicationRequest = ApplicationRequest(
      theirBranchHead = noBranchProvided,
      noCommit = false,
      noFastForward = false,
      minimumMatchSize =
        // Don't allow monograph matches - these would bombard the merge with
        // useless all-sides matches that create a *lot* of overhead.
        2,
      thresholdSizeFractionForMatching = 0,
      minimumAmbiguousMatchSize = 10,
      ambiguousMatchesThreshold = 20
    )
  end ApplicationRequest

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
