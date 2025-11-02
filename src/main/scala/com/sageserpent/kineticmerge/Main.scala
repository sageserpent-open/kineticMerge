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
import os.{FilePath, Path, RelPath}
import scopt.{DefaultOEffectSetup, OParser}

import scala.annotation.varargs
import scala.collection.BuildFrom
import scala.collection.decorators.mapDecorator
import scala.io.Source
import scala.util.Try

object Main extends StrictLogging:
  private type ErrorOrOperationMessage =
    Either[String @@ Tags.ErrorMessage, String]
  private type WorkflowLog                = List[ErrorOrOperationMessage]
  private type WorkflowLogWriter[Payload] = WriterT[IO, WorkflowLog, Payload]
  private type Workflow[Payload]          =
    EitherT[WorkflowLogWriter, String @@ Tags.ErrorMessage, Payload]
  private val whitespaceRun                         = "\\s+"
  private val successfulMerge: Int @@ Tags.ExitCode =
    0.taggedWith[Tags.ExitCode]
  private val conflictedMerge: Int @@ Tags.ExitCode =
    1.taggedWith[Tags.ExitCode]
  private val incorrectCommandLine: Int @@ Tags.ExitCode =
    2.taggedWith[Tags.ExitCode]
  private val error: Int @@ Tags.ExitCode = 3.taggedWith[Tags.ExitCode]
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
      apply(progressRecording = NoProgressRecording, commandLineArguments*)
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
        opt[Unit](name = "quiet")
          .action((noCommit, commandLineArguments) =>
            commandLineArguments.copy(quiet = true)
          )
          .text(
            "Do not report anything on standard output; just write the final output into the directories."
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
        arg[String](name =
          "<directory for the files on one of the three sides of the merge>"
        )
          .validate(directory =>
            Try { os.Path(directory): Unit }.toEither.left.map(_.getMessage)
          )
          .action((directory, commandLineArguments) =>
            commandLineArguments.copy(mergeSideDirectories =
              commandLineArguments.mergeSideDirectories :+ FilePath(directory)
            )
          )
          .required()
          .minOccurs(3)
          .maxOccurs(3),
        checkConfig(commandLineArguments =>
          if 0 > commandLineArguments.thresholdSizeFractionForMatching || 1 < commandLineArguments.thresholdSizeFractionForMatching
          then
            failure(
              s"Match threshold fraction ${underline(commandLineArguments.thresholdSizeFractionForMatching)} should be at least zero and at most one."
            )
          else success
        ),
        note(
          "Utility to perform a global three-way merge between directories, taking code motion into account."
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
          s"Exits with code $error if the filesystem experiences an error; any changes are rolled back."
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
        mergeSides(_)(workingDirectory = os.pwd, progressRecording)
      )
    )
  end apply

  def mergeSides(applicationRequest: ApplicationRequest)(
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

    val workflow =
      val inTopLevelWorkingDirectory = InWorkingDirectory(workingDirectory)

      val Seq(baseDirectory, leftDirectory, rightDirectory) =
        mergeSideDirectories

      for
        (baseContents, baseAbsolutePath) <- inTopLevelWorkingDirectory
          .contentsOf(
            baseDirectory
          )

        (leftContents, leftAbsolutePath) <- inTopLevelWorkingDirectory
          .contentsOf(
            leftDirectory
          )

        (rightContents, rightAbsolutePath) <- inTopLevelWorkingDirectory
          .contentsOf(
            rightDirectory
          )

        ourChanges = inTopLevelWorkingDirectory.changes(
          before = baseContents,
          after = leftContents
        )

        theirChanges = inTopLevelWorkingDirectory.changes(
          before = baseContents,
          after = rightContents
        )

        mergeInputs <- inTopLevelWorkingDirectory.mergeInputsOf(
          baseAbsolutePath,
          leftAbsolutePath,
          rightAbsolutePath
        )(baseContents, ourChanges, theirChanges)

        exitCode <-
          inTopLevelWorkingDirectory.mergeAndLogOutcome(
            baseAbsolutePath,
            leftAbsolutePath,
            rightAbsolutePath,
            configuration
          )(mergeInputs)
      yield exitCode
      end for
    end workflow

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
  end mergeSides

  private def underline(anything: Any): Str =
    fansi.Underlined.On(anything.toString)

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

  case class ApplicationRequest(
      mergeSideDirectories: Seq[FilePath],
      // TODO: this isn't used yet!
      quiet: Boolean,
      minimumMatchSize: Int,
      thresholdSizeFractionForMatching: Double,
      minimumAmbiguousMatchSize: Int,
      ambiguousMatchesThreshold: Int
  )

  enum Change:
    case Modification(
        content: String @@ Tags.Content
    )
    case Addition(
        content: String @@ Tags.Content
    )
    case Deletion
  end Change

  enum MergeInput:
    case JustOurModification(
        ourModification: Change.Modification,
        baseContent: String @@ Tags.Content
    )
    case JustTheirModification(
        theirModification: Change.Modification,
        baseContent: String @@ Tags.Content
    )
    case JustOurAddition(ourAddition: Change.Addition)
    case JustTheirAddition(theirAddition: Change.Addition)
    case JustOurDeletion(bestAncestorCommitIdContent: String @@ Tags.Content)
    case JustTheirDeletion(bestAncestorCommitIdContent: String @@ Tags.Content)
    case OurModificationAndTheirDeletion(
        ourModification: Change.Modification,
        baseContent: String @@ Tags.Content
    )
    case TheirModificationAndOurDeletion(
        theirModification: Change.Modification,
        baseContent: String @@ Tags.Content
    )
    case BothContributeAnAddition(
        ourAddition: Change.Addition,
        theirAddition: Change.Addition
    )
    case BothContributeAModification(
        ourModification: Change.Modification,
        theirModification: Change.Modification,
        baseContent: String @@ Tags.Content
    )
    case BothContributeADeletion(
        baseContent: String @@ Tags.Content
    )
  end MergeInput

  private case class EarlyTermination(exitCode: Int @@ Tags.ExitCode)
      extends RuntimeException

  private case class InWorkingDirectory(
      workingDirectory: Path
  ):
    def contentsOf(
        directory: FilePath
    ): Workflow[(Map[RelPath, String @@ Tags.Content], Path)] =
      for
        absolutePathOfDirectory <- IO { Path(directory, workingDirectory) }
          .labelExceptionWith(
            s"Directory ${underline(directory)} is not a valid path."
          )

        isReallyADirectory <- IO { os.isDir(absolutePathOfDirectory) }
          .labelExceptionWith(
            s"Could not determine whether path ${underline(absolutePathOfDirectory)} is a directory or not."
          )

        containedFiles <-
          if isReallyADirectory then
            IO {
              os.walk(absolutePathOfDirectory, skip = os.isDir): Seq[Path]
            }.labelExceptionWith(
              s"Could not list files within directory tree for ${underline(absolutePathOfDirectory)}."
            )
          else
            left(
              s"Path ${underline(absolutePathOfDirectory)} is not a directory."
            )

        contents <- containedFiles.traverse(path =>
          for
            relativePath <- IO { path.relativeTo(absolutePathOfDirectory) }
              .labelExceptionWith(
                s"Unexpected error: could not determine relative path of ${underline(path)} in relation to ${underline(absolutePathOfDirectory)}."
              )
            content <- IO {
              os
                .read(path)
                .taggedWith[Tags.Content]
            }
              .labelExceptionWith(
                s"Could not read contents of file ${underline(path)}."
              )
          yield relativePath -> content
        )
      yield Map.from(contents) -> absolutePathOfDirectory
    end contentsOf

    def changes(
        before: Map[RelPath, String @@ Tags.Content],
        after: Map[RelPath, String @@ Tags.Content]
    ): Map[RelPath, Change] =
      val deletedPaths  = (before.keySet diff after.keySet).toSeq
      val addedPaths    = (after.keySet diff before.keySet).toSeq
      val modifiedPaths = (before.keySet intersect after.keySet).toSeq

      Map.from(
        deletedPaths.map(path => path -> Change.Deletion) ++ addedPaths.map(
          path => path -> Change.Addition(after(path))
        ) ++ modifiedPaths.map(path => path -> Change.Modification(after(path)))
      )
    end changes

    def mergeInputsOf(
        baseDirectory: Path,
        ourDirectory: Path,
        theirDirectory: Path
    )(
        baseContents: Map[RelPath, String @@ Tags.Content],
        ourChanges: Map[RelPath, Change],
        theirChanges: Map[RelPath, Change]
    ): Workflow[List[(RelPath, MergeInput)]] =

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
              s"Unexpected error: file ${underline(path)} has been added in our directory ${underline(ourDirectory)} and either deleted from or modified in their directory ${underline(theirDirectory)}."
            )

          case (
                path,
                (
                  Some(Change.Deletion | _: Change.Modification),
                  Some(_: Change.Addition)
                )
              ) =>
            left(
              s"Unexpected error: file ${underline(path)} has been either deleted from or modified in our directory ${underline(ourDirectory)} and added in their directory ${underline(theirDirectory)}."
            )

          case (
                path,
                (Some(ourModification: Change.Modification), None)
              ) =>
            val baseContent = baseContents(path)
            right(path -> JustOurModification(ourModification, baseContent))

          case (
                path,
                (None, Some(theirModification: Change.Modification))
              ) =>
            val baseContent = baseContents(path)
            right(path -> JustTheirModification(theirModification, baseContent))

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
            val baseContent = baseContents(path)
            right(path -> JustOurDeletion(baseContent))

          case (
                path,
                (None, Some(Change.Deletion))
              ) =>
            val baseContent = baseContents(path)
            right(path -> JustTheirDeletion(baseContent))

          case (
                path,
                (
                  Some(ourModification: Change.Modification),
                  Some(Change.Deletion)
                )
              ) =>
            val baseContent = baseContents(path)
            right(
              path -> OurModificationAndTheirDeletion(
                ourModification,
                baseContent
              )
            )

          case (
                path,
                (
                  Some(Change.Deletion),
                  Some(theirModification: Change.Modification)
                )
              ) =>
            val baseContent = baseContents(path)
            right(
              path -> TheirModificationAndOurDeletion(
                theirModification,
                baseContent
              )
            )

          case (
                path,
                (
                  Some(ourAddition: Change.Addition),
                  Some(theirAddition: Change.Addition)
                )
              ) =>
            right(path -> BothContributeAnAddition(ourAddition, theirAddition))

          case (
                path,
                (
                  Some(ourModification: Change.Modification),
                  Some(theirModification: Change.Modification)
                )
              ) =>

            val baseContent = baseContents(path)
            right(
              path -> BothContributeAModification(
                ourModification,
                theirModification,
                baseContent
              )
            )

          case (
                path,
                (
                  Some(Change.Deletion),
                  Some(Change.Deletion)
                )
              ) =>
            val baseContent = baseContents(path)
            right(path -> BothContributeADeletion(baseContent))
        }
    end mergeInputsOf

    def mergeAndLogOutcome(
        baseDirectory: Path,
        ourDirectory: Path,
        theirDirectory: Path,
        configuration: Configuration
    )(
        mergeInputs: List[(RelPath, MergeInput)]
    ): Workflow[Int @@ Tags.ExitCode] =
      for
        cleanlyMerged <- merge(
          baseDirectory,
          ourDirectory,
          theirDirectory,
          configuration
        )(mergeInputs)

        exitCodeWhenThereAreNoUnexpectedErrors <-
          if cleanlyMerged then
            for _ <- right(()).logOperation(
                "Successful merge."
              )
            yield successfulMerge
          else
            for _ <- right(()).logOperation(
                "Merge conflicts found, handing over for further resolution..."
              )
            yield conflictedMerge
          end if
      yield exitCodeWhenThereAreNoUnexpectedErrors
    end mergeAndLogOutcome

    private def merge(
        baseDirectory: Path,
        ourDirectory: Path,
        theirDirectory: Path,
        configuration: Configuration
    )(
        mergeInputs: List[(RelPath, MergeInput)]
    ): Workflow[Boolean] =
      given Order[Token]  = Token.comparison
      given Funnel[Token] = Token.funnel
      given HashFunction  = Hashing.murmur3_32_fixed()

      // TODO: why bother to *reconstruct* the content maps when the calling
      // context already has them, albeit in terms of raw content and not
      // tokens?

      val (
        baseContentsByPath,
        leftContentsByPath,
        rightContentsByPath,
        newPathsOnLeftOrRight
      ) =
        mergeInputs.foldLeft(
          (
            Map.empty[RelPath, IndexedSeq[Token]],
            Map.empty[RelPath, IndexedSeq[Token]],
            Map.empty[RelPath, IndexedSeq[Token]],
            Set.empty[RelPath]
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
                    baseContent
                  ) =>
                val unchangedContent = tokens(baseContent).get

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
                    baseContent
                  ) =>
                val unchangedContent = tokens(baseContent).get

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

              case JustOurDeletion(baseContent) =>
                val unchangedContent = tokens(baseContent).get

                (
                  baseContentsByPath + (path -> unchangedContent),
                  leftContentsByPath,
                  rightContentsByPath + (path -> unchangedContent),
                  newPathsOnLeftOrRight
                )

              case JustTheirDeletion(baseContent) =>
                val unchangedContent = tokens(baseContent).get

                (
                  baseContentsByPath + (path -> unchangedContent),
                  leftContentsByPath + (path -> unchangedContent),
                  rightContentsByPath,
                  newPathsOnLeftOrRight
                )

              case OurModificationAndTheirDeletion(
                    ourModification,
                    baseContent
                  ) =>
                (
                  baseContentsByPath + (path -> tokens(
                    baseContent
                  ).get),
                  leftContentsByPath + (path -> tokens(
                    ourModification.content
                  ).get),
                  rightContentsByPath,
                  newPathsOnLeftOrRight
                )

              case TheirModificationAndOurDeletion(
                    theirModification,
                    baseContent
                  ) =>
                (
                  baseContentsByPath + (path -> tokens(
                    baseContent
                  ).get),
                  leftContentsByPath,
                  rightContentsByPath + (path -> tokens(
                    theirModification.content
                  ).get),
                  newPathsOnLeftOrRight
                )

              case BothContributeAnAddition(
                    ourAddition,
                    theirAddition
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
                    baseContent
                  ) =>
                (
                  baseContentsByPath + (path -> tokens(
                    baseContent
                  ).get),
                  leftContentsByPath + (path -> tokens(
                    ourModification.content
                  ).get),
                  rightContentsByPath + (path -> tokens(
                    theirModification.content
                  ).get),
                  newPathsOnLeftOrRight
                )

              case BothContributeADeletion(baseContent) =>
                (
                  baseContentsByPath + (path -> tokens(
                    baseContent
                  ).get),
                  leftContentsByPath,
                  rightContentsByPath,
                  newPathsOnLeftOrRight
                )
        }

      val baseSources = MappedContentSourcesOfTokens(
        baseContentsByPath,
        label = s"BASE: $baseDirectory"
      )

      val leftSources = MappedContentSourcesOfTokens(
        leftContentsByPath,
        label = s"OURS: $ourDirectory"
      )

      val rightSources = MappedContentSourcesOfTokens(
        rightContentsByPath,
        label = s"THEIRS: $theirDirectory"
      )

      case class AccumulatedMergeState(
          cleanlyMerged: Boolean,
          deletedPathsByLeftRenamePath: Map[RelPath, RelPath],
          deletedPathsByRightRenamePath: Map[RelPath, RelPath],
          conflictingDeletedPathsByLeftRenamePath: Map[RelPath, RelPath],
          conflictingDeletedPathsByRightRenamePath: Map[RelPath, RelPath],
          conflictingAdditionPaths: Set[RelPath]
      ):
        // NOTE: no need to yield an updated `AccumulatedMergeState` with
        // `cleanlyMerged` as false - this is done upstream already.
        def reportConflictingAdditionsTakingRenamesIntoAccount: Workflow[Unit] =
          conflictingAdditionPaths.toSeq
            .traverse_ { path =>
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
                    s"Conflict - file ${underline(path)} was added in our directory ${underline(ourDirectory)} and added in their directory ${underline(theirDirectory)}."
                  )
                case (Some(originalPathRenamedOnTheLeft), None) =>
                  right(()).logOperation(
                    s"Conflict - file ${underline(path)} is a rename in our directory ${underline(ourDirectory)} of ${underline(originalPathRenamedOnTheLeft)} and added in their directory ${underline(theirDirectory)}."
                  )
                case (None, Some(originalPathRenamedOnTheRight)) =>
                  right(()).logOperation(
                    s"Conflict - file ${underline(path)} was added in our directory ${underline(ourDirectory)} and is a rename in their directory ${underline(theirDirectory)} of ${underline(originalPathRenamedOnTheRight)}."
                  )
                case (
                      Some(originalPathRenamedOnTheLeft),
                      Some(originalPathRenamedOnTheRight)
                    ) =>
                  right(()).logOperation(
                    s"Conflict - file ${underline(path)} is a rename in our directory ${underline(ourDirectory)} of ${underline(originalPathRenamedOnTheLeft)} and is a rename in their directory ${underline(theirDirectory)} of ${underline(originalPathRenamedOnTheRight)}."
                  )
            }

        def reportLeftRenamesConflictingWithRightDeletions
            : Workflow[AccumulatedMergeState] =
          conflictingDeletedPathsByLeftRenamePath.toSeq
            .foldM(this) {
              case (partialResult, (leftRenamedPath, conflictingDeletedPath)) =>
                for _ <- right(()).logOperation(
                    s"Conflict - file ${underline(conflictingDeletedPath)} was renamed in our directory ${underline(ourDirectory)} to ${underline(leftRenamedPath)} and deleted in their directory ${underline(theirDirectory)}."
                  )
                yield partialResult.copy(cleanlyMerged = false)
            }

        def reportLeftDeletionsConflictingWithRightRenames
            : Workflow[AccumulatedMergeState] =
          conflictingDeletedPathsByRightRenamePath.toSeq
            .foldM(this) {
              case (
                    partialResult,
                    (rightRenamedPath, conflictingDeletedPath)
                  ) =>
                for _ <- right(()).logOperation(
                    s"Conflict - file ${underline(conflictingDeletedPath)} was deleted in our directory ${underline(ourDirectory)} and renamed in their directory ${underline(theirDirectory)} to ${underline(rightRenamedPath)}."
                  )
                yield partialResult.copy(cleanlyMerged = false)
            }
      end AccumulatedMergeState

      object AccumulatedMergeState:
        def initial: AccumulatedMergeState = AccumulatedMergeState(
          cleanlyMerged = true,
          deletedPathsByLeftRenamePath = Map.empty,
          deletedPathsByRightRenamePath = Map.empty,
          conflictingDeletedPathsByLeftRenamePath = Map.empty,
          conflictingDeletedPathsByRightRenamePath = Map.empty,
          conflictingAdditionPaths = Set.empty
        )
      end AccumulatedMergeState

      case class FileRenamingReport(
          description: String,
          leftRenamePaths: Set[RelPath],
          rightRenamePaths: Set[RelPath]
      )

      def fileRenamingReportUsing(
          codeMotionAnalysis: CodeMotionAnalysis[RelPath, Token],
          moveDestinationsReport: MoveDestinationsReport[Section[Token]]
      )(path: RelPath): Option[FileRenamingReport] =
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
            s"in our directory ${underline(ourDirectory)} " ++ (if 1 < leftRenamePaths.size
                                                                then
                                                                  s"into files ${leftRenamePaths.map(underline).mkString(", ")}"
                                                                else
                                                                  s"to file ${underline(leftRenamePaths.head)}")
          )

          val rightRenamingDetail = Option.unless(
            rightRenamePaths.isEmpty
          )(
            s"in their directory ${underline(theirDirectory)} " ++ (if 1 < rightRenamePaths.size
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

      for
        codeMotionAnalysis: CodeMotionAnalysis[RelPath, Token] <- EitherT
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
              ourDirectory: Path,
              theirDirectory: Path
          )(
              partialResult: AccumulatedMergeState,
              path: RelPath,
              leftContent: String @@ Tags.Content,
              rightContent: String @@ Tags.Content
          ) =
            for
              _ <- writeFileFor(ourDirectory)(path, leftContent)
              _ <- writeFileFor(theirDirectory)(path, rightContent)
            yield partialResult.copy(
              cleanlyMerged = false,
              conflictingAdditionPaths =
                partialResult.conflictingAdditionPaths + path
            )
            end for
          end recordConflictedMergeOfAddedFile

          def recordConflictedMergeOfModifiedFile(
              baseDirectory: Path,
              ourDirectory: Path,
              theirDirectory: Path
          )(
              partialResult: AccumulatedMergeState,
              path: RelPath,
              baseContent: String @@ Tags.Content,
              leftContent: String @@ Tags.Content,
              rightContent: String @@ Tags.Content
          ) =
            for
              _ <- writeFileFor(baseDirectory)(path, baseContent)
              _ <- writeFileFor(ourDirectory)(path, leftContent)
              _ <- writeFileFor(theirDirectory)(path, rightContent).logOperation(
                s"Conflict - file ${underline(path)} was modified in our directory ${underline(
                    ourDirectory
                  )} and modified in their directory ${underline(theirDirectory)}."
              )
            yield partialResult.copy(cleanlyMerged = false)
            end for
          end recordConflictedMergeOfModifiedFile

          def recordCleanMergeOfFile(
              baseDirectory: Option[Path],
              ourDirectory: Path,
              theirDirectory: Path
          )(
              partialResult: AccumulatedMergeState,
              path: RelPath,
              mergedFileContent: String @@ Tags.Content
          ) =
            for
              _ <- baseDirectory.fold(ifEmpty = right(()))(
                writeFileFor(_)(path, mergedFileContent)
              )
              _ <- writeFileFor(ourDirectory)(path, mergedFileContent)
              _ <- writeFileFor(theirDirectory)(path, mergedFileContent)
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
                  baseContent
                ) =>
              mergeResultsByPath(path) match
                case FullyMerged(tokens) =>
                  val mergedFileContent = reconstituteTextFrom(tokens)

                  val ourModificationWasTweakedByTheMerge =
                    mergedFileContent != ourModification.content

                  if ourModificationWasTweakedByTheMerge then
                    recordCleanMergeOfFile(
                      Some(baseDirectory),
                      ourDirectory,
                      theirDirectory
                    )(
                      partialResult,
                      path,
                      mergedFileContent
                    )
                  else
                    for _ <- copyFileOver(ourDirectory, theirDirectory)(path)
                    yield partialResult
                  end if

                case MergedWithConflicts(baseTokens, leftTokens, rightTokens) =>
                  val baseContent  = reconstituteTextFrom(baseTokens)
                  val leftContent  = reconstituteTextFrom(leftTokens)
                  val rightContent = reconstituteTextFrom(rightTokens)

                  recordConflictedMergeOfModifiedFile(
                    baseDirectory,
                    ourDirectory,
                    theirDirectory
                  )(
                    partialResult,
                    path,
                    baseContent,
                    leftContent,
                    rightContent
                  )

            case JustTheirModification(
                  theirModification,
                  baseContent
                ) =>
              mergeResultsByPath(path) match
                case FullyMerged(tokens) =>
                  val mergedFileContent = reconstituteTextFrom(tokens)

                  val theirModificationWasTweakedByTheMerge =
                    mergedFileContent != theirModification.content

                  if theirModificationWasTweakedByTheMerge then
                    recordCleanMergeOfFile(
                      Some(baseDirectory),
                      ourDirectory,
                      theirDirectory
                    )(
                      partialResult,
                      path,
                      mergedFileContent
                    )
                  else
                    for _ <- copyFileOver(theirDirectory, ourDirectory)(path)
                    yield partialResult
                  end if

                case MergedWithConflicts(baseTokens, leftTokens, rightTokens) =>
                  val baseContent  = reconstituteTextFrom(baseTokens)
                  val leftContent  = reconstituteTextFrom(leftTokens)
                  val rightContent = reconstituteTextFrom(rightTokens)

                  recordConflictedMergeOfModifiedFile(
                    baseDirectory,
                    ourDirectory,
                    theirDirectory
                  )(
                    partialResult,
                    path,
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
                      None,
                      ourDirectory,
                      theirDirectory
                    )(
                      partialResult,
                      path,
                      mergedFileContent
                    )
                  else
                    for _ <- copyFileOver(ourDirectory, theirDirectory)(path)
                    yield partialResult
                  end if

                case MergedWithConflicts(baseTokens, leftTokens, rightTokens) =>
                  val leftContent  = reconstituteTextFrom(leftTokens)
                  val rightContent = reconstituteTextFrom(rightTokens)

                  if baseTokens.nonEmpty then
                    val baseContent = reconstituteTextFrom(baseTokens)

                    recordConflictedMergeOfModifiedFile(
                      baseDirectory,
                      ourDirectory,
                      theirDirectory
                    )(
                      partialResult,
                      path,
                      baseContent,
                      leftContent,
                      rightContent
                    )
                  else
                    recordConflictedMergeOfAddedFile(
                      ourDirectory,
                      theirDirectory
                    )(
                      partialResult,
                      path,
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
                      None,
                      ourDirectory,
                      theirDirectory
                    )(
                      partialResult,
                      path,
                      mergedFileContent
                    )
                  else
                    for _ <- copyFileOver(theirDirectory, ourDirectory)(path)
                    yield partialResult
                  end if

                case MergedWithConflicts(baseTokens, leftTokens, rightTokens) =>
                  val leftContent  = reconstituteTextFrom(leftTokens)
                  val rightContent = reconstituteTextFrom(rightTokens)

                  if baseTokens.nonEmpty then
                    val baseContent = reconstituteTextFrom(baseTokens)

                    recordConflictedMergeOfModifiedFile(
                      baseDirectory,
                      ourDirectory,
                      theirDirectory
                    )(
                      partialResult,
                      path,
                      baseContent,
                      leftContent,
                      rightContent
                    )
                  else
                    recordConflictedMergeOfAddedFile(
                      ourDirectory,
                      theirDirectory
                    )(
                      partialResult,
                      path,
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
              for
                _                      <- deleteFile(baseDirectory)(path)
                _                      <- deleteFile(theirDirectory)(path)
                decoratedPartialResult <-
                  captureRenamesOfPathDeletedOnJustOneSide
              yield decoratedPartialResult

            case JustTheirDeletion(_) =>
              // NOTE: we don't consult `mergeResultsByPath` because we know the
              // outcome already. This is important, because deletion of an
              // entire file on just one side is treated as a special case by
              // `CodeMotionAnalysisExtension.mergeResultsByPath` and does not
              // necessarily remove the content.
              for
                _                      <- deleteFile(baseDirectory)(path)
                _                      <- deleteFile(ourDirectory)(path)
                decoratedPartialResult <-
                  captureRenamesOfPathDeletedOnJustOneSide
              yield decoratedPartialResult

            case OurModificationAndTheirDeletion(
                  ourModification,
                  baseContent
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

              if ourModificationWasTweakedByTheMerge then
                if mergedFileContent.nonEmpty then
                  for _ <- writeFileFor(ourDirectory)(path, mergedFileContent)
                      .logOperation(
                        s"Conflict - file ${underline(path)} was modified in our directory ${underline(ourDirectory)} and deleted from their directory ${underline(theirDirectory)}."
                      )
                  yield partialResult.copy(cleanlyMerged = false)
                else
                  for
                    _                      <- deleteFile(baseDirectory)(path)
                    _                      <- deleteFile(ourDirectory)(path)
                    decoratedPartialResult <-
                      captureRenamesOfPathDeletedOnJustOneSide
                  yield decoratedPartialResult
              else
                // The modified file is already present in our directory; we
                // just leave it there.
                right(partialResult.copy(cleanlyMerged = false))
                  .logOperation(
                    s"Conflict - file ${underline(path)} was modified in our directory ${underline(ourDirectory)} and deleted from their directory ${underline(theirDirectory)}."
                  )
              end if

            case TheirModificationAndOurDeletion(theirModification, _) =>
              val tokens = mergeResultsByPath(path) match
                case FullyMerged(mergedTokens) => mergedTokens
                case MergedWithConflicts(_, _, theirMergedTokens) =>
                  // We don't care about our view of the merge - our side
                  // simply deleted the whole file, so it contributes
                  // nothing interesting to the merge; the only point of the
                  // merge here was to pick up propagated edits / deletions
                  // and to note move destinations.
                  // TODO: is this even necessary? How would there be merge
                  // conflicts?
                  theirMergedTokens

              val mergedFileContent = reconstituteTextFrom(tokens)
              val theirModificationWasTweakedByTheMerge =
                mergedFileContent != theirModification.content

              if theirModificationWasTweakedByTheMerge then
                if mergedFileContent.nonEmpty then
                  for _ <- writeFileFor(theirDirectory)(path, mergedFileContent)
                      .logOperation(
                        s"Conflict - file ${underline(path)} was deleted from our directory ${underline(ourDirectory)} and modified in their directory ${underline(theirDirectory)}."
                      )
                  yield partialResult.copy(cleanlyMerged = false)
                else
                  for
                    _                      <- deleteFile(baseDirectory)(path)
                    _                      <- deleteFile(theirDirectory)(path)
                    decoratedPartialResult <-
                      captureRenamesOfPathDeletedOnJustOneSide
                  yield decoratedPartialResult
              else
                // The modified file is already present in their directory; we
                // just leave it there.
                right(partialResult.copy(cleanlyMerged = false))
                  .logOperation(
                    s"Conflict - file ${underline(path)} was deleted from our directory ${underline(ourDirectory)} and modified in their directory ${underline(theirDirectory)}."
                  )
              end if

            case BothContributeAnAddition(_, _) =>
              mergeResultsByPath(path) match
                case FullyMerged(tokens) =>
                  val mergedFileContent = reconstituteTextFrom(tokens)

                  recordCleanMergeOfFile(
                    None,
                    ourDirectory,
                    theirDirectory
                  )(
                    partialResult,
                    path,
                    mergedFileContent
                  )

                case MergedWithConflicts(baseTokens, leftTokens, rightTokens) =>
                  val leftContent  = reconstituteTextFrom(leftTokens)
                  val rightContent = reconstituteTextFrom(rightTokens)

                  if baseTokens.nonEmpty then
                    val baseContent = reconstituteTextFrom(baseTokens)

                    recordConflictedMergeOfModifiedFile(
                      baseDirectory,
                      ourDirectory,
                      theirDirectory
                    )(
                      partialResult,
                      path,
                      baseContent,
                      leftContent,
                      rightContent
                    )
                  else
                    recordConflictedMergeOfAddedFile(
                      ourDirectory,
                      theirDirectory
                    )(
                      partialResult,
                      path,
                      leftContent,
                      rightContent
                    )
                  end if

            case BothContributeAModification(
                  _,
                  _,
                  _
                ) =>
              mergeResultsByPath(path) match
                case FullyMerged(tokens) =>
                  val mergedFileContent = reconstituteTextFrom(tokens)

                  recordCleanMergeOfFile(
                    Some(baseDirectory),
                    ourDirectory,
                    theirDirectory
                  )(
                    partialResult,
                    path,
                    mergedFileContent
                  )

                case MergedWithConflicts(baseTokens, leftTokens, rightTokens) =>
                  val baseContent  = reconstituteTextFrom(baseTokens)
                  val leftContent  = reconstituteTextFrom(leftTokens)
                  val rightContent = reconstituteTextFrom(rightTokens)

                  recordConflictedMergeOfModifiedFile(
                    baseDirectory,
                    ourDirectory,
                    theirDirectory
                  )(
                    partialResult,
                    path,
                    baseContent,
                    leftContent,
                    rightContent
                  )

            case BothContributeADeletion(_) =>
              fileRenamingReport(path).fold(ifEmpty =
                right(partialResult).logOperation(
                  s"Coincidental deletion of file ${underline(path)} from our directory ${underline(ourDirectory)} and from their directory ${underline(theirDirectory)}."
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
      yield withRenameVersusDeletionConflicts.cleanlyMerged
      end for
    end merge

    private def deleteFile(directory: Path)(path: RelPath): Workflow[Unit] =
      IO {
        os.remove(directory / path): Unit
      }.labelExceptionWith(errorMessage =
        s"Unexpected error: could not delete file ${underline(path)} from directory tree ${underline(directory)}."
      )

    private def copyFileOver(sourceDirectory: Path, targetDirectory: Path)(
        path: RelPath
    ) =
      IO {
        os.copy.over(sourceDirectory / path, targetDirectory / path)
      }.labelExceptionWith(errorMessage =
        s"Unexpected error: could not copy file ${underline(path)} from ${underline(sourceDirectory)} into target directory ${underline(targetDirectory)}."
      )

    private def reconstituteTextFrom(
        tokens: Seq[Token]
    ): String @@ Main.Tags.Content =
      tokens.map(_.text).mkString.taggedWith[Tags.Content]

    private def writeFileFor(directory: Path)(
        path: RelPath,
        content: String @@ Tags.Content
    ): Workflow[Unit] =
      for
        absolutePath <- IO { directory / path }.labelExceptionWith(
          s"Unexpected error: could not create absolute path for ${underline(path)} relative to directory ${underline(directory)}."
        )
        _ <- IO {
          os.write.over(absolutePath, content)
        }.labelExceptionWith(errorMessage =
          s"Unexpected error - could not write to file ${underline(absolutePath)}."
        )
      yield ()

  end InWorkingDirectory

  object ApplicationRequest:
    val default: ApplicationRequest = ApplicationRequest(
      mergeSideDirectories = Seq.empty,
      quiet = true,
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
