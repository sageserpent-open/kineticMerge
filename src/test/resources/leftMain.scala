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
    case OurModificationAndTheirDeletion(
        ourModification: Change.Modification,
        baseContent: String @@ Tags.Content
    )
    case TheirModificationAndOurDeletion(
        theirModification: Change.Modification,
        baseContent: String @@ Tags.Content
    )
  end MergeInput

  private case class EarlyTermination(exitCode: Int @@ Tags.ExitCode)
      extends RuntimeException

  private case class InWorkingDirectory(
      workingDirectory: Path
  ):
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
                      baseDirectory,
                      ourDirectory,
                      theirDirectory
                    )(
                      partialResult,
                      path,
                      mergedFileContent
                    )
                  else
                    for
                      _ <- copyFileOver(ourDirectory, baseDirectory)(path)
                      _ <- copyFileOver(ourDirectory, theirDirectory)(path)
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
                      baseDirectory,
                      ourDirectory,
                      theirDirectory
                    )(
                      partialResult,
                      path,
                      mergedFileContent
                    )
                  else
                    for
                      _ <- copyFileOver(theirDirectory, baseDirectory)(path)
                      _ <- copyFileOver(theirDirectory, ourDirectory)(path)
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
                  // If our content is modified to being empty, this is taken to
                  // mean that all of our original content has been migrated to
                  // one or more other files.
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
                  // If their content is modified to being empty, this is taken
                  // to mean that all of our original content has been migrated
                  // to one or more other files.
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
  end InWorkingDirectory
end Main
