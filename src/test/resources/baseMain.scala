package com.sageserpent.kineticmerge

import cats.Order
import cats.data.{EitherT, WriterT}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.foldable.toFoldableOps
import cats.syntax.functor.toFunctorOps
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
  enum Change:
    case Modification(
        mode: String @@ Tags.Mode,
        blobId: String @@ Tags.BlobId,
        content: Option[String @@ Tags.Content]
    )
    case Addition(
        mode: String @@ Tags.Mode,
        blobId: String @@ Tags.BlobId,
        content: Option[String @@ Tags.Content]
    )
    case Deletion(binaryContentDeleted: Boolean)
  end Change

  enum MergeInput:
    case OurModificationAndTheirDeletion(
        ourModification: Change.Modification,
        bestAncestorCommitIdMode: String @@ Tags.Mode,
        bestAncestorCommitIdBlobId: String @@ Tags.BlobId,
        bestAncestorCommitIdContent: Option[String @@ Tags.Content]
    )
    case TheirModificationAndOurDeletion(
        theirModification: Change.Modification,
        bestAncestorCommitIdMode: String @@ Tags.Mode,
        bestAncestorCommitIdBlobId: String @@ Tags.BlobId,
        bestAncestorCommitIdContent: Option[String @@ Tags.Content]
    )
  end MergeInput

  private case class EarlyTermination(exitCode: Int @@ Tags.ExitCode)
      extends RuntimeException

  private case class InWorkingDirectory(
      workingDirectory: Path
  ):
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
                passThrough @ (
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
                    _,
                    _,
                    bestAncestorCommitIdContent
                  ) =>
                (
                  bestAncestorCommitIdContent.fold(ifEmpty =
                    baseContentsByPath
                  )(baseContent =>
                    baseContentsByPath + (path -> baseContent.asTokens)
                  ),
                  ourModification.content.fold(ifEmpty = leftContentsByPath)(
                    ourContent =>
                      leftContentsByPath + (path -> ourContent.asTokens)
                  ),
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
                  bestAncestorCommitIdContent.fold(ifEmpty =
                    baseContentsByPath
                  )(baseContent =>
                    baseContentsByPath + (path -> baseContent.asTokens)
                  ),
                  leftContentsByPath,
                  theirModification.content.fold(ifEmpty = rightContentsByPath)(
                    theirContent =>
                      rightContentsByPath + (path -> theirContent.asTokens)
                  ),
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
              result    <- writeConflictedIndexEntriesForAddition(
                partialResult,
                path,
                mode,
                lastMinuteResolution,
                leftBlob,
                rightBlob
              )
            yield result
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

              baseBlobId  <- storeBlobFor(path, baseContent)
              leftBlobId  <- storeBlobFor(path, leftContent)
              rightBlobId <- storeBlobFor(path, rightContent)

              result <- writeConflictedIndexEntriesForModification(
                partialResult,
                path,
                bestAncestorCommitIdMode,
                mode,
                lastMinuteResolution,
                baseBlobId,
                leftBlobId,
                rightBlobId
              )
            yield result
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

          def justOurSidesViewOfTheMergedContentAt(path: Path) =
            mergeResultsByPath(path) match
              case FullyMerged(mergedTokens)                  => mergedTokens
              case MergedWithConflicts(_, ourMergedTokens, _) => ourMergedTokens

          def justTheirSidesViewOfTheMergedContentAt(path: Path) =
            mergeResultsByPath(path) match
              case FullyMerged(mergedTokens)                    => mergedTokens
              case MergedWithConflicts(_, _, theirMergedTokens) =>
                theirMergedTokens

          mergeInput match
            case JustOurModification(
                  ourModification,
                  bestAncestorCommitIdMode,
                  _
                ) =>
              ourModification.content.fold(ifEmpty = right(partialResult))(
                ourContent =>
                  mergeResultsByPath(path) match
                    case FullyMerged(tokens) =>
                      val mergedFileContent = reconstituteContentFrom(tokens)

                      val ourModificationWasTweakedByTheMerge =
                        mergedFileContent != ourContent

                      if ourModificationWasTweakedByTheMerge then
                        recordCleanMergeOfFile(
                          partialResult,
                          path,
                          mergedFileContent,
                          ourModification.mode
                        )
                      else right(partialResult)
                      end if

                    case MergedWithConflicts(
                          baseTokens,
                          leftTokens,
                          rightTokens
                        ) =>
                      val baseContent  = reconstituteContentFrom(baseTokens)
                      val leftContent  = reconstituteContentFrom(leftTokens)
                      val rightContent = reconstituteContentFrom(rightTokens)

                      recordConflictedMergeOfModifiedFile(
                        partialResult,
                        path,
                        bestAncestorCommitIdMode,
                        ourModification.mode,
                        baseContent,
                        leftContent,
                        rightContent
                      )
              )

            case JustTheirModification(
                  theirModification,
                  bestAncestorCommitIdMode,
                  _
                ) =>
              theirModification.content.fold(ifEmpty =
                bringInFileContentFromTheirBranch(
                  partialResult,
                  path,
                  theirModification.mode,
                  theirModification.blobId
                )
              )(theirContent =>
                mergeResultsByPath(path) match
                  case FullyMerged(tokens) =>
                    val mergedFileContent = reconstituteContentFrom(tokens)

                    val theirModificationWasTweakedByTheMerge =
                      mergedFileContent != theirContent

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

                  case MergedWithConflicts(
                        baseTokens,
                        leftTokens,
                        rightTokens
                      ) =>
                    val baseContent  = reconstituteContentFrom(baseTokens)
                    val leftContent  = reconstituteContentFrom(leftTokens)
                    val rightContent = reconstituteContentFrom(rightTokens)

                    recordConflictedMergeOfModifiedFile(
                      partialResult,
                      path,
                      bestAncestorCommitIdMode,
                      theirModification.mode,
                      baseContent,
                      leftContent,
                      rightContent
                    )
              )

            case JustOurAddition(ourAddition) =>
              ourAddition.content.fold(ifEmpty = right(partialResult))(
                ourContent =>
                  mergeResultsByPath(path) match
                    case FullyMerged(tokens) =>
                      val mergedFileContent = reconstituteContentFrom(tokens)

                      val ourAdditionWasTweakedByTheMerge =
                        mergedFileContent != ourContent

                      if ourAdditionWasTweakedByTheMerge then
                        recordCleanMergeOfFile(
                          partialResult,
                          path,
                          mergedFileContent,
                          ourAddition.mode
                        )
                      else right(partialResult)
                      end if

                    case MergedWithConflicts(
                          baseTokens,
                          leftTokens,
                          rightTokens
                        ) =>
                      val leftContent  = reconstituteContentFrom(leftTokens)
                      val rightContent = reconstituteContentFrom(rightTokens)

                      if baseTokens.nonEmpty then
                        val baseContent = reconstituteContentFrom(baseTokens)

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
              )

            case JustTheirAddition(theirAddition) =>
              theirAddition.content.fold(ifEmpty =
                bringInFileContentFromTheirBranch(
                  partialResult,
                  path,
                  theirAddition.mode,
                  theirAddition.blobId
                )
              )(theirContent =>
                mergeResultsByPath(path) match
                  case FullyMerged(tokens) =>
                    val mergedFileContent = reconstituteContentFrom(tokens)

                    val theirAdditionWasTweakedByTheMerge =
                      mergedFileContent != theirContent

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

                  case MergedWithConflicts(
                        baseTokens,
                        leftTokens,
                        rightTokens
                      ) =>
                    val leftContent  = reconstituteContentFrom(leftTokens)
                    val rightContent = reconstituteContentFrom(rightTokens)

                    if baseTokens.nonEmpty then
                      val baseContent = reconstituteContentFrom(baseTokens)

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
              )

            case JustOurDeletion(bestAncestorCommitIdContent) =>
              // NOTE: we don't consult `mergeResultsByPath` because we know the
              // outcome already. This is important, because deletion of an
              // entire file on just one side is treated as a special case by
              // `CodeMotionAnalysisExtension.mergeResultsByPath` and does not
              // necessarily remove the content.
              if bestAncestorCommitIdContent.isDefined then
                captureRenamesOfPathDeletedOnJustOneSide
              else right(partialResult)

            case JustTheirDeletion(bestAncestorCommitIdContent) =>
              // NOTE: we don't consult `mergeResultsByPath` because we know the
              // outcome already. This is important, because deletion of an
              // entire file on just one side is treated as a special case by
              // `CodeMotionAnalysisExtension.mergeResultsByPath` and does not
              // necessarily remove the content.
              for
                _                      <- recordDeletionInIndex(path)
                _                      <- deleteFile(path)
                decoratedPartialResult <-
                  if bestAncestorCommitIdContent.isDefined then
                    captureRenamesOfPathDeletedOnJustOneSide
                  else right(partialResult)
              yield decoratedPartialResult

            case OurModificationAndTheirDeletion(
                  ourModification,
                  bestAncestorCommitIdMode,
                  bestAncestorCommitIdBlobId,
                  bestAncestorCommitIdContent
                ) =>
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

              def writeConflictingEntries =
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

              ourModification.content.fold(ifEmpty = writeConflictingEntries)(
                ourContent =>
                  val tokens = justOurSidesViewOfTheMergedContentAt(path)

                  val mergedFileContent = reconstituteContentFrom(tokens)
                  val ourModificationWasTweakedByTheMerge =
                    mergedFileContent != ourContent

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
                      // If our content is modified to being empty, this is
                      // taken to mean that all of our original content has been
                      // migrated to one or more other files. We can therefore
                      // resolve this as a deletion.
                      for
                        _                      <- recordDeletionInIndex(path)
                        _                      <- deleteFile(path)
                        decoratedPartialResult <-
                          captureRenamesOfPathDeletedOnJustOneSide
                      yield decoratedPartialResult
                  else writeConflictingEntries
                  end if
              )

            case TheirModificationAndOurDeletion(
                  theirModification,
                  bestAncestorCommitIdMode,
                  bestAncestorCommitIdBlobId,
                  bestAncestorCommitIdContent
                ) =>
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

              def writeConflictingEntries =
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

              theirModification.content.fold(ifEmpty = writeConflictingEntries)(
                theirContent =>
                  val tokens = justTheirSidesViewOfTheMergedContentAt(path)

                  val mergedFileContent = reconstituteContentFrom(tokens)
                  val theirModificationWasTweakedByTheMerge =
                    mergedFileContent != theirContent

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
                      // If their content is modified to being empty, this is
                      // taken to mean that all of our original content has been
                      // migrated to one or more other files. We can therefore
                      // resolve this as a deletion.
                      for
                        _                      <- recordDeletionInIndex(path)
                        decoratedPartialResult <-
                          captureRenamesOfPathDeletedOnJustOneSide
                      yield decoratedPartialResult
                  else writeConflictingEntries
                  end if
              )
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
  end InWorkingDirectory
end Main
