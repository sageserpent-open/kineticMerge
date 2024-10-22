package com.sageserpent.kineticmerge.core

import cats.Eval
import cats.data.StateT

case class LongestCommonSubsequence[Element]():
end LongestCommonSubsequence

object LongestCommonSubsequence:
  def of[Element](): LongestCommonSubsequence[Element] =

    type PartialResultKey = Int

    type PartialResultsCache =
      Map[PartialResultKey, LongestCommonSubsequence[Element]]

    type EvalWithPartialResultState[X] = StateT[Eval, PartialResultsCache, X]

    def _of(
        index: Int
    ): EvalWithPartialResultState[LongestCommonSubsequence[Element]] = ???

    def of(
        index: Int
    ): EvalWithPartialResultState[LongestCommonSubsequence[Element]] =
      StateT.get.flatMap { partialResultsCache =>
        val cachedResult = partialResultsCache.get(index)

        cachedResult.fold(
          ifEmpty = _of(index)
            .flatMap(computedResult =>
              StateT
                .set(
                  partialResultsCache + (index -> computedResult)
                ) >> StateT.pure(computedResult)
            )
        )(StateT.pure)
      }
    end of
  end of
end LongestCommonSubsequence
