package com.sageserpent.kineticmerge.core

import cats.Eval
import cats.data.StateT

case class LongestCommonSubsequence[Element]():
end LongestCommonSubsequence

object LongestCommonSubsequence:
  def of[Element](
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  ): LongestCommonSubsequence[Element] =

    type PartialResultKey = (Int, Int, Int)

    type PartialResultsCache =
      Map[PartialResultKey, LongestCommonSubsequence[Element]]

    type EvalWithPartialResultState[X] = StateT[Eval, PartialResultsCache, X]

    def _of(
        onePastBaseIndex: Int,
        onePastLeftIndex: Int,
        onePastRightIndex: Int
    ): EvalWithPartialResultState[LongestCommonSubsequence[Element]] = ???

    def of(
        onePastBaseIndex: Int,
        onePastLeftIndex: Int,
        onePastRightIndex: Int
    ): EvalWithPartialResultState[LongestCommonSubsequence[Element]] =
      StateT.get.flatMap { partialResultsCache =>
        val cachedResult = partialResultsCache.get(
          (onePastBaseIndex, onePastLeftIndex, onePastRightIndex)
        )

        cachedResult.fold(
          ifEmpty = _of(onePastBaseIndex, onePastLeftIndex, onePastRightIndex)
            .flatMap(computedResult =>
              StateT
                .set(
                  partialResultsCache + ((
                    onePastBaseIndex,
                    onePastLeftIndex,
                    onePastRightIndex
                  ) -> computedResult)
                ) >> StateT.pure(computedResult)
            )
        )(StateT.pure)
      }
    end of
  end of
end LongestCommonSubsequence
