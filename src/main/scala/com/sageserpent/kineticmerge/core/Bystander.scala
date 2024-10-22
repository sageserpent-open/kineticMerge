package com.sageserpent.kineticmerge.core

import cats.Eval
import cats.data.StateT

case class Bystander():
end Bystander

object Bystander:
  type PartialResultKey = Int

  type PartialResultsCache =
    Map[PartialResultKey, Bystander]

  type EvalWithPartialResultState[X] = StateT[Eval, PartialResultsCache, X]

  def _of(
           index: Int
         ): EvalWithPartialResultState[Bystander] = ???

  def of(
          index: Int
        ): EvalWithPartialResultState[Bystander] =
    StateT.get.flatMap { partialResultsCache =>
      _of(index)
        .flatMap(computedResult =>
          StateT
            .set(
              partialResultsCache + (index -> computedResult)
            ) >> StateT.pure(computedResult)
        )
    }
  end of
end Bystander
