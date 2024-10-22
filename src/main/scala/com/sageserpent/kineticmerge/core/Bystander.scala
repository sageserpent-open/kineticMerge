package com.sageserpent.kineticmerge.core

import cats.Eval
import cats.data.StateT

case class Bystander():
end Bystander

object Bystander:
  type PartialResultsCache = Unit

  type EvalWithPartialResultState[X] = StateT[Eval, PartialResultsCache, X]

  def of(
      index: Int
  ): EvalWithPartialResultState[Bystander] =
    StateT.get.flatMap { partialResultsCache =>
      _of(index)
        .flatMap(computedResult =>
          StateT
            .set(???)
          // This last call to `>>` with the rhs operand triggers the assertion
          // failure. Commenting it out yields an expected compilation error...
            >> StateT.pure(
              computedResult
            )
        )
    }
  end of

  def _of(
      index: Int
  ): EvalWithPartialResultState[Bystander] = ???
end Bystander
