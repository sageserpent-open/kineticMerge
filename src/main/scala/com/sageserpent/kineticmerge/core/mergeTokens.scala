package com.sageserpent.kineticmerge.core

import com.google.common.hash.{Hashing, PrimitiveSink}
import com.sageserpent.kineticmerge.core.PartitionedThreeWayTransform.Input
import com.sageserpent.kineticmerge.core.Token.{Significant, Whitespace, WithTrailingWhitespace}

import scala.annotation.tailrec

object mergeTokens:
  private val partitionedThreeWayTransform = new PartitionedThreeWayTransform

  def apply(
      base: IndexedSeq[Token],
      left: IndexedSeq[Token],
      right: IndexedSeq[Token]
  ): Either[merge.Divergence.type, merge.Result[Token]] =
    def threeWayTransform(
        input: Input[Token]
    ): Either[merge.Divergence.type, merge.Result[Token]] =
      if input.isCommonPartition then Right(merge.FullyMerged(input.left))
      else merge.of(input.base, input.left, input.right)(equality)

    partitionedThreeWayTransform(base, left, right)(
      targetCommonPartitionSize = 20,
      equality = equality,
      hashFunction = Hashing.murmur3_32_fixed(),
      funnel = funnel
    )(
      threeWayTransform,
      reduction
    )
  end apply

  @tailrec
  private def equality(lhs: Token, rhs: Token): Boolean =
    lhs -> rhs match
      case (
            WithTrailingWhitespace(lhsCoreToken, _),
            WithTrailingWhitespace(rhsCoreToken, _)
          ) =>
        equality(lhsCoreToken, rhsCoreToken)
      case _ => lhs == rhs
  end equality

  private def funnel(element: Token, primitiveSink: PrimitiveSink): Unit =
    element match
      case Whitespace(blanks)   =>
      case Significant(letters) => letters.foreach(primitiveSink.putChar)
      case WithTrailingWhitespace(coreToken, _) =>
        funnel(coreToken, primitiveSink)
    end match
  end funnel

  private def reduction(
      lhs: Either[merge.Divergence.type, merge.Result[Token]],
      rhs: Either[merge.Divergence.type, merge.Result[Token]]
  ): Either[merge.Divergence.type, merge.Result[Token]] =
    for
      lhsMerge <- lhs
      rhsMerge <- rhs
    yield lhsMerge -> rhsMerge match
      case (
            merge.FullyMerged(lhsElements),
            merge.FullyMerged(rhsElements)
          ) =>
        merge.FullyMerged(lhsElements ++ rhsElements)
      case (
            merge.FullyMerged(lhsElements),
            merge.MergedWithConflicts(rhsLeftElements, rhsRightElements)
          ) =>
        merge.MergedWithConflicts(
          lhsElements ++ rhsLeftElements,
          lhsElements ++ rhsRightElements
        )
      case (
            merge.MergedWithConflicts(lhsLeftElements, lhsRightElements),
            merge.FullyMerged(rhsElements)
          ) =>
        merge.MergedWithConflicts(
          lhsLeftElements ++ rhsElements,
          lhsRightElements ++ rhsElements
        )
      case (
            merge.MergedWithConflicts(lhsLeftElements, lhsRightElements),
            merge.MergedWithConflicts(rhsLeftElements, rhsRightElements)
          ) =>
        merge.MergedWithConflicts(
          lhsLeftElements ++ rhsLeftElements,
          lhsRightElements ++ rhsRightElements
        )

end mergeTokens
