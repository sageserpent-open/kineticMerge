package com.sageserpent.kineticmerge.core

import com.google.common.hash.{Hashing, PrimitiveSink}
import com.sageserpent.kineticmerge.core.PartitionedThreeWayTransform.Input
import com.sageserpent.kineticmerge.core.Token
import com.sageserpent.kineticmerge.core.Token.{Significant, Whitespace, WithTrailingWhitespace}
import org.rabinfingerprint.polynomial.Polynomial
import org.rabinfingerprint.polynomial.Polynomial.{Reducibility, createFromBytes}

import scala.annotation.tailrec
import scala.util.Random
import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

object mergeTokens:
  private val partitionedThreeWayTransform = new PartitionedThreeWayTransform(
    createIrreducible()
  )

  def apply(
      base: Vector[Token],
      left: Vector[Token],
      right: Vector[Token]
  ): Either[merge.Divergence.type, merge.Result[Token]] =
    def threeWayTransform(
        input: Input[Token]
    ): Either[merge.Divergence.type, merge.Result[Token]] =
      if input.isCommonPartition then
        Right(merge.Result.FullyMerged(input.left))
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
            merge.Result.FullyMerged(lhsElements),
            merge.Result.FullyMerged(rhsElements)
          ) =>
        merge.Result.FullyMerged(lhsElements ++ rhsElements)
      case (
            merge.Result.FullyMerged(lhsElements),
            merge.Result.MergedWithConflicts(rhsLeftElements, rhsRightElements)
          ) =>
        merge.Result.MergedWithConflicts(
          lhsElements ++ rhsLeftElements,
          lhsElements ++ rhsRightElements
        )
      case (
            merge.Result.MergedWithConflicts(lhsLeftElements, lhsRightElements),
            merge.Result.FullyMerged(rhsElements)
          ) =>
        merge.Result.MergedWithConflicts(
          lhsLeftElements ++ rhsElements,
          lhsRightElements ++ rhsElements
        )
      case (
            merge.Result.MergedWithConflicts(lhsLeftElements, lhsRightElements),
            merge.Result.MergedWithConflicts(rhsLeftElements, rhsRightElements)
          ) =>
        merge.Result.MergedWithConflicts(
          lhsLeftElements ++ rhsLeftElements,
          lhsRightElements ++ rhsRightElements
        )

  private def createIrreducible(): Polynomial =
    val random = new Random(45877L)

    val degree = 51

    @tailrec
    def tryPolynomial: Polynomial =
      val result =
        createFromBytes(random.nextBytes((degree / 8) + 1), degree)
      end result
      if result.getReducibility == Reducibility.IRREDUCIBLE then result
      else tryPolynomial
      end if
    end tryPolynomial

    tryPolynomial
  end createIrreducible
end mergeTokens
