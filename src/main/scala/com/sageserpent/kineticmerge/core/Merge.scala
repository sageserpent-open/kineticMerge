package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.CodeMotionAnalysis.Match
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Contribution
import com.sageserpent.kineticmerge.core.Merge.Result.*

import scala.annotation.tailrec

object Merge:
  def of(
      base: IndexedSeq[Section],
      left: IndexedSeq[Section],
      right: IndexedSeq[Section]
  )(matchFor: Section => Option[Match]): Either[Divergence.type, Result] =
    def equivalent(lhs: Section, rhs: Section) =
      matchFor(lhs) -> matchFor(rhs) match
        case (None, None)    => false
        case (None, Some(_)) => false
        case (Some(_), None) => false
        case (Some(lhsMatch), Some(rhsMatch)) =>
          lhsMatch.dominantSection == rhsMatch.dominantSection

    @tailrec
    def mergeBetweenRunsOfCommonElements(
        base: Seq[Contribution[Section]],
        left: Seq[Contribution[Section]],
        right: Seq[Contribution[Section]]
    )(partialResult: Result): Result =
      (base, left, right) match
        case (
              Seq(Contribution.Common(baseSection), baseTail*),
              Seq(Contribution.Common(leftSection), leftTail*),
              Seq(Contribution.Common(rightSection), rightTail*)
            ) => // Preservation.
          // Invariant - these must all belong to the same match; progress
          // through common elements is synchronized.
          val Some(matchForBaseSection) = matchFor(baseSection): @unchecked

          assume(matchFor(leftSection).contains(matchForBaseSection))
          assume(matchFor(rightSection).contains(matchForBaseSection))

          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            partialResult match
              case FullyMerged(sections) =>
                FullyMerged(sections :+ matchForBaseSection.dominantSection)
              case MergedWithConflicts(leftSections, rightSections) =>
                partialResult
            // TODO: this is just a placeholder.
          )

        case (
              _,
              Seq(
                Contribution.CommonToLeftAndRightOnly(leftSection),
                leftTail*
              ),
              Seq(
                Contribution.CommonToLeftAndRightOnly(rightSection),
                rightTail*
              )
            ) => // Coincident insertion.
          // Invariant - these must all belong to the same match; progress
          // through common elements is synchronized.
          val Some(matchForLeftSection) = matchFor(leftSection): @unchecked

          assume(matchFor(rightSection).contains(matchForLeftSection))

          mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
            partialResult match
              case FullyMerged(sections) =>
                FullyMerged(sections :+ matchForLeftSection.dominantSection)
              case MergedWithConflicts(leftSections, rightSections) =>
                partialResult
            // TODO: this is just a placeholder.
          )

        case (
              Seq(
                Contribution.CommonToBaseAndRightOnly(baseSection),
                baseTail*
              ),
              Seq(Contribution.Difference(leftSection), leftTail*),
              Seq(
                Contribution.CommonToBaseAndRightOnly(rightSection),
                rightTail*
              )
            ) => // Left edit.
          // Invariant - these must all belong to the same match; progress
          // through common elements is synchronized.
          val Some(matchForBaseSection) = matchFor(baseSection): @unchecked

          assume(matchFor(rightSection).contains(matchForBaseSection))

          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            partialResult match
              case FullyMerged(sections) =>
                FullyMerged(sections :+ leftSection)
              case MergedWithConflicts(leftSections, rightSections) =>
                partialResult
            // TODO: this is just a placeholder.
          )

        case (
              Seq(
                Contribution.CommonToBaseAndRightOnly(baseSection),
                baseTail*
              ),
              _,
              Seq(
                Contribution.CommonToBaseAndRightOnly(rightSection),
                rightTail*
              )
            ) => // Left deletion.
          // Invariant - these must all belong to the same match; progress
          // through common elements is synchronized.
          val Some(matchForBaseSection) = matchFor(baseSection): @unchecked

          assume(matchFor(rightSection).contains(matchForBaseSection))

          mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
            partialResult match
              case FullyMerged(_) =>
                partialResult
              case MergedWithConflicts(leftSections, rightSections) =>
                partialResult
            // TODO: this is just a placeholder.
          )

        case (
              Seq(
                Contribution.CommonToBaseAndLeftOnly(baseSection),
                baseTail*
              ),
              Seq(
                Contribution.CommonToBaseAndLeftOnly(leftSection),
                leftTail*
              ),
              Seq(Contribution.Difference(rightSection), rightTail*)
            ) => // Right edit.
          // Invariant - these must all belong to the same match; progress
          // through common elements is synchronized.
          val Some(matchForBaseSection) = matchFor(baseSection): @unchecked

          assume(matchFor(leftSection).contains(matchForBaseSection))

          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            partialResult match
              case FullyMerged(sections) =>
                FullyMerged(sections :+ rightSection)
              case MergedWithConflicts(leftSections, rightSections) =>
                partialResult
            // TODO: this is just a placeholder.
          )

        case (
              Seq(
                Contribution.CommonToBaseAndLeftOnly(baseSection),
                baseTail*
              ),
              Seq(
                Contribution.CommonToBaseAndLeftOnly(leftSection),
                leftTail*
              ),
              _
            ) => // Right deletion.
          // Invariant - these must all belong to the same match; progress
          // through common elements is synchronized.
          val Some(matchForBaseSection) = matchFor(baseSection): @unchecked

          assume(matchFor(leftSection).contains(matchForBaseSection))

          mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
            partialResult match
              case FullyMerged(_) =>
                partialResult
              case MergedWithConflicts(leftSections, rightSections) =>
                partialResult
            // TODO: this is just a placeholder.
          )

        case (
              Seq(Contribution.Difference(baseSection), baseTail*),
              _,
              _
            ) => // Coincident deletion.
          mergeBetweenRunsOfCommonElements(baseTail, left, right)(partialResult)

        case (
              Seq(
                Contribution.Common(_) |
                Contribution.CommonToBaseAndLeftOnly(_) |
                Contribution.CommonToLeftAndRightOnly(_) |
                Contribution.CommonToBaseAndRightOnly(_),
                _*
              ) | Seq(),
              Seq(Contribution.Difference(leftSection), leftTail*),
              _
            ) => // Left insertion.
          // Invariant - the left section cannot be part of a match when it is
          // different.
          assume(matchFor(leftSection).isEmpty)

          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            partialResult match
              case FullyMerged(sections) =>
                FullyMerged(sections :+ leftSection)
              case MergedWithConflicts(leftSections, rightSections) =>
                partialResult
            // TODO: this is just a placeholder.
          )

        case (
              Seq(
                Contribution.Common(_) |
                Contribution.CommonToBaseAndRightOnly(_) |
                Contribution.CommonToLeftAndRightOnly(_) |
                Contribution.CommonToBaseAndLeftOnly(_),
                _*
              ) | Seq(),
              _,
              Seq(Contribution.Difference(rightSection), rightTail*)
            ) => // Right insertion.
          // Invariant - the left section cannot be part of a match when it is
          // different.
          assume(matchFor(rightSection).isEmpty)

          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            partialResult match
              case FullyMerged(sections) =>
                FullyMerged(sections :+ rightSection)
              case MergedWithConflicts(leftSections, rightSections) =>
                partialResult
            // TODO: this is just a placeholder.
          )

        case (
              _,
              Seq(Contribution.Difference(leftSection), leftTail*),
              Seq(Contribution.Difference(rightSection), rightTail*)
            ) => // Insertion conflict.
          partialResult // TODO: this is just a placeholder.

        case (Seq(), Seq(), Seq()) => // Terminating case!
          partialResult
      end match
    end mergeBetweenRunsOfCommonElements

    val longestCommonSubsequence =
      LongestCommonSubsequence.of(base, left, right)(equivalent _)

    val emptyResult: Result = FullyMerged(IndexedSeq.empty)

    // TODO: for now, failure is not tolerated, but obviously that will have to
    // be accommodated - both merge conflicts and later divergences.
    Right(
      mergeBetweenRunsOfCommonElements(
        longestCommonSubsequence.base,
        longestCommonSubsequence.left,
        longestCommonSubsequence.right
      )(emptyResult)
    )
  end of

  // TODO: "Something went wrong!" - "What was it?"
  case object Divergence

  enum Result:
    /** @return
      *   A map from moved sections in either the left or the right to their
      *   associated rewrites, which may represent edits or outright deletions.
      *   If a section is from the base, or has not moved, or has no associated
      *   rewrite then there will be no entry for it in the map.
      * @note
      *   A section serving as an edit rewrite is wrapped in a [[Some]]
      *   instance. A deletion rewrite of a section is represented by an
      *   associated [[None]] instance.
      */
    def movedSectionRewrites: Map[Section, Option[Section]] = ???

    case FullyMerged(sections: IndexedSeq[Section])
    case MergedWithConflicts(
        leftSections: IndexedSeq[Section],
        rightSections: IndexedSeq[Section]
    )
  end Result

end Merge
