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

    def addCommon(partialResult: Result, commonSection: Section): Result =
      partialResult match
        case FullyMerged(sections) =>
          FullyMerged(sections :+ commonSection)
        case MergedWithConflicts(leftSections, rightSections) =>
          MergedWithConflicts(
            leftSections :+ commonSection,
            rightSections :+ commonSection
          )

    def addLeftEditConflictingWithRightDelete(
        partialResult: Result,
        leftSection: Section
    ): Result =
      partialResult match
        case FullyMerged(sections) =>
          MergedWithConflicts(
            leftSections = sections :+ leftSection,
            rightSections = sections
          )
        case MergedWithConflicts(leftSections, rightSections) =>
          MergedWithConflicts(leftSections :+ leftSection, rightSections)

    def addRightEditConflictingWithLeftDelete(
        partialResult: Result,
        rightSection: Section
    ): Result =
      partialResult match
        case FullyMerged(sections) =>
          MergedWithConflicts(
            leftSections = sections,
            rightSections = sections :+ rightSection
          )
        case MergedWithConflicts(leftSections, rightSections) =>
          MergedWithConflicts(leftSections, rightSections :+ rightSection)

    def addConflicting(
        partialResult: Result,
        leftSection: Section,
        rightSection: Section
    ): Result =
      partialResult match
        case FullyMerged(sections) =>
          MergedWithConflicts(
            sections :+ leftSection,
            sections :+ rightSection
          )
        case MergedWithConflicts(leftSections, rightSections) =>
          MergedWithConflicts(
            leftSections :+ leftSection,
            rightSections :+ rightSection
          )

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
            addCommon(partialResult, matchForBaseSection.dominantSection)
          )

        case (
              Seq(Contribution.Difference(baseSection), baseTail*),
              Seq(
                Contribution.CommonToLeftAndRightOnly(leftSection),
                leftTail*
              ),
              Seq(
                Contribution.CommonToLeftAndRightOnly(rightSection),
                rightTail*
              )
            ) => // Coincident edit.
          // Invariant - these must both belong to the same match; progress
          // through common elements is synchronized.
          val Some(matchForLeftSection) = matchFor(leftSection): @unchecked

          assume(matchFor(rightSection).contains(matchForLeftSection))

          // Invariant - the base section cannot be part of a match when it is
          // different.
          assume(matchFor(baseSection).isEmpty)

          baseTail match
            case Seq(Contribution.Difference(_), _*) =>
              // If the following element in the base would also be
              // coincidentally deleted, coalesce into a single coincident
              // edit.
              mergeBetweenRunsOfCommonElements(baseTail, left, right)(
                partialResult
              )

            case _ =>
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                addCommon(partialResult, matchForLeftSection.dominantSection)
              )
          end match

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
          // Invariant - these must both belong to the same match; progress
          // through common elements is synchronized.
          val Some(matchForLeftSection) = matchFor(leftSection): @unchecked

          assume(matchFor(rightSection).contains(matchForLeftSection))

          val dominantSection = matchForLeftSection.dominantSection

          mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
            addCommon(partialResult, dominantSection)
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

          leftTail match
            case Seq(Contribution.Difference(_), _*) =>
              // If the following element on the left would also be inserted,
              // coalesce into a single edit.
              mergeBetweenRunsOfCommonElements(base, leftTail, right)(
                addCommon(partialResult, leftSection)
              )

            case _ =>
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                addCommon(partialResult, leftSection)
              )
          end match

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
            partialResult
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

          rightTail match
            case Seq(Contribution.Difference(_), _*) =>
              // If the following element on the right would also be inserted,
              // coalesce into a single edit.
              mergeBetweenRunsOfCommonElements(base, left, rightTail)(
                addCommon(partialResult, rightSection)
              )

            case _ =>
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                addCommon(partialResult, rightSection)
              )
          end match

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
            partialResult
          )

        case (
              Seq(Contribution.Difference(baseSection), baseTail*),
              Seq(Contribution.Difference(leftSection), leftTail*),
              Seq(Contribution.Difference(rightSection), rightTail*)
            ) => // Conflict, multiple possibilities.
          baseTail match
            case Seq(Contribution.Difference(_), _*) =>
              // If the following element in the base would also be edited,
              // coalesce into a single coincident edit conflict.
              mergeBetweenRunsOfCommonElements(baseTail, left, right)(
                partialResult
              )

            case Seq(
                  Contribution.CommonToBaseAndLeftOnly(_),
                  _*
                ) => // Left edit / right delete conflict with pending right edit.
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
                addLeftEditConflictingWithRightDelete(
                  partialResult,
                  leftSection
                )
              )

            case Seq(
                  Contribution.CommonToBaseAndRightOnly(_),
                  _*
                ) => // Right edit / left delete conflict with pending left edit.
              mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
                addRightEditConflictingWithLeftDelete(
                  partialResult,
                  rightSection
                )
              )

            case _ =>
              // Edit conflict.
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                addConflicting(partialResult, leftSection, rightSection)
              )
          end match

        case (
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(leftSection), leftTail*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*)
            ) => // Left insertion with pending coincident edit.
          // Invariant - the left section cannot be part of a match when it is
          // different.
          assume(matchFor(leftSection).isEmpty)

          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            addCommon(partialResult, leftSection)
          )

        case (
              Seq(Contribution.Difference(_), baseTail*),
              Seq(Contribution.Difference(leftSection), leftTail*),
              _
            ) => // Left edit / right delete conflict.
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
            addLeftEditConflictingWithRightDelete(partialResult, leftSection)
          )

        case (
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*),
              Seq(Contribution.Difference(rightSection), rightTail*)
            ) => // Right insertion with pending left coincident edit.
          // Invariant - the left section cannot be part of a match when it is
          // different.
          assume(matchFor(rightSection).isEmpty)

          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            addCommon(partialResult, rightSection)
          )

        case (
              Seq(Contribution.Difference(_), baseTail*),
              _,
              Seq(Contribution.Difference(rightSection), rightTail*)
            ) => // Right edit / left delete conflict.
          mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
            addRightEditConflictingWithLeftDelete(partialResult, rightSection)
          )

        case (
              Seq(Contribution.Difference(_), baseTail*),
              _,
              _
            ) => // Coincident deletion.
          mergeBetweenRunsOfCommonElements(baseTail, left, right)(partialResult)

        case (
              Seq(Contribution.CommonToBaseAndLeftOnly(_), _*),
              Seq(Contribution.Difference(leftSection), leftTail*),
              Seq(Contribution.Difference(_), _*)
            ) => // Left insertion with pending right edit.
          // Invariant - the left section cannot be part of a match when it is
          // different.
          assume(matchFor(leftSection).isEmpty)

          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            addCommon(partialResult, leftSection)
          )

        case (
              Seq(Contribution.CommonToBaseAndRightOnly(_), _*),
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(rightSection), rightTail*)
            ) => // Right insertion with pending left edit.
          // Invariant - the left section cannot be part of a match when it is
          // different.
          assume(matchFor(rightSection).isEmpty)

          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            addCommon(partialResult, rightSection)
          )

        case (
              _,
              Seq(Contribution.Difference(leftSection), leftTail*),
              Seq(Contribution.Difference(rightSection), rightTail*)
            ) => // Insertion conflict.
          mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
            addConflicting(partialResult, leftSection, rightSection)
          )

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
            addCommon(partialResult, leftSection)
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
            addCommon(partialResult, rightSection)
          )

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
