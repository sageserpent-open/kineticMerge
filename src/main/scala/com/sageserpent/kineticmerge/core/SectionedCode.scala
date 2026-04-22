package com.sageserpent.kineticmerge.core

import cats.Eq
import com.google.common.hash.{Funnel, HashFunction}
import com.sageserpent.kineticmerge
import com.sageserpent.kineticmerge.core
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.{
  Contribution,
  Sized
}
import com.sageserpent.kineticmerge.core.MatchAnalysis.{
  AbstractConfiguration,
  AdmissibleFailure
}
import com.typesafe.scalalogging.StrictLogging

trait SectionedCode[Path, Element]:
  def base: Map[Path, File[Element]]
  def left: Map[Path, File[Element]]
  def right: Map[Path, File[Element]]

  def matchesFor(
      section: Section[Element]
  ): collection.Set[Match[Section[Element]]]

  def lcsFor(path: Path): LongestCommonSubsequence[Section[Element]]

  def basePathFor(baseSection: Section[Element]): Path
  def leftPathFor(leftSection: Section[Element]): Path
  def rightPathFor(rightSection: Section[Element]): Path
end SectionedCode

object SectionedCode extends StrictLogging:
  /** @param baseSources
    *   The common base sources from which the left and right sources are
    *   derived.
    * @param leftSources
    *   'Our' sources, from the Git standpoint...
    * @param rightSources
    *   'Their' sources, from the Git standpoint...
    * @param configuration
    *   [[Configuration]] parameter object.
    * @tparam Path
    * @return
    *   A [[SectionedCode]] that contains a breakdown into [[File]] instances
    *   and thence into [[Section]] instances for each of the three sources.
    */
  def of[Path, Element: Eq: Funnel](
      baseSources: Sources[Path, Element],
      leftSources: Sources[Path, Element],
      rightSources: Sources[Path, Element]
  )(
      configuration: AbstractConfiguration,
      suppressMatchesInvolvingOverlappingSections: Boolean = true
  )(using
      hashFunction: HashFunction
  ): Either[Throwable, SectionedCode[Path, Element]] =
    val withAllMatchesOfAtLeastTheSureFireWindowSize =
      MatchAnalysis.of(baseSources, leftSources, rightSources)(configuration)

    val withAllMatchesOfAtLeastTheMinimumWindowSize =
      withAllMatchesOfAtLeastTheSureFireWindowSize.withAllSmallFryMatches()

    val parallelMatchesOnly =
      withAllMatchesOfAtLeastTheMinimumWindowSize.parallelMatchesOnly

    try
      val (matchesAndTheirSections, tinyMatchesAndTheirSectionsOnly) =
        parallelMatchesOnly.reconcileMatches
          .purgedOfMatchesWithOverlappingSections(
            suppressMatchesInvolvingOverlappingSections
          ) -> parallelMatchesOnly
          .tinyMatchesOnly()
          .reconcileMatches
          .purgedOfMatchesWithOverlappingSections(enabled = true)
      end val

      val sectionsAndTheirMatches =
        matchesAndTheirSections.sectionsAndTheirMatches

      // Use the sections covered by the tiny matches to break up gap fills on
      // all sides. This gives the downstream merge a chance to make last-minute
      // matches of its own between small unmatched sections that are deleted
      // from the base and their counterparts on the left or right.
      // See https://github.com/sageserpent-open/kineticMerge/issues/42 and
      // https://github.com/sageserpent-open/kineticMerge/issues/43.

      val baseFilesByPath =
        baseSources.filesByPathUtilising(mandatorySections =
          matchesAndTheirSections.baseSections ++ tinyMatchesAndTheirSectionsOnly.baseSections
        )
      val leftFilesByPath =
        leftSources.filesByPathUtilising(mandatorySections =
          matchesAndTheirSections.leftSections ++ tinyMatchesAndTheirSectionsOnly.leftSections
        )
      val rightFilesByPath =
        rightSources.filesByPathUtilising(mandatorySections =
          matchesAndTheirSections.rightSections ++ tinyMatchesAndTheirSectionsOnly.rightSections
        )

      Right(new SectionedCode[Path, Element]:
        private val lcsByPath: Map[Path, LongestCommonSubsequence[Section[
          Element
        ]]] =
          val parallelMatchesGroupIdsByMatch =
            matchesAndTheirSections.parallelMatchesGroupIdsByMatch
          val tinyParallelMatchesGroupIdsByMatch =
            tinyMatchesAndTheirSectionsOnly.parallelMatchesGroupIdsByMatch

          val tinyIdOffset =
            1 + (parallelMatchesGroupIdsByMatch.values.toSeq.maxOption
              .getOrElse(0))

          def groupIdFor(m: Match[Section[Element]]): Option[Int] =
            parallelMatchesGroupIdsByMatch
              .get(m)
              .headOption
              .orElse(
                tinyParallelMatchesGroupIdsByMatch
                  .get(m)
                  .headOption
                  .map(_ + tinyIdOffset)
              )

          val allMatches =
            matchesAndTheirSections.matches ++ tinyMatchesAndTheirSectionsOnly.matches

          val blockSizes: Map[Int, Int] =
            allMatches.toSeq
              .flatMap(m =>
                groupIdFor(m).map(g =>
                  g -> (m match
                    case m: Match.AllSides[Section[Element]] =>
                      m.baseElement.size
                    case m: Match.BaseAndLeft[Section[Element]] =>
                      m.baseElement.size
                    case m: Match.BaseAndRight[Section[Element]] =>
                      m.baseElement.size
                    case m: Match.LeftAndRight[Section[Element]] =>
                      m.leftElement.size
                  )
                )
              )
              .groupBy(_._1)
              .map { case (g, pairs) => g -> pairs.map(_._2).sum }

          enum ThreeWaySide {
            case Base, Left, Right
          }

          case class Block(groupId: Int)
          given Sized[Block] with
            override def sizeOf(block: Block): Int = blockSizes(block.groupId)
          given Eq[Block] = Eq.fromUniversalEquals

          def sectionLcsFor(
              path: Path
          ): LongestCommonSubsequence[Section[Element]] = {
            val baseSections =
              baseFilesByPath
                .get(path)
                .map(_.sections)
                .getOrElse(IndexedSeq.empty)
            val leftSections =
              leftFilesByPath
                .get(path)
                .map(_.sections)
                .getOrElse(IndexedSeq.empty)
            val rightSections =
              rightFilesByPath
                .get(path)
                .map(_.sections)
                .getOrElse(IndexedSeq.empty)

            def blocksFor(
                sections: IndexedSeq[Section[Element]]
            ): IndexedSeq[Block] =
              sections
                .flatMap(s =>
                  matchesAndTheirSections.sectionsAndTheirMatches.get(s) ++
                    tinyMatchesAndTheirSectionsOnly.sectionsAndTheirMatches
                      .get(s)
                )
                .flatMap(groupIdFor)
                .distinct
                .map(Block.apply)

            val baseBlocks  = blocksFor(baseSections)
            val leftBlocks  = blocksFor(leftSections)
            val rightBlocks = blocksFor(rightSections)

            val blockLcs = LongestCommonSubsequence.of(
              baseBlocks,
              leftBlocks,
              rightBlocks
            )(using Eq[Block], summon[Sized[Block]], com.sageserpent.kineticmerge.NoProgressRecording)

            def blockContributionsByGroupId(
                side: ThreeWaySide,
                contributions: IndexedSeq[Contribution[Block]]
            ): Map[Int, Contribution[Block]] =
              contributions.collect {
                case c @ Contribution.Common(Block(g)) => g -> c
                case c @ Contribution.CommonToBaseAndLeftOnly(Block(g))
                    if side != ThreeWaySide.Right =>
                  g -> c
                case c @ Contribution.CommonToBaseAndRightOnly(Block(g))
                    if side != ThreeWaySide.Left =>
                  g -> c
                case c @ Contribution.CommonToLeftAndRightOnly(Block(g))
                    if side != ThreeWaySide.Base =>
                  g -> c
                case c @ Contribution.Difference(Block(g)) => g -> c
              }.toMap

            val blockContributionByGroupIdOnBase =
              blockContributionsByGroupId(ThreeWaySide.Base, blockLcs.base)
            val blockContributionByGroupIdOnLeft =
              blockContributionsByGroupId(ThreeWaySide.Left, blockLcs.left)
            val blockContributionByGroupIdOnRight =
              blockContributionsByGroupId(ThreeWaySide.Right, blockLcs.right)

            def contributionFor(
                section: Section[Element],
                side: ThreeWaySide
            ): Contribution[Section[Element]] =
              val matches =
                matchesAndTheirSections.sectionsAndTheirMatches.get(section) ++
                  tinyMatchesAndTheirSectionsOnly.sectionsAndTheirMatches
                    .get(section)

              val currentPath = path

              val alignedContributions = matches.flatMap { m =>
                groupIdFor(m).flatMap { g =>
                  val blockContrib = side match
                    case ThreeWaySide.Base =>
                      blockContributionByGroupIdOnBase.get(g)
                    case ThreeWaySide.Left =>
                      blockContributionByGroupIdOnLeft.get(g)
                    case ThreeWaySide.Right =>
                      blockContributionByGroupIdOnRight.get(g)
                  blockContrib.map(bc => m -> bc)
                }
              }

              def isCompatible(
                  m: Match[Section[Element]],
                  bc: Contribution[Block]
              ): Boolean = bc match
                case Contribution.Common(_) => true
                case Contribution.CommonToBaseAndLeftOnly(_) =>
                  side != ThreeWaySide.Right
                case Contribution.CommonToBaseAndRightOnly(_) =>
                  side != ThreeWaySide.Left
                case Contribution.CommonToLeftAndRightOnly(_) =>
                  side != ThreeWaySide.Base
                case Contribution.Difference(_) => true

              val compatibleAligned =
                alignedContributions.filter { case (m, bc) =>
                  isCompatible(m, bc)
                }

              if compatibleAligned.isEmpty then
                Contribution.Difference(section)
              else
                val (m, bc) = compatibleAligned.maxBy { case (m, bc) =>
                  bc match
                    case Contribution.Common(_) => 3
                    case Contribution.CommonToBaseAndLeftOnly(_) |
                        Contribution.CommonToBaseAndRightOnly(_) |
                        Contribution.CommonToLeftAndRightOnly(_) =>
                      2
                    case _ => 1
                }

                bc match
                  case Contribution.Common(_) =>
                    m match
                      case Match.AllSides(b, l, r) =>
                        val baseIsLocal  = basePathFor(b) == currentPath
                        val leftIsLocal  = leftPathFor(l) == currentPath
                        val rightIsLocal = rightPathFor(r) == currentPath

                        (baseIsLocal, leftIsLocal, rightIsLocal) match
                          case (true, true, true) => Contribution.Common(section)
                          case (true, true, false) if side != ThreeWaySide.Right =>
                            Contribution.CommonToBaseAndLeftOnly(section)
                          case (true, false, true) if side != ThreeWaySide.Left =>
                            Contribution.CommonToBaseAndRightOnly(section)
                          case (false, true, true) if side != ThreeWaySide.Base =>
                            Contribution.CommonToLeftAndRightOnly(section)
                          case _ => Contribution.Difference(section)

                      case Match.BaseAndLeft(b, l) if side != ThreeWaySide.Right =>
                        if basePathFor(b) == currentPath && leftPathFor(
                            l
                          ) == currentPath
                        then Contribution.CommonToBaseAndLeftOnly(section)
                        else Contribution.Difference(section)
                      case Match.BaseAndRight(b, r) if side != ThreeWaySide.Left =>
                        if basePathFor(b) == currentPath && rightPathFor(
                            r
                          ) == currentPath
                        then Contribution.CommonToBaseAndRightOnly(section)
                        else Contribution.Difference(section)
                      case Match.LeftAndRight(l, r) if side != ThreeWaySide.Base =>
                        if leftPathFor(l) == currentPath && rightPathFor(
                            r
                          ) == currentPath
                        then Contribution.CommonToLeftAndRightOnly(section)
                        else Contribution.Difference(section)
                      case _ => Contribution.Difference(section)
                  case Contribution.CommonToBaseAndLeftOnly(_) =>
                    m match
                      case Match.AllSides(b, l, _) if side != ThreeWaySide.Right =>
                        if basePathFor(b) == currentPath && leftPathFor(
                            l
                          ) == currentPath
                        then Contribution.CommonToBaseAndLeftOnly(section)
                        else Contribution.Difference(section)
                      case Match.BaseAndLeft(b, l) if side != ThreeWaySide.Right =>
                        if basePathFor(b) == currentPath && leftPathFor(
                            l
                          ) == currentPath
                        then Contribution.CommonToBaseAndLeftOnly(section)
                        else Contribution.Difference(section)
                      case _ => Contribution.Difference(section)
                  case Contribution.CommonToBaseAndRightOnly(_) =>
                    m match
                      case Match.AllSides(b, _, r) if side != ThreeWaySide.Left =>
                        if basePathFor(b) == currentPath && rightPathFor(
                            r
                          ) == currentPath
                        then Contribution.CommonToBaseAndRightOnly(section)
                        else Contribution.Difference(section)
                      case Match.BaseAndRight(b, r) if side != ThreeWaySide.Left =>
                        if basePathFor(b) == currentPath && rightPathFor(
                            r
                          ) == currentPath
                        then Contribution.CommonToBaseAndRightOnly(section)
                        else Contribution.Difference(section)
                      case _ => Contribution.Difference(section)
                  case Contribution.CommonToLeftAndRightOnly(_) =>
                    m match
                      case Match.AllSides(_, l, r) if side != ThreeWaySide.Base =>
                        if leftPathFor(l) == currentPath && rightPathFor(
                            r
                          ) == currentPath
                        then Contribution.CommonToLeftAndRightOnly(section)
                        else Contribution.Difference(section)
                      case Match.LeftAndRight(l, r) if side != ThreeWaySide.Base =>
                        if leftPathFor(l) == currentPath && rightPathFor(
                            r
                          ) == currentPath
                        then Contribution.CommonToLeftAndRightOnly(section)
                        else Contribution.Difference(section)
                      case _ => Contribution.Difference(section)
                  case _ => Contribution.Difference(section)
                end match
              end if
            end contributionFor

            def elementSize(section: Section[Element]): Int = section.size

            val baseContributions =
              baseSections.map(contributionFor(_, ThreeWaySide.Base))
            val leftContributions =
              leftSections.map(contributionFor(_, ThreeWaySide.Left))
            val rightContributions =
              rightSections.map(contributionFor(_, ThreeWaySide.Right))

            LongestCommonSubsequence(
              baseContributions,
              leftContributions,
              rightContributions
            )(elementSize)
          }

          val paths =
            baseFilesByPath.keySet ++ leftFilesByPath.keySet ++ rightFilesByPath.keySet
          paths.map(path => path -> sectionLcsFor(path)).toMap

        {
          // Invariant: the matches are referenced only by their participating
          // sections.
          val allMatchKeys = sectionsAndTheirMatches.keySet

          val allParticipatingSections =
            sectionsAndTheirMatches.values.toSet
              .map {
                case Match.AllSides(baseSection, leftSection, rightSection) =>
                  Set(baseSection, leftSection, rightSection)
                case Match.BaseAndLeft(baseSection, leftSection) =>
                  Set(baseSection, leftSection)
                case Match.BaseAndRight(baseSection, rightSection) =>
                  Set(baseSection, rightSection)
                case Match.LeftAndRight(leftSection, rightSection) =>
                  Set(leftSection, rightSection)
              }
              .reduceOption(_ union _)
              .getOrElse(Set.empty)

          require(allMatchKeys == allParticipatingSections)

          // Invariant - every section across all paths on all three sides is
          // unique. This is vital for `CodeMotionAnalysisExtension` to be able
          // to work with move sources, destinations and to recognise where to
          // perform substitutions.

          val allSections =
            (baseFilesByPath.values ++ leftFilesByPath.values ++ rightFilesByPath.values)
              .flatMap(_.sections)
              .toSeq

          val allDistinctSections = allSections.toSet

          require(allSections.size == allDistinctSections.size)

          // Invariant: all the match keys should belong to the breakdown
          // of sections.

          val rogueMatches =
            (sectionsAndTheirMatches.keySet diff allDistinctSections).flatMap(
              sectionsAndTheirMatches.get
            )

          require(
            rogueMatches.isEmpty,
            s"Found rogue matches whose sections do not belong to the breakdown: ${pprintCustomised(rogueMatches)}."
          )
        }

        override def base: Map[Path, File[Element]] = baseFilesByPath

        override def left: Map[Path, File[Element]] = leftFilesByPath

        override def right: Map[Path, File[Element]] = rightFilesByPath

        override def matchesFor(
            section: Section[Element]
        ): collection.Set[Match[Section[Element]]] =
          sectionsAndTheirMatches.get(section)

        override def lcsFor(
            path: Path
        ): LongestCommonSubsequence[Section[Element]] = lcsByPath(path)

        export baseSources.pathFor as basePathFor
        export leftSources.pathFor as leftPathFor
        export rightSources.pathFor as rightPathFor)
    catch
      // NOTE: don't convert this to use of `Try` with a subsequent `.toEither`
      // conversion. We want most flavours of exception to propagate, as they
      // are likely to be logic errors or something just as unwholesome.
      case admissibleException: AdmissibleFailure => Left(admissibleException)
    end try
  end of
end SectionedCode
