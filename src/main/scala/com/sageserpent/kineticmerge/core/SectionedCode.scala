package com.sageserpent.kineticmerge.core

import cats.Eq
import com.google.common.hash.{Funnel, HashFunction}
import com.sageserpent.kineticmerge
import com.sageserpent.kineticmerge.core
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
