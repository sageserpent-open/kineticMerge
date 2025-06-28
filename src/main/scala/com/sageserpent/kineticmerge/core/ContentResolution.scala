package com.sageserpent.kineticmerge.core

import cats.Eq
import cats.kernel.instances.SeqEq

object ContentResolution:
  given Eq[IndexedSeq[Token]] = SeqEq[Token].asInstanceOf[Eq[IndexedSeq[Token]]]

  def apply(
      multiSided: MultiSided[Section[Token]]
  ): IndexedSeq[Token] =
    multiSided match
      case MultiSided.Unique(section)                       => section.content
      case MultiSided.Coincident(leftSection, rightSection) =>
        // Break the symmetry - choose the left.
        leftSection.content
      case MultiSided.Preserved(
            baseSection,
            leftSection,
            rightSection
          ) =>
        // Look at the content and use *exact* comparison.

        val lhsIsCompletelyUnchanged =
          baseSection.content == leftSection.content
        val rhsIsCompletelyUnchanged =
          baseSection.content == rightSection.content

        (lhsIsCompletelyUnchanged, rhsIsCompletelyUnchanged) match
          case (false, true) => leftSection.content
          case (true, false) => rightSection.content
          case _             =>
            // Break the symmetry - choose the left.
            leftSection.content
        end match
end ContentResolution
