package com.sageserpent.kineticmerge.core

import org.junit.jupiter.api.Assertions.fail

import scala.annotation.tailrec

extension [Element](sequence: Seq[Element])
  /* Replacement for ```should contain inOrderElementsOf```; as I'm not sure if
   * that actually detects subsequences correctly in the presence of duplicates. */
  def isSubsequenceOf(
      anotherSequence: Seq[? >: Element]
  ): Unit =
    isSubsequenceOf(anotherSequence, negated = false)

  def isNotSubsequenceOf(
      anotherSequence: Seq[? >: Element]
  ): Unit =
    isSubsequenceOf(anotherSequence, negated = true)

  private def isSubsequenceOf[ElementSupertype >: Element](
      anotherSequence: Seq[ElementSupertype],
      negated: Boolean
  ): Unit =
    @tailrec
    def verify(
        sequenceRemainder: Seq[Element],
        anotherSequenceRemainder: Seq[ElementSupertype],
        matchingPrefix: Seq[Element]
    ): Unit =
      if sequenceRemainder.isEmpty then
        if negated then
          fail(
            s"Assertion failed because $sequence is a subsequence of $anotherSequence."
          )
      else if anotherSequenceRemainder.isEmpty then
        if !negated then
          if matchingPrefix.isEmpty then
            fail(
              s"Assertion failed because $sequence is not a subsequence of $anotherSequence - no prefix matches found, either."
            )
          else
            fail(
              s"Assertion failed because $sequence is not a subsequence of $anotherSequence, matched prefix $matchingPrefix but failed to find the remaining $sequenceRemainder."
            )
      else if sequenceRemainder.head == anotherSequenceRemainder.head then
        verify(
          sequenceRemainder.tail,
          anotherSequenceRemainder.tail,
          matchingPrefix :+ sequenceRemainder.head
        )
      else
        verify(
          sequenceRemainder,
          anotherSequenceRemainder.tail,
          matchingPrefix
        )
      end if
    end verify

    verify(sequence, anotherSequence, sequence.empty)
  end isSubsequenceOf
end extension
