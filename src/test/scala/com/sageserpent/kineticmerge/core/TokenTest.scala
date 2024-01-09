package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.Token.{Significant, tokens}
import com.sageserpent.kineticmerge.core.TokenTest.{checkTokenization, scalaStuff, stringLiteral}
import org.apache.commons.text.StringEscapeUtils
import org.junit.jupiter.api.Test
import pprint.pprintln

object TokenTest:
  val scalaStuff =
    """
      |package com.sageserpent.kineticmerge.core
      |
      |import org.junit.jupiter.api.Assertions.fail
      |
      |import scala.annotation.tailrec
      |
      |extension [Element](sequence: Seq[Element])
      |  /* Replacement for ```should contain inOrderElementsOf```; as I'm not sure if
      |   * that actually detects subsequences correctly in the presence of duplicates. */
      |  def isSubsequenceOf(
      |      anotherSequence: Seq[? >: Element]
      |  ): Unit =
      |    isSubsequenceOf(anotherSequence, negated = false)
      |
      |  def isNotSubsequenceOf(
      |      anotherSequence: Seq[? >: Element]
      |  ): Unit =
      |    isSubsequenceOf(anotherSequence, negated = true)
      |
      |  private def isSubsequenceOf[ElementSupertype >: Element](
      |      anotherSequence: Seq[ElementSupertype],
      |      negated: Boolean
      |  ): Unit =
      |    @tailrec
      |    def verify(
      |        sequenceRemainder: Seq[Element],
      |        anotherSequenceRemainder: Seq[ElementSupertype],
      |        matchingPrefix: Seq[Element]
      |    ): Unit =
      |      if sequenceRemainder.isEmpty then
      |        if negated then
      |          fail(
      |            s"Assertion failed because $sequence is a subsequence of $anotherSequence."
      |          )
      |      else if anotherSequenceRemainder.isEmpty then
      |        if !negated then
      |          if matchingPrefix.isEmpty then
      |            fail(
      |              s"Assertion failed because $sequence is not a subsequence of $anotherSequence - no prefix matches found, either."
      |            )
      |          else
      |            fail(
      |              s"Assertion failed because $sequence is not a subsequence of $anotherSequence, matched prefix $matchingPrefix but failed to find the remaining $sequenceRemainder."
      |            )
      |      else if sequenceRemainder.head == anotherSequenceRemainder.head then
      |        verify(
      |          sequenceRemainder.tail,
      |          anotherSequenceRemainder.tail,
      |          matchingPrefix :+ sequenceRemainder.head
      |        )
      |      else
      |        verify(
      |          sequenceRemainder,
      |          anotherSequenceRemainder.tail,
      |          matchingPrefix
      |        )
      |      end if
      |    end verify
      |
      |    verify(sequence, anotherSequence, sequence.empty)
      |  end isSubsequenceOf
      |end extension
      |""".stripMargin

  val stringLiteral =
    // NOTE: only the *body* of the string literal is escaped, so we have to
    // add the enclosing quotation.
    s"\"${StringEscapeUtils.escapeJava("This is a:\nstring\tliteral")}\""

  def checkTokenization(prose: String): Unit =
    val proseTokens = tokens(prose)
    pprintln(proseTokens)
    assert(proseTokens.get.foldLeft("")(_ ++ _.text) == prose)
  end checkTokenization
end TokenTest

class TokenTest extends ProseExamples:
  @Test
  def tokenizationCoversTheText(): Unit =
    checkTokenization(wordsworth)
    checkTokenization(jobsworth)
    checkTokenization(emsworth)
    checkTokenization(scalaStuff)
  end tokenizationCoversTheText

  @Test
  def aStringLiteralIsTreatedAsASingleToken(): Unit =
    val prose = s"Not a string literal...$stringLiteral, but again, this isn't."

    checkTokenization(prose)

    assert(tokens(prose).get.contains(Significant(stringLiteral)))
  end aStringLiteralIsTreatedAsASingleToken

  @Test
  def aStringLiteralHasSignificantLiteralWhitespace(): Unit =
    val stringLiteralWithMangledWhitespace = stringLiteral.flatMap {
      case space @ ' ' =>
        s"$space$space" // Double the literal whitespace as a person would type it in.
      case character => s"$character"
    }

    val prose =
      s"Not a string literal...$stringLiteralWithMangledWhitespace, but again, this isn't."

    assume(
      tokens(prose).get.contains(
        Significant(stringLiteralWithMangledWhitespace)
      )
    )

    assert(!tokens(prose).get.contains(Significant(stringLiteral)))
  end aStringLiteralHasSignificantLiteralWhitespace

end TokenTest
