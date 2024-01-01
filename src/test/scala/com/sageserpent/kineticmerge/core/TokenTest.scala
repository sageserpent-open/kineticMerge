package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.Token.{Significant, tokens}
import com.sageserpent.kineticmerge.core.TokenTest.{checkTokenization, stringLiteral}
import org.apache.commons.text.StringEscapeUtils
import org.junit.jupiter.api.Test
import pprint.pprintln

object TokenTest:
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
