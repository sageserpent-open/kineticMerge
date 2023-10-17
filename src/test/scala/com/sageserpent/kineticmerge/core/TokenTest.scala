package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.Token.{Significant, tokens}
import com.sageserpent.kineticmerge.core.TokenTest.checkTokenization
import org.apache.commons.text.StringEscapeUtils
import org.junit.jupiter.api.Test
import pprint.pprintln

object TokenTest:
  def checkTokenization(prose: String): Unit =
    val proseTokens = tokens(prose)
    pprintln(proseTokens)
    assert(proseTokens.get.foldLeft("")(_ ++ _.text) == prose)
  end checkTokenization
end TokenTest

class TokenTest extends ProseExamples:
  @Test
  def tokenization(): Unit =
    checkTokenization(wordsworth)
    checkTokenization(jobsworth)
    checkTokenization(emsworth)
    checkTokenization(scalaStuff)

  end tokenization

  @Test
  def aStringLiteralsIsTreatedAsASingleToken(): Unit =
    val literal =
      // NOTE: only the *body* of the string literal is escaped, so we have to
      // add the enclosing quotation.
      s"\"${StringEscapeUtils.escapeJava("This is a:\nstring\tliteral")}\""

    val prose = s"Not a string literal...$literal, but again, this isn't."

    checkTokenization(prose)

    assert(tokens(prose).get.contains(Significant(literal)))
  end aStringLiteralsIsTreatedAsASingleToken

end TokenTest
