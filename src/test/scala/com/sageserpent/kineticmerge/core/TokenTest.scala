package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.Token.tokens
import org.junit.jupiter.api.Test
import pprint.pprintln

class TokenTest extends ProseExamples:
  @Test
  def tokenization(): Unit =
    def checkTokenization(prose: String): Unit =
      val proseTokens = tokens(prose)
      pprintln(proseTokens)
      assert(proseTokens.get.foldLeft("")(_ ++ _.text) == prose)
    end checkTokenization

    checkTokenization(wordsworth)
    checkTokenization(jobsworth)
    checkTokenization(emsworth)
    checkTokenization(scalaStuff)

  end tokenization
end TokenTest
