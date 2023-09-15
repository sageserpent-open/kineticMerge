package com.sageserpent.kineticmerge.core

import cats.Eq
import com.google.common.hash.{Hashing, PrimitiveSink}
import com.sageserpent.kineticmerge.core.Merge.Result.MergedWithConflicts
import com.sageserpent.kineticmerge.core.MergingTextTest.*
import com.sageserpent.kineticmerge.core.MergingTextTest.Token.*
import com.sageserpent.kineticmerge.core.PartitionedThreeWayTransform.Input
import org.junit.jupiter.api.Test
import org.rabinfingerprint.polynomial.Polynomial
import org.rabinfingerprint.polynomial.Polynomial.{Reducibility, createFromBytes}
import pprint.*

import scala.annotation.tailrec
import scala.util.Random
import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

class MergingTextTest:
  @Test
  def tokenization(): Unit =
    pprintln(tokenizer(wordsworth))
    pprintln(tokenizer(jobsworth))
    pprintln(tokenizer(emsworth))

  end tokenization

  @Test
  def proseCanBeMerged(): Unit =
    val Right(MergedWithConflicts(leftElements, rightElements)) =
      merge(
        base = tokenizer(wordsworth).get,
        left = tokenizer(jobsworth).get,
        right = tokenizer(emsworth).get
      )(equality): @unchecked

    pprintln(leftElements.map(_.text).mkString)
    println("*********************************")
    pprintln(rightElements.map(_.text).mkString)
  end proseCanBeMerged
end MergingTextTest

object MergingTextTest:

  private val wordsworth =
    """
      |I wandered lonely as a cloud
      |That floats on high o'er vales and hills,
      |When all at once I saw a crowd,
      |A host, of golden daffodils;
      |Beside the lake, beneath the trees,
      |Fluttering and dancing in the breeze.
      |
      |Continuous as the stars that shine
      |And twinkle on the milky way,
      |They stretched in never-ending line
      |Along the margin of a bay:
      |Ten thousand saw I at a glance,
      |Tossing their heads in sprightly dance.
      |
      |The waves beside them danced; but they
      |Out-did the sparkling waves in glee:
      |A poet could not but be gay,
      |In such a jocund company:
      |I gazed—and gazed—but little thought
      |What wealth the show to me had brought:
      |
      |For oft, when on my couch I lie
      |In vacant or in pensive mood,
      |They flash upon that inward eye
      |Which is the bliss of solitude;
      |And then my heart with pleasure fills,
      |And dances with the daffodils.
      |""".stripMargin

  private val jobsworth =
    """
      |I wandered lonely as a cloud
      |That floats on high o'er vales and hills,
      |When all at once I saw a crowd,
      |A host, of golden daffodils;
      |Beside the lake, beneath the trees,
      |Fluttering and dancing in the breeze.
      |
      |I thought, 'Was this part of the job role?'.
      |'Should I be expected to deal with flowers?'
      |The waves beside them danced; but they
      |Out-did the sparkling waves in glee:
      |A poet could not but be gay,
      |In such a jocund company:
      |I gazed—and gazed—but thought only of
      |raising this in the next Zoom meeting.
      |
      |For oft, when on my Aeron I slouch
      |In vacant or in pensive mood,
      |They flash upon that inward eye
      |Which is the bliss of solitude;
      |And then my heart with pleasure fills,
      |And sends an email to human resources.
      |""".stripMargin

  private val emsworth =
    """
      |I wandered lonely as a cloud
      |That floats on high o'er vales and hills,
      |When all at once I saw a crowd,
      |A host, of small fishing boats;
      |Astride the sea, beneath the quay,
      |Rocking and swaying in the breeze.
      |
      |Why this allusion?
      |I Havant a clue!
      |Along the margin of a bay:
      |Ten thousand (well, maybe not quite) saw I at a glance,
      |Tossing their heads in sprightly dance.
      |
      |The waves beside them danced; but they
      |Out-did the sparkling waves in glee:
      |A poet could not but be gay,
      |In such a jocund company:
      |I gazed—and gazed—but little thought
      |What wealth the show to me had brought:
      |
      |For oft, when on my couch I lie
      |In vacant or in pensive mood,
      |They flash upon that inward eye
      |Which is the bliss of solitude;
      |And then my heart with pleasure fills,
      |And sashays with the fishing boats.
      |""".stripMargin

  private val partitionedThreeWayTransform = new PartitionedThreeWayTransform(
    createIrreducible()
  )

  private def merge(
      base: Vector[Token],
      left: Vector[Token],
      right: Vector[Token]
  )(
      equality: Eq[Token]
  ): Either[Merge.Divergence.type, Merge.Result[Token]] =
    def threeWayTransform(
        input: Input[Token]
    ): Either[Merge.Divergence.type, Merge.Result[Token]] =
      if input.isCommonPartition then
        Right(Merge.Result.FullyMerged(input.left))
      else Merge.of(input.base, input.left, input.right)(equality)

    partitionedThreeWayTransform(base, left, right)(
      targetCommonPartitionSize = 20,
      equality = equality,
      hashFunction = Hashing.murmur3_32_fixed(),
      funnel = funnel _
    )(
      threeWayTransform,
      reduction
    )
  end merge

  @tailrec
  private def equality(lhs: Token, rhs: Token): Boolean =
    lhs -> rhs match
      case (
            WithTrailingWhitespace(lhsCoreToken, _),
            WithTrailingWhitespace(rhsCoreToken, _)
          ) =>
        equality(lhsCoreToken, rhsCoreToken)
      case _ => lhs == rhs
  end equality

  private def funnel(element: Token, primitiveSink: PrimitiveSink): Unit =
    element match
      case Whitespace(blanks)   =>
      case Significant(letters) => letters.foreach(primitiveSink.putChar)
      case WithTrailingWhitespace(coreToken, _) =>
        funnel(coreToken, primitiveSink)
    end match
  end funnel

  private def reduction(
      lhs: Either[Merge.Divergence.type, Merge.Result[Token]],
      rhs: Either[Merge.Divergence.type, Merge.Result[Token]]
  ): Either[Merge.Divergence.type, Merge.Result[Token]] =
    for
      lhsMerge <- lhs
      rhsMerge <- rhs
    yield lhsMerge -> rhsMerge match
      case (
            Merge.Result.FullyMerged(lhsElements),
            Merge.Result.FullyMerged(rhsElements)
          ) =>
        Merge.Result.FullyMerged(lhsElements ++ rhsElements)
      case (
            Merge.Result.FullyMerged(lhsElements),
            Merge.Result.MergedWithConflicts(rhsLeftElements, rhsRightElements)
          ) =>
        Merge.Result.MergedWithConflicts(
          lhsElements ++ rhsLeftElements,
          lhsElements ++ rhsRightElements
        )
      case (
            Merge.Result.MergedWithConflicts(lhsLeftElements, lhsRightElements),
            Merge.Result.FullyMerged(rhsElements)
          ) =>
        Merge.Result.MergedWithConflicts(
          lhsLeftElements ++ rhsElements,
          lhsRightElements ++ rhsElements
        )
      case (
            Merge.Result.MergedWithConflicts(lhsLeftElements, lhsRightElements),
            Merge.Result.MergedWithConflicts(rhsLeftElements, rhsRightElements)
          ) =>
        Merge.Result.MergedWithConflicts(
          lhsLeftElements ++ rhsLeftElements,
          lhsRightElements ++ rhsRightElements
        )

  private def createIrreducible(): Polynomial =
    val random = new Random(45877L)

    val degree = 51

    @tailrec
    def tryPolynomial: Polynomial =
      val result =
        createFromBytes(random.nextBytes((degree / 8) + 1), degree)
      end result
      if result.getReducibility == Reducibility.IRREDUCIBLE then result
      else tryPolynomial
      end if
    end tryPolynomial

    tryPolynomial
  end createIrreducible

  object tokenizer extends JavaTokenParsers:
    override def skipWhitespace: Boolean = false

    def apply(input: String): ParseResult[Vector[Token]] =
      parse(tokens, input).map(_.toVector)

    def tokens: Parser[List[Token]] = phrase(
      opt(whitespaceRun) ~ rep(tokenWithPossibleFollowingWhitespace) ^^ {
        case Some(whitespace) ~ tokens =>
          whitespace +: tokens
        case None ~ tokens =>
          tokens
      }
    )

    def tokenWithPossibleFollowingWhitespace: Parser[Token] =
      ((ident | wholeNumber | decimalNumber | floatingPointNumber | stringLiteral | miscellaneous) ^^ Significant.apply) ~ opt(
        whitespaceRun
      ) ^^ {
        case coreToken ~ Some(whitespace) =>
          WithTrailingWhitespace(coreToken, whitespace)
        case coreToken ~ None =>
          coreToken
      }

    def whitespaceRun: Parser[Token.Whitespace] =
      whiteSpace ^^ Token.Whitespace.apply

    def miscellaneous: Parser[String] =
      ".".r
  end tokenizer

  enum Token:
    def text: String = this match
      case Whitespace(blanks)   => blanks
      case Significant(letters) => letters
      case WithTrailingWhitespace(coreToken, whitespace) =>
        coreToken.text ++ whitespace.text

    case Whitespace(blanks: String)
    case Significant(letters: String)
    case WithTrailingWhitespace(
        coreToken: Token,
        whitespace: Whitespace
    )
  end Token

end MergingTextTest
