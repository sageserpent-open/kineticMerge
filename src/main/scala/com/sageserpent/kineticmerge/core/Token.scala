package com.sageserpent.kineticmerge.core

import cats.kernel.Order
import com.google.common.hash.PrimitiveSink
import com.sageserpent.kineticmerge.core.Token.{
  Significant,
  Whitespace,
  WithTrailingWhitespace,
  ident,
  opt,
  parse,
  phrase,
  rep
}

import scala.annotation.tailrec
import scala.util.parsing.combinator.JavaTokenParsers

object Token extends JavaTokenParsers:
  private val miscellaneous: Parser[String] =
    """.""".r
  private val horizontalWhitespaceRun: Parser[Whitespace] =
    """\h+""".r ^^ (content => Whitespace(content))
  private val tokenWithPossibleFollowingWhitespace: Parser[Token] =
    ((ident | wholeNumber | decimalNumber | floatingPointNumber | stringLiteral | miscellaneous) ^^ Significant.apply) ~ opt(
      horizontalWhitespaceRun
    ) ^^ {
      case coreToken ~ Some(whitespace) =>
        WithTrailingWhitespace(coreToken, whitespace)
      case coreToken ~ None =>
        coreToken
    }
    // NOTE: don't handle linebreaks as *individual* tokens, because the idea is
    // to condense a run of linebreaks with the last token in the preceding
    // line. Otherwise, we'll end up with the first linebreak being condensed
    // and then a run of individual linebreaks, and this causes trivial sections
    // to be generated that can in turn play havoc with merging; this is exposed
    // by `MainTest.cleanMergeOfAFileAddedInBothBranches`.
  private val linebreakRun: Parser[Whitespace] =
    """\R+""".r ^^ (content => Whitespace(content))
  private val line: Parser[List[Token]] =
    opt(horizontalWhitespaceRun) ~ rep(
      tokenWithPossibleFollowingWhitespace
    ) ^^ {
      case Some(leadingIndentation) ~ tokens =>
        leadingIndentation +: tokens
      case None ~ tokens =>
        tokens
    }
  private val tokens: Parser[List[Token]] = phrase(
    (rep(line ~ linebreakRun ^^ { case line ~ linebreak =>
      if line.isEmpty then List(linebreak)
      else
        val allButLastToken = line.init
        val lastToken       = line.last

        val lastTokenCondensedWithLinebreak = lastToken match
          case Whitespace(blanks) => Whitespace(blanks ++ linebreak.blanks)
          case significant: Significant =>
            WithTrailingWhitespace(significant, linebreak)
          case WithTrailingWhitespace(coreToken, Whitespace(blanks)) =>
            WithTrailingWhitespace(
              coreToken,
              Whitespace(blanks ++ linebreak.blanks)
            )

        allButLastToken :+ lastTokenCondensedWithLinebreak
    }) ^^ (_.flatten)) ~ opt(line) ^^ {
      case contentFromLeadingLines ~ Some(finalLineWithoutLinebreak) =>
        contentFromLeadingLines ++ finalLineWithoutLinebreak
      case completeContent ~ None => completeContent
    }
  )

  override def skipWhitespace: Boolean = false

  def tokens(input: String): ParseResult[Vector[Token]] =
    parse(tokens, input).map(_.toVector)
  
  def equality(lhs: Token, rhs: Token): Boolean = 0 == comparison(lhs, rhs)

  @tailrec
  def comparison(lhs: Token, rhs: Token): Int =
    lhs -> rhs match
      // This first case is implied by the following two in combination via
      // recursion, but it's clearer and more efficient this way.
      case (
            WithTrailingWhitespace(lhsCoreToken, _),
            WithTrailingWhitespace(rhsCoreToken, _)
          ) =>
        comparison(lhsCoreToken, rhsCoreToken)
      case (
            WithTrailingWhitespace(lhsCoreToken, _),
            _
          ) =>
        comparison(lhsCoreToken, rhs)
      case (
            _,
            WithTrailingWhitespace(rhsCoreToken, _)
          ) =>
        comparison(lhs, rhsCoreToken)
      case (Whitespace(_), Significant(_))                    => -1
      case (Significant(_), Whitespace(_))                    => 1
      case (Whitespace(lhsBlanks), Whitespace(rhsBlanks))     => 0
      case (Significant(lhsContent), Significant(rhsContent)) =>
        Order.compare(lhsContent, rhsContent)
  end comparison

  @tailrec
  def funnel(token: Token, primitiveSink: PrimitiveSink): Unit =
    token match
      case Whitespace(blanks)   =>
      case Significant(content) => content.foreach(primitiveSink.putChar)
      case WithTrailingWhitespace(coreToken, _) =>
        funnel(coreToken, primitiveSink)
    end match
  end funnel

  case class Whitespace(blanks: String) extends Token:
    require(blanks.isBlank)
  end Whitespace

  case class Significant(content: String) extends Token

  case class WithTrailingWhitespace(
      coreToken: Significant,
      whitespace: Whitespace
  ) extends Token

end Token

trait Token:
  def text: String = this match
    case Whitespace(blanks)                            => blanks
    case Significant(content)                          => content
    case WithTrailingWhitespace(coreToken, whitespace) =>
      coreToken.text ++ whitespace.text
end Token
