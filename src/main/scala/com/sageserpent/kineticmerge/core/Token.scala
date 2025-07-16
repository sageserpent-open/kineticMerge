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
  private val whitespaceRun: Parser[Whitespace] =
    whiteSpace ^^ Whitespace.apply
  private val tokenWithPossibleFollowingWhitespace: Parser[Token] =
    ((ident | wholeNumber | decimalNumber | floatingPointNumber | stringLiteral | miscellaneous) ^^ Significant.apply) ~ opt(
      whitespaceRun
    ) ^^ {
      case coreToken ~ Some(whitespace) =>
        WithTrailingWhitespace(coreToken, whitespace)
      case coreToken ~ None =>
        coreToken
    }
  private val tokens: Parser[List[Token]] = phrase(
    opt(whitespaceRun) ~ rep(tokenWithPossibleFollowingWhitespace) ^^ {
      case Some(whitespace) ~ tokens =>
        whitespace +: tokens
      case None ~ tokens =>
        tokens
    }
  )

  override def skipWhitespace: Boolean = false

  def tokens(input: String): ParseResult[Vector[Token]] =
    parse(tokens, input).map(_.toVector)

  @tailrec
  def equality(lhs: Token, rhs: Token): Boolean =
    lhs -> rhs match
      // This first case is implied by the following two in combination via
      // recursion, but it's clearer and more efficient this way.
      case (
            WithTrailingWhitespace(lhsCoreToken, _),
            WithTrailingWhitespace(rhsCoreToken, _)
          ) =>
        equality(lhsCoreToken, rhsCoreToken)
      case (
            WithTrailingWhitespace(lhsCoreToken, _),
            _
          ) =>
        equality(lhsCoreToken, rhs)
      case (
            _,
            WithTrailingWhitespace(rhsCoreToken, _)
          ) =>
        equality(lhs, rhsCoreToken)
      case (Whitespace(_), Significant(_))                    => false
      case (Significant(_), Whitespace(_))                    => false
      case (Whitespace(lhsBlanks), Whitespace(rhsBlanks))     => true
      case (Significant(lhsContent), Significant(rhsContent)) =>
        lhsContent == rhsContent
  end equality

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
