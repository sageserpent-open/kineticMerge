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
  rep,
  whiteSpace
}

import scala.annotation.tailrec
import scala.util.parsing.combinator.JavaTokenParsers

object Token extends JavaTokenParsers:
  override def skipWhitespace: Boolean = false

  def tokens(input: String): ParseResult[Vector[Token]] =
    parse(tokens, input).map(_.toVector)

  private def tokens: Parser[List[Token]] = phrase(
    opt(whitespaceRun) ~ rep(tokenWithPossibleFollowingWhitespace) ^^ {
      case Some(whitespace) ~ tokens =>
        whitespace +: tokens
      case None ~ tokens =>
        tokens
    }
  )

  private def tokenWithPossibleFollowingWhitespace: Parser[Token] =
    ((ident | wholeNumber | decimalNumber | floatingPointNumber | stringLiteral | miscellaneous) ^^ Significant.apply) ~ opt(
      whitespaceRun
    ) ^^ {
      case coreToken ~ Some(whitespace) =>
        WithTrailingWhitespace(coreToken, whitespace)
      case coreToken ~ None =>
        coreToken
    }

  private def miscellaneous: Parser[String] =
    ".".r

  private def whitespaceRun: Parser[Token.Whitespace] =
    whiteSpace ^^ Token.Whitespace.apply

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
      case _ => lhs == rhs
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
      case (Whitespace(_), Significant(_)) => -1
      case (Significant(_), Whitespace(_)) => 1
      case (Whitespace(lhsBlanks), Whitespace(rhsBlanks)) =>
        Order.compare(lhsBlanks, rhsBlanks)
      case (Significant(lhsLetters), Significant(rhsLetters)) =>
        Order.compare(lhsLetters, rhsLetters)
  end comparison

  @tailrec
  def funnel(token: Token, primitiveSink: PrimitiveSink): Unit =
    token match
      case Whitespace(blanks)   =>
      case Significant(letters) => letters.foreach(primitiveSink.putChar)
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
    case Whitespace(blanks)   => blanks
    case Significant(letters) => letters
    case WithTrailingWhitespace(coreToken, whitespace) =>
      coreToken.text ++ whitespace.text
end Token
