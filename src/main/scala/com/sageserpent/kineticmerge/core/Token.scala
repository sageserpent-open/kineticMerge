package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.Token.{
  Significant,
  WithTrailingWhitespace,
  ident,
  opt,
  parse,
  phrase,
  rep,
  whiteSpace
}

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

end Token

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
