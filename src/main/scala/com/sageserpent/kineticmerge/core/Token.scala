package com.sageserpent.kineticmerge.core

import cats.kernel.Order
import com.google.common.hash.PrimitiveSink
import com.sageserpent.kineticmerge.core.Token.*

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
        coreToken.condenseWithFollowingWhitespace(whitespace)
      case coreToken ~ None =>
        coreToken
    }
  private val leadingIndentation: Parser[LeadingIndentation] =
    """\h+""".r ^^ (content => LeadingIndentation(content))
  private val line: Parser[List[Token]] =
    opt(leadingIndentation) ~ rep(
      tokenWithPossibleFollowingWhitespace
    ) ^^ {
      case Some(leadingIndentation) ~ tokens =>
        tokens match
          case Nil                           => List(leadingIndentation)
          case firstToken :: remainingTokens =>
            WithLeadingIndentation(
              leadingIndentation,
              firstToken
            ) :: remainingTokens
      case None ~ tokens =>
        tokens
    }
  // NOTE: don't handle linebreaks as *individual* tokens, because the idea is
  // to condense a run of linebreaks with the last token in the preceding
  // line. Otherwise, we'll end up with the first linebreak being condensed
  // and then a run of individual linebreaks, and this causes trivial sections
  // to be generated that can in turn play havoc with merging; this is exposed
  // by `MainTest.cleanMergeOfAFileAddedInBothBranches`.
  private val linebreakRun: Parser[Whitespace] =
    """\R+""".r ^^ (content => Whitespace(content))
  private val tokens: Parser[List[Token]] = phrase(
    (rep(line ~ linebreakRun ^^ { case line ~ linebreakRun =>
      if line.isEmpty then List(linebreakRun)
      else
        // This is only here because we're forced to work with `List`, and
        // traversing the same list twice for the init list and the last token
        // just feels wrong.
        @tailrec
        def condenseLastTokenWithLinebreak(
            tokens: List[Token],
            allButLastToken: Vector[Token]
        ): Vector[Token] =
          // This is guarded by virtue of the enclosing if-statement.
          (tokens: @unchecked) match
            case lastToken :: Nil =>
              allButLastToken :+ lastToken.condenseWithFollowingWhitespace(
                linebreakRun
              )
            case head :: tail =>
              condenseLastTokenWithLinebreak(
                tail,
                allButLastToken.appended(head)
              )
          end match
        end condenseLastTokenWithLinebreak

        condenseLastTokenWithLinebreak(line, allButLastToken = Vector.empty)
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
            WithLeadingIndentation(_, lhsCoreToken),
            WithLeadingIndentation(_, rhsCoreToken)
          ) =>
        comparison(lhsCoreToken, rhsCoreToken)
      case (
            WithLeadingIndentation(_, lhsCoreToken),
            _
          ) =>
        comparison(lhsCoreToken, rhs)
      case (
            _,
            WithLeadingIndentation(_, rhsCoreToken)
          ) =>
        comparison(lhs, rhsCoreToken)
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
      case (LeadingIndentation(_), Significant(_)) => -1
      case (Significant(_), LeadingIndentation(_)) => 1
      // NOTE: treat leading indentation as being equal to whitespace, even if
      // the tokens aren't condensed.
      case (LeadingIndentation(_), Whitespace(_))             => 0
      case (Whitespace(_), LeadingIndentation(_))             => 0
      case (Whitespace(_), Significant(_))                    => -1
      case (Significant(_), Whitespace(_))                    => 1
      case (LeadingIndentation(_), LeadingIndentation(_))     => 0
      case (Whitespace(_), Whitespace(_))                     => 0
      case (Significant(lhsContent), Significant(rhsContent)) =>
        Order.compare(lhsContent, rhsContent)
  end comparison

  @tailrec
  def funnel(token: Token, primitiveSink: PrimitiveSink): Unit =
    token match
      case LeadingIndentation(_) =>
      case Whitespace(_)         =>
      case Significant(content)  => content.foreach(primitiveSink.putChar)
      case WithTrailingWhitespace(coreToken, _) =>
        funnel(coreToken, primitiveSink)
      case WithLeadingIndentation(_, coreToken) =>
        funnel(coreToken, primitiveSink)
    end match
  end funnel

end Token

enum Token:
  this match
    case LeadingIndentation(indentation) => require(indentation.isBlank)
    case Whitespace(blanks)              => require(blanks.isBlank)
    case Significant(content)            => require(!content.isBlank)
    case _                               =>
  end match

  def text: String = this match
    case LeadingIndentation(indentation)               => indentation
    case Whitespace(blanks)                            => blanks
    case Significant(content)                          => content
    case WithTrailingWhitespace(coreToken, whitespace) =>
      coreToken.text ++ whitespace.text
    case WithLeadingIndentation(leadingIndentation, coreToken) =>
      leadingIndentation.text ++ coreToken.text

  def condenseWithFollowingWhitespace(whitespace: Whitespace): Token =
    this match
      case Whitespace(blanks) =>
        Whitespace(blanks ++ whitespace.blanks)
      case significant: Significant =>
        WithTrailingWhitespace(significant, whitespace)
      case WithTrailingWhitespace(
            coreToken,
            Whitespace(blanks)
          ) =>
        WithTrailingWhitespace(
          coreToken,
          Whitespace(blanks ++ whitespace.blanks)
        )
      case leadingIndentation: LeadingIndentation =>
        WithLeadingIndentation(leadingIndentation, whitespace)
      case WithLeadingIndentation(leadingIndentation, coreToken) =>
        WithLeadingIndentation(
          leadingIndentation,
          coreToken.condenseWithFollowingWhitespace(whitespace)
        )

  case Significant(content: String)

  case Whitespace(blanks: String)

  case LeadingIndentation(indentation: String)

  case WithTrailingWhitespace(coreToken: Significant, whitespace: Whitespace)

  case WithLeadingIndentation(
      leadingIndentation: LeadingIndentation,
      coreToken: Token
  )
end Token
