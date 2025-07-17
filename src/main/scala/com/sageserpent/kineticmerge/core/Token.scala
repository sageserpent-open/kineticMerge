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
        WithTrailingWhitespace(coreToken, whitespace)
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
        leadingIndentation +: tokens
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
              val lastTokenCondensedWithLinebreak = lastToken match
                case Whitespace(blanks) =>
                  Whitespace(blanks ++ linebreakRun.blanks)
                case significant: Significant =>
                  WithTrailingWhitespace(significant, linebreakRun)
                case WithTrailingWhitespace(
                      coreToken,
                      Whitespace(blanks)
                    ) =>
                  WithTrailingWhitespace(
                    coreToken,
                    Whitespace(blanks ++ linebreakRun.blanks)
                  )

              allButLastToken :+ lastTokenCondensedWithLinebreak
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
      case (LeadingIndentation(_), Significant(_))            => -1
      case (Significant(_), LeadingIndentation(_))            => 1
      case (LeadingIndentation(_), Whitespace(_))             => -1
      case (Whitespace(_), LeadingIndentation(_))             => 1
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
    end match
  end funnel

  case class Significant(content: String) extends Token

  private case class Whitespace(blanks: String) extends Token:
    require(blanks.isBlank)
  end Whitespace

  private case class LeadingIndentation(indentation: String) extends Token:
    require(indentation.isBlank)
  end LeadingIndentation

  private case class WithTrailingWhitespace(
      coreToken: Significant,
      whitespace: Whitespace
  ) extends Token

end Token

trait Token:
  def text: String = this match
    case LeadingIndentation(indentation)               => indentation
    case Whitespace(blanks)                            => blanks
    case Significant(content)                          => content
    case WithTrailingWhitespace(coreToken, whitespace) =>
      coreToken.text ++ whitespace.text
end Token
