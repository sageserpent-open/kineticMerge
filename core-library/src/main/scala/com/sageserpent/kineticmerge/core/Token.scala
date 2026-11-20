package com.sageserpent.kineticmerge.core

import cats.kernel.Order
import com.google.common.collect.{BiMap, HashBiMap, Maps}
import com.google.common.hash.PrimitiveSink
import com.sageserpent.kineticmerge.core.Token.*
import com.softwaremill.tagging.*

import scala.annotation.tailrec
import scala.util.parsing.combinator.JavaTokenParsers

object Token extends JavaTokenParsers:
  private object Tags:
    trait UniqueId
  end Tags

  private val uniqueIdsForContent: BiMap[String, Int @@ Tags.UniqueId] =
    // SBT runs tests in parallel, so we need to use a thread-safe wrapper.
    Maps.synchronizedBiMap(HashBiMap.create())
  private var nextUniqueId: Int @@ Tags.UniqueId = 0.taggedWith[Tags.UniqueId]

  private val miscellaneous: Parser[String] =
    """.""".r
  private val horizontalWhitespaceRun: Parser[Whitespace] =
    """\h+""".r ^^ (content => Whitespace(content))
  private val tokenWithPossibleFollowingWhitespace: Parser[Token] =
    ((ident | wholeNumber | decimalNumber | floatingPointNumber | stringLiteral | miscellaneous) ^^ significant) ~ opt(
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

  def significant(content: String): Significant =
    Significant(
      uniqueIdsForContent.computeIfAbsent(
        content,
        _ =>
          val uniqueId = nextUniqueId
          // TODO: finesse this - why can't we just use `+=`, or at least drop
          // the re-tagging?
          nextUniqueId = (1 + nextUniqueId).taggedWith[Tags.UniqueId]

          uniqueId
      )
    )

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
      case (LeadingIndentation(_), Whitespace(_))         => 0
      case (Whitespace(_), LeadingIndentation(_))         => 0
      case (Whitespace(_), Significant(_))                => -1
      case (Significant(_), Whitespace(_))                => 1
      case (LeadingIndentation(_), LeadingIndentation(_)) => 0
      case (Whitespace(_), Whitespace(_))                 => 0
      case (Significant(lhsId), Significant(rhsId))       =>
        Order[Int].compare(lhsId, rhsId)
  end comparison

  @tailrec
  def funnel(token: Token, primitiveSink: PrimitiveSink): Unit =
    token match
      case LeadingIndentation(_) =>
      case Whitespace(_)         =>
      case Significant(uniqueId) => primitiveSink.putInt(uniqueId)
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
    case Significant(uniqueId)           => require(0 <= uniqueId)
    case _                               =>
  end match

  def text: String = this match
    case LeadingIndentation(indentation) => indentation
    case Whitespace(blanks)              => blanks
    case Significant(uniqueId) => uniqueIdsForContent.inverse().get(uniqueId)
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

  case Significant private[Token] (uniqueId: Int @@ Token.Tags.UniqueId)

  case Whitespace private[Token] (blanks: String)

  case LeadingIndentation private[Token] (indentation: String)

  case WithTrailingWhitespace private[Token] (
      coreToken: Significant,
      whitespace: Whitespace
  )

  case WithLeadingIndentation private[Token] (
      leadingIndentation: LeadingIndentation,
      coreToken: Token
  )
end Token
