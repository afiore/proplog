package sentential.parser

import atto._
import Atto._
import sentential.ast.Expression
import Expression._
import compat.cats._
import cats.syntax.either._

object Parser {
  def parseExpression(s: String): Expression.Result[Expression] =
    expression.parse(s).either.leftMap(Expression.ParserError)

  private object Tokens {
    val conj = """/\"""
    val disj = """\/"""
    val impl = "=>"
    val neg = '¬'
  }

  private def neg = char(Tokens.neg)
  private def disj = string(Tokens.disj)
  private def conj = string(Tokens.conj)
  private def impl = string(Tokens.impl)

  private val lit0: Parser[Expression] =
    letter.filter(_.isLower) -| Var.apply

  private def lit: Parser[Expression] =
    lit0 | (neg ~> lit0) -| Neg

  private def expression: Parser[Expression] = for {
    left <- lit | expInBraces
    _ <- skipWhitespace
    op <- conj | disj | impl
    _ <- skipWhitespace
    right <- lit | expInBraces
  } yield {
    op match {
      case Tokens.conj => Conj(left, right)
      case Tokens.disj => Disj(left, right)
      case Tokens.impl => Impl(left, right)
    }
  }

  private def expInBraces0: Parser[Expression] =
    char('(') ~> skipWhitespace ~> expression <~ skipWhitespace <~ char(')')

  private def expInBraces: Parser[Expression] =
    (neg ~> expInBraces0) -| Neg | expInBraces0
}
