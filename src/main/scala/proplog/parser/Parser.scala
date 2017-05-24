package proplog.parser

import atto._
import Atto._
import proplog.ast.Expression
import Expression._
import compat.cats._
import cats.syntax.either._

object Parser {
  def parseExpression(s: String): Result[Expression] =
    (expression | expInBraces | lit)
      .parseOnly(s)
      .either
      .leftMap(Expression.ParserError)

  private object Tokens {
    val conj = """/\"""
    val disj = """\/"""
    val impl = "=>"
    val iff = "<=>"
    val neg1 = '¬'
    val neg2 = '~'
  }

  private def neg  = char(Tokens.neg1) | char(Tokens.neg2)
  private def disj = string(Tokens.disj)
  private def conj = string(Tokens.conj)
  private def impl = string(Tokens.impl)
  private def iff  = string(Tokens.iff)

  val lit0: Parser[Expression] =
    letter.filter(_.isLower) -| Var.apply

  def lit: Parser[Expression] =
    lit0 | (neg ~> lit0) -| Neg

  def expression: Parser[Expression] = for {
    left <- lit | expInBraces
    _ <- skipWhitespace
    op <- conj | disj | impl | iff
    _ <- skipWhitespace
    right <- lit | expInBraces
  } yield {
    op match {
      case Tokens.conj => Conj(left, right)
      case Tokens.disj => Disj(left, right)
      case Tokens.impl => Impl(left, right)
      case Tokens.iff  => Iff(left, right)
    }
  }

  def expInBraces0: Parser[Expression] =
    char('(') ~> skipWhitespace ~> expression <~ skipWhitespace <~ char(')')

  def expInBraces: Parser[Expression] =
    (neg ~> expInBraces0) -| Neg | expInBraces0
}
