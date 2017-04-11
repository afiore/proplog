package sentential.parser


import atto._
import Atto._
import sentential.ast.Expression
import Expression._

object Parser {
  object Tokens {
    val conj = """/\"""
    val disj = """\/"""
    val impl = "=>"
    val neg = '¬'
  }

  def neg = char(Tokens.neg)
  def disj = string(Tokens.disj)
  def conj = string(Tokens.conj)
  def impl = string(Tokens.impl)

  private val lit0: Parser[Expression] =
    letter.filter(_.isLower) -| Lit.apply

  def lit: Parser[Expression] =
    lit0 | (neg ~> lit0) -| Neg

  def expression: Parser[Expression] = for {
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

  def expInBraces: Parser[Expression] =
    (neg ~> expInBraces0) -| Neg | expInBraces0
}
