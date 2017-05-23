package sentential.ast

import cats.Show
import cats.data.{Kleisli, NonEmptyList}
import cats.instances.either._
import scala.annotation.tailrec

/**
 * An abstract syntax tree for propositional logic.
 *
 * Supports conjunction, disjunction, negation, implication and equivalence. Boolean literals are not
 * encoded directly as part of the tree, but are bound to variables at evaluation time
 * (see Expression.Bindings and Expression.BoundBoolean)
 */
sealed trait Expression {
  def eval: Expression.BoundBoolean
}

sealed trait BinaryExpression extends Expression {
  def left: Expression
  def right: Expression
  def symbol: String

  def evalLR(op: (Boolean, Boolean) => Boolean): Expression.BoundBoolean = for {
    l <- left.eval
    r <- right.eval
  } yield op(l,r)
}

object Expression {
  sealed trait Error {
    def msg: String
  }
  final case class BindingError(label: Char) extends Error {
    override def msg = s"Unbound variable: $label"
  }
  final case class ParserError(msg: String) extends Error

  type Bindings = Map[Char, Boolean]
  type Result[A] = Either[Error, A]

  // Bindings => Result[Boolean]
  type BoundBoolean = Kleisli[Result, Bindings, Boolean]

  final case class Var(label: Char, eval: BoundBoolean) extends Expression
  object Var {
    def apply(c: Char): Expression =
      Var(c, Kleisli[Result, Bindings, Boolean](m =>
        m.get(c).toRight(BindingError(c))))
  }

  final case class Neg(exp: Expression) extends Expression {
    override def eval = exp.eval.map(!_)
  }

  final case class Conj(left: Expression, right: Expression) extends BinaryExpression {
    override def eval = evalLR(_ && _)
    override val symbol = "∧"
  }

  final case class Disj(left: Expression, right: Expression) extends BinaryExpression {
    override def eval = evalLR(_ || _)
    override val symbol = "∨"
  }

  final case class Impl(left: Expression, right: Expression) extends BinaryExpression {
    override def eval = evalLR { (l, r) => if (l && !r) false else true }
    override def symbol = "⇒"
  }

  final case class Iff(left: Expression, right: Expression) extends BinaryExpression {
    override def eval = evalLR { (l, r) => l & r || !l && !r}
    override def symbol = "⇔"
  }

  final case class Node(expression: Expression, depth: Int)

  implicit class ExpressionOps(exp: Expression) {
    /* Returns the expression's sub-trees sorted in descending order (deepest first)
     *
     * Given a sample tree a:
     *
     *              a
     *            /   \
     *           b    c
     *              /   \
     *            d      e
     *          / \     / \
     *        d1  d2   f   g
     *
     *
     *  c.subTrees
     *  => Seq(d1, d2, f, g, d, e)
     *
     *  a.binaryExpressions
     *  => Seq(d, e, c, a)
     *
     * */
    def subTrees: Seq[Node] = {
      def go(e: Expression, acc: Seq[Expression.Node], depth: Int): Seq[Expression.Node] = e match {
        case bi: BinaryExpression =>
          go(bi.left, acc, depth + 1) ++ (Node(e, depth) +: go(bi.right, acc, depth + 1))
        case _ => Node(e, depth) +: acc
      }
      go(exp, Nil, 0)
        .sortBy(_.depth)(Ordering[Int].reverse)
    }

    def childExpressions: Seq[Expression] = subTrees.map(_.expression)

    def binaryExpressions: Seq[Expression] =
      subTrees.collect { case n@Node(e: BinaryExpression, _) => n.expression }

    def truthTable: NonEmptyList[Bindings] = {
      val names = varNames
      booleanCombinations(names.size).map { values =>
        names.zip(values).toMap
      }
    }

    def varNames: Set[Char] = {
      def go(e: Expression, acc: Set[Char]): Set[Char] = e match {
        case Var(c, _) => acc + c
        case Neg(ee) => go(ee, acc)
        case ee: BinaryExpression => go(ee.left, acc) ++ go(ee.right, acc)
      }
      go(exp, Set.empty[Char])
    }

    private def booleanCombinations(n: Int): NonEmptyList[List[Boolean]] = {
      val trueFalse = List(true, false)
      @tailrec
      def go(i: Int, acc: List[List[Boolean]]): List[List[Boolean]] = {
        if (i == 0)
          acc
        else
          go(i -1, acc.flatMap(xs => trueFalse.map(y => xs :+ y )))
      }
      NonEmptyList.fromListUnsafe(go(n - 1, trueFalse.map(List(_))))
    }
  }

  private def prettyPrint(exp: Expression): String = exp match {
    case Var(c, _) =>
      s"$c"
    case Neg(e) =>
      s"¬${prettyPrint(e)}"
    case e: BinaryExpression =>
      s"(${prettyPrint(e.left)} ${e.symbol} ${prettyPrint(e.right)})"
  }

  private val BracesPattern = "\\((.*?)\\)".r
  private def removeOuterBraces(s: String): String = s match {
    case BracesPattern(e) => e
    case _ => s
  }

  implicit def expShow: Show[Expression] =
    Show.show(prettyPrint _ andThen removeOuterBraces)
}

