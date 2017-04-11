package sentential.ast

import cats.data.{Kleisli, ReaderT}
import cats.instances.either._
import cats.syntax.cartesian._

sealed trait Expression {
  def eval: Expression.BoundValue
}

object Expression {
  type Bindings = Map[Char, Boolean]
  type Result[A] = Either[BindingError, A]
  type BoundValue = Kleisli[Result, Bindings, Boolean]

  def bindings(exp: Expression): Set[Char] = {
    def go(e: Expression, acc: Set[Char]): Set[Char] = e match {
      case Lit(c, _) => acc + c
      case Neg(ee) => go(ee, acc)
      case Conj(l, r) => go(l, acc) ++ go(r, acc)
      case Disj(l, r) => go(l, acc) ++ go(r, acc)
      case Impl(l, r) => go(l, acc) ++ go(r, acc)
    }
    go(exp, Set.empty[Char])
  }

  final case class BindingError(label: Char)

  final case class Lit(label: Char, eval: BoundValue) extends Expression
  object Lit {
    def apply(c: Char): Expression =
      Lit(c, ReaderT[Result, Bindings, Boolean](m =>
        m.get(c).toRight(BindingError(c))))
  }

  final case class Neg(exp: Expression) extends Expression {
    override def eval = exp.eval.map(!_)
  }

  final case class Conj(left: Expression, right: Expression) extends Expression {
    override def eval = for {
      a <- left.eval
      b <- right.eval
    } yield a && b
  }

  final case class Disj(left: Expression, right: Expression) extends Expression {
     override def eval = for {
      a <- left.eval
      b <- right.eval
    } yield a || b
  }

  final case class Impl(left: Expression, right: Expression) extends Expression {
    override def eval = for {
      a <- left.eval
      b <- right.eval
    } yield if (a && !b) false else true
  }
}

