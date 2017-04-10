package sentential.ast

import cats.data.Reader

sealed trait Expression {
  def value: Boolean
}

object Expression {
  type Bindings = Map[Char, Boolean]
  type Result[A] = Reader[Bindings, A]

  final case class Lit(label: Char, value: Boolean) extends Expression

  final case class Neg(exp: Expression) extends Expression {
    override def value = !exp.value
  }

  final case class Conj(left: Expression, right: Expression) extends Expression {
    override def value = left.value && right.value
  }

  final case class Disj(left: Expression, right: Expression) extends Expression {
    override def value = left.value || right.value
  }

  final case class Impl(left: Expression, right: Expression) extends Expression {
    override def value = !right.value
  }
}

