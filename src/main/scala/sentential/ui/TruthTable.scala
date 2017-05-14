package sentential.ui

import cats.data.NonEmptyList
import sentential.ast.Expression
import cats.syntax.show._
import cats.instances.either._
import cats.syntax.traverse._
import cats.syntax.either._
import sentential.ast.Expression.BindingError

object TruthTable {
  case class Variable(name: String, toBoolean: Boolean)

  case class Row(vars: Seq[Variable]) {
    def :+ (v: Variable): Row = copy(vars :+ v)
  }

  object Row {
    def apply(bindings: Expression.Bindings): Row =
      Row(bindings.toSeq.map { case (k, v) => Variable(k.toString, v) })
  }

  def apply(exp: Expression): Expression.Result[TruthTable] = for {
    combinations <- exp.truthTable.asRight[BindingError]
    rows <- combinations.traverseU { env =>
      exp.eval.run(env).map { v =>
        Row(env) :+ Variable(exp.show, v)
      }
    }
  } yield TruthTable(rows)
}

case class TruthTable(rows: NonEmptyList[TruthTable.Row]) {
  val varNames: Seq[String] = rows.head.vars.map(_.name)
}
