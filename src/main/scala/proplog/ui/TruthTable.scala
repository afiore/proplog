package proplog.ui

import cats.data.NonEmptyList
import proplog.ast.Expression
import cats.syntax.show._
import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.either._
import proplog.ast.Expression.BindingError

object TruthTable {
  case class Term(name: String, toBoolean: Boolean)

  case class Row(vars: Seq[Term]) {
    def :+ (v: Term): Row = copy(vars :+ v)
  }

  object Row {
    def fromExpressionNoSubtrees(env: Expression.Bindings)
                                (exp: Expression): Expression.Result[Row] =
      exp.eval.run(env).map { expValue =>
        Row(env) :+ Term(exp.show, expValue)
      }

    def fromExpression(env: Expression.Bindings)(
                       exp: Expression,
                       includeSubtrees: Boolean): Expression.Result[Row] =
      if (includeSubtrees) {
        exp.binaryExpressions.toList.traverseU { e =>
          e.eval.run(env).map(Term(e.show, _))
        }.map { vs =>
          vs.foldLeft[Row](Row(env))(_ :+ _)
        }
      } else fromExpressionNoSubtrees(env)(exp)

    def apply(env: Expression.Bindings): Row =
      Row(env.toSeq.map { case (k, v) => Term(k.toString, v) })
  }

  def apply(exp: Expression, includeSubtrees: Boolean): Expression.Result[TruthTable] = for {
    combinations <- exp.truthTable.asRight[BindingError]
    rows <- combinations.traverseU { env =>
      Row.fromExpression(env)(exp, includeSubtrees)
    }
  } yield TruthTable(rows)
}

case class TruthTable(rows: NonEmptyList[TruthTable.Row]) {
  val varNames: Seq[String] = rows.head.vars.map(_.name)
}
