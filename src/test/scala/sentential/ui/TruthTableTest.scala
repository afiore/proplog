package sentential.ui

import org.scalatest.Matchers
import org.scalatest.FreeSpec

import sentential.ast.Expression._
import sentential.ui.TruthTable._

class TruthTableTest extends FreeSpec with Matchers {
  object Example {
    val a = Var('a')
    val b = Var('b')
    val c = Var('c')
    val d = Var('d')
    val exp = Disj(Conj(a, b), Conj(c, d))
    val env = Map('a' -> false, 'b' -> false, 'c' -> true, 'd' -> true)
  }

  import Example._

  "Row.fromExpression" - {
    "renders an expression without its child expressions" in {
      assertResult(Right(
        Row(Seq(
          Term("a", false),
          Term("b", false),
          Term("c", true),
          Term("d", true),
          Term("((a ∧ b) ∨ (c ∧ d))", true)))))(Row.fromExpression(env)(exp, includeSubtrees = false))
    }

    "renders an expression with one column for each binary sub expression" in {
      assertResult(Right(
        Row(Seq(
          Term("a", false),
          Term("b", false),
          Term("c", true),
          Term("d", true),
          Term("a ∧ b", false),
          Term("c ∧ d", true),
          Term("((a ∧ b) ∨ (c ∧ d))", true)))))(Row.fromExpression(env)(exp, includeSubtrees = true))
    }
  }
}
