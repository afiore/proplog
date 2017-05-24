package proplog.ast

import org.scalatest.FreeSpec
import cats.syntax.show._
import cats.syntax.either._
import proplog.parser.Parser

class ExpressionTest extends FreeSpec {
  val expression =
    Parser.parseExpression("""m \/ (n /\ (x \/ ¬y))""").valueOr(e => sys.error(e.msg))

  "subTrees" - {
    "returns deepest nodes first" in {
      val xNotY = expression.subTrees.take(2)
      assert(xNotY.map(_.depth) === Seq(3, 3))
      assert(xNotY.map(_.expression.show).toSet === Set("x", "¬y"))
    }

    "contains all the leaf nodes" in {
      val leafs = Set("m", "n", "x", "¬y")
      assert((expression.subTrees.map(_.expression.show).toSet & leafs) === leafs)
    }
  }
  "binaryExpressions" - {
    "returns all the non-leaf subtrees in the expected order" in {
      val nonLeafs = expression.binaryExpressions.map(_.show)
      assert(nonLeafs === Seq("""x ∨ ¬y""", """(n ∧ (x ∨ ¬y))""", """(m ∨ (n ∧ (x ∨ ¬y)))"""))
    }
  }
}
