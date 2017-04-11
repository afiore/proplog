package sentential

import sentential.parser.Parser
import atto._
import atto.Atto._
import compat.cats._
import sentential.ast.Expression
import cats.syntax.show._
import scala.scalajs.js.JSApp

object Main extends JSApp {
  def main() = {
    val exp = Parser.expression.parse("""a => b""").either
    exp.fold(exitWithError, e => evalExpression(e))
  }

  private def exitWithError(msg: String): Unit = {
    System.err.println(s"Failed parsing expression: $msg")
    System.exit(1)
  }

  private def evalExpression(e: Expression): Unit = {
    println(s"expression: ${e.show}")

    val varNames = Expression.varNames(e)

    Expression.booleanCombinations(varNames.size).foreach { values =>
      val bindings = varNames.zip(values).toMap
      println(bindings)
      println(e.eval.run(bindings))
      println("================================================================")
    }
  }

}
