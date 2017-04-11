import sentential.parser.Parser
import atto._
import Atto._
import compat.cats._
import cats.syntax.either._
import cats.syntax.show._

import scala.annotation.tailrec

//val y = Parser.simpleExp.parse("¬a\\/b")

val exp = Parser.expression.parse("""( a /\ b) \/ b""").option.get
exp.show


exp.eval.run(Map('a' -> false, 'b' -> true))


