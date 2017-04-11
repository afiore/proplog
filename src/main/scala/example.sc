import sentential.parser.Parser

import atto._
import Atto._
import compat.cats._
import cats.syntax.either._

//val y = Parser.simpleExp.parse("¬a\\/b")

val x = Parser.expression.parse("""( a \/ b) /\ ( x => y)""")
x.option.get.eval
