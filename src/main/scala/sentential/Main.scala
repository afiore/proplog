package sentential

import sentential.parser.Parser
import sentential.ui.AsHtml._
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import sentential.ui.{InputExpression, TruthTable}

import scalatags.JsDom.all._
import dom.document

object Main extends JSApp {
  @JSExport
  def main(): Unit = {
    val input = InputExpression("").render
    val output = div(id := "output").render

    input.onkeyup = (_: dom.Event) => {
      val expResult = Parser.parseExpression(input.value)
      val truthTableResult = expResult.flatMap(e => TruthTable(e).map(e -> _))

      val newNode = truthTableResult.fold[dom.Element]({ err =>
        p(cls := "error", err.msg).render
      },{ case (e, truthTable) => truthTable.render })

      Option(output).foreach { o =>
        if (o.hasChildNodes()) {
          o.removeChild(o.firstElementChild)
        }
        o.appendChild(newNode)
      }
    }

    document.body.appendChild(input)
    document.body.appendChild(output)
  }
}
