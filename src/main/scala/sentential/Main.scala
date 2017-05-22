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
    val expInput = InputExpression("").render
    val output = div(id := "output").render
    val includeSubtreeCheckbox = input(tpe := "checkbox").render

    def renderTable(includeSubtree: Boolean): Unit = {
      val expResult = Parser.parseExpression(expInput.value)
      val truthTableResult = expResult.flatMap(e => TruthTable(e, includeSubtree).map(e -> _))

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

    includeSubtreeCheckbox.onchange = (e: dom.Event) =>
      renderTable(includeSubtreeCheckbox.checked)

    expInput.onkeyup = (_: dom.Event) =>
      renderTable(includeSubtreeCheckbox.checked)

    Seq(expInput, includeSubtreeCheckbox, output).foreach(document.body.appendChild(_))
  }
}
