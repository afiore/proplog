package sentential

import sentential.parser.Parser
import sentential.ui.AsHtml._
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import sentential.ui.{InputExpression, TruthTable, State}

import scalatags.JsDom.all._
import dom.document

object Main extends JSApp {
  @JSExport
  def main(): Unit = {
    val expInput = InputExpression("").render
    val output = div(id := "output").render
    val checkbox = input(tpe := "checkbox").render
    val msg = p("Evaluate intermediate expressions").render
    val msgDiv = div(cls := "msg").render

    Seq(checkbox, msg, div(cls := "clear").render)
      .foreach(msgDiv.appendChild)

    def renderTable(state: ui.State): Unit = {
      val expResult = Parser.parseExpression(state.rawExpression)
      val truthTableResult = expResult.flatMap(TruthTable(_, state.expandSubtree))

      val newNode = truthTableResult.fold[dom.Element]({ err =>
        p(cls := "error", err.msg).render
      }, _.render)

      Option(output).foreach { o =>
        if (o.hasChildNodes()) {
          o.removeChild(o.firstElementChild)
        }
        o.appendChild(newNode)
      }
    }

    def readState: State =
      State(expInput.value, checkbox.checked)

    checkbox.onchange = (e: dom.Event) =>
      renderTable(readState)

    expInput.onkeyup = (_: dom.Event) =>
      renderTable(readState)

    Seq(expInput, msgDiv, output)
      .foreach(document.body.appendChild)
  }
}
