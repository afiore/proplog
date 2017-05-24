package sentential

import sentential.parser.Parser
import sentential.ui.AsHtml._
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import sentential.ui.{TruthTable, State}

import scalatags.JsDom.all._
import dom.document

object Main extends JSApp {
  @JSExport
  def main(): Unit = {
    document.addEventListener("DOMContentLoaded", { (e: dom.Event) =>
      bindElements()
    }, useCapture = false)
  }

  private def bindElements(): Unit = {
    val expInput = getInput("expression")
    val checkbox = getInput("expand-exp")
    val o = document.getElementById("output")

    def readState: State =
      State(expInput.value, checkbox.checked)

    def renderTable(state: ui.State) = {
      val expResult = Parser.parseExpression(state.rawExpression)
      val truthTableResult = expResult.flatMap(TruthTable(_, state.expandSubtree))
      val newNode = truthTableResult.fold[dom.Element]({ err =>
        p(cls := "error", err.msg).render
      }, _.render)

      while (o.hasChildNodes()) {
        if (o.hasChildNodes()) {
          o.removeChild(o.firstChild)
        }
      }
      o.appendChild(newNode)
    }

    checkbox.onchange = (e: dom.Event) =>
      renderTable(readState)

    expInput.onkeyup = (_: dom.Event) =>
      renderTable(readState)
  }

  private def getInput(id: String): dom.html.Input =
    document.getElementById(id).asInstanceOf[dom.html.Input]
}
