package proplog.ui

import org.scalajs.dom
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import dom.html
import TruthTable._

trait AsHtml[A, Out <: dom.Element] {
  def asHtml(a: A): TypedTag[Out]
}

object AsHtml {
  def render[A, Out <: dom.Element](f: A => TypedTag[Out])
    : AsHtml[A, Out] = (a: A) => f(a)

  implicit class HtmlTagSyntax[A, Out <: dom.Element](a: A)(implicit ev: AsHtml[A, Out]) {
    def asHtml: TypedTag[Out] = ev.asHtml(a)
    def render: Out = asHtml.render
  }

  implicit val renderVar = AsHtml.render[TruthTable.Term, html.TableCell] { v =>
    td(if(v.toBoolean) "T" else "F")
  }

  implicit val renderRow = AsHtml.render[Row, html.TableRow] { r =>
    tr(r.vars.map(_.asHtml))
  }

  implicit val renderTruthTable = AsHtml.render[TruthTable, html.Table] { t =>
    table(cls := "table table-striped table-bordered",
      thead(
        t.varNames.map(c => th(c.toString))
      ),
      tbody(
        t.rows.toList.map(_.asHtml)
      )
    )
  }
}
