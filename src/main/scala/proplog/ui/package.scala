package proplog

package object ui {
  case class State(rawExpression: String,
                   expandSubtree: Boolean)
}
