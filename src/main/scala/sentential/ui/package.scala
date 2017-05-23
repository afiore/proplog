package sentential

package object ui {
  case class InputExpression(expAsString: String)

  case class State(rawExpression: String,
                   expandSubtree: Boolean)
}
