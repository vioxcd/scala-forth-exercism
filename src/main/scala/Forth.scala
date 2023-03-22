import ForthError.ForthError
import scala.collection.mutable.Stack

class State extends ForthEvaluatorState {
  var stack = Stack[Int]()

  override def toString = {
    stack.reverse.mkString(" ")
  }
}

class Forth extends ForthEvaluator {
  def eval(text: String): Either[ForthError, ForthEvaluatorState] = {
    val inputs = text
      .split(" ")
      .map(_.toInt)
      .foldLeft[State](new State)((s, i) => {
        s.stack.push(i)
        s
      })

    Right(inputs)
  }
}
