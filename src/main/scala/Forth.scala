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
    text
      .split(" ")
      .foldLeft[Either[ForthError, State]](Right(new State))((state, str) => {
        state match {
          case Right(s) => run(s, str)
          case Left(s)  => state
        }
      })
  }

  def run(state: State, str: String): Either[ForthError, State] = {
    str match {
      case "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => {
        state.stack.push(str.toInt)
        Right(state)
      }
      case "+" => {
        val result = state.stack.take(2).sum
        val drop = state.stack.drop(2)
        state.stack = drop.push(result)
        Right(state)
      }
      case _ => Left(ForthError.UnknownWord)
    }
  }
}
