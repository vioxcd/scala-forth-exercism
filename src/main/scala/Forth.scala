import ForthError.ForthError
import scala.collection.mutable.Stack
import scala.util.{Try, Success, Failure}

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

  private def run(state: State, str: String): Either[ForthError, State] = {
    str match {
      case str if str.forall(Character.isDigit) => {
        state.stack.push(str.toInt)
        Right(state)
      }
      case "+" => _run(state, (x, y) => x + y)
      case "-" => _run(state, (x, y) => x - y)
      case "*" => _run(state, (x, y) => x * y)
      case "/" => _run(state, (x, y) => x / y)
      case _   => Left(ForthError.UnknownWord)
    }
  }

  private def _run(state: State, f: (Int, Int) => Int) = {
    if (state.stack.length < 2) {
      Left(ForthError.StackUnderflow)
    } else {
      val result = Try { state.stack.take(2).reverse.reduce(f) }

      result match {
        case Success(res) => {
          val drop = state.stack.drop(2)
          state.stack = drop.push(res)
          Right(state)
        }
        case Failure(res) => Left(ForthError.DivisionByZero)
      }
    }
  }
}
