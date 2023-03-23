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
      case "+"    => executeArithmeticOps(state, (x, y) => x + y)
      case "-"    => executeArithmeticOps(state, (x, y) => x - y)
      case "*"    => executeArithmeticOps(state, (x, y) => x * y)
      case "/"    => executeArithmeticOps(state, (x, y) => x / y)
      case "dup"  => executeStackOps(state, str, 1)
      case "drop" => executeStackOps(state, str, 1)
      case "swap" => executeStackOps(state, str, 2)
      case _      => Left(ForthError.UnknownWord)
    }
  }

  private def executeArithmeticOps(
      state: State,
      f: (Int, Int) => Int
  ): Either[ForthError, State] = {
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

  private def executeStackOps(
      state: State,
      ops: String,
      k: Int
  ): Either[ForthError, State] = {
    if (state.stack.length < k) {
      Left(ForthError.StackUnderflow)
    } else {
      ops match {
        case "dup" => {
          state.stack.push(state.stack.top)
          Right(state)
        }
        case "drop" => {
          state.stack.pop
          Right(state)
        }
        case "swap" => {
          val top = state.stack.pop
          val next = state.stack.pop
          state.stack.push(top, next)
          Right(state)
        }
      }
    }
  }
}
