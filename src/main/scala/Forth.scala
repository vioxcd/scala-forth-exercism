import ForthError.ForthError
import scala.collection.mutable.Stack
import scala.util.{Try, Success, Failure}

class State extends ForthEvaluatorState {
  var stack = Stack[Int]()

  override def toString = {
    stack.reverse.mkString(" ")
  }
}

class UDWState {
  private var currentlyParsing = false
  private var currentlyParsedName = ""
  private var currentlyParsedInputs = ""
  private var udws: Map[String, String] = Map()

  def markParsingStart = currentlyParsing = true

  def markParsingEnd = {
    currentlyParsing = false
    currentlyParsedName = ""
    currentlyParsedInputs = ""
  }

  def isParsingAndNameNotSet(): Boolean =
    currentlyParsing && currentlyParsedName == ""

  def isParsingAndNameIsSet(): Boolean =
    currentlyParsing && currentlyParsedName != ""

  def setUDWName(name: String) = {
    currentlyParsedName = name
    udws += (name -> "")
  }

  def updateUDWInputs(input: String) =
    // https://stackoverflow.com/a/55405528
    udws = udws.updatedWith(currentlyParsedName)(_.map(_ + " " + input))
}

class Forth extends ForthEvaluator {
  val udwState = new UDWState

  def eval(text: String): Either[ForthError, ForthEvaluatorState] = {
    // TODO: split the text splitting part and the `foldLeft` operation
    // so that the `eval` can be reused with UDWs and parametrized by `State`
    // step:
    // (1) create brand new State object, delegate the calculation elsewhere
    // (2) use that same calculation function to delegate udws run
    val state = new State
    evaluate(state, text)
  }

  private def evaluate(
      state: State,
      inputs: String
  ): Either[ForthError, State] = {
    inputs
      .split(" ")
      .foldLeft[Either[ForthError, State]](Right(state))((_state, str) => {
        _state match {
          case Right(s) => run(s, str)
          case Left(s)  => _state
        }
      })
  }

  private def run(state: State, str: String): Either[ForthError, State] = {
    // TODO: create a new class and private ref that maintains the creation of UDWs
    str match {
      case ":" => {
        udwState.markParsingStart
        Right(state)
      }
      case ";" => {
        udwState.markParsingEnd
        Right(state)
      }
      case str if udwState.isParsingAndNameNotSet => {
        udwState.setUDWName(str)
        Right(state)
      }
      case str if udwState.isParsingAndNameIsSet => {
        udwState.updateUDWInputs(str)
        Right(state)
      }
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
      case "over" => executeStackOps(state, str, 2)
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
        case "over" => {
          val top = state.stack.pop
          val next = state.stack.top
          state.stack.push(top, next)
          Right(state)
        }
      }
    }
  }
}
