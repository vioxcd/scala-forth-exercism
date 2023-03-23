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

  def updateUDWInputs(input: String) = {
    // if current input is an already defined UDW, then use its value
    // instead of taking it as a reference (reference might broke)
    val checkedInput =
      if (isUDWExists(input)) udws(input) else input

    // updatedWith: https://stackoverflow.com/a/55405528
    udws = if (udws(currentlyParsedName) == "") {
      udws.updated(currentlyParsedName, checkedInput)
    } else {
      udws.updatedWith(currentlyParsedName)(_.map(_ + " " + checkedInput))
    }
  }

  def isUDWExists(name: String): Boolean =
    udws.contains(name)

  def getInputString(name: String): String =
    udws(name)
}

class Forth extends ForthEvaluator {
  val udwState = new UDWState

  def eval(text: String): Either[ForthError, ForthEvaluatorState] = {
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
        // println(s"evaluate _state: ", _state)
        // println(s"evaluate str: $str")
        _state match {
          case Right(s) => run(s, str)
          case Left(s)  => _state
        }
      })
  }

  private def run(state: State, str: String): Either[ForthError, State] = {
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
      case str if udwState.isUDWExists(str) =>
        // ' putting this here (and above the last case)
        // ' because of the rules about `able to override built-in words & ops`
        executeUDWOps(state, str, udwState)
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

  private def executeUDWOps(
      state: State,
      udwName: String,
      udwState: UDWState
  ): Either[ForthError, State] = {
    // val inputString = udwState.getInputString(udwName)
    // println(s"executeUDWOps state: $state")
    // println(s"executeUDWOps udwName: $udwName")
    // println(s"executeUDWOps inputString: $inputString")
    evaluate(state, udwState.getInputString(udwName))
  }
}
