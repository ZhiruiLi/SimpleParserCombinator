package parser

import SimpleParserTypes._

import scala.util.matching.Regex

object SimpleParserTypes {

  type Parser[+A] = State => Result[A]

  case class State(pos: Position) {
    lazy val currentInput: String = pos.input.substring(pos.offset)
    lazy val fullInput: String = pos.input
    lazy val currentLine = pos.currentLine
    lazy val lineNumber = pos.line
    lazy val columnNumber = pos.column
    def moveForward(n: Int): State = copy(pos = pos.moveForward(n))
    def toError(name: String, msg: String): ParseError = pos.toError(name, msg)
  }

  sealed trait Result[+A]
  case class Success[+A](get: A, inputConsumed: String) extends Result[A]
  case class Failure(error: ParseError) extends Result[Nothing]
}

object SimpleParsers extends Parsers[Parser] {

  def run[A](parser: Parser[A])(str: String): Either[ParseError, A] =
    parser(State(Position(str))) match {
      case Success(a, _) => Right(a)
      case Failure(err) => Left(err)
    }

  def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = state => {
    p(state) match {
      case Success(a, s1) =>
        val newState = state.moveForward(s1.length)
        f(a)(newState) match {
          case Success(b, s2) => Success(b, s1 + s2)
          case f@Failure(_) => f
        }
      case f@Failure(_) => f
    }
  }

  def beautifyString(str: String): String = str.map {
    case '\n' => "\\n"
    case '\t' => "\\t"
    case '\f' => "\\f"
    case '\'' => "\\'"
    case c => c.toString
  }.mkString

  implicit def string(s: String): Parser[String] = state => {
    val current = state.currentInput
    if (current.startsWith(s)) Success(s, s)
    else {
      val originalS = beautifyString(current.substring(0, Math.min(s.length, current.length)))
      Failure(state.toError("string",
        s"require '$s', but found '$originalS'"
      ))
    }
  }

  implicit def regex(r: Regex): Parser[String] = state => {
    val current = state.currentInput
    r.findPrefixOf(current) match {
      case Some(s) => Success(s, s)
      case None => {
        val originalS =
          beautifyString(if (current.length > 5) current.substring(0, 5) + " ..." else current)
        Failure(state.toError("regex",
          s"require regex '${r.toString}', but found '$originalS'"
        ))
      }
    }
  }

  /**
    * Due to the lack of tail call optimization, the default many and many1 implementation is not efficient.
    * @param p the parser
    * @tparam A the return type of the parser
    * @return a parser of List[A]
    */
  override def many[A](p: Parser[A]): Parser[List[A]] = {
    def rec(state: State, acc: List[A], consumedLen: Int): (List[A], Int) = p(state) match {
      case Success(a, s) => rec(state.moveForward(s.length), a::acc, consumedLen + s.length)
      case Failure(_) => (acc.reverse, consumedLen)
    }
    state => {
      val (res, len) = rec(state, Nil, 0)
      Success(res, state.currentInput.substring(0, len))
    }
  }

  override def item: Parser[Char] = state => {
    val current = state.currentInput
    if (current == "") Failure(state.toError("item", "end of the input"))
    else {
      val res = current.charAt(0)
      Success(res, res.toString)
    }
  }

  def lookAhead(n: Int): Parser[String] = state => {
    val current = state.currentInput
    if (current.length < n)
      Failure(state.toError("lookAhead", s"length of input string is less than $n"))
    else Success(current.substring(0, n), "")
  }

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = state => {
    p1(state) match {
      case s1@Success(_, _) => s1
      case Failure(error1) => {
        val latestPos = error1.latestPos.getOrElse(state.pos)
        p2(State(latestPos)) match {
          case s2@Success(_, _) => s2
          case Failure(error2) =>
            Failure(ParseError(error2.infoStack ++ error1.infoStack))
        }
      }
    }
  }

  def succeed[A](a: A): Parser[A] = state => Success(a, "")

  def failure(msg: String = "always fail"): Parser[Nothing] =
    state => Failure(state.pos.toError("failure", msg))

  def slice(p: Parser[Any]): Parser[String] = state => {
    p(state) match {
      case Success(_, s) => Success(s, s)
      case f@Failure(_) => f
    }
  }

  def attempt[A](p: Parser[A]): Parser[A] = state => {
    p(state) match {
      case s@Success(_, _) => s
      case Failure(error) =>
        val newError = error.infoStack match {
          case Nil => ParseError(Nil)
          case h::t => ParseError(t).push(state.pos, h.name, h.message)
        }
        Failure(newError)
    }
  }

  def label[A](name: String, msg: String)(p: Parser[A]): Parser[A] = state => {
    p(state) match {
      case s@Success(_, _) => s
      case Failure(error) => Failure(error.label(name, msg))
    }
  }

  def scope[A](name: String, msg: String)(p: Parser[A]): Parser[A] = state => {
    p(state) match {
      case s@Success(_, _) => s
      case Failure(error) => Failure(error.push(state.pos, name, msg))
    }
  }

  def parseUntil(s: String): Parser[String] = state => {
    val current = state.currentInput
    current.indexOf(s) match {
      case -1 => Failure(state.pos.toError("parseUntil", s"can't find string '$s'"))
      case i =>
        val res = current.substring(0, i)
        Success(res, res)
    }
  }

}
