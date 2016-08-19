package parser

import parser.ParserTypes.{ErrorInfo, ErrorMessage, ParserName, infoOperator}

import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>

  def run[A](parser: Parser[A])(str: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  def many[A](p: Parser[A]): Parser[List[A]] = or(many1(p), succeed(Nil))

  def lookAhead(n: Int): Parser[String]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def failure(msg: String = "always fail"): Parser[Nothing]

  def slice(p: Parser[Any]): Parser[String]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def attempt[A](p: Parser[A]): Parser[A]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def parseUntil(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A]
  (a: A)(implicit converter: A => Parser[String]): ParserOps[String] =
    ParserOps(converter(a))

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def item: Parser[Char] = ".".r.map(_.charAt(0))

  def many1[A](p: Parser[A]): Parser[List[A]] =
    for {
      a <- p
      as <- many(p)
    } yield a::as

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p flatMap (a => succeed(f(a)))

  def map2[A, B, C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- pa
      b <- pb
    } yield f(a, b)

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def unit[A](a: A): Parser[A] = succeed(a)

  def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] =
    for {
      a <- pa
      b <- pb
    } yield (a, b)

  def skipLeft[R](pl: Parser[Any], pr: Parser[R]): Parser[R] =
    for {
      _ <- slice(pl)
      r <- pr
    } yield r

  def skipRight[L](pl: Parser[L], pr: Parser[Any]): Parser[L] =
    for {
      l <- pl
      _ <- slice(pr)
    } yield l

  def maySkipLeft[R](pl: Parser[Any], pr: Parser[R]): Parser[R] =
    (pl >> pr).attempt | pr

  def maySkipRight[L](pl: Parser[L], pr: Parser[Any]): Parser[L] =
    (pl << pr).attempt | pl

  def surround[A](p: Parser[A])(pl: Parser[Any], pr: Parser[Any]): Parser[A] =
    pl >> p << pr

  def filter[A](p: Parser[A])(pre: A => Boolean): Parser[A] =
    p.flatMap(x => if (pre(x)) succeed(x) else failure("fail of filter"))

  def convert[A, B](p: Parser[A])(b: => B): Parser[B] = p.map(_ => b)

  def eof: Parser[Unit] =
    regex("\\z".r).as(()).labelError("unexpected trailing characters")

  def rootParser[A](p: Parser[A]): Parser[A] = p << eof

  def sep1[A](p: Parser[A])(separator: Parser[Any]): Parser[List[A]] =
    for {
      a <- p
      as <- (separator >> p) *
    } yield a::as

  def sep[A](p: Parser[A])(separator: Parser[Any]): Parser[List[A]] =
    sep1(p)(separator) | succeed(Nil)

  def letter: Parser[Char] = "[a-zA-Z]".r.map(_.charAt(0)).labelError("not a letter")

  def capitalLetter: Parser[Char] = "[A-Z]".r.map(_.charAt(0)).labelError("not a capital letter")

  def smallLetter: Parser[Char] = "[a-z]".r.map(_.charAt(0)).labelError("not a ")

  def digit: Parser[Char] = "\\d".r.map(_.charAt(0))

  def whiteSpace: Parser[Char] = "\\s".r.map(_.charAt(0))

  def letters: Parser[String] = "[a-zA-Z]+".r

  def digits: Parser[String] = "\\d+".r

  def whiteSpaces: Parser[String] = "\\s+".r

  def maySkipLeft[R](pl: Parser[Any], pr: Parser[R]): Parser[R] =
    (pl >> pr).attempt | pr

  def maySkipRight[L](pl: Parser[L], pr: Parser[Any]): Parser[L] =
    (pl << pr).attempt | pl

  def doubleString: Parser[String] = "[+-]?([0-9]*\\.?[0-9]+|[0-9]+\\.?[0-9]*)([eE][+-]?[0-9]+)?".r

  def double: Parser[Double] = doubleString.map(_.toDouble)

  def nonNegativeIntString: Parser[String] = "\\d+".r

  def nonNegativeInt: Parser[Int] = nonNegativeIntString.map(_.toInt)

  def intString: Parser[String] = "-?\\d+".r

  def int: Parser[Int] = intString.map(_.toInt)

  def parseTo(s: String): Parser[String] =
    for {
      s1 <- parseUntil(s)
      s2 <- string(s)
    } yield s1 + s2

  case class ParserOps[A](p: Parser[A]) {
    def parse(input: String): Either[ParseError, A] = run(p)(input)
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def * : Parser[List[A]] = self.many(p)
    def + : Parser[List[A]] = self.many1(p)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def slice: Parser[String] = self.slice(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[B](pb: Parser[B]) = self.product(p, pb)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def attempt: Parser[A] = self.attempt(p)
    def >>[B](p2: Parser[B]): Parser[B] = self.skipLeft(p, p2)
    def <<(p2: Parser[Any]): Parser[A] = self.skipRight(p, p2)
    def surroundedBy(pl: Parser[Any], pr: Parser[Any]): Parser[A] = self.surround(p)(pl, pr)
    def surroundedBy(ps: Parser[Any]): Parser[A] = self.surround(p)(ps, ps)
    def filter(pre: A => Boolean): Parser[A] = self.filter(p)(pre)
    def as[B](b: => B): Parser[B] = self.convert(p)(b)
    def labelError(msg: String): Parser[A] = self.label(msg)(p)
    def scopeError(msg: String): Parser[A] = self.scope(msg)(p)
    def toRootParser: Parser[A] = self.rootParser(p)
    def sepBy(separator: Parser[Any]): Parser[List[A]] = self.sep(p)(separator)
    def sepBy1(separator: Parser[Any]): Parser[List[A]] = self.sep1(p)(separator)
    def ?>>[B](p2: Parser[B]): Parser[B] = self.skipLeft(p, p2)
    def <<?(p2: Parser[Any]): Parser[A] = self.skipRight(p, p2)
  }
}

object ParserTypes {
  type ParserName = String
  type ErrorMessage = String
  type ErrorInfo = (Position, ParserName, ErrorMessage)

  implicit def infoOperator(info: ErrorInfo) = ErrorInfoOps(info)

  case class ErrorInfoOps(info: ErrorInfo) {
    def position = info._1
    def name = info._2
    def message = info._3
  }
}

case class Position(input: String, offset: Int = 0) {
  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val column = input.slice(0,offset+1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(name: ParserName, msg: ErrorMessage): ParseError =
    ParseError(List((this, name, msg)))

  def moveForward(n: Int) = copy(offset = offset+n)

  def currentLine: String =
    if (input.length > 1)
      input.lines.drop(line - 1).next
    else ""
}

case class ParseError(infoStack: List[ErrorInfo]) {

  def push(pos: Position, name: ParserName, msg: ErrorMessage): ParseError =
    copy(infoStack = (pos, name, msg) :: infoStack)

  def label[A](name: ParserName, msg: ErrorMessage): ParseError =
    ParseError(latestPos.map((_, name, msg)).toList)

  def latest: Option[ErrorInfo] =
    infoStack.lastOption

  def latestPos: Option[Position] =
    latest map (_._1)

  override def toString =
    if (infoStack.isEmpty) "no error message"
    else {
      val collapsed = collapseStack(infoStack)
      collapsed.map { case (pos, name, msg) => name + ": [" + formatPos(pos) + "] " + msg }.mkString("\n")
    }

  def collapseStack(s: List[ErrorInfo]): List[ErrorInfo] =
    s.groupBy(_._1).
      mapValues { lst => (lst.map(_.name).mkString(" & "), lst.map(_.message).mkString("; ")) }.
      toList.
      map { case (pos, (name, msg)) => (pos, name, msg) }.
      sortBy(_._1.offset)

  def formatPos(l: Position): String = l.line + "," + l.column
}
