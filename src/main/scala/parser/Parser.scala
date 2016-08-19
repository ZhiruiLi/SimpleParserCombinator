package parser

import parser.ParserTypes.{ErrorInfo, ErrorMessage, ParserName, infoOperator}

import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>

  /**
    * Parses the input string using the given parser.
    * @param parser parser
    * @param str input string
    * @tparam A the return type if the parsing succeed
    * @return If parsing succeed return Right(a). Otherwise, return Left(error) containing the error message.
    */
  def run[A](parser: Parser[A])(str: String): Either[ParseError, A]

  /**
    * Create a parser of a specific string.
    * @param s the string
    * @return a parser of String
    */
  implicit def string(s: String): Parser[String]

  /**
    * Creates a parser of string matching a specific regex.
    * @param r regex
    * @return a parser of String
    */
  implicit def regex(r: Regex): Parser[String]

  /**
    * Parses the input string 0 or more times using the given parser. It will always succeed.
    * @param p the parser
    * @tparam A the return type of the parser
    * @return a parser of List[A]
    */
  def many[A](p: Parser[A]): Parser[List[A]] = or(many1(p), succeed(Nil))

  /**
    * Reads a n-length string from the input without consuming the input string.
    * @param n number of chars to look ahead
    * @return a parser that parse the input string but do not consume it
    */
  def lookAhead(n: Int): Parser[String]

  /**
    * If p1 fails, then try to parse using p2
    * @param p1 parser that will be apply first
    * @param p2 parser that will be apply second
    * @tparam A return type of both parsers
    * @return if p1 succeed, return the result of p1, otherwise, parse the string using p2,
    *         if p2 succeed, return the result of p2, otherwise, return an error
    */
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  /**
    * a parser that will always succeed with a given value.
    * @param a the result value
    * @tparam A the result type
    * @return a parser will always succeed
    */
  def succeed[A](a: A): Parser[A]

  /**
    * a parser that always fail
    * @return
    */
  def failure(msg: String = "always fail"): Parser[Nothing]

  /**
    * Parses a string and return the string that had been parsed.
    * @param p the parser
    * @return if p succeed, return the string that had been consume, otherwise, return an error
    */
  def slice(p: Parser[Any]): Parser[String]

  /**
    * Builds a parser that will first parse the input string using the original parser, then it will generate a new
    * parser by applying the function to the result of the original parser. After that, it will parse the remain string
    * using the new parser.
    * @param p the original parser
    * @param f the function to apply to the parsing result
    * @tparam A type of the original result
    * @tparam B type of the new result
    * @return a new parser produce a result of type B
    */
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  /**
    * Try to parse the input string using a parser, if it fails,
    * the cursor will recover to the position where it begins.
    * @param p the parser
    * @tparam A the result type of the parser
    * @return a parser that will produce the same success result of the original one
    */
  def attempt[A](p: Parser[A]): Parser[A]

  /**
    * Labels a error message to a parser, this will ignore the original messages.
    * @param name the name of parser
    * @param msg the error message
    * @param p the parser
    * @tparam A the result type of the parser
    * @return a parser that will produce the same success result of the original one
    */
  def label[A](name: String, msg: String)(p: Parser[A]): Parser[A]

  /**
    * Pushes an error message to a parser. This will keep the original messages.
    * @param name the name of parser
    * @param msg the error message
    * @param p the parser
    * @tparam A the result type of the parser
    * @return a parser that will produce the same success result of the original one
    */
  def scope[A](name: String, msg: String)(p: Parser[A]): Parser[A]

  /**
    * Parses the input string until a specific string occurs (excluding that string).
    * @param s the specific string
    * @return a parser
    */
  def parseUntil(s: String): Parser[String]

  /**
    * Converts a parser to a ParserOps value, which can support some handy operators.
    * @param p the parser
    * @tparam A the result type of the parser
    * @return a wrapped parser with some operators
    */
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  /**
    * Builds a parser using the function and converts it to a ParserOps value directly.
    * @param a the material to build a parser
    * @param converter the function to create a parser
    * @tparam A the type of the material to build the parser
    * @return a wrapped parser with some operators
    */
  implicit def asStringParser[A]
  (a: A)(implicit converter: A => Parser[String]): ParserOps[String] =
    ParserOps(converter(a))

  /**
    * A parser of a specific char.
    * @param c the specific char
    * @return a parser of Char
    */
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  /**
    * Parses the first char of the input string. It will always succeed except the end of the string occurs.
    * @return a parser of Char
    */
  def item: Parser[Char] = ".".r.map(_.charAt(0))

  /**
    * Parses the input string 1 or more times using the given parser.
    * @param p the parser
    * @tparam A the return type of the parser
    * @return a parser of List[A]
    */
  def many1[A](p: Parser[A]): Parser[List[A]] =
    for {
      a <- p
      as <- many(p)
    } yield a::as

  /**
    * Parses the input string and converts the result using the given function if the parsing succeed.
    * @param p the parser
    * @param f the converting function
    * @tparam A the type of result of the original parser
    * @tparam B the type of new result
    * @return a parser
    */
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p flatMap (a => succeed(f(a)))

  /**
    * Parses the input string using two parses. If both of them succeed, apply the function to the two results,
    * and map them to a new result.
    * @param pa parser that will be apply first
    * @param pb parser that will be apply second
    * @param f the converting function
    * @tparam A the result type of the first parser
    * @tparam B the result type of the second parser
    * @tparam C the result type of the new parser
    * @return the new parser
    */
  def map2[A, B, C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- pa
      b <- pb
    } yield f(a, b)

  /**
    * aliase of succeed
    * @param a the result value
    * @tparam A the result type
    * @return a parser will always succeed
    */
  def unit[A](a: A): Parser[A] = succeed(a)

  /**
    * Parses the input string using the first parser, then parses the remain string with the second parser,
    * if both succeed, produce both result in a tuple.
    * @param pa parser that will be apply first
    * @param pb parser that will be apply second
    * @tparam A the result type of the first parser
    * @tparam B the result type of the second parser
    * @return a parser of tuple
    */
  def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] =
    for {
      a <- pa
      b <- pb
    } yield (a, b)

  /**
    * Parses the input string using the left parser, throws the result away,
    * then parses the remain string using the right one.
    * If either of the left parser or the right parser fails, the whole parsing will fail.
    * @param pl the left parser
    * @param pr the right parser
    * @tparam R the result type
    * @return a parser
    */
  def skipLeft[R](pl: Parser[Any], pr: Parser[R]): Parser[R] =
    for {
      _ <- slice(pl)
      r <- pr
    } yield r

  /**
    * Parses the input string using the left parser,
    * then parses the remain string using the right one, and throw the result of the right parser.
    * If either of the left parser or the right parser fails, the whole parsing will fail.
    * @param pl the left parser
    * @param pr the right parser
    * @tparam L the result type
    * @return a parser
    */
  def skipRight[L](pl: Parser[L], pr: Parser[Any]): Parser[L] =
    for {
      l <- pl
      _ <- slice(pr)
    } yield l

  /**
    * Parses the input string using left surrounder parser, content parser and right surrounder parser in order.
    * The results of the surrounder parsers will be throw away.
    * The whole parser will only succeed if the three parsers all succeed.
    * @param p the parser of the final result
    * @param pl the left surrounder parser
    * @param pr the right surrounder parser
    * @tparam A the result type
    * @return a parser
    */
  def surround[A](p: Parser[A])(pl: Parser[Any], pr: Parser[Any]): Parser[A] =
    pl >> p << pr

  /**
    * If the parsing succeed, filter the result with a predicate function.
    * @param p the parser
    * @param pre the predicate function
    * @tparam A the result type
    * @return a parser
    */
  def filter[A](p: Parser[A])(pre: A => Boolean): Parser[A] =
    p.flatMap(x => if (pre(x)) succeed(x) else failure("fail of filter"))

  /**
    * Parses the input string with the given parser,
    * if succeed, throw away the result then convert it to the given value.
    * @param p the original parser
    * @param b the result
    * @tparam A the result type of the original parser
    * @tparam B the result type of the new parser
    * @return a parser
    */
  def convert[A, B](p: Parser[A])(b: => B): Parser[B] = p.map(_ => b)

  /**
    * a parser that will only succeed when parsing a empty string
    * @return a parser
    */
  def eof: Parser[Unit] =
    regex("\\z".r).as(()).labelError("eof", "unexpected trailing characters")

  /**
    * Convert a parser to a root parser, it will succeed only when the given parser succeed
    * and there is no trailing characters.
    * @param p the parser
    * @tparam A the result type
    * @return a root parser
    */
  def rootParser[A](p: Parser[A]): Parser[A] = p << eof

  /**
    * Parse the string at least one times using a given parser p, each two parsing between that should use the
    * separator parser to parse the string. The result of the separator will be throw away.
    * @param p the parser of content
    * @param separator the parser of separator
    * @tparam A the result type of the content parser
    * @return a parser of list
    */
  def sep1[A](p: Parser[A])(separator: Parser[Any]): Parser[List[A]] =
    for {
      a <- p
      as <- (separator >> p) *
    } yield a::as

  /**
    * Parse the string at least zero or more times using a given parser p, each two parsing between that should use the
    * separator parser to parse the string. The result of the separator will be throw away.
    * @param p the parser of content
    * @param separator the parser of separator
    * @tparam A the result type of the content parser
    * @return a parser of list
    */
  def sep[A](p: Parser[A])(separator: Parser[Any]): Parser[List[A]] =
    sep1(p)(separator) | succeed(Nil)

  /**
    * parser for a letter char
    * @return parser of char
    */
  def letter: Parser[Char] = "[a-zA-Z]".r.map(_.charAt(0)).labelError("letter", "not a letter")

  /**
    * parser for a capital letter char
    * @return parser of char
    */
  def capitalLetter: Parser[Char] = "[A-Z]".r.map(_.charAt(0)).labelError("capitalLetter", "not a capital letter")

  /**
    * parser for a small letter char
    * @return parser of char
    */
  def smallLetter: Parser[Char] = "[a-z]".r.map(_.charAt(0)).labelError("smallLetter", "not a small letter")

  /**
    * parser for a digit char
    * @return parser of char
    */
  def digit: Parser[Char] = "\\d".r.map(_.charAt(0)).labelError("digit", "not a digit")

  /**
    * parser for a white space char
    * @return parser of char
    */
  def whiteSpace: Parser[Char] = "\\s".r.map(_.charAt(0)).labelError("whiteSpace", "not a white space")

  /**
    * parser for a string of letters
    * @return parser of string
    */
  def letters: Parser[String] = "[a-zA-Z]+".r.labelError("letters", "found no letters")

  /**
    * parser for a string of digits
    * @return parser of string
    */
  def digits: Parser[String] = "\\d+".r.labelError("digits", "found no digits")

  /**
    * parser for a string of white spaces
    * @return parser of string
    */
  def whiteSpaces: Parser[String] = "\\s+".r.labelError("whiteSpaces", "found no white spaces")

  /**
    * Try to parse the string using the left parser, if succeed, parse the remain string using the right parser.
    * Otherwise, parse the original string using the right parser directly.
    * @param pl the left parser
    * @param pr the right parser
    * @tparam R the result type
    * @return a parser
    */
  def maySkipLeft[R](pl: Parser[Any], pr: Parser[R]): Parser[R] =
    (pl >> pr).attempt | pr

  /**
    * Parses the string using the left parser, if succeed, try to parse the remain string using the right parser.
    * If the right parsing succeed, the string will be consumed by right parser, otherwise, the string will keep
    * the state before being parsed by the right parser.
    * @param pl the left parser
    * @param pr the right parser
    * @tparam L the result type
    * @return a parser
    */
  def maySkipRight[L](pl: Parser[L], pr: Parser[Any]): Parser[L] =
    (pl << pr).attempt | pl

  /**
    * a parser of decimal fraction string
    * @return a parser of string
    */
  def doubleString: Parser[String] =
    "[+-]?([0-9]*\\.?[0-9]+|[0-9]+\\.?[0-9]*)([eE][+-]?[0-9]+)?".r.
      labelError("doubleString", "not a decimal fraction string")

  /**
    * a parser of decimal fraction
    * @return a parser of double
    */
  def double: Parser[Double] = doubleString.map(_.toDouble).labelError("double", "not a decimal fraction")

  /**
    * a parser of non-negative integer number string
    * @return a parser of string
    */
  def nonNegativeIntString: Parser[String] = "\\d+".r.
    labelError("nonNegativeIntString", "not a non-negative integer number string")

  def nonNegativeInt: Parser[Int] = nonNegativeIntString.map(_.toInt).
    labelError("nonNegativeIntString", "not a non-negative integer number")

  def intString: Parser[String] = "-?\\d+".r.
    labelError("nonNegativeIntString", "not a integer number string")

  def int: Parser[Int] = intString.map(_.toInt).
    labelError("nonNegativeIntString", "not a integer number")

  /**
    * Parses the input string to where a specific string occurs (including that string).
    * @param s the specific string
    * @return a parser
    */
  def parseTo(s: String): Parser[String] =
    (for {
      s1 <- parseUntil(s)
      s2 <- string(s)
    } yield s1 + s2).labelError("parseTo", s"can't find string '$s'")

  /**
    * a wrapper provide some operators to parsers
    * @param p parser
    * @tparam A the result type of parser
    */
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
    def labelError(name: String, msg: String): Parser[A] = self.label(name, msg)(p)
    def scopeError(name: String, msg: String): Parser[A] = self.scope(name, msg)(p)
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

  def toError(name: String, msg: String): ParseError =
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

  def label[A](name: String, msg: String): ParseError =
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
