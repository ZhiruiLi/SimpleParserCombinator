package examples

import parser.{ParseError, Parsers, SimpleParserTypes, SimpleParsers}

object JsonTest extends App {

  import SimpleParsers._
//  println(JSON.parse("null "))
//  println(JSON.parse("null"))
//  println(JSON.parse("null abc"))
//
//  println("\\z".r.parse(""))
//  println(eof.parse(""))
//  println((succeed("") << whiteSpaces) parse "  ")
//  println((succeed("") << whiteSpaces) parse "")
//  println((succeed("") <<? whiteSpaces).toRootParser parse "  ")

  val p = for {
    a <- "ab" <<? whiteSpaces
    b <- "cd" << whiteSpace
  } yield (a, b)

  println(p.parse("ab    cd  "))

}

sealed trait JSON
case class JNumber(value: Double) extends JSON
case class JBoolean(value: Boolean) extends JSON
case class JString(value: String) extends JSON
case class JArray(value: IndexedSeq[JSON]) extends JSON
case class JObject(value: Map[String, JSON]) extends JSON
case object JNull extends JSON

object JSON {

  import SimpleParsers._

  def parse(input: String): Either[ParseError, JSON] = {
    getParser(SimpleParsers) parse input
  }

  def getParser[Parser[+_]](parsers: Parsers[Parser]): Parser[JSON] = {

    import parsers._

    def jNumber: Parser[JSON] = (double map JNumber) <<? whiteSpaces

    def jNull: Parser[JSON] = ("null" as JNull) <<? whiteSpaces

    def jBoolean: Parser[JSON] = (("true" as JBoolean(true)) | ("false" as JBoolean(false))) <<? whiteSpaces

    def stringLiteral: Parser[String] = parseUntil("\"").surroundedBy("\"")

    def jString: Parser[JSON] = stringLiteral.map(JString) <<? whiteSpaces

    def jObjectElement: Parser[(String, JSON)] = for {
      key <- stringLiteral
      _ <- whiteSpaces ?>> ":" <<? whiteSpaces
      value <- jValue
    } yield (key, value)

    def comma: Parser[String] = "," <<? whiteSpaces

    def openBrace: Parser[String] = "{" <<? whiteSpaces

    def closeBrace: Parser[String] = "}" <<? whiteSpaces

    def openBracket: Parser[String] = "[" <<? whiteSpaces

    def closeBracket: Parser[String] = "]" <<? whiteSpaces

    def jObject: Parser[JSON] = jObjectElement.
      sepBy(comma).
      surroundedBy(openBrace, closeBrace).
      map(_.toMap).
      map(JObject)

    def jArray: Parser[JSON] = jValue.
      sepBy(comma).
      surroundedBy(openBracket, closeBracket).
      map(_.toIndexedSeq).
      map(JArray)

    def jValue: Parser[JSON] =
      jNumber.attempt | jNull.attempt | jBoolean.attempt | jString.attempt | jArray.attempt | jObject

    jValue
  }
}