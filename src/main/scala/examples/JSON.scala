package examples

import parser.{ParseError, Parsers, SimpleParsers}

object JsonTest extends App {

  val s = """

            { "a" : 123,
       "bc":45.6        ,




       "1ab": true,
       "e*3" : [ "aaa" , 3.2,null , {

       "abc" : false

       }
]

       }
    """

  println(JSON parse s)
}

sealed trait JSON
case class JNumber(value: Double) extends JSON
case class JBoolean(value: Boolean) extends JSON
case class JString(value: String) extends JSON
case class JArray(value: IndexedSeq[JSON]) extends JSON
case class JObject(value: Map[String, JSON]) extends JSON
case object JNull extends JSON

object JSON {

  def parse(input: String): Either[ParseError, JSON] = {
    import SimpleParsers._
    getRootParser(SimpleParsers) parse input
  }

  def getParser[Parser[+_]](parsers: Parsers[Parser]): Parser[JSON] = {
    import parsers._

    def token[A](p: Parser[A]): Parser[A] = p <<? whiteSpaces

    def jNumber: Parser[JSON] = token(double map JNumber)

    def jNull: Parser[JSON] = token("null" as JNull)

    def jBoolean: Parser[JSON] = token(("true" as JBoolean(true)) | ("false" as JBoolean(false)))

    def stringLiteral: Parser[String] = token(parseUntil("\"").surroundedBy("\""))

    def jString: Parser[JSON] = stringLiteral map JString

    def jObjectElement: Parser[(String, JSON)] = for {
      key <- stringLiteral
      _ <- token(":")
      value <- jValue
    } yield (key, value)

    def comma: Parser[String] = token(",")

    def openBrace: Parser[String] = token("{")

    def closeBrace: Parser[String] = token("}")

    def openBracket: Parser[String] = token("[")

    def closeBracket: Parser[String] = token("]")

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

  def getRootParser[Parser[+_]](parsers: Parsers[Parser]): Parser[JSON] = {
    import parsers._
    (whiteSpaces ?>> getParser(parsers)).toRootParser
  }
}