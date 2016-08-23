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

    def jNumber: Parser[JSON] = double map JNumber asToken

    def jNull: Parser[JSON] = "null" as JNull asToken

    def jBoolean: Parser[JSON] = (("true" as JBoolean(true)) | ("false" as JBoolean(false))) asToken

    def stringLiteral: Parser[String] = parseUntil("\"").surroundedBy("\"") asToken

    def jString: Parser[JSON] = stringLiteral map JString

    def jObjectElement: Parser[(String, JSON)] = for {
      key <- stringLiteral
      _ <- ":".asToken
      value <- jValue
    } yield (key, value)

    def comma: Parser[String] = ",".asToken

    def openBrace: Parser[String] = "{".asToken

    def closeBrace: Parser[String] = "}".asToken

    def openBracket: Parser[String] = "[".asToken

    def closeBracket: Parser[String] = "]".asToken

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
    (whiteSpaces ?>> getParser(parsers)) asRoot
  }
}