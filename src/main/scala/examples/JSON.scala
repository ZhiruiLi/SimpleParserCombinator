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

    def jNumber: Parser[JSON] = (double map JNumber asToken) scopeError ("jNumber", "not a JSON number")

    def jNull: Parser[JSON] = ("null" as JNull asToken) scopeError ("jNull", "not a JSON null")

    def jBoolean: Parser[JSON] =
      ((("true" as JBoolean(true)) | ("false" as JBoolean(false))) asToken).
        scopeError ("jBoolean", "not a JSON boolean")

    def stringLiteral: Parser[String] = parseUntil("\"").surroundedBy("\"") asToken

    def jString: Parser[JSON] = stringLiteral map JString scopeError ("jString", "not a JSON string")

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
      map(JObject).scopeError("jObject", "not a JSON object")

    def jArray: Parser[JSON] = jValue.
      sepBy(comma).
      surroundedBy(openBracket, closeBracket).
      map(_.toIndexedSeq).
      map(JArray).scopeError("jArray", "not a JSON array")

    def jValue: Parser[JSON] =
      (jNull.attempt |
        jNumber.attempt |
        jBoolean.attempt |
        jString.attempt |
        jArray.attempt |
        jObject) scopeError ("jValue", "not a JSON value")

    jValue
  }

  def getRootParser[Parser[+_]](parsers: Parsers[Parser]): Parser[JSON] = {
    import parsers._
    (whiteSpaces ?>> getParser(parsers)) asRoot
  }
}