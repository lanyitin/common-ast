package tw.lanyitin.common.ast

object TokenType extends Enumeration {
  type TokenType = Value
  val SpaceToken = Value("<space>")
  val NewLineToken = Value("<newline>")
  val EOFToken = Value("<eof>")
  val UnexpectedToken = Value("UnexpectedToken")
  val NumberToken = Value("<number>")
  val StringToken = Value("<string>")
  val LParanToken = Value("(")
  val RParanToken = Value(")")
  val LCurlyBracket = Value("{")
  val RCurlyBracket = Value("}")
  val CommaToken = Value(",")
  val ColumnToken = Value(":")
  val DefToken = Value("def")
  val IfToken = Value("if")
  val ElseToken = Value("else")
  val IdentifierToken = Value("<identifier>")
  val EqualToken = Value("==")
  val GreaterEqualToken = Value(">=")
  val LessEqualToken = Value("<=")
  val NotEqualToken = Value("!=")
  val GreaterToken = Value(">")
  val LessToken = Value("<")
  val BooleanAndToken = Value("and")
  val BooleanOrToken = Value("or")
  val ArithMultToken = Value("*")
  val ArithDivideToken = Value("/")
  val PlusToken = Value("+")
  val MinusToken = Value("-")
  val AssignToken = Value("=")
  val CommentHeadToken = Value("#")
  val CommentBodyToken = Value("<comment>")
  val QuoteToken = Value("\"")
  val BooleanConstantToken = Value("<boolean>")
  val LetToken = Value("let")
  val ModToken = Value("%")
// TODO: is it possible to eliminate NotExistToken?
  val NotExistToken = Value("")
}

class Token(val tokenType: TokenType.TokenType,
                   val txt: String,
                   val line: Integer = 0,
                   val col: Integer = 0) {
  val id = Token.tokenId
  Token.tokenId = Token.tokenId + 1

  override def toString: String = {
    val espedTxt = txt.replace("\n", "\\n").replace("\r", "\\r")
    s"${tokenType}('${espedTxt}')"
  }
}

object Token {
  var tokenId: Integer = 0
  def apply(tokenType: TokenType.TokenType,
            txt: String,
            line: Integer = 0,
            col: Integer = 0): Token = {
    new Token(tokenType, txt, line, col)
  }
}

case class NullToken() extends Token(TokenType.NotExistToken, "")