package wacc

import parsley.Parsley
import parsley.errors.combinator.ErrorMethods

object Lexer {

  import parsley.token.descriptions.numeric.NumericDesc
  import parsley.token.descriptions.text.{EscapeDesc, TextDesc}
  import parsley.token.descriptions.{LexicalDesc, NameDesc, SpaceDesc, SymbolDesc}
  import parsley.token.{Lexer, predicate}

  private val escapedLiterals = Set('\\', '\"', '\'')

  private val desc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic((c: Char) => (c == '_') || c.isLetter),
      identifierLetter = predicate.Basic((c: Char) => (c == '_') || c.isLetterOrDigit)
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = Set(
        "true", "false", "null", "len", "ord", "chr", "skip", "read", "free", "return", "exit",
        "print", "println", "if", "then", "else", "fi", "while", "do", "done", "begin", "end",
        "int", "bool", "char", "string", "fst", "snd", "call", "pair", "newpair", "is"
      ),
      hardOperators = Set("!", "*", "/", "%", "+", "-", ">=", ">", "<", "<=", "==", "!=", "&&", "||", ";")
    ),
    numericDesc = NumericDesc.plain.copy(),
    textDesc = TextDesc.plain.copy(
      escapeSequences = EscapeDesc.plain.copy(
        literals = escapedLiterals,
        singleMap = Map('0' -> 0x00, 'b' -> 0x08, 't' -> 0x09, 'n' -> 0x0a, 'f' -> 0x0c, 'r' -> 0x0d)
      ),
      graphicCharacter = parsley.token.predicate.Unicode(c =>
        c >= ' '.toInt && !escapedLiterals.map(_.toInt).contains(c))
    ),
    spaceDesc = SpaceDesc.plain.copy(
      commentLine = "#",
      space = predicate.Basic(Character.isWhitespace)
    )
  )

  private val lexer = new Lexer(desc)
  val integer: Parsley[Int] = lexer.lexeme.numeric.signed.decimal32.label("Integer")
  val unsigned: Parsley[Int] = lexer.lexeme.numeric.unsigned.decimal32.label("Unsigned")
  val character: Parsley[Char] = lexer.lexeme.text.character.ascii.label("Character")
  val boolean: Parsley[Boolean] = ((lexer.lexeme.symbol.apply("true", "true") #> true) <|>
    (lexer.lexeme.symbol.apply("false", "false") #> false)).label("Boolean")
  val string: Parsley[String] = lexer.lexeme.text.string.ascii.label("String")
  val emptyPair: Parsley[Unit] = lexer.lexeme.symbol.apply("null", "null")
  val identifier: Parsley[String] = lexer.lexeme.names.identifier.label("Variable")

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

  val implicits = lexer.lexeme.symbol.implicits
}
