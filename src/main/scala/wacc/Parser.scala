package wacc
import AbstractSyntaxTree.{BoolLiteral, CharLiteral, IntLiteral, StringLiteral}
import parsley.Parsley
import parsley.character.{digit, letter, string, stringOfMany}
import parsley.expr.Atoms
import parsley.implicits.character.{charLift, stringLift}

object Parser {
  val integer: Parsley[Int] = stringOfMany(digit).map(_.toInt)
  val bool: Parsley[Boolean] = ("true" #> true) <|> ("false" #> false)

  //var lexer = new LexicalDesc
  object Expression {
    def withBracket[A](p: Parsley[A], bracket: Char): Parsley[A] = bracket ~> p <~ bracket
    val intLiteral = '-' #> (-1 * _) <*> integer.map(IntLiteral)
    val boolLiteral = bool.map(BoolLiteral)
    val charLiteral = withBracket(letter, '\'').map(CharLiteral)
    val stringLiteral = withBracket(stringOfMany(letter), '\"').map(StringLiteral)
}
