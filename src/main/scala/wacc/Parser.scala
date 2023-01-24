package wacc
import AbstractSyntaxTree.{BoolLiteral, CharLiteral, Expr, IdentLiteral, IntLiteral, PairLiteral, StringLiteral}
import parsley.Parsley
import parsley.lift.lift2
import parsley.Parsley.{attempt, notFollowedBy}
import parsley.character.{digit, item, letter, letterOrDigit, satisfy, string, stringOfMany, stringOfSome}
import parsley.implicits.character.{charLift, stringLift}

object Parser {
  val integer: Parsley[Int] = stringOfSome(digit).map(_.toInt)
  val bool: Parsley[Boolean] = ("true" #> true) <|> ("false" #> false)

  //var lexer = new LexicalDesc
  object Expression {
    private def neg(x: Int): Int = -x
    private def id (x: Int): Int = x
    def withBracket[A](p: Parsley[A], bracket: Char): Parsley[A] = bracket ~> p <~ bracket
    private lazy val sign: Parsley[Int => Int] = '+' #> (identity[Int] _) <|> '-' #> (x => -x)

    private lazy val intLiteral = (sign <*> integer <|> integer).map(IntLiteral)
    private lazy val boolLiteral = bool.map(BoolLiteral)
    private lazy val charLiteral = withBracket(letter, '\'').map(CharLiteral)
    private lazy val stringLiteral = withBracket(stringOfMany(satisfy(_ != '"')), '"').map(StringLiteral)
    private lazy val pairLiteral = attempt((string("null") ~> notFollowedBy(letterOrDigit)) #> PairLiteral())
    private lazy val identLiteral = lift2((a: Char, b: String) => (a + b),
      ('_' <|> letter), stringOfSome('_' <|> letterOrDigit)).map(IdentLiteral)



    lazy val parseExp: Parsley[Expr] =
      intLiteral    <|>
      boolLiteral   <|>
      charLiteral   <|>
      stringLiteral <|>
      pairLiteral   <|>
      identLiteral
  }
}