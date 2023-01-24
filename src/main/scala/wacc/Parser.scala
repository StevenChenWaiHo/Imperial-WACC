package wacc
import AbstractSyntaxTree._
import parsley.Parsley
import parsley.combinator.{choice, eof, many, some}
import parsley.lift.lift2
import parsley.Parsley.{attempt, notFollowedBy, pure}
import parsley.character.{digit, endOfLine, item, letter, letterOrDigit, satisfy, string, stringOfMany, stringOfSome}
import parsley.implicits.character.{charLift, stringLift}
import wacc.Parser.Expression.parseExpr

object Parser {
  val integer: Parsley[Int] = stringOfSome(digit).map(_.toInt)
  val bool: Parsley[Boolean] = ("true" #> true) <|> ("false" #> false)

//  var lexer = new LexicalDesc()
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
    private lazy val ident = lift2((a: Char, b: String) => (a + b),
      ('_' <|> letter), stringOfSome('_' <|> letterOrDigit))
    private lazy val maybeArrayElem: Parsley[String => Expr] =
      choice(some('['~> parseExpr <~']').map(ArrayElem(_)), pure(IdentLiteral(_)))

    lazy val parseExpr: Parsley[Expr] =
      intLiteral    <|>
      boolLiteral   <|>
      charLiteral   <|>
      stringLiteral <|>
      pairLiteral   <|>
      (ident <**> maybeArrayElem) // Both an identifier and an array element can start with an 'ident'

  }
}