package wacc
import AbstractSyntaxTree.{BoolLiteral, CharLiteral, Expr, IntLiteral, StringLiteral}
import parsley.{Parsley, Result}
import parsley.character.{digit, item, letter, satisfy, stringOfMany, stringOfSome}
import parsley.implicits.character.{charLift, stringLift}
import parsley.Parsley.notFollowedBy
import parsley.debug._
import parsley.expr.{InfixL, Ops, precedence}

object Parser {
  val integer: Parsley[Int] = stringOfSome(digit).map(_.toInt)
  val bool: Parsley[Boolean] = ("true" #> true) <|> ("false" #> false)

  //var lexer = new LexicalDesc
  object Expression {
    private def neg(x: Int): Int = -x
    private def id (x: Int): Int = x
    def withBracket[A](p: Parsley[A], bracket: Char): Parsley[A] = bracket ~> p <~ bracket
    // precedence() requires at least one operator and these are all atoms, so I linked their definitions for now
    // (intLiteral references boolLiteral, etc.). We can delete this when more definitions are implemented.

    private lazy val sign: Parsley[Int => Int] = '+' #> (identity[Int] _) <|> '-' #> (x => -x)
    private lazy val intLiteral = (sign <*> integer <|> integer).map(IntLiteral) <|> boolLiteral
    private lazy val boolLiteral = bool.map(BoolLiteral) <|> charLiteral
    private lazy val charLiteral = withBracket(letter, '\'').map(CharLiteral) <|> stringLiteral
    private lazy val stringLiteral = withBracket(stringOfMany(satisfy(_ != '"')), '"').map(StringLiteral)

    lazy val parseExp: Parsley[Expr] = intLiteral
  }
}