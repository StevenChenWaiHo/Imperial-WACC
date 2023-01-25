package wacc
import AbstractSyntaxTree._
import wacc.AbstractSyntaxTree.UnaryOpType._
import wacc.AbstractSyntaxTree.BinaryOpType._
import parsley.Parsley
import parsley.combinator.{choice, some}
import parsley.lift.lift2
import parsley.Parsley.{attempt, notFollowedBy, pure}
import parsley.character.{char, digit, letter, letterOrDigit, satisfy, string, stringOfMany, stringOfSome}
import parsley.expr.{InfixL, Ops, Prefix, precedence}
import parsley.implicits.character.{charLift, stringLift}

object Parser {
  val integer: Parsley[Int] = stringOfSome(digit).map(_.toInt)
  val bool: Parsley[Boolean] = ("true" #> true) <|> ("false" #> false)
  //  var lexer = new LexicalDesc()
  object Expression {
    //TODO:
    private lazy val escapedChar = (char('\\') ~> letter).map(identity[Char])
    private def withBracket[A](p: Parsley[A], bracket: Char): Parsley[A] = bracket ~> p <~ bracket
    private lazy val sign: Parsley[Int => Int] = '+' #> (identity[Int] _) <|> '-' #> (x => -x)

    private lazy val intLiteral = (sign <*> integer <|> integer).map(IntLiteral)
    private lazy val boolLiteral = bool.map(BoolLiteral)
    private lazy val charLiteral = withBracket((letter <|> escapedChar), '\'').map(CharLiteral)
    private lazy val stringLiteral = withBracket(stringOfMany(satisfy(_ != '"') <|> escapedChar), '"').map(StringLiteral)
    private lazy val pairLiteral = attempt((string("null") ~> notFollowedBy(letterOrDigit)) #> PairLiteral())
    private lazy val identStart = ('_' <|> letter)
    private lazy val identCont = stringOfMany('_' <|> letterOrDigit)
    private lazy val ident = lift2((a: Char, b: String) => (a + b), identStart, identCont)
    private lazy val maybeArrayElem: Parsley[String => Expr] = {
      choice(some('[' ~> parseExpr <~ ']').map(ArrayElem(_)), pure(IdentLiteral(_)))
    }
    private lazy val parseExprAtom: Parsley[Expr] =
      intLiteral <|>
        boolLiteral <|>
        charLiteral <|>
        stringLiteral <|>
        pairLiteral <|>
        (ident <**> maybeArrayElem) // Both an identifier and an array element can start with an 'ident'

    // Not the nicest solution, but it works for now...:
    private def without[A](q: Parsley[A], p: Parsley[A]) = attempt (p <~ notFollowedBy(q))

    lazy val parseExpr: Parsley[Expr] = precedence(
      Ops[Expr](Prefix)('!' #> UnaryOp(Not),
        without(intLiteral, '-') #> UnaryOp(Neg),
        without(identCont, "len") #> UnaryOp(Len),
        without(identCont, "ord") #> UnaryOp(Ord),
        without(identCont, "chr") #> UnaryOp(Chr)),
      Ops[Expr](InfixL)("*" #> BinaryOp(Mul), "/" #> BinaryOp(Div), "%" #> BinaryOp(Mod)),
      Ops[Expr](InfixL)("+" #> BinaryOp(Add), "-" #> BinaryOp(Sub)),
      Ops[Expr](InfixL)(">=" #> BinaryOp(Gte), ">" #> BinaryOp(Gt)),
      Ops[Expr](InfixL)("<=" #> BinaryOp(Lte), "<" #> BinaryOp(Lt)),
      Ops[Expr](InfixL)("==" #> BinaryOp(Eq), "!=" #> BinaryOp(Neq)),
      Ops[Expr](InfixL)("&&" #> BinaryOp(And)),
      Ops[Expr](InfixL)("||" #> BinaryOp(Or))
    )(parseExprAtom <|> ('(' ~> parseExpr <~ ')'))
  }
}