package wacc
import AbstractSyntaxTree._
import wacc.AbstractSyntaxTree.UnaryOpType._
import wacc.AbstractSyntaxTree.BinaryOpType._
import parsley.Parsley
import parsley.combinator.{choice, some}
import parsley.Parsley.{attempt, notFollowedBy, pure}
import parsley.character.{letterOrDigit, stringOfMany}
import parsley.expr.{InfixL, Ops, Prefix, precedence}
import parsley.implicits.character.charLift

object Parser {
  import wacc.Lexer._

  object Expression {
    import wacc.Lexer.implicits._

    private lazy val intLiteral = integer.map(IntLiteral)
    private lazy val boolLiteral = boolean.map(BoolLiteral)
    private lazy val charLiteral = character.map(CharLiteral)
    private lazy val stringLiteral = string.map(StringLiteral)
    private lazy val pairLiteral = emptyPair #> PairLiteral()
    private lazy val maybeArrayElem: Parsley[String => Expr] = {
      choice(some('[' ~> _parseExpr <~ ']').map(ArrayElem(_)), pure(IdentLiteral(_)))
    }

    lazy val parseExprAtom: Parsley[Expr] =
      intLiteral <|>
        boolLiteral <|>
        charLiteral <|>
        stringLiteral <|>
        pairLiteral <|>
        (identifier <**> maybeArrayElem) // Both an identifier and an array element can start with an 'ident'

    private def without[A](q: Parsley[A], p: Parsley[A]): Parsley[A] = attempt(p <~ notFollowedBy(q))
    private lazy val identCont = stringOfMany('_' <|> letterOrDigit)

    private lazy val _parseExpr: Parsley[Expr] = precedence(parseExprAtom, '(' ~> _parseExpr <~ ')')(
      Ops[Expr](Prefix)(without(intLiteral, "-") #> UnaryOp(Neg),
        "!" #> UnaryOp(Not), "len" #> UnaryOp(Len),
        "ord" #> UnaryOp(Ord), "chr" #> UnaryOp(Chr)),
      Ops[Expr](InfixL)("*" #> BinaryOp(Mul), "/" #> BinaryOp(Div), "%" #> BinaryOp(Mod)),
      Ops[Expr](InfixL)("+" #> BinaryOp(Add), "-" #> BinaryOp(Sub)),
      Ops[Expr](InfixL)(">=" #> BinaryOp(Gte), ">" #> BinaryOp(Gt)),
      Ops[Expr](InfixL)("<=" #> BinaryOp(Lte), "<" #> BinaryOp(Lt)),
      Ops[Expr](InfixL)("==" #> BinaryOp(Eq), "!=" #> BinaryOp(Neq)),
      Ops[Expr](InfixL)("&&" #> BinaryOp(And)),
      Ops[Expr](InfixL)("||" #> BinaryOp(Or))
    )

    lazy val parseExpr: Parsley[Expr] = fully(_parseExpr)
  }

//  object Statement {
//    import wacc.Lexer.implicits._
//    private lazy val declaration
//    private lazy val assignment
//    private lazy val read
//    private lazy val
//
//    private lazy val parseStatementAtom = "skip" <|>
//
//  }

}
