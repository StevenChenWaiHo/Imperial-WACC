package wacc
import AbstractSyntaxTree._
import wacc.AbstractSyntaxTree.UnaryOpType._
import wacc.AbstractSyntaxTree.BinaryOpType._
import parsley.Parsley
import parsley.combinator.{choice, many, manyUntil, sepBy, sepBy1, some}
import parsley.Parsley.{attempt, lookAhead, notFollowedBy, pure}
import parsley.character.{endOfLine, item, letterOrDigit, newline, satisfy, stringOfMany}
import parsley.expr.{InfixL, Ops, Postfix, Prefix, precedence}
import parsley.implicits.character.charLift
import wacc.Parser.ExpressionParser.expression

object Parser {
  import wacc.Lexer._
  import wacc.Lexer.implicits._

  object ArrayParser {
    import parsley.combinator.sepBy1
    import Parser.ExpressionParser.expression

    lazy val arrayIndices: Parsley[String => ArrayElem] = some("[" ~> expression <~ "]").map(ArrayElem(_))
    lazy val maybeArrayElem: Parsley[String => Expr with LVal] = choice(arrayIndices, pure(IdentLiteral(_)))
    lazy val arrayLiteral = ("[" ~> sepBy1(expression, ",") <~ "]").map(ArrayLiteral)
  }

  /*
  object DeclarationTypeParser {
    import AbstractSyntaxTree.BaseT._
    import parsley.implicits.lift.{Lift2}
    import parsley.expr.chain
    import parsley.expr.chain.postfix1

    lazy val baseTypeType = "int" #> Int_T <|> "bool" #> Bool_T <|> "char" #> Char_T <|> "string" #> String_T
    private lazy val baseType = baseTypeType.map(BaseType)
    private lazy val canBeArray: Ops[DeclarationType, DeclarationType] =
      Ops[DeclarationType](Postfix)(("[" ~> "]") #> ArrayType)

    private lazy val declarationType: Parsley[DeclarationType] = precedence(baseType, pairType)(canBeArray)
    private lazy val pairElemType = baseType <|> ("pair" #> NestedPair()) <|>

    private lazy val isArray = lookAhead(manyUntil(notFollowedBy(endOfLine), attempt("[" ~> "]" ~> endOfLine)))
    private lazy val pairType: Parsley[DeclarationType] = PairType.lift("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")
  }
  
   */

  object PairParser {
    import LValueParser.lValue

    lazy val pairValue = pure(PairValue.tupled) <*> (("(" ~> expression <~ ",") <~> (expression <~ ")"))
    private lazy val pairElementType = ("fst" #> PairElemT.Fst <|> "snd" #> PairElemT.Snd)
    lazy val pairElement = pure(PairElement.tupled) <*> (pairElementType <~> lValue)
    lazy val pairLiteral = emptyPair #> PairLiteral()
  }

  object ExpressionParser {
    import Parser.ArrayParser.maybeArrayElem
    import Parser.PairParser.pairLiteral

    private lazy val intLiteral = integer.map(IntLiteral)
    private lazy val boolLiteral = boolean.map(BoolLiteral)
    private lazy val charLiteral = character.map(CharLiteral)
    private lazy val stringLiteral = string.map(StringLiteral)

    lazy val parseExprAtom: Parsley[Expr] =
      intLiteral <|>
        boolLiteral <|>
        charLiteral <|>
        stringLiteral <|>
        pairLiteral <|>
        (identifier <**> maybeArrayElem) // Both an identifier and an array element can start with an 'ident'

    private def without[A](q: Parsley[A], p: Parsley[A]): Parsley[A] = attempt(p <~ notFollowedBy(q))
    private lazy val identCont = stringOfMany('_' <|> letterOrDigit)

    private lazy val _parseExpr: Parsley[Expr] = precedence(parseExprAtom, "(" ~> _parseExpr <~ ")")(
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

    lazy val expression: Parsley[Expr] = _parseExpr
  }

  object LValueParser {
    import PairParser.pairElement
    import ArrayParser.maybeArrayElem

    lazy val lValue: Parsley[LVal] = (identifier <**> maybeArrayElem) <|> pairElement
  }

  object RValueParser {
    import parsley.combinator.sepBy
    import ArrayParser.arrayLiteral
    import wacc.Parser.PairParser.{pairValue, pairElement}
    import wacc.Parser.ExpressionParser.expression

    private lazy val newPair = "newpair" ~> pairValue
    private lazy val call =  pure(Call.tupled) <*>
      (("call" ~> identifier.map(IdentLiteral)) <~> (sepBy(expression, ",")))

    lazy val rValue: Parsley[RVal] =
      expression <|>
      newPair <|>
      arrayLiteral <|>
      pairElement <|>
        call
  }

  object StatementParser {
    import parsley.implicits.lift.{Lift1, Lift2, Lift3}
    import parsley.expr.chain.right1
    import wacc.AbstractSyntaxTree.CmdT._
    import LValueParser.lValue
    import RValueParser.rValue
    import wacc.AbstractSyntaxTree.BaseT._


    private lazy val commandType =
      "free" #> Free <|> "return" #> Ret <|> "exit" #> Exit <|> "print" #> Print <|> "println" #> PrintLn
    private lazy val baseTypeType = "int" #> Int_T <|> "bool" #> Bool_T <|> "char" #> Char_T <|> "string" #> String_T

    lazy val baseType = baseTypeType.map(BaseType)
    private lazy val skipStat = "skip" #> SkipStat()
    private lazy val identLiteral = IdentLiteral.lift(identifier)
    private lazy val declaration = Declaration.lift(baseType, identLiteral, "=" ~> rValue)
    private lazy val assignment = Assignment.lift(lValue, "=" ~> rValue)
    private lazy val read = "read" ~> Read.lift(lValue)
    private lazy val command = Command.lift(commandType, expression)
    private lazy val ifStat =
      IfStat.lift("if" ~> expression, "then" ~> statement, "else" ~> statement <~ "fi")
    private lazy val whileLoop = WhileLoop.lift("while" ~> expression, "do" ~> statement<~ "done")
    private lazy val program = BeginEndStat.lift("begin" ~> statement<~ "end")

    private lazy val statementAtom: Parsley[Stat] =
      skipStat <|>
        declaration <|>
        assignment <|>
        read <|>
        command <|>
        ifStat <|>
        whileLoop <|>
        program

    lazy val statement: Parsley[Stat] =
      attempt(((statementAtom <~ ";") <::> sepBy1(statementAtom, ";")).map(StatList)) <|> statementAtom
  }

  object FunctionParser {
    import StatementParser.baseType
    import parsley.implicits.lift.{Lift4, Lift1}
    import StatementParser.statement

    private lazy val ident = IdentLiteral.lift(identifier)
    lazy val func = Func.lift(
      baseType,
      ident,
      "(" ~> sepBy1(baseType <~> ident, ","),
      ")" ~> "is" ~> statement <~ "end"
    )
  }

  object ProgramParser {
    import parsley.implicits.lift.Lift2
    import FunctionParser.func
    import StatementParser.statement
    lazy val program = fully("begin" ~> Program.lift(many(attempt(func)), statement <~ "end"))
  }

}
