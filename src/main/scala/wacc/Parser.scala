package wacc

import parsley.Parsley
import parsley.Parsley.{attempt, notFollowedBy, pure}
import parsley.character.{letterOrDigit, stringOfMany}
import parsley.combinator._
import parsley.errors.combinator.ErrorMethods
import parsley.expr._
import parsley.implicits.character.charLift
import wacc.AbstractSyntaxTree.BinaryOpType._
import wacc.AbstractSyntaxTree.UnaryOpType._
import wacc.AbstractSyntaxTree._
import wacc.Parser.ExpressionParser.expression

object Parser {

  import wacc.Lexer._
  import wacc.Lexer.implicits._

  object ArrayParser {

    import Parser.ExpressionParser.expression

    lazy val arrayIndices: Parsley[String => ArrayElem] = some("[" ~> expression <~ "]").map(ArrayElem(_))
    lazy val maybeArrayElem: Parsley[String => Expr with LVal] = choice(arrayIndices, pure(IdentLiteral(_)))
    lazy val arrayLiteral = ("[" ~> sepBy(expression, ",") <~ "]").map(ArrayLiteral)
  }


  object DeclarationTypeParser {

    import AbstractSyntaxTree.BaseT._
    import parsley.implicits.lift.Lift2

    lazy val baseTypeType = "int" #> Int_T <|> "bool" #> Bool_T <|> "char" #> Char_T <|> "string" #> String_T
    private lazy val baseType = baseTypeType.map(BaseType)
    private lazy val pairType = PairType.lift("pair" ~> "(" ~> pairElemType <~ ",", pairElemType <~ ")")
    private lazy val pairElemType: Parsley[DeclarationType] =
      (("pair" <~ notFollowedBy("(")) #> NestedPair()) <|> declarationType
    lazy val declarationType: Parsley[DeclarationType] = precedence[DeclarationType](pairType, baseType)(
      Ops[DeclarationType](Postfix)("[" ~> "]" #> ArrayType)
    )
  }

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

    private lazy val identCont = stringOfMany('_' <|> letterOrDigit)

    private lazy val _parseExpr: Parsley[Expr] = precedence(parseExprAtom, "(" ~> _parseExpr <~ ")")(
      Ops[Expr](Prefix)(attempt("-" <~ notFollowedBy(intLiteral)) #> UnaryOp(Neg),
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

    import ArrayParser.maybeArrayElem
    import PairParser.pairElement

    lazy val lValue: Parsley[LVal] = (identifier <**> maybeArrayElem) <|> pairElement
  }

  object RValueParser {

    import ArrayParser.arrayLiteral
    import parsley.combinator.sepBy
    import parsley.implicits.lift.Lift2
    import wacc.Parser.ExpressionParser.expression
    import wacc.Parser.PairParser.{pairElement, pairValue}

    private lazy val newPair = "newpair" ~> pairValue

    private lazy val call = Call.lift("call" ~> identifier.map(IdentLiteral),
      "(" ~> (sepBy(expression, ",") <~ ")"))

    lazy val rValue: Parsley[RVal] =
      expression <|>
        newPair <|>
        arrayLiteral <|>
        pairElement <|>
        call
  }

  def noReturnStat(stat: Stat): Boolean = stat match {
    case IfStat(cond, stat1, stat2) => noReturnStat(stat1) || noReturnStat(stat2)
    case WhileLoop(cond, stat1) => noReturnStat(stat1)
    case BeginEndStat(stat1) => noReturnStat(stat1)
    case StatList(statList) => noReturnStat(statList.last)
    case Command(CmdT.Exit, _) | Command(CmdT.Ret, _) => false
    case _ => true
  }

  object StatementParser {

    import DeclarationTypeParser.declarationType
    import LValueParser.lValue
    import RValueParser.rValue
    import parsley.implicits.lift.{Lift1, Lift2, Lift3}
    import wacc.AbstractSyntaxTree.CmdT._

    private lazy val commandType =
      "free" #> Free <|> "return" #> Ret <|> "exit" #> Exit <|> "print" #> Print <|> "println" #> PrintLn

    private lazy val skipStat = "skip" #> SkipStat()
    private lazy val identLiteral = IdentLiteral.lift(identifier)
    private lazy val declaration = Declaration.lift(declarationType, identLiteral, "=" ~> rValue)
    private lazy val assignment = Assignment.lift(lValue, "=" ~> rValue)
    private lazy val read = "read" ~> Read.lift(lValue)
    private lazy val command = Command.lift(commandType, expression)
    private lazy val ifStat =
      IfStat.lift("if" ~> expression, "then" ~> statement, "else" ~> statement <~ "fi")
    private lazy val whileLoop = WhileLoop.lift("while" ~> expression, "do" ~> statement <~ "done")
    private lazy val scopeStat = BeginEndStat.lift("begin" ~> statement <~ "end")

    private lazy val statementAtom: Parsley[Stat] =
      skipStat <|>
        declaration <|>
        assignment <|>
        read <|>
        command <|>
        ifStat <|>
        whileLoop <|>
        scopeStat

    lazy val statement: Parsley[Stat] =
      attempt(((statementAtom <~ ";") <::> sepBy1(statementAtom, ";")).map(StatList)) <|> statementAtom
  }

  object FunctionParser {

    import DeclarationTypeParser.declarationType
    import StatementParser.statement
    import parsley.implicits.lift.{Lift1, Lift4}

    private lazy val ident = IdentLiteral.lift(identifier)
    lazy val func = Func.lift(
      declarationType.label("Function return type"),
      ident.label("Function name"),
      "(" ~> sepBy(declarationType.label("Type of Parameter") <~> ident.("Function parameter"), ","),
      ")" ~> "is" ~> statement.filterOut {
        case s if noReturnStat(s) => s"No exit or return statement"
      } <~ "end"
    ).label("Function Declaration").explain("Function declaration are <type> <function_name> (<parameter_list> is <body>)")
  }

  object ProgramParser {

    import FunctionParser.func
    import StatementParser.statement
    import parsley.implicits.lift.Lift2

    lazy val program = fully("begin" ~> Program.lift(many(attempt(func)).label("Program"), statement <~ "end"))
  }

}
