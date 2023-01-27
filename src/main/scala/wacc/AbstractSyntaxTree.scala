package wacc

import wacc.AbstractSyntaxTree.BaseT._
import wacc.AbstractSyntaxTree.BinaryOpType.{Add, BinOp}
import wacc.AbstractSyntaxTree.UnaryOpType.UnOp

// This should probably be a class which takes in a lookup table
object AbstractSyntaxTree {

  sealed trait PairLit extends Expr with RVal
  case class PairLiteral() extends PairLit

  sealed trait ArrayE extends Expr with LVal
  case class ArrayElem(val name: String, val indices: List[Expr]) extends ArrayE

  sealed trait IdentLit extends Expr with LVal
  case class IdentLiteral(val name: String) extends IdentLit


  sealed trait Expr extends RVal
  case class IntLiteral(val x: Int) extends Expr
  case class BoolLiteral(val x: Boolean) extends Expr
  case class CharLiteral(val x: Char) extends Expr
  case class StringLiteral(val x: String) extends Expr
  case class UnaryOp(val op: UnOp, val expr: Expr) extends Expr
  case class BinaryOp(val op: BinOp, val expr1: Expr, val expr2: Expr) extends Expr

  /* Constructors and Factories */
  object ArrayElem {
    def apply(types: List[Expr]): String => ArrayElem = (name: String) => ArrayElem(name, types)
  }

  object UnaryOp {
    def apply(op: UnOp): Expr => UnaryOp = (expr: Expr) => UnaryOp(op, expr)
  }

  object BinaryOp {
    def apply(op: BinOp): (Expr, Expr) => BinaryOp = (expr1: Expr, expr2: Expr) => BinaryOp(op, expr1, expr2)
  }

  /* Enums */
  object UnaryOpType extends Enumeration {
    type UnOp = Value
    val Not, Neg, Len, Ord, Chr = Value
  }

  object BinaryOpType extends Enumeration {
    type BinOp = Value
    val Mul, Div, Mod, Add, Sub, Gt, Gte, Lt, Lte, Eq, Neq, And, Or = Value
  }

  sealed trait Stat
  case class SkipStat() extends Stat
  case class Declaration(dataType: BaseType, ident: IdentLiteral, rvalue: RVal) extends Stat
  case class Assignment(lvalue: LVal, rvalue: RVal) extends Stat
  case class Read(lvalue: LVal) extends Stat // Read has a different input and output type to the other commands
  case class Command(command: CmdT.Cmd, input: Expr) extends Stat
  case class IfStat(cond: Expr, stat1: Stat, stat2: Stat) extends Stat
  case class WhileLoop(cond: Expr, stat: Stat) extends Stat
  case class Program(stat: Stat) extends Stat
  case class StatList(stat1: Stat, stat2: Stat) extends Stat

  object CmdT extends Enumeration {
    type Cmd = Value
    val Free, Ret, Exit, Print, PrintLn = Value
  }
  object BaseT extends Enumeration {
    type BaseType = Value
    val Int_T, Bool_T, Char_T, String_T = Value
  }


  sealed trait PairElem extends LVal with RVal
  case class PairElement(elem: PairElemT.Elem, lvalue: LVal) extends PairElem

  object PairElemT extends Enumeration {
    type Elem = Value
    val Fst, Snd = Value
  }



  sealed trait RVal
  case class ArrayLiteral(elements: List[Expr]) extends RVal
  case class Call(ident: IdentLiteral, args: List[Expr]) extends RVal
  case class PairValue(exp1: Expr, exp2: Expr) extends RVal



  sealed trait LVal
  case class IdentReference(name: String)


}



