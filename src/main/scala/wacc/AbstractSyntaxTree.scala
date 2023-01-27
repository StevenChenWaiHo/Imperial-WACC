package wacc

import wacc.AbstractSyntaxTree.BaseT._
import wacc.AbstractSyntaxTree.BinaryOpType.{Add, BinOp}
import wacc.AbstractSyntaxTree.UnaryOpType.UnOp

// This should probably be a class which takes in a lookup table
object AbstractSyntaxTree {

  sealed trait PairLit extends Expr with RVal
  case class PairLiteral() extends PairLit

  sealed trait ArrayE extends Expr with LValue
  case class ArrayElem(val name: String, val indices: List[Expr]) extends ArrayE

  sealed trait IdentLit extends Expr with LValue
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
  case class Declaration(dataType: BaseType, ident: IdentLiteral, rvalue: RVal) extends Stat
  case class Assignment(lvalue: LValue, rvalue: RVal)
  case class Command(command: CmdT.Cmd, expr: Expr)
  case class IfStat(cond: Expr, stat1: Stat, stat2: Stat)
  case class WhileLoop(cond: Expr, stat: Stat)
  case class Program(stat: Stat)
  case class StatList(stat1: Stat, stat2: Stat)

  object CmdT extends Enumeration {
    type Cmd = Value
    val Free, Return, Exit, Print, PrintLn = Value
  }
  object BaseT extends Enumeration {
    type BaseType = Value
    val Int_T, Bool_T, Char_T, String_T = Value
  }
  object Declaration {
    def apply(dataType: BaseType): (IdentLiteral => RVal => Declaration) = (ident: IdentLiteral) => (rvalue: RVal) => Declaration(dataType, ident, rvalue)
  }


  sealed trait PairElem extends LValue with RVal
  case class PairElement(elem: PairElemT.Elem, lvalue: LValue) extends PairElem

  object PairElemT extends Enumeration {
    type Elem = Value
    val Fst, Snd = Value
  }



  sealed trait RVal
  case class ArrayLiteral(elements: List[Expr]) extends RVal
  case class Call(ident: String, args: List[Expr]) extends RVal
  case class PairValue(exp1: Expr, exp2: Expr) extends RVal



  sealed trait LValue
  case class IdentReference(name: String)


}



