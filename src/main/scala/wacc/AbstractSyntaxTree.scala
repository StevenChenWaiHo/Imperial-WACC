package wacc

import wacc.AbstractSyntaxTree.BaseT._
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.UnaryOpType.UnOp

// This should probably be a class which takes in a lookup table
object AbstractSyntaxTree {
  object BaseT extends Enumeration {
    type Type = Value
    val Int_T, Bool_T, Char_T, String_T, None_T = Value
  }

  sealed trait Expr
  case class IntLiteral(val x: Int) extends Expr
  case class BoolLiteral(val x: Boolean) extends Expr
  case class CharLiteral(val x: Char) extends Expr
  case class StringLiteral(val x: String) extends Expr
  case class PairLiteral() extends Expr
  case class IdentLiteral(val name: String) extends Expr
  case class ArrayElem(val name: String, val indices: List[Expr]) extends Expr
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

  case class baseType()

}



