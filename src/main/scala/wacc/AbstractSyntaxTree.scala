package wacc

import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.UnaryOpType.UnOp

object AbstractSyntaxTree {
  sealed trait Expr
  case class IntLiteral(x: Int) extends Expr
  case class BoolLiteral(x: Boolean) extends Expr
  case class CharLiteral(x: Char) extends Expr
  case class StringLiteral(x: String) extends Expr
  case class PairLiteral() extends Expr
  case class IdentLiteral(name: String) extends Expr
  case class ArrayElem(name: String, types: List[Expr]) extends Expr
  case class UnaryOp(op: UnOp, expr: Expr) extends Expr
  case class BinaryOp(op: BinOp, expr1:Expr, expr2:Expr) extends Expr


  /* Constructors and Factories */
  object ArrayElem {
    def apply(types: List[Expr]): String => ArrayElem = (name: String) => ArrayElem(name, types)
  }

  object UnaryOp {
    def apply(op: UnOp): Expr => UnaryOp = (expr: Expr) => UnaryOp(op, expr)
  }

  object BinaryOp {
    def apply(op: BinOp): Expr => Expr => BinaryOp = (expr1: Expr) => (expr2: Expr) => BinaryOp(op, expr1, expr2)
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
}



