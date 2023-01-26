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

  /**
   * The type system could be described in terms of LitTs: For example, a String literal is of type () -> String
   * and a declaration (for example, 'int x') or void-returning function could be of type Int_T -> ().
   * This could be used during semantic analysis.
   * Nothing takes a None_T as input (for no inputs, just leave the input list empty)
   * References could be dealt with by the RefT class (which can act like a lazy pointer)
   * */
  abstract class LitT(output: Type = BaseT.None_T, input: List[Type] = Nil, deps: List[LitT] = Nil) {
    def in = input
    def out = output
    def validateType: Boolean =
      if (in.length != deps.length) throw new Error("parser error - incorrect argument count")
      else in.zip(deps).foldLeft(true)((b, ts) => b && (ts._1 == ts._2.out) && ts._2.validateType)
  }
  abstract class RefT(reference: String, deps: List[LitT] = Nil) extends LitT { // (implicit IdentifierTable context)
    override def in = throw new NotImplementedError("Lookups not yet implemented")
    override def out = throw new NotImplementedError("Lookups not yet implemented")
  }
  abstract class KeywordT




  sealed trait Expr
  case class IntLiteral(val x: Int) extends LitT(Int_T) with Expr
  case class BoolLiteral(val x: Boolean) extends LitT(Bool_T) with Expr
  case class CharLiteral(val x: Char) extends LitT(Char_T) with Expr
  case class StringLiteral(val x: String) extends LitT(String_T) with Expr
  case class PairLiteral() extends LitT(None_T) with Expr //TODO: May need to change this type
  case class IdentLiteral(val name: String) extends RefT(name) with Expr
  case class ArrayElem(val name: String, val indices: List[LitT with Expr]) extends RefT(name, indices) with Expr
  case class UnaryOp(val op: UnOp, val expr: LitT with Expr) extends RefT(op.toString(), List(expr)), Int_T) with Expr
  case class BinaryOp(val op: BinOp, val expr1: Expr, val expr2: Expr) extends LitT(output = Int_T) with Expr


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



