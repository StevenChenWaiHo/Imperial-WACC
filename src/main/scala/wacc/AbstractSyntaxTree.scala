package wacc

import wacc.AbstractSyntaxTree.BaseT.{Any_T, BaseTypeType}
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.UnaryOpType.UnOp

// This should probably be a class which takes in a lookup table
object AbstractSyntaxTree {
  sealed trait ASTNode

  sealed trait PairLit extends Expr with RVal
  case class PairLiteral() extends PairLit

  sealed trait ArrayE extends Expr with LVal
  case class ArrayElem(name: String, indices: List[Expr]) extends ArrayE

  sealed trait IdentLit extends Expr with LVal
  case class IdentLiteral(name: String) extends IdentLit


  sealed trait Expr extends RVal
  case class IntLiteral(x: Int) extends Expr
  case class BoolLiteral(x: Boolean) extends Expr
  case class CharLiteral(x: Char) extends Expr
  case class StringLiteral(x: String) extends Expr
  case class UnaryOp(op: UnOp, expr: Expr) extends Expr
  case class BinaryOp(op: BinOp, expr1: Expr, expr2: Expr) extends Expr

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


  // Note: AnyType and NoneType will never arise in the AST, so it should be OK to override their equality checking
  sealed trait DeclarationType extends ASTNode {
    private def isAny (decl: DeclarationType): Boolean = decl match {
      case BaseType(x) => x == Any_T
      case _ => false
    }
    /* BaseType(Any_T) matches any other declaration type. */
//    override def equals(obj: Any): Boolean = obj match {
//      case obj: DeclarationType if isAny(obj) || isAny(this) => true
//      case _ => super.equals(obj)
//    }
  }

  case class NestedPair() extends DeclarationType
  case class BaseType(baseType: BaseTypeType) extends DeclarationType
  case class ArrayType(dataType: DeclarationType) extends DeclarationType
  case class PairType(fstType: DeclarationType, sndType: DeclarationType) extends DeclarationType

  case class Program(funcs: List[Func], stats: Stat) extends ASTNode

  case class Func(returnType: DeclarationType, ident: IdentLiteral, types: List[(DeclarationType, IdentLiteral)], code: Stat) extends ASTNode

  sealed trait Stat extends ASTNode
  case class SkipStat() extends Stat
  case class Declaration(dataType: DeclarationType, ident: IdentLiteral, rvalue: RVal) extends Stat
  case class Assignment(lvalue: LVal, rvalue: RVal) extends Stat
  case class Read(lvalue: LVal) extends Stat // Read has a different input and output type to the other commands
  case class Command(command: CmdT.Cmd, input: Expr) extends Stat
  case class IfStat(cond: Expr, stat1: Stat, stat2: Stat) extends Stat
  case class WhileLoop(cond: Expr, stat: Stat) extends Stat
  case class BeginEndStat(stat: Stat) extends Stat
  case class StatList(stats: List[Stat]) extends Stat

  object CmdT extends Enumeration {
    type Cmd = Value
    val Free, Ret, Exit, Print, PrintLn = Value
  }
  object BaseT extends Enumeration {
    type BaseTypeType = Value
    val Int_T, Bool_T, Char_T, String_T, Any_T, None_T = Value
  }

  sealed trait PairElem extends LVal with RVal
  case class PairElement(elem: PairElemT.Elem, lvalue: LVal) extends PairElem

  object PairElemT extends Enumeration {
    type Elem = Value
    val Fst, Snd = Value
  }

  sealed trait RVal extends ASTNode
  case class ArrayLiteral(elements: List[Expr]) extends RVal
  case class Call(ident: IdentLiteral, args: List[Expr]) extends RVal
  case class PairValue(exp1: Expr, exp2: Expr) extends RVal

  sealed trait LVal extends ASTNode
}



