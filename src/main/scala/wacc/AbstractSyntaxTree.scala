package wacc

import wacc.AbstractSyntaxTree.BaseT.{Any_T, BaseTypeType, None_T}
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.UnaryOpType.UnOp

// This should probably be a class which takes in a lookup table
object AbstractSyntaxTree {
  sealed trait ASTNode {
    var context: VarInfo = null
    def attachContext(context: VarInfo): Unit = {
      this.context = context
    }
  }

  sealed trait PairLit extends Expr with RVal

  case class PairLiteral() extends PairLit with Literal

  sealed trait ArrayE extends Expr with LVal

  case class ArrayElem(name: IdentLiteral, indices: List[Expr]) extends ArrayE {
    def this(n: String, is: List[Expr]) = this(IdentLiteral(n), is)
  }

  sealed trait IdentLit extends Expr with LVal

  case class IdentLiteral(name: String) extends IdentLit with Literal


  sealed trait Expr extends RVal
  sealed trait Literal

  case class IntLiteral(x: Int) extends Expr with Literal

  case class BoolLiteral(x: Boolean) extends Expr with Literal

  case class CharLiteral(x: Char) extends Expr with Literal

  case class StringLiteral(x: String) extends Expr with Literal

  case class UnaryOp(op: UnOp, expr: Expr) extends Expr

  case class BinaryOp(op: BinOp, expr1: Expr, expr2: Expr) extends Expr

  /* Constructors and Factories */
  object ArrayElem {
    def apply(types: List[Expr]): String => ArrayElem = (name: String) => new ArrayElem(name, types)
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


  sealed trait DeclarationType extends ASTNode {
    val isAny: Boolean = this match {
      case BaseType(Any_T) => true
      case _ => false
    }

    val isNone: Boolean = this match {
      case BaseType(None_T) => true
      case _ => false
    }

    def is(other: DeclarationType): Boolean = {
      if(this.isNone || other.isNone) return false
      if(this.isAny || other.isAny) return true

      this match {
        case NestedPair() => other match {
          case other: NestedPair => true
          case other: PairType => true
          case _ => false
        }
        case BaseType(Any_T) => true

        case BaseType(a) => other match {
          case BaseType(b) => ((a != None_T && b != None_T) && a == b) || (a == Any_T) || (b == Any_T)
          case _ => false
        }
        case ArrayType(a, aSize) => other match {
          case ArrayType(b, bSize) => (aSize == -1 || bSize == -1 || aSize == bSize) && (a is b)
          case _ => false
        }
        case PairType(a, b) => other match {
          case other: NestedPair => true
          case PairType(a2, b2) => (a is a2) && (b is b2)
          case _ => false
        }
        // Programs and functions probably shouldn't be compared with anything...
        case _ => false
      }
    }
  }

  case class NestedPair() extends DeclarationType

  case class BaseType(baseType: BaseTypeType) extends DeclarationType

  case class ArrayType(dataType: DeclarationType, length: Int = -1) extends DeclarationType

  case class PairType(fstType: DeclarationType, sndType: DeclarationType) extends DeclarationType

  case class Program(funcs: List[Func], stats: Stat) extends ASTNode

  case class Func(returnType: DeclarationType, ident: IdentLiteral, types: List[(DeclarationType, IdentLiteral)], code: Stat) extends ASTNode

  sealed trait Stat extends ASTNode {}

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
    type  Cmd = Value
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



