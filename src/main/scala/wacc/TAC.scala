package wacc

import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.CmdT.Cmd

object TAC {
    sealed trait TAC

  case class BinaryOpTAC(op: BinOp, t1: Operand, t2: Operand, res: TRegister) extends TAC {
    override def toString(): String = res + " = " + t1 + " " + op + " " + t2
  }
  case class UnaryOpTAC(op: UnOp, t1: Operand, res: TRegister) extends TAC
  final case class AssignmentTAC(t1: Operand, res: TRegister) extends TAC {
    override def toString(): String = res + " = " + t1
  }
  case class IfTAC(t1: Operand, goto: Label) extends TAC {
    override def toString(): String = "if " + t1 + " then goto " + goto.name
  }
  case class CommandTAC(cmd: Cmd, t1: Operand) extends TAC
  case class PushParamTAC(t1: Operand) extends TAC
  case class PopParamTAC(t1: TRegister) extends TAC
  case class CallTAC(lbl: Label, args: List[TRegister]) extends TAC
  case class BeginFuncTAC() extends TAC
  case class EndFuncTAC() extends TAC
  case class GOTO(label: Label) extends TAC {
    override def toString(): String = "goto: " + label.name
  }
  case class Label(name: String = "label") extends TAC {
    override def toString(): String = name + ":"
  }

  sealed trait Operand
  case class TRegister(num: Int) extends Operand {
    override def toString(): String = "_T" + num
  }
  class LiteralTAC() extends Operand
    case class IdentLiteralTAC(name: String) extends LiteralTAC {
      override def toString(): String = name
    }
    case class IntLiteralTAC(value: Int) extends LiteralTAC {
      override def toString(): String = value.toString()
    }
    case class StringLiteralTAC(str: String) extends LiteralTAC {
      override def toString(): String = "\"" + str + "\""
    }
    case class BoolLiteralTAC(b: Boolean) extends LiteralTAC
    case class CharLiteralTAC(c: Char) extends LiteralTAC
  case class ArrayOp(elems: List[Operand]) extends Operand {
    override def toString(): String = elems.toString()
  }
  case class ArrayElemTAC(arr: Operand, indices: List[Operand]) extends Operand {
    override def toString(): String = arr + "[" + indices + "]"
  }
  
}