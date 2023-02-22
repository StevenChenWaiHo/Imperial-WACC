package wacc

import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.CmdT.Cmd

object TAC {
    sealed trait TAC

  case class BinaryOpTAC(op: BinOp, t1: Operand, t2: Operand, res: TRegister) extends TAC
  case class UnaryOpTAC(op: UnOp, t1: Operand, res: TRegister) extends TAC
  final case class AssignmentTAC(t1: Operand, res: TRegister) extends TAC {
    override def toString(): String = res.toString + " = " + t1.toString()
  }
  case class IfTAC(t1: Operand, goto: Label) extends TAC
  case class CommandTAC(cmd: Cmd, t1: Operand) extends TAC
  case class PushParamTAC(t1: Operand) extends TAC
  case class PopParamTAC(t1: TRegister) extends TAC
  case class CallTAC(f: Label) extends TAC
  case class BeginFuncTAC() extends TAC
  case class EndFuncTAC() extends TAC
  case class GOTO(label: Label) extends TAC
  case class Label(name: String = "label") extends TAC

  sealed trait Operand
  class TRegister(num: Int) extends Operand with TAC {
    override def toString(): String = "_T" + num
  }
  class LiteralTAC() extends Operand
    class IdentLiteralTAC(name: String) extends LiteralTAC {
      override def toString(): String = name
    }
    class IntLiteralTAC(value: Int) extends LiteralTAC {
      override def toString(): String = value.toString()
    }
    class StringLiteralTAC(str: String) extends LiteralTAC {
      override def toString(): String = "\"" + str + "\""
    }
    class BoolLiteralTAC(b: Boolean) extends LiteralTAC
    class CharLiteralTAC(c: Char) extends LiteralTAC
  class ArrayOp(elems: List[Operand]) extends Operand {
    override def toString(): String = "[" + elems + "]"
  }
  
}