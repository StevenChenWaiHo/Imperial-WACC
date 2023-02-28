package wacc

import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.CmdT.Cmd
import wacc.AbstractSyntaxTree.DeclarationType

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
  case class DataSegmentTAC() extends TAC {
    override def toString(): String = ".data"
  }
  case class TextSegmentTAC() extends TAC {
    override def toString(): String = ".text"
  }
  case class StringDefinitionTAC(str: String, lbl: Label) extends TAC {
     override def toString(): String = lbl.toString() + "\n\t.asciz \"" + str + "\""
  }
  case class StringLengthDefinitionTAC(len: Int, lbl: Label) extends TAC {
     override def toString(): String = "\t.word " + len.toString
  }
  case class GOTO(label: Label) extends TAC {
    override def toString(): String = "goto: " + label.name
  }
  case class Label(name: String = "label") extends TAC with Operand {
    override def toString(): String = name + ":"
  }
  case class CreatePairFstElem(fstType: DeclarationType, fstReg: TRegister) extends TAC
  case class CreatePairSndElem(sndType: DeclarationType, sndReg: TRegister) extends TAC
  case class CreatePair(fstReg: TRegister, sndReg: TRegister, dstReg: TRegister) extends TAC

  case class Comments(string: String) extends TAC {
    override def toString(): String = "@ " + string
  }

  sealed trait Operand
  class TRegister(num: Int) extends Operand {
    override def toString(): String = "_T" + num
  }
  class LiteralTAC() extends Operand
    class IdentLiteralTAC(name: String) extends LiteralTAC {
      override def toString(): String = name
    }
    class IntLiteralTAC(value: Int) extends LiteralTAC {
      override def toString(): String = value.toString()
    }
    class BoolLiteralTAC(b: Boolean) extends LiteralTAC {
      override def toString(): String = b.toString()
    }
    class CharLiteralTAC(c: Char) extends LiteralTAC {
      override def toString(): String = "\'" + c + "\'"
    }
  class ArrayOp(elems: List[Operand]) extends Operand {
    override def toString(): String = elems.toString()
  }
  class ArrayElemTAC(arr: Operand, indices: List[Operand]) extends Operand {
    override def toString(): String = arr + "[" + indices + "]"
  }
  
}