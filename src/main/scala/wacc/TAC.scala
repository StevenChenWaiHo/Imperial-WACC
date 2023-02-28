package wacc

import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.CmdT.Cmd
import wacc.AbstractSyntaxTree.DeclarationType
import wacc.AbstractSyntaxTree.PairElemT

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

  // Pairs Operations to ARM
  //   --- CreatePairFstElem() --- 
  // Save r8 and r12
  // malloc fst elem with reference to its type
  // mov r8 fstReg
  // mov r12 r0
  // str r8, [r12, #0]
  // push r12
  //   --- CreatePairSndElem() --- 
  // Save r8 and r12
  // r8 = sndReg = register with sndElem
  // malloc snd elem with reference to its type
  // mov r8 sndReg
  // mov r12 r0
  // str r8, [r12, #0]
  // push r12
  //   --- CreatePair() --- 
  // malloc 2 * 4 bytes for 2 pointers
  // mov r12 r0
  // pop r8
  // str r8 [r12, #4]
  // pop r8  
  // str r8 [r12, #0]
  // mov dstReg r12
  case class CreatePairFstElem(fstType: DeclarationType, fstReg: TRegister) extends TAC
  case class CreatePairSndElem(sndType: DeclarationType, sndReg: TRegister) extends TAC
  case class CreatePair(fstType: DeclarationType, sndType: DeclarationType, 
                        fstReg: TRegister, sndReg: TRegister, dstReg: TRegister) extends TAC

  // StorePairElem
  // str srcReg [pairReg, pos], where (pairPos == fst) ? #0 : #4
  case class StorePairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, srcReg: TRegister) extends TAC
  // GetPairElem
  // ldr dstReg [pairReg, pos], where (pairPos == fst) ? #0 : #4
  case class GetPairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, dstReg: TRegister) extends TAC

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
    class StringLiteralTAC(str: String) extends LiteralTAC {
      override def toString(): String = "\"" + str + "\""
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