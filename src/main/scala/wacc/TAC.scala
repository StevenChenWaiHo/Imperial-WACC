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

  
  /* array declaration in assembly
    @ 4 element array, 4 per element and 4 for array pointer
		mov r0, #20
		bl malloc
		mov r12, r0
		@ array pointers are shifted forwards by 4 bytes (to account for size)
		add r12, r12, #4
		mov r8, #4
		str r8, [r12, #-4]
		mov r8, #43
		str r8, [r12, #0]
		mov r8, #2
		str r8, [r12, #4]
		mov r8, #18
		str r8, [r12, #8]
		mov r8, #1
		str r8, [r12, #12]
  */
  //delegates each element in an array
  case class CreateArrayElem(elemType: DeclarationType, elemReg: TRegister) extends TAC
  //delegates an array with all of its elements
  case class CreateArray(elemType: DeclarationType, elemsReg: List[TRegister], dstReg: TRegister) extends TAC
  // StoreArrayElem
  // str srcReg [arrReg, pos], where pos = arrPos * 4 + 4
  case class StoreArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: Int, srcReg: TRegister) extends TAC
  // GetArrayElem
  // ldr dstReg [arrReg, pos], where pos = arrPos * 4 + 4
  case class GetArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: Int, dstReg: TRegister) extends TAC

  case class Comments(string: String) extends TAC {
    override def toString(): String = "@ " + string
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
    class BoolLiteralTAC(b: Boolean) extends LiteralTAC {
      override def toString(): String = b.toString()
    }
    class CharLiteralTAC(c: Char) extends LiteralTAC {
      override def toString(): String = "\'" + c + "\'"
    }
  class ArrayOp(elems: List[Operand]) extends Operand {
    override def toString(): String = elems.toString()
  }
  case class ArrayElemTAC(arr: Operand, indices: List[Operand]) extends Operand {
    override def toString(): String = arr + "[" + indices + "]"
  }
  
}