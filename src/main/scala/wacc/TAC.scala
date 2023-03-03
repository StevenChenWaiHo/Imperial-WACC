package wacc

import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.CmdT.Cmd
import wacc.AbstractSyntaxTree.DeclarationType
import wacc.AbstractSyntaxTree.Expr
import wacc.AbstractSyntaxTree.PairElemT

object TAC {

  /* Quick and easy way to label data segments. Used in Label. */
  private val assignmentList = collection.mutable.Map[String, Int]()

  def getId(category: String): Int = {
    val id = assignmentList.getOrElseUpdate(category, 0)
    assignmentList.update(category, id + 1)
    id
  }

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
  case class CommandTAC(cmd: Cmd, t1: Operand, opType: DeclarationType) extends TAC
  case class PushParamTAC(t1: Operand) extends TAC
  case class PopParamTAC(datatype: DeclarationType, t1: TRegister, index: Int) extends TAC
  case class CallTAC(lbl: Label, args: List[TRegister], dstReg: TRegister) extends TAC 
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

  case class Label(var name: String = "label") extends TAC with Operand {
    this.name = name // TODO: change to include id but only in some cases
    override def toString(): String = name + ":"
  }

  // Pairs Operations to ARM
  //   --- CreatePairElem(Fst) --- 
  // Save r8 and r12
  // malloc fst elem with reference to its type
  // mov r8 pairElemReg
  // mov r12 r0
  // str r8, [r12, #0]
  // mov fstReg r12
  // Restore r8 and r12
  // push pairElemReg
  //   --- CreatePairElem(Snd) --- 
  // Save r8 and r12
  // r8 = sndReg = register with sndElem
  // malloc snd elem with reference to its type
  // mov r8 pairElemReg
  // mov r12 r0
  // str r8, [r12, #4]
  // mov sndReg r12
  // Restore r8 and r12
  // push pairElemReg
  //   --- CreatePair() --- 
  // malloc 2 * 4 bytes for 2 pointers
  // mov r12 r0
  // pop r8
  // str r8 [r12, #4]
  // pop r8  
  // str r8 [r12, #0]
  // mov dstReg r12
  case class CreatePairElem(pairElemType: DeclarationType, pairPos: PairElemT.Elem, pairElemReg: TRegister) extends TAC
  case class CreatePair(fstType: DeclarationType, sndType: DeclarationType, 
                        fstReg: TRegister, sndReg: TRegister, dstReg: TRegister) extends TAC

  // StorePairElem
  // str srcReg [pairReg, pairPos], where (pairPos == fst) ? #0 : #4
  case class StorePairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, srcReg: TRegister) extends TAC
  // GetPairElem
  // ldr dstReg [pairReg, pairPos], where (pairPos == fst) ? #0 : #4
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
  // str srcReg [arrReg, pos], where pos = arrPos * 4 + 4 (if not nested)
  case class StoreArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: List[Expr], srcReg: TRegister) extends TAC
  // GetArrayElem
  // ldr dstReg [arrReg, pos], where pos = arrPos * 4 + 4 (if not nested)
  case class GetArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: List[TRegister], dstReg: TRegister) extends TAC

  case class ReadTAC(dataType: DeclarationType, readReg: TRegister) extends TAC

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
    case class PairLiteralTAC() extends LiteralTAC {
      override def toString(): String = "null"
    }
    case class IntLiteralTAC(value: Int) extends LiteralTAC {
      override def toString(): String = value.toString()
    }
    case class BoolLiteralTAC(b: Boolean) extends LiteralTAC {
      override def toString(): String = b.toString()
    }
    case class CharLiteralTAC(c: Char) extends LiteralTAC {
      override def toString(): String = "\'" + c + "\'"
    }
  class ArrayOp(elems: List[Operand]) extends Operand {
    override def toString(): String = elems.toString()
  }
  case class ArrayElemTAC(arr: Operand, indices: List[Operand]) extends Operand {
    override def toString(): String = arr + "[" + indices + "]"
  }
  
}