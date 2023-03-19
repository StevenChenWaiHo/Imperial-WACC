package wacc

import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.CmdT.Cmd
import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.AbstractSyntaxTree.{DeclarationType, PairElemT}

object TAC {

  /* Quick and easy way to label data segments. Used in Label. */
  private val assignmentList = collection.mutable.Map[String, Int]()

  def getId(category: String): Int = {
    val id = assignmentList.getOrElseUpdate(category, 0)
    assignmentList.update(category, id + 1)
    id
  }

  sealed trait defines {
    val res: TRegister
  }
  sealed trait uses {
    print("hi")
  }

  /*
  * TAC -> copy() with name = param
  * */

  sealed trait TAC


  /** Alias: A new name for the same value. Can be pushed to the same place in memory as the old value */
  case class ReservedPushTAC(alias: TRegister, location: Int, original: TRegister) extends TAC
  case class ReservedPopTAC(location: Int, alias: TRegister, original:TRegister) extends TAC
  case class AllocateStackTAC(size: Int) extends TAC

  case class PushTAC(reg: TRegister) extends TAC
  case class PopTAC(reg: TRegister) extends TAC

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

  case class CommandTAC(cmd: Cmd, t1: TRegister, opType: DeclarationType) extends TAC
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
    this.name = name

    override def toString(): String = name + ":"
  }

  //   --- CreatePairElem(Fst/Snd) --- 
  // mov r0, #TypeSize
  // malloc fst elem with reference to its type
  // str pairElemReg, [r0, #0]
  // push r0
  case class CreatePairElem(pairElemType: DeclarationType, pairPos: PairElemT.Elem, pairElemReg: TRegister) extends TAC

  //   --- CreatePair --- 
  // The two pointer to pair elem is on stack in reversed order
  // mov r0, #PointerSize * 2
  // malloc 2 * 4 bytes for 2 pointers
  // mov dst r0
  // pop r2
  // str r2 [dst, #4]
  // pop r1
  // str r1 [dst, #0]
  case class CreatePair(dstReg: TRegister) extends TAC

  // StorePairElem
  // ldr r1 [pairReg, pairPos]
  // str srcReg [r1, pairPos], where (pairPos == fst) ? #0 : #4
  case class StorePairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, srcReg: TRegister) extends TAC

  // GetPairElem
  // Check Null
  // ldr r1 [pairReg, pairPos], where (pairPos == fst) ? #0 : #4\
  // ldr dstReg [r1, 0]
  case class GetPairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, dstReg: TRegister) extends TAC


  /* array declaration in assembly
		mov r0, #PointerSize * Length
		bl malloc
		mov dstReg, r0
		@ array pointers are shifted forwards by 4 bytes (to account for size)
		add dstReg, dstReg, #4
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
  case class InitialiseArray(arrLen: Int, dstReg: TRegister) extends TAC

  //delegates each element in an array
  case class CreateArrayElem(arrayElemType: DeclarationType, elemPos: Int, arrReg: TRegister, elemReg: TRegister) extends TAC

  // StoreArrayElem
  // str srcReg [arrReg, pos], where pos = arrPos * 4 + 4 (if not nested)
  case class StoreArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: List[TRegister], srcReg: TRegister) extends TAC

  // LoadArrayElem
  // ldr dstReg [arrReg, pos], where pos = arrPos * 4 + 4 (if not nested)
  case class LoadArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: List[TRegister], dstReg: TRegister) extends TAC

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

  case class ArrayOp(elems: List[Operand]) extends Operand {
    override def toString(): String = elems.toString()
  }

  case class ArrayElemTAC(arr: Operand, indices: List[Operand]) extends Operand {
    override def toString(): String = arr + "[" + indices + "]"
  }

}