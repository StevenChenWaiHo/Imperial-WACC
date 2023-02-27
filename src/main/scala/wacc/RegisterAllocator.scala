package wacc

import wacc.TAC._
import wacc.Translator._
import scala.collection.mutable

object RegisterAllocator {
  //also in assembler
  sealed trait Register
  object r0 extends Register
  object r1 extends Register
  object r3 extends Register
  object r2 extends Register
  object r5 extends Register
  object r4 extends Register
  object r7 extends Register
  object r6 extends Register
  object r9 extends Register
  object r8 extends Register
  object r11 extends Register
  object r10 extends Register
  object r13 extends Register
  object r12 extends Register
  object r14 extends Register
  object fp extends Register
  object lr extends Register
  object pc extends Register
  var stackOfRegisters = Stack(r4, r5, r6, r7, r8, r9, r10, r11, r12)
  var registerMap = mutable.Map[Register, TRegister]()

  def allocateRegisters(tacs: List[TAC]): List[Register] = {
    val regs = ListBuffer[Register]()
    tacs.foreach(t => {
      t match {
        case BinaryOpTAC(op, t1, t2, res) => regs += getRegister(res)
        case UnaryOpTAC(op, t1, res) => regs += getRegister(res)
        case AssignmentTAC(t1, res) => regs += getRegister(res)
        case PopParamTAC(t1) => regs += getRegister(t1)
      }
    })
    regs.toList()
  }

  def getRegister(tReg: TRegister): Register = {
    if (stackOfRegisters.isEmpty) {
      //push into stack, map tReg to stack
      //return
    }
    val r = stackOfRegisters.pop()
    registerMap += (r, tReg)
    r
  }

  def releaseRegister(reg: Register) = {
    registerMap -= reg
    stackOfRegisters.push(reg)
  }

  //release tReg from stack
}
