package wacc

import wacc.TAC._

import wacc.Assembler.Register
import wacc.Assembler._

object RegisterAllocator {
  //also in assembler

//  class Register {
//    def toEither(): ImmediateValueOrRegister = {
//      new ImmediateValueOrRegister(Left(this))
//    }
//  }
//
//  object r0 extends Register {
//    override def toString(): String = "r0"
//  }
//
//  object r1 extends Register {
//    override def toString(): String = "r1"
//  }
//
//  object r2 extends Register {
//    override def toString(): String = "r2"
//  }
//
//  object r3 extends Register {
//    override def toString(): String = "r3"
//  }
//
//  object r4 extends Register {
//    override def toString(): String = "r4"
//  }
//
//  object r5 extends Register {
//    override def toString(): String = "r5"
//  }
//
//  object r6 extends Register {
//    override def toString(): String = "r6"
//  }
//
//  object r7 extends Register {
//    override def toString(): String = "r7"
//  }
//
//  object r8 extends Register {
//    override def toString(): String = "r8"
//  }
//
//  object r9 extends Register {
//    override def toString(): String = "r9"
//  }
//
//  object r10 extends Register {
//    override def toString(): String = "r10"
//  }
//
//  object r11 extends Register {
//    override def toString(): String = "r11"
//  }
//
//  object r12 extends Register {
//    override def toString(): String = "r12"
//  }
//
//  object r13 extends Register {
//    override def toString(): String = "r13"
//  }
//
//  object r14 extends Register {
//    override def toString(): String = "r14"
//  }
//
//  object fp extends Register {
//    override def toString(): String = "fp"
//  }
//
//  object lr extends Register {
//    override def toString(): String = "lr"
//  }
//
//  object pc extends Register {
//    override def toString(): String = "pc"
//  }
//
//  object sp extends Register {
//    override def toString(): String = "sp"
//  }

//  val listOfRegisters = Map[Register, Int](r0 -> 0, r1 -> 0, r2 -> 0, r3 -> 0, r4 -> 0, r5 -> 0, r6 -> 0,
//    r7 -> 0, r8 -> 0, r9 -> 0, r10 -> 0, r11 -> 0, r12 -> 0, r13 -> 0, r14 -> 0)

  var stackOfRegisters = collection.mutable.Stack(r4, r5, r6, r7, r8, r9, r10, r11, r12)
  var registerMap = collection.mutable.Map[Register, TRegister]()



  def allocateRegisters(tacs: List[TAC]): List[Register] = {
    val regs = collection.mutable.ListBuffer[Register]()
    tacs.foreach(t => {
      t match {
        case BinaryOpTAC(op, t1, t2, res) => regs += getRegister(res)
        case UnaryOpTAC(op, t1, res) => regs += getRegister(res)
        case AssignmentTAC(t1, res) => regs += getRegister(res)
        case PopParamTAC(t1) => regs += getRegister(t1)
      }
    })
    regs.toList
  }

  def getRegister(tReg: TRegister): Register = {
    if (stackOfRegisters.isEmpty) {
      //push into stack, map tReg to stack
      //return
    }
    val r = stackOfRegisters.pop()
    registerMap.addOne(r, tReg)
    r
  }

  def releaseRegister(reg: Register) = {
    registerMap -= reg
    stackOfRegisters.push(reg)
  }

  //release tReg from stack
}
