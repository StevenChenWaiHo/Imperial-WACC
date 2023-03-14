package wacc

import wacc.StatelessAssembler.{assembleAdd, assembleLdr, assemblePush, assembleStr, assembleSub}
import wacc.AssemblerTypes._
import wacc.TAC._
import wacc.FinalIR.FinalIR

import scala.collection.mutable.ListBuffer

object RegisterAllocator {

  class AssemblerState(var code: ListBuffer[FinalIR],
                       var available: ListBuffer[Register],
                       var used: ListBuffer[(TRegister, Register)],
                       var memory: ListBuffer[ListBuffer[TRegister]]) {
    //var assembler: Assembler[Register]) extends StateTracker[Register, TRegister] {

    def this(available: ListBuffer[Register]) = //, assembler: Assembler[Register]) =
      this(ListBuffer(), available, ListBuffer(), ListBuffer(ListBuffer()))

    def addInstruction(instr: FinalIR): AssemblerState = {
      code = code.addOne(instr)
      this
    }

    def addInstructions(instrs: List[FinalIR]): AssemblerState = {
      code = code.addAll(instrs)
      this
    }

    override def toString: String = code.toString

    def ::(prev: AssemblerState): AssemblerState = this
    def ++(next: AssemblerState): AssemblerState = this :: next
  }
}
