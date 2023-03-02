package wacc

import wacc.Assembler.{translateLdr, translatePop, translatePush}
import wacc.AssemblerTypes.{ImmediateInt, Register, sp}
import wacc.TAC._

import scala.collection.AbstractSeq
import scala.collection.mutable.ListBuffer

object RegisterAllocator {

  class AssemblerState(var code: ListBuffer[String],
                       var available: ListBuffer[Register],
                       var used: ListBuffer[(TRegister, Register)],
                       var inMemory: ListBuffer[TRegister]) {
    //var assembler: Assembler[Register]) extends StateTracker[Register, TRegister] {

    def this(available: ListBuffer[Register]) = //, assembler: Assembler[Register]) =
      this(ListBuffer(), available, ListBuffer(), ListBuffer())

    /* Push the least-recently-used register to the stack, freeing it */
    //TODO: I think only r0-r7 can be pushed ("low registers" only)(?)
    private def freeRegister: AssemblerState = {
      this :: (translatePush("", List(used.head._2)))
      available = available.addOne(used.head._2)
      inMemory = inMemory.addOne(used.head._1)
      used = used.tail
      this
    }

    def addInstruction(instr: String): AssemblerState = {
      code = code.addOne(instr)
      this
    }

    def addInstructions(instrs: List[String]): AssemblerState = {
      code = code.addAll(instrs)
      this
    }

    private def logicallyAllocateRegisterTo(target: TRegister): Register = {
      val reg = available.head
      used = used.addOne((target, reg))
      available = available.tail
      reg
    }

    /* Get a guaranteed register. If 'target' already exists in memory or in the registers, returns a register
     containing its value; otherwise returns a random unallocated register. */
    def getRegister(target: TRegister): Register = {
      /* Check currently-loaded registers */
      val inReg = used.find(x => x._1 == target)
      if (inReg.isDefined) return inReg.get._2

      /* Free a register */
      if (available.isEmpty) freeRegister

      /* Check the stack */
      val stackLocation: Int = inMemory.indexOf(target)
      if (stackLocation != (-1)) {
        this :: translateLdr("", available.head, sp, new ImmediateInt(4 * stackLocation))
        inMemory = inMemory.updated(stackLocation, null)
      }

      logicallyAllocateRegisterTo(target)
    }

    override def toString: String = code.toString

    // TODO: inefficient, and should probably be an implicit in Assembler since it doesn't generalize well.
    def ::(prev: AssemblerState): AssemblerState = this
    def ++(next: AssemblerState): AssemblerState = this :: next
  }
}
