package wacc

import wacc.StatelessAssembler.{translateAdd, translateLdr, translatePush, translateStr, translateSub}
import wacc.AssemblerTypes.{ImmediateInt, Register, fp, sp}
import wacc.TAC._

import scala.collection.AbstractSeq
import scala.collection.mutable.ListBuffer

object RegisterAllocator {

  class AssemblerState(var code: ListBuffer[String],
                       var available: ListBuffer[Register],
                       var used: ListBuffer[(TRegister, Register)],
                       var inMemory: ListBuffer[TRegister]) {
    //var assembler: Assembler[Register]) extends StateTracker[Register, TRegister] {
    val offset = 1024
    var currentOffset = offset

    def enterFunction: RegisterAllocator.AssemblerState = {
      code.addOne(translateSub("", AssemblerTypes.None(), sp, sp, ImmediateInt(offset)))
      currentOffset = offset
      this
    }
    def exitFunction: RegisterAllocator.AssemblerState = {
      code.addOne(translateAdd("", AssemblerTypes.None(), sp, sp, ImmediateInt(offset)))
      currentOffset = 0
      this
    }

    def this(available: ListBuffer[Register]) = //, assembler: Assembler[Register]) =
      this(ListBuffer(), available, ListBuffer(), ListBuffer())

    /* Push the least-recently-used register to the stack, freeing it */
    //TODO: I think only r0-r7 can be pushed ("low registers" only)(?)
    private def safePush: AssemblerState = {
      code = code.addOne(translateStr("", used.head._2, fp, ImmediateInt(-currentOffset)))
      currentOffset -= 4
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
      if (available.isEmpty) safePush

      /* Check the stack */
      val stackLocation: Int = inMemory.indexOf(target)
      if (stackLocation != (-1)) {
        code = code.addOne(translateLdr("", available.head, fp, new ImmediateInt(-currentOffset)))
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
