package wacc

import wacc.AssemblerTypes._
import wacc.TAC._
import wacc.FinalIR.FinalIR

import scala.collection.mutable.ListBuffer

object RegisterAllocator {

  class AssemblerState(var code: ListBuffer[FinalIR],
                       var available: ListBuffer[GeneralRegister],
                       var used: ListBuffer[(TRegister, GeneralRegister)],
                       var memory: ListBuffer[ListBuffer[TRegister]]) {
    
    val offset = 1024

    def storeRegister = {
      val currentScope = memory.head
      var index = currentScope.indexOf(used.head._1)
      if(index == -1) {
        index = currentScope.length
        currentScope.addOne(used.head._1)
      }
      code = code.addOne(FinalIR.Str("", new FPRegister, new ImmediateInt(-offset + (4 * index)), used.head._2))
      available.addOne(used.head._2)
      used = used.tail
      this
    }

    /** When entering and exiting a function, the scope is completely redefined.
    * allocate some stack space by moving the stack pointer, and add 'memory(0)' to track it. */
    def enterFunction: RegisterAllocator.AssemblerState = {
      code.addOne(FinalIR.Sub("", new AssemblerTypes.None, new SPRegister, new ImmediateInt(offset), new SPRegister))
      memory.addOne(ListBuffer[TRegister]())
      this
    }

    /** Put the stack pointer back, and revert to the previous scope. */
    def deleteFunctionScope: RegisterAllocator.AssemblerState = {
      memory.remove(0)
      this
    }

    def exitFunction: RegisterAllocator.AssemblerState = {
      code.addOne(FinalIR.Add("", new AssemblerTypes.None, new SPRegister, new ImmediateInt(offset), new SPRegister))
      this
    }

    /** When passing a label definition, make sure all variables are stored in memory.
     * A label could be reached from multiple paths, so we can't be sure which tRegister is in which register. */
    def enterBranch: RegisterAllocator.AssemblerState = {
      while(used.nonEmpty) {
        storeRegister
      }
      this
    }
    def this(available: ListBuffer[GeneralRegister]) = //, assembler: Assembler[GeneralRegister]) =
      this(ListBuffer(), available, ListBuffer(), ListBuffer(ListBuffer()))

    def addInstruction(instr: FinalIR): AssemblerState = {
      code = code.addOne(instr)
      this
    }

    def addInstructions(instrs: List[FinalIR]): AssemblerState = {
      code = code.addAll(instrs)
      this
    }

    /** Move a register from 'available' to 'used', declaring that 'target' is stored within it.
     * Note that this does not modify the code: there's no guarantee that 'target' really is stored there. */
    private def logicallyAllocateRegisterTo(target: TRegister): GeneralRegister = {
      val reg = available.head
      used = used.addOne((target, reg))
      available = available.tail
      reg
    }

    /** Get a guaranteed register. If 'target' already exists in memory or in the registers, returns a register
     containing its value; otherwise returns a random unallocated register. */
    def getRegister(target: TRegister): GeneralRegister = {
      /* Check currently-loaded registers */
      val inReg = used.find(x => x._1 == target)
      if (inReg.isDefined) return inReg.get._2

      /* Free a register */
      if (available.isEmpty) storeRegister

      /* Check memory */
      val index: Int = memory.head.indexOf(target)
      if (index != (-1)) {
        code = code.addOne(FinalIR.Ldr("", available.head, new ImmediateInt(-offset + (4 * index)), new FPRegister))
      }

      logicallyAllocateRegisterTo(target)
    }

    override def toString: String = code.toString

    def ::(prev: AssemblerState): AssemblerState = this
    def ++(next: AssemblerState): AssemblerState = this :: next
  }
}
