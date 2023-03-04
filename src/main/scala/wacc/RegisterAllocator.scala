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
                       var memory: ListBuffer[ListBuffer[TRegister]]) {
    //var assembler: Assembler[Register]) extends StateTracker[Register, TRegister] {
    val offset = 1024
    var currentOffset = offset

    def storeRegister = {
      val currentScope = memory.head
      var index = currentScope.indexOf(used.head._1)
      if(index == -1) {
        index = memory.length
        currentScope.addOne(used.head._1)
      }
      code = code.addOne(translateStr("", used.head._2, fp, ImmediateInt(-offset + 4 * index)))
      available.addOne(used.head._2)
      used.remove(0)
      this
    }

    /* When entering and exiting a function, the scope is completely redefined */
    def enterFunction: RegisterAllocator.AssemblerState = {
      code.addOne(translateSub("", AssemblerTypes.None(), sp, sp, ImmediateInt(offset)))
      currentOffset = offset
      memory.addOne(ListBuffer[TRegister]())
      this
    }
    def exitFunction: RegisterAllocator.AssemblerState = {
      code.addOne(translateAdd("", AssemblerTypes.None(), sp, sp, ImmediateInt(offset)))
      currentOffset = 0
      memory.remove(0)
      this
    }

    /* When passing a label, make sure all variables are stored in memory. */
    def enterLabel: RegisterAllocator.AssemblerState = {
      while(used.nonEmpty) storeRegister
      this
    }
    def this(available: ListBuffer[Register]) = //, assembler: Assembler[Register]) =
      this(ListBuffer(), available, ListBuffer(), ListBuffer())

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
      if (available.isEmpty) storeRegister

      /* Check memory */
      val index: Int = memory.head.indexOf(target)
      if (index != (-1)) {
        code = code.addOne(translateLdr("", available.head, fp, new ImmediateInt(-1024 + (index * 4))))
      }

      logicallyAllocateRegisterTo(target)
    }

    override def toString: String = code.toString

    // TODO: inefficient, and should probably be an implicit in Assembler since it doesn't generalize well.
    def ::(prev: AssemblerState): AssemblerState = this
    def ++(next: AssemblerState): AssemblerState = this :: next
  }
}
