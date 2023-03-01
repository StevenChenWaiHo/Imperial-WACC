package wacc

import wacc.Assembler._
import wacc.TAC._

import scala.collection.mutable.ListBuffer
import wacc.Assembler.Register
import wacc.Assembler.LHSop

object RegisterAllocator {

   object r0 extends Register {
     override def toString(): String = "r0"
   }
  
   object r1 extends Register {
     override def toString(): String = "r1"
   }
  
   object r2 extends Register {
     override def toString(): String = "r2"
   }
  
   object r3 extends Register {
     override def toString(): String = "r3"
   }
  
   object r4 extends Register {
     override def toString(): String = "r4"
   }
  
   object r5 extends Register {
     override def toString(): String = "r5"
   }
  
   object r6 extends Register {
     override def toString(): String = "r6"
   }
  
   object r7 extends Register {
     override def toString(): String = "r7"
   }
  
   object r8 extends Register {
     override def toString(): String = "r8"
   }
  
   object r9 extends Register {
     override def toString(): String = "r9"
   }
  
   object r10 extends Register {
     override def toString(): String = "r10"
   }
  
   object r11 extends Register {
     override def toString(): String = "r11"
   }
  
   object r12 extends Register {
     override def toString(): String = "r12"
   }
  
   object r13 extends Register {
     override def toString(): String = "r13"
   }
  
   object r14 extends Register {
     override def toString(): String = "r14"
   }
  
   object fp extends Register {
     override def toString(): String = "fp"
   }
  
   object lr extends Register {
     override def toString(): String = "lr"
   }
  
   object pc extends Register {
     override def toString(): String = "pc"
   }
  
   object sp extends Register {
     override def toString(): String = "sp"
   }

   val listOfRegisters = Map[Register, Int](r0 -> 0, r1 -> 0, r2 -> 0, r3 -> 0, r4 -> 0, r5 -> 0, r6 -> 0,
     r7 -> 0, r8 -> 0, r9 -> 0, r10 -> 0, r11 -> 0, r12 -> 0, r13 -> 0, r14 -> 0)

  var stackOfRegisters = collection.mutable.Stack(r4, r5, r6, r7, r8, r9, r10, r11, r12)
  var registerMap = collection.mutable.Map[TRegister, Register]() // TReg -> Reg || TReg -> Stack[index]


  def allocateRegisters(tacs: List[TAC]): List[Register] = {
    val regs = collection.mutable.ListBuffer[Register]()
    tacs.foreach(t => {
      t match {
        case BinaryOpTAC(op, t1, t2, res) => regs += getRegister(res)
        case UnaryOpTAC(op, t1, res) => regs += getRegister(res)
        case AssignmentTAC(t1, res) => regs += getRegister(res)
        case PopParamTAC(t1) => regs += getRegister(t1)
        case _ =>
      }
    })
    regs.toList
  }

  def translateRegister(treg: TRegister): LHSop = {
    // TODO: add handling of treg mapped to stack
    registerMap.getOrElse(treg, r0)
  }

  def getRegister(tReg: TRegister): Register = {
    if (stackOfRegisters.isEmpty) {
      //push into stack, map tReg to stack
      //return
    }
    val r = stackOfRegisters.pop()
    registerMap.addOne(tReg, r)
    r
  }

  def releaseRegister(reg: Register) = {
    // TODO: remove register mapping
    stackOfRegisters.push(reg)
  }

  //release tReg from stack


  /* code: the resultant assembly code so far.
     available: available general-purpose registers.
     used: registers currently occupied.
     inMemory: the location of in-scope stored TRegisters (the TRegister number is used internally to identify the value).
     Note: I'm using a ListBuffer instead of a map to maintain the order of elements.
     */
  class AssemblerState(var code: ListBuffer[String], var available: ListBuffer[Register],
                       var used: ListBuffer[(TRegister, Register)], var inMemory: ListBuffer[TRegister]) {

    /* Push the least-recently-used register to the stack, freeing it */
    //TODO: I think only r0-r7 can be pushed ("low registers" only)(?)
    private def saveRegister = {
      code = code.addOne(translatePush("", List(used.head._2)))
      available = available.addOne(used.head._2)
      inMemory = inMemory.addOne(used.head._1)
      used = used.tail
      this
    }

    private def addInstruction(instr: String): AssemblerState = {
      code = code.addOne(instr)
      this
    }

    private def addInstructions(instrs: List[String]): AssemblerState = {
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
    def getRegister(target: TRegister):Register = {
      /* Check currently-loaded registers */
      val inReg = used.find(x => x._1 == target)
      if (inReg.isDefined) return inReg.get._2

      /* Free a register */
      if (available.isEmpty) saveRegister

      /* Check the stack */
      val stackLocation: Int = inMemory.indexOf(target)
      if (stackLocation != (-1)) {
        if (stackLocation == inMemory.length - 1) {
          /* Target is on the top of the stack: pop from the stack */
          addInstruction(translatePop("", List(available.head)))
          inMemory = inMemory.init
          /*/* Correct the stack pointer */
          val length = inMemory.length
          inMemory = inMemory.reverse.dropWhile(_ == null).reverse */
        }
        /* Target is not on the top: load from the stack. TODO: 4 a is magic number (I don't remember if it's right) */
        else addInstruction(translateLdr("", available.head, sp, new ImmediateInt(4 * stackLocation)))
        inMemory = inMemory.updated(stackLocation, null)
      }

      logicallyAllocateRegisterTo(target)
    }

    /* Converts Operands to Operand2s by assigning them registers.
    * Could be delegated to a different class later if we want to use a fancier method. */
    private def toOperand2(op: Operand) = op match {
      case op: TRegister => ImmediateValueOrRegister(Left(getRegister(op)))
      //TODO
      //  It would be nice if ImmediateValueOrRegister could take non-integer constants (since everything in assembly is a number)
      case anything => new ImmediateInt(0)
      case _ => throw new NotImplementedError("this shouldn't happen (i think)")
    }

    //    private def translateBinOp(binOp: BinaryOpTAC): AssemblerState = {
    //      val BinaryOpTAC(op, a1, b1, res1) = binOp
    //      val (res2, a2, b2) =
    //        (getRegister(res1.asInstanceOf(Register)), getRegister(a1.asInstanceOf(Register)), toOperand2(b1))
    //
    //      op match {
    //        case Add => translateAdd(res2, a2, b2)
    //        case _ => null
    //      }
    //
    //      instructionTemplate(args)
    //    }

  }
}
