package wacc

import wacc.AbstractSyntaxTree.ASTNode

object Assembler {
  val stack = Array[Register]()
  val memory = Array[Int]()
  /*object Registers extends Enumeration {
    sealed case class RegisterNum(name: String)
    val r1 = RegisterNum("r1")
    val r2 = RegisterNum("r2")
    val r3 = RegisterNum("r3")
    val r4 = RegisterNum("r4")
    val r5 = RegisterNum("r5")
    val r6 =RegisterNum("r6")
    val r7 =RegisterNum("r7")
    val r8 = RegisterNum("r8")
    val r9 = RegisterNum("r9")
    val r10 = RegisterNum("r10")
    val r11 = RegisterNum("r11")
    val r12 = RegisterNum("r12")
    val r13 = RegisterNum("r13")
    val r14 = RegisterNum("r14")
    val r15 = RegisterNum("r15")
  }

  object Registers extends Enumeration {
    val RegisterNumber = Value
    val r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15 = Value
  }
  */

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
  val listOfRegisters = Map[Register, Int](r0 -> 0, r1 -> 0, r2 -> 0, r3 -> 0, r4 -> 0, r5 -> 0, r6 -> 0,
    r7 -> 0, r8 -> 0, r9 -> 0, r10 -> 0, r11 -> 0, r12 -> 0, r13 -> 0, r14 -> 0)
  def push(register: Register): Unit = {
    //TODO: implement push
  }
  def pop(register: Register): Int = {
    //TODO: pop implement
    1
  }

  def mov(registerDest: Register, registerSrc: Register): Unit = {
    listOfRegisters.updated(registerDest, listOfRegisters(registerSrc))
  }

  def movImm(registerDest: Register, operand: Int): Unit = {
    listOfRegisters.updated(registerDest, operand)
  }

  def store(registerDest: Register, registerSrc: Register, operand: Int = 0): Unit = {
    val memoryLocation : Int = listOfRegisters(registerSrc) + operand
    listOfRegisters.updated(registerDest, memory(listOfRegisters(registerSrc) + operand))
  }

  def compare(registerDest: Register, registerSrc: Register): Boolean = {
    listOfRegisters(registerDest) == listOfRegisters(registerSrc)
  }

  //TODO: implement other commands


}
