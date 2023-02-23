package wacc

import sun.jvm.hotspot.asm.Operand
import wacc.AbstractSyntaxTree.{ASTNode, BeginEndStat, Command, Func, Program, SkipStat, Stat}

import javax.print.attribute.standard.Destination

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
  object fp extends Register
  object lr extends Register
  object pc extends Register
  val listOfRegisters = Map[Register, Int](r0 -> 0, r1 -> 0, r2 -> 0, r3 -> 0, r4 -> 0, r5 -> 0, r6 -> 0,
    r7 -> 0, r8 -> 0, r9 -> 0, r10 -> 0, r11 -> 0, r12 -> 0, r13 -> 0, r14 -> 0)

  def value() : Unit = {
    //TODO: implement register value func
  }

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
  def delegateASTNode(node: ASTNode, context : ScopeContext) : List[String] = {
    node match {
      case Program(funcs, stat) => translateProgram(funcs, stat, context)
      case BeginEndStat(stat) => translateBeginEnd(stat, context)
      case SkipStat() => translateSkip()
      case Command(cmd, expr) => translateCommand(cmd, expr)
      case Func(returnType, ident, types, code) => translateFunction(returnType, ident, types, code)
      case _ => List("")
    }
  }

  def translateProgram(l: List[Func], s: Stat, context: ScopeContext): List[String] = {
    var str = List("")
    for (function: Func <- l) {
      str = str ++ delegateASTNode(function, context) // Not actually sure about the structure of this thing
    }
    str = str ++ delegateASTNode(s, context)
    return str
  }

  def translateBeginEnd(stat : Stat, context: ScopeContext) : List[String] = {
    //Seems like it takes as many variables as it can find in every scope and pushes the corresponding
    //number of registers, instead of just this scope.
    var str: List[String] = List("")
    var defaultRegistersList: List[Register] = List()
    if (context.scopeLevel() == 0) {
      defaultRegistersList = List(r8, r10, r12)
    } else {
      defaultRegistersList = List(r0)
    }
    val registersList: List[Register] = List(r6, r4, r7, r5, r1, r2)
    if (context.scopeVarSize() >= 4) {
      defaultRegistersList = defaultRegistersList ++ registersList
    } else {
      defaultRegistersList = defaultRegistersList ++ registersList.slice(0, context.scopeVarSize())
    }
    str = str ++ translatePush(List(fp, lr)) //Maybe not meant to be in BeginEnd
    str = str ++ translatePush(defaultRegistersList) //dependent on context
    str = str ++ delegateASTNode(stat, context)
    str = str ++ translatePop(defaultRegistersList) // dependent on context
    str = str ++ translatePop(List(fp, pc)) //Maybe meant to be in prog
    return str
  }

  def translateSkip() : List[String] = {
    var str = List("")
    return str
  }

  def translateCommand(cmd : AbstractSyntaxTree.CmdT.Cmd, expr : AbstractSyntaxTree.Expr) : List[String] = {
    List("")
  }

  def translateFunction(returnType : AbstractSyntaxTree.DeclarationType,
                        ident : AbstractSyntaxTree.IdentLiteral,
                        types : List[(AbstractSyntaxTree.DeclarationType,
                          AbstractSyntaxTree.IdentLiteral)],
                        code : Stat) : List[String] = {
    List("")
  }
  def translateARM(command: String, operand: String, operand2: String = "") : String = {
    //Maybe add check to make sure command is valid
    if (operand2 == "") {
      command + " " + operand
    }
    command + " " + operand + ", " + operand2
  }

  def pushPopAssist(registers: List[Register]): String = {
    var str = "{"
    for (register <- registers) {
      if (register != registers.last) {
        str = str + register.toString + ", "
      } else {
        str = str + register.toString
      }
    }
    str = str + "}"
    return str
  }

  def translatePush(registers: List[Register]): String = {
    return "push " + pushPopAssist(registers)
  }

  def translatePop(registers: List[Register]): String = {
    return "pop " + pushPopAssist(registers)
  }

  def ldrStrAssist(destinationRegister: Register, sourceRegister: Register, operand: Either[Register, Int] = Right(0)) : String = {
    var str = destinationRegister.toString
    operand match {
      case Left(x) => {str = str + ", [" + sourceRegister + ", " + x.toString + "]"}
      case Right(0) => {str = str + ", " + sourceRegister}
      case Right(x) => {str = str + ", [" + sourceRegister + ", " + "#" + x.toString + "]"}
    }
    return str
  }

  def translateLdr(destinationRegister: Register, sourceRegister: Register, operand: Either[Register, Int] = Right(0)): String = {
    //Incomplete
    return "ldr " + ldrStrAssist(destinationRegister, sourceRegister, operand)
  }

  def translateStr(destinationRegister: Register, sourceRegister: Register, operand: Either[Register, Int] = Right(0)): String = {
    //Incomplete
    return "str " + ldrStrAssist(destinationRegister, sourceRegister, operand)
  }

  def addSubMulAssist(destinationRegister: Register, sourceRegister: Register, operand: Either[Register, Int]): Unit = {
    var str = destinationRegister.toString
    operand match {
      case Left(x) => {str = str + ", " + sourceRegister + ", " + x.toString}
      case Right(x) => {str = str + ", " + sourceRegister + ", " + "#" + x.toString}
    }
    return str
  }

  //Incomplete, no condition
  def translateAdd(destinationRegister: Register, sourceRegister: Register, operand: Either[Register, Int]): String = {
    return "add" + addSubMulAssist(destinationRegister, sourceRegister, operand)
  }

  def translateSub(destinationRegister: Register, sourceRegister: Register, operand: Either[Register, Int]): String = {
    return "sub" + addSubMulAssist(destinationRegister, sourceRegister, operand)
  }

  def translateMul(destinationRegister: Register, sourceRegister: Register, sourceRegisterTwo: Register): String = {
    return "mul" + addSubMulAssist(destinationRegister, sourceRegister, Left(sourceRegisterTwo))
  }

  def translateCompare(register1: Register, operand: Either[Register, Int]): String = {
    var str = "cmp " + register1.toString
    operand match {
      case Left(x) => {return str + ", " + x.toString}
      case Right(x) => {return str + ", " + "#" + x.toString}
    }
  }

  def translateMove(destinationRegister: Register, operand: Either[Register, Int]) : String = {
    var str = "cmp " + destinationRegister
    operand match {
      case Left(x) => {str = str + ", " + x.toString}
      case Right(x) => {str = str + ", " + "#" + x.toString}
    }
    return str
  }

  def translateBranch(operand: String): String = {
    return "b " + operand
  }

  def translateBranchLink(operand: String): String = {
    return "bl " + operand
  }
  //TODO: implement other commands

}
