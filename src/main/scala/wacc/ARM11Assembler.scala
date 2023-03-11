package wacc

import wacc.AssemblerTypes._
import wacc.FinalIR._

class ARM11Assembler {

  def assemble(irCode: List[FinalIR]): String = {
    irCode.map(assembleIR).mkString("\n")
  }

  def assembleIR(irCode: FinalIR): String = {
    irCode match {
        case Str(cond, src, operand, dst) => assembleStr(cond, src, operand, dst)
        case StrPre(cond, src, operand, dst) => assembleStr(cond, src, operand, dst) // TODO: change to correct assemble func
        case Ldr(cond, src, operand, dst) => assembleLdr(cond, src, operand, dst)
        case Push(cond, regs) => assemblePush(cond, regs)
        case Pop(cond, regs) => assemblePop(cond, regs)
        case Add(cond, flag, op1, op2, dst) => assembleAdd(cond, flag, op1, op2, dst)
        case Sub(cond, flag, op1, op2, dst) => assembleSub(cond, flag, op1, op2, dst)
        case Rsb(cond, flag, op1, op2, dst) => assembleRsb(cond, flag, op1, op2, dst)
        case Mul(cond, flag, op1, op2, dst) => assembleMul(cond, flag, op1, op2, dst)
        case Smull(cond, flag, op1, op2, dst, _) => assembleMul(cond, flag, op1, op2, dst) // TODO: change tp actuallt assemble smull
        case Mov(cond, src, dst) => assembleMove(cond, src, dst)
        case Branch(cond, name) => assembleBranch(cond, name)
        case BranchLink(cond, name) => assembleBranchLink(cond, name)
        case Cmp(cond, op1, op2) => assembleCmp(cond, op1, op2)
        case Global(name) => assembleGlobal(name)
        case Lbl(name) => assembleLabel(name)
    }
  }

  val endFuncsIR = collection.mutable.Map[String, List[FinalIR]]()

  def addEndFunc(name: String, code: List[FinalIR]): Unit = {
    if (!endFuncsIR.contains(name)) {
      endFuncsIR.addOne(name, code)
    }
  }

  def endFuncsToList(): String = {
    endFuncsIR.toList.map(entry => entry match {
      case (name, code) => assemble(code)
    }).mkString("\n") // TODO: check if newline is correct here
  }

  def assembleStr(condition: String, src: LHSop, operand: LHSop, dst: Register): String = {
    "str" + ldrStrAssist(condition, src, operand, dst)
  }
  
  def assembleLdr(condition: String, src: Register, operand: LHSop, dst: Register): String = {
    "ldr" + ldrStrAssist(condition, src, operand, dst)
  }

  def ldrStrAssist(condition: String, src: LHSop, operand: LHSop, dst: Register): String = {
    var str = condition + " " + dst.toString + ", "
    operand match {
      case ImmediateInt(x) => {
        str = str + "[" + src.toString + ", #" + x + "]"
      }
      case LabelString(x) => {
        str = str + "=" + x
      }
    }
    str
  }

  def assemblePush(condition: String, registers: List[Register]): String = {
    "push" + pushPopAssist(condition, registers)
  }

  def assemblePop(condition: String, registers: List[Register]): String = {
    "pop" + pushPopAssist(condition, registers)
  }

  def pushPopAssist(condition: String, registers: List[Register]): String = {
    var str = condition + " {"
    for (register <- registers) {
      if (register != registers.last) {
        str = str + register.toString + ", "
      } else {
        str = str + register.toString
      }
    }
    str + "}"
  }

  def assembleAdd(condition: String, setflag: Suffi, op1: LHSop, op2: LHSop, dst: LHSop): String = {
    "add" + addSubMulAssist(condition, setflag, op1, op2, dst)
  }

  def assembleSub(condition: String, setflag: Suffi, op1: LHSop, op2: LHSop, dst: LHSop): String = {
    "sub" + addSubMulAssist(condition, setflag, op1, op2, dst)
  }

  def assembleRsb(condition: String, setflag: Suffi, op1: LHSop, op2: LHSop, dst: LHSop): String = {
    "rsb" + addSubMulAssist(condition, setflag, op1, op2, dst)
  }

  def assembleMul(condition: String, setflag: Suffi, op1: LHSop, op2: LHSop, dst: LHSop): String = {
    "mul" + addSubMulAssist(condition, setflag, op1, op2, dst)
  }

  def addSubMulAssist(condition: String, setflag: Suffi, op1: LHSop, op2: LHSop, dst: LHSop): String = {
    addEndFunc("_errOverflow", new HelperFunctions().assemble_errOverflow())
    addEndFunc("_prints", new HelperFunctions().assemble_prints())
    condition + setflag + " " + dst + ", " + op1 + ", " + op2 + "\nblvs _errOverflow"
  }

  def assembleCmp(condition: String, op1: LHSop, op2: LHSop): String = {
    "cmp" + condition + " " + op1.toString + ", " + op2.toString
  }

  // Determine when a mov is allowed
  def checkMovCases(i: Int): Boolean = {
    for (j <- 0 to 12) {
      val mask = (0xFF << (j * 2))
      if ((i & ~mask) == 0) return true
    }
    if ((i & ~0xFC000003) == 0) return true
    if ((i & ~0xF000000F) == 0) return true
    if ((i & ~0xC000003F) == 0) return true
    false
  }

  def assembleMove(condition: String, src: LHSop, dst: Register): String = {
    src match {
      case ImmediateInt(i) if !checkMovCases(i) => "ldr " + condition + " " + dst.toString() + ", =" + i
      case _ => "mov" + condition + " " + dst.toString + ", " + src.toString()
    }
  }

  def assembleBranch(condition: String, name: String): String = {
    "b" + condition + " " + name
  }

  def assembleBranchLink(condition: String, name: LHSop): String = {
    "bl" + condition + " " + name
  }

  def assembleGlobal(name: String) = {
    ".global " + name
  }

  def assembleLabel(name: String): String = {
      name + ":"
  }

  def assembleComment(comment: String) = {
    "@ " + comment
  }

}
