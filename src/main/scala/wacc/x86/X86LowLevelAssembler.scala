package wacc

import wacc.X86AssemblerTypes._
import wacc.AssemblerTypes._
import wacc.FinalIR._
import wacc.TAC.ReservedPushTAC

import collection.mutable


object X86LowLevelAssembler {
  var count: Int = 0

  def assemble(irCode: List[FinalIR], endFuncs: mutable.Map[String, List[FinalIR]]): String = {
    endFuncsIR = endFuncs
    (irCode.map(assembleIR) ++ endFuncsToList()).mkString("\n")
  }

  def assembleIR(irCode: FinalIR): String = {
    irCode match {
        case Str(cond, src, operand, dst) => assembleStr(cond, src, operand, dst)
        case StrPre(cond, src, operand, dst) => assembleStrPre(cond, src, operand, dst)
        case Ldr(cond, src, operand, dst) => assembleLdr(cond, src, operand, dst)
        case Push(cond, regs) => assemblePush(cond, regs)
        case Pop(cond, regs) => assemblePop(cond, regs)
        case Add(cond, flag, op1, op2, dst) => assembleAdd(cond, flag, op1, op2, dst)
        case Sub(cond, flag, op1, op2, dst) => assembleSub(cond, flag, op1, op2, dst)
        case Rsb(cond, flag, op1, op2, dst) => assembleRsb(cond, flag, op1, op2, dst)
        case Mul(cond, flag, op1, op2, dst) => assembleMul(cond, flag, op1, op2, dst)
        case Smull(cond, flag, dst, dst2, op1, op2) => assembleSmull(cond, flag, dst, dst2, op1, op2)
        case And(cond, dst, value) => assembleAnd(cond, dst, value)
        case Mov(cond, src, dst) => assembleMove(cond, src, dst)
        case Lea(cond, src, dst) => assembleLea(cond, src, dst)
        case Branch(cond, name) => assembleBranch(cond, name)
        case BranchLink(cond, name) => assembleBranchLink(cond, name)
        case Cmp(cond, op1, op2) => assembleCmp(cond, op1, op2)
        case Global(name) => assembleGlobal(name)
        case Lbl(name) => assembleLabel(name)
        case Comment(str) => assembleComment(str)
        case Ret() => assembleRet()
        case DataSeg() => assembleDataSeg()
        case TextSeg() => assembleTextSeg()
        case AsciiZ(str) => assembleAsciiZ(str)
        case Word(len) => assembleWord(len)
        case Special(str) => str
    }
  }

  var endFuncsIR = collection.mutable.Map[String, List[FinalIR]]()

  def addEndFunc(name: String, code: List[FinalIR]): Unit = {
    if (!endFuncsIR.contains(name)) {
      endFuncsIR.addOne(name, code)
    }
  }

  def endFuncsToList(): List[String] = {
    endFuncsIR.toList.map(entry => entry match {
      case (name, code) => code.map(c => assembleIR(c))
    }).flatten
  }

  // Uses mov/lea instead of str/ldr
  def assembleStr(condition: String, src: LHSop, operand: LHSop, dst: Register): String = {
    ldrStrAssist(condition, src, operand, dst)
  }

  def assembleStrPre(condition: String, src: LHSop, operand: LHSop, dst: Register): String = {
    strPreAssist(condition, src, operand, dst)
  }

  def strPreAssist(condition: String, src: LHSop, operand: LHSop, dst: Register): String = {
    "mov" + condition + " [" + dst.toString + "], " + src.toString
  }
  
  def assembleLdr(condition: String, src: Register, operand: LHSop, dst: Register): String = {
    ldrStrAssist(condition, src, operand, dst)
  }

  //split out lea?
  def ldrStrAssist(condition: String, src: LHSop, operand: LHSop, dst: Register): String = {
    var str = "mov" + condition + " " + dst.toString + ", " //src null issue
    operand match {
      case X86ImmediateInt(x) => {
        str = str + "[" + src.toString + " "
        if (x < 0) {
          str = str + "-"
        } else {
          str = str + "+"
        }
        str + " " + (x).abs + "]"
      }
      case X86LabelString(x) => {
        "lea " + dst.toString + ", [rip + " + x + "]" //rip is instruction pointer
      }
      case null => {
        str + "[" + src.toString + "]"
      }
    }
  }

  def assemblePush(condition: String, registers: List[Register]): String = {
    pushPopAssist("push", condition, registers)
  }

  def assemblePop(condition: String, registers: List[Register]): String = {
    pushPopAssist("pop", condition, registers)
  }

  def pushPopAssist(instr: String, condition: String, registers: List[Register]): String = {
    var str = ""
    for (register <- registers) {
      if (register != registers.last) {
        str = str + instr + condition + " " + register.toString + "\n"
      } else {
        str = str + instr + condition + " " + register.toString
      }
    }
    str
  }

  def assembleAdd(condition: String, setflag: Suffi, op1: LHSop, op2: LHSop, dst: LHSop): String = {
    addSubMulAssist("add", condition, setflag, op1, op2, dst)
  }

  def assembleSub(condition: String, setflag: Suffi, op1: LHSop, op2: LHSop, dst: LHSop): String = {
    addSubMulAssist("sub", condition, setflag, op1, op2, dst)
  }

  def assembleRsb(condition: String, setflag: Suffi, op1: LHSop, op2: LHSop, dst: LHSop): String = {
    addSubMulAssist("rsb", condition, setflag, op1, op2, dst)
  }

  def assembleMul(condition: String, setflag: Suffi, op1: LHSop, op2: LHSop, dst: LHSop): String = {
    addSubMulAssist("mul", condition, setflag, op1, op2, dst)
  }

  def assembleSmull(condition: String, setflag: Suffi, dst: LHSop, dst2: LHSop, op1: LHSop, op2: LHSop): String = {
    val dstHigh = if (dst == op1) op2 else op1
    "smull" + fourMulAssist(condition, setflag, dst, dstHigh, op1, op2)
  }

  def fourMulAssist(condition: String, setflag: Suffi, destinationLow: LHSop, destinationHigh: LHSop,
                    sourceRegister: LHSop, operand: LHSop): String = {
    addEndFunc("_errOverflow", new X86HelperFunctions().assemble_errOverflow())
    addEndFunc("_prints", new X86HelperFunctions().assemble_prints())

    condition + setflag + " " + destinationLow + "," + " " + destinationHigh + "," + " " + sourceRegister +
      "," + " " + operand +
      "\ncmp " + destinationHigh + ", " + destinationLow + ", asr #31" +
      "\nbne _errOverflow"
  }


  def addSubMulAssist(instr: String, condition: String, setflag: Suffi, op1: LHSop, op2: LHSop, dst: LHSop): String = {
    addEndFunc("_errOverflow", new X86HelperFunctions().assemble_errOverflow())
    addEndFunc("_prints", new X86HelperFunctions().assemble_prints())
    if (op1 == dst) {
      instr + condition + setflag + " " + dst + ", " + op2 + "\njo _errOverflow"
    }
    "mov " + dst + ", " + op1 + "\n" +
      instr + condition + setflag + " " + dst + ", " + op2 + "\njo _errOverflow"
  }

  def assembleAnd(condition: String, dst: LHSop, value: LHSop): String = {
    "and" + condition + " " + dst.toString + ", " + value.toString
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
    var ins = "j"
    if (condition.isEmpty) {
      "mov" + " " + dst.toString + ", " + src.toString()
    } else if (src.isInstanceOf[Register]) {
      "cmov" + condition + " " + dst.toString + ", " + src.toString()
    } else {
      condition match { //x86 should only use mov or cmov
        // case ImmediateInt(i) if !checkMovCases(i) => "ldr " + condition + " " + dst.toString() + ", =" + i
        // helper needed
        case "ne" => ins = ins + "e"
        case "e" => ins = ins + "ne"
        case "nz" => ins = ins + "z"
        case "z" => ins = ins + "nz"
        case "nc" => ins = ins + "c"
        case "c" => ins = ins + "nc"
        case "ng" => ins = ins + "g"
        case "g" => ins = ins + "ng"
        case "nl" => ins = ins + "l"
        case "l" => ins = ins + "nl"
        case "le" => ins = ins + "g"
        case "ge" => ins = ins + "l"
        case "" => ins = ins + "mp"
        case _ => "cmov" + condition + " " + dst.toString + ", " + src.toString()
      }
      count += 1
      ins + " skip" + count + "\n" +
        "mov" + " " + dst.toString + ", " + src.toString() + "\n" + //src must be register
        "skip" + count + ":"
    }
  }

  def assembleLea(condition: String, src: LHSop, dst: LHSop) = {
    "lea" + condition + " " + dst.toString + ", [" + src.toString + "]"
  }

  def assembleBranch(condition: String, name: String): String = {
    "j" + condition + " " + name // jmp treated as condition of "j"
  }

  def assembleBranchLink(condition: String, name: LHSop): String = {
    "call" + condition + " " + name
  }

  def assembleGlobal(name: String) = {
    ".intel_syntax noprefix\n.globl " + name
  }

  def assembleLabel(name: String): String = {
      name + ":"
  }

  def assembleComment(comment: String): String = {
    "# " + comment
  }

  def assembleRet(): String = {
    "ret"
  }

  def assembleDataSeg(): String = {
    ".section .rodata"
  }

  def assembleTextSeg(): String = {
    ".text"
  }

  def assembleAsciiZ(str: String): String = {
    ".asciz " + str
  }

  def assembleWord(len: Int): String = {
    ".word " + len.toString()
  }

}
