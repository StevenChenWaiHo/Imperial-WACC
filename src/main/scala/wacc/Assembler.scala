package wacc

import wacc.AbstractSyntaxTree._
import wacc.AssemblerTypes._
import wacc.RegisterAllocator._
import wacc.TAC._

import scala.collection.mutable.ListBuffer

object StatelessAssembler {
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

  def translatePush(condition: String, registers: List[Register]): String = {
    "push" + pushPopAssist(condition, registers)
  }

  def translateLdr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): String = {
    //Incomplete
    "ldr" + ldrStrAssist(condition, destinationRegister, sourceRegister, operand)
  }

  def ldrStrAssist(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): String = {
    var str = condition + " " + destinationRegister.toString + ", "
    operand match {
      case ImmediateInt(x) => {
        str = str + "[" + sourceRegister.toString + ", #" + x + "]"
      }
      case LabelString(x) => {
        str = str + "=" + x
      }
    }
    str
  }
}

class Assembler {
  private[this] val state = new AssemblerState(ListBuffer(r4, r5, r6, r7, r8, r9, r10, r11))
  val endFuncs = collection.mutable.Map[String, List[String]]()
  var labelCount = 0



  def translateRegister(t: TRegister) = state.getRegister(t)

  /** The state needs to be able to add instructions to the code at the points when translateRegister is called.
   * It therefore needs to be kept up-to-date. These implicit methods ensure that it can be updated without modifying
   * too much of the code.
   * This isn't the nicest, but hopefully it shouldn't break things. */
  implicit private[this] def updateState(str: String): AssemblerState = {
    state.addInstruction(str)
  }

  implicit private[this] def updateState(strs: List[String]): AssemblerState = {
    state.addInstructions(strs)
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
    str = str + "}"
    return str
  }

  def translatePush(condition: String, registers: List[Register]): AssemblerState = {
    return "push" + pushPopAssist(condition, registers)
  }

  def translatePop(condition: String, registers: List[Register]): AssemblerState = {
    return "pop" + pushPopAssist(condition, registers)
  }

  def ldrStrAssist(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): String = {
    var str = condition + " " + destinationRegister.toString + ", "
    operand match {
      case ImmediateInt(x) => {
        str = str + "[" + sourceRegister.toString + ", #" + x + "]"
      }
      case LabelString(x) => {
        str = str + "=" + x
      }
      case default => {
        str = str + "[" + sourceRegister.toString + ", " +operand.toString + "]"
      }
    }
    return str
  }

  def translateLdr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): AssemblerState = {
    return "ldr" + ldrStrAssist(condition, destinationRegister, sourceRegister, operand)
  }

  def translateStr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): AssemblerState = {
    return "str" + ldrStrAssist(condition, destinationRegister, sourceRegister, operand)
  }

  def translateStrPre(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): AssemblerState = {
    "str" + ldrStrAssist(condition, destinationRegister, sourceRegister, operand).toString + "!".toString()
  }

  def generateLabel(): Label = {
    labelCount += 1
    new Label(".La" + labelCount.toString())
  }

  def addEndFunc(name: String, code: List[String]): Unit = {
    if (!endFuncs.contains(name)) {
      endFuncs.addOne(name, code)
    }
  }

  def endFuncsToList(): List[String] = {
    endFuncs.toList.map(entry => entry match {
      case (name, code) => code
    }).flatten
  }


  def addSubMulAssist(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): String = {
    return condition + setflag + " " + destinationRegister + ", " + sourceRegister + ", " + operand
  }

  //Incomplete, no condition
  def translateAdd(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): AssemblerState = {
    "add" + addSubMulAssist(condition, setflag, destinationRegister, sourceRegister, operand)
  }

  def translateSub(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): AssemblerState = {
    return "sub" + addSubMulAssist(condition, setflag, destinationRegister, sourceRegister, operand)
  }

  def translateRsb(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): AssemblerState = {
    return "rsb" + addSubMulAssist(condition, setflag, destinationRegister, sourceRegister, operand)
  }

  def translateMul(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, sourceRegisterTwo: LHSop): AssemblerState = {
    return "mul" + addSubMulAssist(condition, setflag, destinationRegister, sourceRegister, sourceRegisterTwo)
  }

  def fourMulAssist(condition: String, setflag: Suffi, destinationLow: Register, destinationHigh: Register,
                    sourceRegister: Register, operand: Register): String = {
    var str = condition + setflag + " " + destinationLow + "," + " " + destinationHigh + "," + " " + sourceRegister +
      "," + " " + operand
    return str
  }

  def translateMla(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: Register, operand1: Register, operand2: Register): AssemblerState = {
    return "mla" + fourMulAssist(condition, setflag, destinationRegister, sourceRegister, operand1, operand2)
  }

  def translateUmull(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: Register, operand1: Register, operand2: Register): AssemblerState = {
    return "umull" + fourMulAssist(condition, setflag, destinationRegister, sourceRegister, operand1, operand2)
  }

  def translateUmlal(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: Register, operand1: Register, operand2: Register): AssemblerState = {
    return "umlal" + fourMulAssist(condition, setflag, destinationRegister, sourceRegister, operand1, operand2)
  }

  def translateSmull(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: Register, operand1: Register, operand2: Register): AssemblerState = {
    return "smull" + fourMulAssist(condition, setflag, destinationRegister, sourceRegister, operand1, operand2)
  }

  def translateSmlal(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: Register, operand1: Register, operand2: Register): AssemblerState = {
    return "smlal" + fourMulAssist(condition, setflag, destinationRegister, sourceRegister, operand1, operand2)
  }

  def translateCompare(condition: String, register1: LHSop, operand: LHSop): AssemblerState = {
    return "cmp" + condition + " " + register1.toString + ", " + operand.toString
  }

  def translateCompareNeg(condition: String, register1: Register, operand: LHSop): AssemblerState = {
    return "cmn" + condition + " " + register1.toString + ", " + operand.toString
  }

  def checkMovCases(i: Int): Boolean = {
    for (j <- 0 to 12) { // magicNums
      val mask = (0xFF << (j * 2))
      if ((i & ~mask) == 0) return true
    }
    if ((i & ~0xFC000003) == 0) return true
    if ((i & ~0xF000000F) == 0) return true
    if ((i & ~0xC000003F) == 0) return true
    false
  }

  def translateMove(condition: String, dst: Register, operand: LHSop): AssemblerState = {
    operand match {
      case ImmediateInt(i) if !checkMovCases(i) => "ldr " + condition + " " + dst.toString() + ", =" + i
      case _ => "mov" + condition + " " + dst.toString + ", " + operand.toString()
    }
  }

  def translateBranch(condition: String, operand: String): AssemblerState = {
    return "b" + condition + " " + operand
  }

  def translateBranchLink(condition: String, operand: LHSop): AssemblerState = {
    return "bl" + condition + " " + operand
  }

  //TODO: implement other commands
  val OperandToLiteral = Map[TAC.Operand, Either[String, Either[Register, Int]]]()

  def translateOperand2(operand: TAC.Operand): Either[String, Either[Register, Int]] = {
    if (!(!OperandToLiteral.contains(operand))) {
      return OperandToLiteral(operand)
    } else {
      operand match {
        case TRegister(num) => {
          OperandToLiteral.updated(operand, Right(Left(r0)))
          return Right(Left(r0))
        }
        case lit: LiteralTAC => lit match {
          case CharLiteralTAC(c) => {
            OperandToLiteral.updated(operand, Left(c.toString))
            return Left(c.toString)
          }
          //case IntLiteralTAC(int) => {
          //OperandToLiteral.updated(operand, Right(Right(int)))
          //}
          case IdentLiteralTAC(ident) => {
            OperandToLiteral.updated(operand, Left(ident))
            return Left(ident)
          }
          case BoolLiteralTAC(bool) => {
            OperandToLiteral.updated(operand, Left("Not complete"))
            return Left("Not complete")
          }
        }
        case ArrayElemTAC(ar, in) => {
          OperandToLiteral.updated(operand, Left("Not complete"))
          Left("Not complete")
        }
      }
    }
  }

  def determineLdr(x: Int): Boolean = {
    if (x <= 255) {
      return false
    } else {
      for (i <- 1 to 16) {
        if ((x >> i) << i == x) {
          return false
        }
      }
      return true
    }
  }
  
  def translateOperand(op: Operand): LHSop = {
    op match {
      case reg: TRegister => translateRegister(reg)
      case IntLiteralTAC(value) => new ImmediateInt(value)
      case CharLiteralTAC(chr) => new ImmediateInt(chr.toInt)
      case BoolLiteralTAC(b) => new ImmediateInt(b.compare(true) + 1)
      case Label(name) => new LabelString(name)
      case PairLiteralTAC() => new ImmediateInt(0)
      case a => println("translateOperand fail: " + a); null // TODO: this should not match
    }
  }

  def translateTAC(tripleAddressCode: TAC): AssemblerState = {
    //Need to figure out how registers work
    //Push and pop might not be in right place
    //Algorithm for determining if ldr is needed
    tripleAddressCode match {
      case Label(name) => assembleLabel(name)
      case Comments(str) => List("@ " + str)
      case DataSegmentTAC() => List("\n.data")
      case TextSegmentTAC() => List(".text")
      case StringLengthDefinitionTAC(len, lbl) => assembleStringLengthDef(len, lbl)
      case StringDefinitionTAC(str, lbl) => assembleStringDef(str, lbl)
      case BeginFuncTAC() => assembleBeginFunc()
      case EndFuncTAC() => assembleEndFunc()
      case AssignmentTAC(operand, reg) => assembleAssignment(operand, reg)
      case CommandTAC(cmd, operand, opType) => assembleCommand(cmd, operand, opType)
      case BinaryOpTAC(operation, op1, op2, res) => assembleBinOp(operation, op1, op2, res)
      case IfTAC(t1, goto) => assembleIf(t1, goto)
      case GOTO(label) => assembleGOTO(label)
      case CreatePairElem(pairElemType, pairPos, srcReg) => assemblePairElem(pairElemType, pairPos, srcReg)
      case CreatePair(fstType, sndType, fstReg, sndReg, dstReg) => assemblePair(fstType, sndType, dstReg)
      case UnaryOpTAC(op, t1, res) => assembleUnaryOp(op, t1, res)
      case GetPairElem(datatype, pairReg, pairPos, dstReg) => assembleGetPairElem(datatype, pairReg, pairPos, dstReg)
      case StorePairElem(datatype, pairReg, pairPos, srcReg) => assembleStorePairElem(datatype, pairReg, pairPos, srcReg)
      case CallTAC(lbl, args, dstReg) => assembleCall(lbl, args, dstReg)
      case PopParamTAC(datatype, t1, index) => List()
      case PushParamTAC(op) => List()
      case ReadTAC(dataType, readReg) => assembleRead(dataType, readReg)
    }
  }

  def getInstructionType(decType: DeclarationType): String = {
    decType match {
      case BaseType(BaseT.Char_T) => "b"
      case _ => ""
    }
  }

  def getTypeSize(decType: DeclarationType): Int = {
    decType match {
      case BaseType(BaseT.Int_T) => 4
      case BaseType(BaseT.Char_T) => 1
      case _ => 4
    }
  }

  val POINTER_BYTE_SIZE = 4

  def assemblePair(fstType: DeclarationType, sndType: DeclarationType, dstReg: TRegister): AssemblerState = {
    // r12: pointer to pair r8: pointer to pairElem
    translatePush("", List(r8, r12)) ::
    translateMove("", r0, new ImmediateInt(2 * POINTER_BYTE_SIZE)) ::
      translateBranchLink("", new BranchString("malloc")) ::
      translateMove("", r12, r0) ::
      translatePop("", List(r8)) ::
      translateStr("", r8, r12, new ImmediateInt(POINTER_BYTE_SIZE)) ::
      translatePop("", List(r8)) ::
      translateStr("", r8, r12, new ImmediateInt(0)) ::
      translateMove("", translateRegister(dstReg), r12) ::
      translatePop("", List(r8, r12))
  }

  def assemblePairElem(pairElemType: DeclarationType, pairPos: PairElemT.Elem, srcReg: TRegister): AssemblerState = {
    translatePush("", List(r8, r12)) ::
      translateMove("", r0, new ImmediateInt(getTypeSize(pairElemType))) ::
      translateBranchLink("", new BranchString("malloc")) ::
      translateMove("", r8, translateRegister(srcReg)) ::
      translateMove("", r12, r0) ::
      translateStr("", r8, r12, new ImmediateInt(if (pairPos == PairElemT.Fst) 0 else 4)) ::
      translatePush("", List(r12)) ::
      translatePop("", List(r8, r12))
  }

  def assembleUnaryOp(op: UnaryOpType.UnOp, t1: Operand, res: TRegister): AssemblerState = {
    op match {
      case UnaryOpType.Neg => {
        translateRsb("", Status(), translateRegister(res), translateOperand(t1), new ImmediateInt(0))
      }
      case UnaryOpType.Not => {
        translateCompare("", translateOperand(t1), new ImmediateInt(1)) ::
          translateMove("ne", translateRegister(res), new ImmediateInt(1)) ::
          translateMove("eq", translateRegister(res), new ImmediateInt(0))
      }
      case UnaryOpType.Chr | UnaryOpType.Ord => {
        translateMove("", translateRegister(res), translateOperand(t1))
      }
      case UnaryOpType.Len => {
        // ldr r8, [r4, #-4]
        translateLdr("", translateRegister(res), r0, translateOperand(t1))
      }
    }
  }

  def assembleRead(datatype: DeclarationType, readReg: TRegister): AssemblerState = {
    val bl = datatype match {
      case BaseType(baseType) => {
        baseType match {
          case BaseT.Int_T => "_readi"
          case BaseT.Char_T => "_readc"
          case BaseT.String_T => "_reads"
          case BaseT.Bool_T => "_readb"
          case _ => "_readi"
        }
      }
      case _ => "_readi"
    }
    addEndFunc(bl, new HardcodeFunctions().translate_read(bl))
    translateBranchLink("", new BranchString(bl))
    translateMove("", translateRegister(readReg), r0)
  }

  def assembleCall(lbl: Label, args: List[TRegister], dstReg: TRegister): AssemblerState = {
    // save arg registers on the stack
    var output = translatePush("", List(r0, r1, r2, r3))
    // move all the args in to arg registers
    val regs = List(r0, r1, r2, r3)
    args.slice(0, 4).zip(regs).foreach(elem => {
      elem match {
        case (arg, reg) => output = output ++ translateMove("", reg, translateRegister(arg))
      }
    })
    output = output ++ (translateBranchLink("", new BranchString(lbl.name)))
    // move the result into dst before r0 is popped back
    output = output ++ (translateMove("", translateRegister(dstReg), r0))
    // get previous registers from stack
    output ++ (translatePop("", regs))
  }

  def assembleGetPairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, dstReg: TRegister): AssemblerState = {
    translateLdr(getInstructionType(datatype), translateRegister(dstReg), translateRegister(pairReg), new ImmediateInt(if (pairPos == PairElemT.Fst) 0 else 4))
  }

  def assembleStorePairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, srcReg: TRegister): AssemblerState = {
    translateStr(getInstructionType(datatype), translateRegister(srcReg), translateRegister(pairReg), new ImmediateInt(if (pairPos == PairElemT.Fst) 0 else 4))
  }

  def assembleProgram(tacList: List[TAC]): String = {
    tacList.map(translateTAC)
    //TODO: It's possible some lines shouldn't have a new line after them. It's better if the translateX functions
    // Added a new line at the end of their return value instead.
    state.code.addAll(endFuncsToList())
    state.code.mkString("\n")
  }

  def assembleLabel(name: String): AssemblerState = {
    if (name == "main") {
      List(".global main", name + ":")
    } else {
      List(name + ":")
    }
  }

  def assembleGOTO(label: Label): AssemblerState = {
    translateBranch("", label.name)
  }

  def assembleIf(t1: Operand, goto: Label): AssemblerState = {
    translateCompare("", translateOperand(t1), new ImmediateInt(1)) ::
      translateBranch("eq", goto.name)
  }

  def assembleBinOp(operation: BinaryOpType.BinOp, op1: Operand, op2: Operand, res: TRegister): AssemblerState = {
    operation match {
      case BinaryOpType.Add => {
        translateAdd("", Status(), translateRegister(res), translateOperand(op1), translateOperand(op2))
      }
      case BinaryOpType.Sub => {
        translateSub("", Status(), translateRegister(res), translateOperand(op1), translateOperand(op2))
      }
      case BinaryOpType.Mul => {
        translateMul("", Status(), translateRegister(res), translateOperand(op1), translateOperand(op2))
      }
      case BinaryOpType.Div => {
        addEndFunc("_errDivZero", new HardcodeFunctions().translate_errDivZero())
        translateMove("", r4, translateOperand(op1)) ::
          translateMove("", r1, translateOperand(op2)) ::
          translateCompare("", r1, new ImmediateInt(0)) ::
          translateBranchLink("eq", new BranchString("_errDivZero")) ::
          translateBranchLink("", new BranchString("__aeabi_idivmod"))
      }
      case BinaryOpType.Eq => {
        translateMove("", translateRegister(res), new ImmediateInt(0)) ::
          translateCompare("", translateOperand(op1), translateOperand(op2)) ::
          translateMove("eq", translateRegister(res), new ImmediateInt(1))
      }
      case BinaryOpType.Neq => {
        translateMove("", translateRegister(res), new ImmediateInt(0)) ::
          translateCompare("", translateOperand(op1), translateOperand(op2)) ::
          translateMove("ne", translateRegister(res), new ImmediateInt(1))
      }
      case BinaryOpType.Lt => {
        translateMove("", translateRegister(res), new ImmediateInt(0)) ::
          translateCompare("", translateOperand(op1), translateOperand(op2)) ::
          translateMove("lt", translateRegister(res), new ImmediateInt(1))
      }
      case BinaryOpType.Gt => {
        translateMove("", translateRegister(res), new ImmediateInt(0)) ::
          translateCompare("", translateOperand(op1), translateOperand(op2)) ::
          translateMove("gt", translateRegister(res), new ImmediateInt(1))
      }
      case BinaryOpType.Lte => {
        translateMove("", translateRegister(res), new ImmediateInt(0)) ::
          translateCompare("", translateOperand(op1), translateOperand(op2)) ::
          translateMove("le", translateRegister(res), new ImmediateInt(1))
      }
      case BinaryOpType.Gte => {
        translateMove("", translateRegister(res), new ImmediateInt(0)) ::
          translateCompare("", translateOperand(op1), translateOperand(op2)) ::
          translateMove("ge", translateRegister(res), new ImmediateInt(1))
      }
      case BinaryOpType.And => {
        val lbl = generateLabel()
        translateMove("", translateRegister(res), new ImmediateInt(0)) ::
          translateCompare("", translateOperand(op1), new ImmediateInt(1)) ::
          translateCompare("eq", translateOperand(op2), new ImmediateInt(1)) ::
          translateBranch("ne", lbl.name) ::
          translateMove("eq", translateRegister(res), new ImmediateInt(1)) ::
          translateTAC(lbl)
      }
      case BinaryOpType.Or => {
        val lbl = generateLabel()
        translateMove("", translateRegister(res), new ImmediateInt(0)) ::
          translateCompare("", translateOperand(op1), new ImmediateInt(1)) ::
          translateMove("eq", translateRegister(res), ImmediateInt(1)) ::
          translateBranch("eq", lbl.name) ::
          translateCompare("", translateOperand(op2), new ImmediateInt(1)) ::
          translateMove("eq", translateRegister(res), ImmediateInt(1)) ::
          translateTAC(lbl)
      }
    }
  }

  def assembleStringLengthDef(len: Int, lbl: Label) = {
    List(".word " + len.toString())
  }

  def escape(s: String): String = "\"" + s.flatMap(escapeChar) + "\""
  def escapeChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"'  => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _    => {
      if (ch.isControl) {
        "\\0" + Integer.toOctalString(ch.toInt)
      } else {
        String.valueOf(ch)
      }
    }
  }

  def assembleStringDef(str: String, lbl: Label): AssemblerState = {
    translateTAC(lbl) ++
      List(".asciz " + escape(str))
  }

  def assembleBeginFunc() = {
    translatePush("", List(fp, lr)) ::
      translatePush("", List(r8, r10, r12)) ::
      translateMove("", fp, sp)
  }

  def assembleEndFunc() = {
    translateMove("", r0, new ImmediateInt(0)) ::
      translatePop("", List(r8, r10, r12)) ::
      translatePop("", List(fp, pc))
  }

  def assembleAssignment(operand: Operand, reg: TRegister) = {
    operand match {
      case Label(name) => translateLdr("", translateRegister(reg), r0, translateOperand(operand))
      case _ => translateMove("", translateRegister(reg), translateOperand(operand))
    }

  }

  def assembleCommand(cmd: CmdT.Cmd, operand: Operand, opType: DeclarationType): AssemblerState = {
    cmd match {
      case CmdT.Exit => {
        translateMove("", r0, translateOperand(operand)) ::
        translateBranchLink("", new BranchString("exit"))
    }

    case CmdT.Print | CmdT.PrintLn => {
      val bl = opType match {
      case ArrayType(dataType, length) => "_prints"
      case BaseType(baseType) => baseType match {
        case BaseT.String_T => "_prints"
        case BaseT.Char_T => "_printc"
        case BaseT.Bool_T => "_printb"
        case BaseT.Int_T => "_printi"
        case _ => "_printi"
        }
      case NestedPair() => "_printp"
      case PairType(fstType, sndType) => "_printp"
      }
      addEndFunc(bl, new HardcodeFunctions().translate_print(bl))
      if (cmd == CmdT.PrintLn) {
        addEndFunc("_println", new HardcodeFunctions().translate_print("_println"))
        translateMove("", r0, translateOperand(operand)) ::
        translateBranchLink("", new BranchString(bl)) ::
        translateBranchLink("", new BranchString("_println"))
      }
      else{
        translateMove("", r0, translateOperand(operand)) ::
        translateBranchLink("", new BranchString(bl))
      }
    }
    case CmdT.Ret => {
      translateMove("", r0, translateOperand(operand)) ::
        translateMove("", sp, fp) ::
        translatePop("", List(r8, r10, r12)) ::
        translatePop("", List(fp, pc)) ::
        ".ltorg"
    }

    case _ => List("Command not implemented")
    }
    
  }
}