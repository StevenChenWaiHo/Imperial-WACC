package wacc

import wacc.AbstractSyntaxTree.BaseT.Char_T
import wacc.AbstractSyntaxTree._
import wacc.AssemblerTypes._
import wacc.RegisterAllocator._
import wacc.TAC._

import scala.collection.mutable.ListBuffer

object StatelessAssembler {
  val argRegs = List(r0, r1, r2, r3)

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

  def translateStr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): String = {
    return "str" + ldrStrAssist(condition, destinationRegister, sourceRegister, operand)
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

  def translateAdd(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): String = {
    "add" + addSubMulAssist(condition, setflag, destinationRegister, sourceRegister, operand)
  }

  def translateSub(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): String = {
    return "sub" + addSubMulAssist(condition, setflag, destinationRegister, sourceRegister, operand)
  }

  def addSubMulAssist(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): String = {
    return condition + setflag + " " + destinationRegister + ", " + sourceRegister + ", " + operand
  }

}

class Assembler {
  private[this] val state = new AssemblerState(ListBuffer(r4, r5, r6, r7, r8, r10))
  val endFuncs = collection.mutable.Map[String, List[String]]()
  var labelCount = 0
  val argRegs = StatelessAssembler.argRegs

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
        str = str + "[" + sourceRegister.toString + ", " + operand.toString + "]"
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
      endFuncs.addOne(name, "" :: code)
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
    state.enterBranch
    return "b" + condition + " " + operand
  }

  def translateBranchLink(condition: String, operand: LHSop): AssemblerState = {
    state.enterBranch
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
      case Label(name) => {
        state.enterBranch
        assembleLabel(name)
      }
      case Comments(str) => List("@ " + str)
      case DataSegmentTAC() => List(".data")
      case TextSegmentTAC() => List(".text")
      case StringLengthDefinitionTAC(len, lbl) => assembleStringLengthDef(len, lbl)
      case StringDefinitionTAC(str, lbl) => assembleStringDef(str, lbl)
      case BeginFuncTAC() => {
        assembleBeginFunc()
        state.enterFunction
      }
      case EndFuncTAC() => {
        state.exitFunction
        state.deleteFunctionScope
        assembleEndFunc()
      }
      case AssignmentTAC(operand, reg) => assembleAssignment(operand, reg)
      case CommandTAC(cmd, operand, opType) => assembleCommand(cmd, operand, opType)
      case BinaryOpTAC(operation, op1, op2, res) => assembleBinOp(operation, op1, op2, res)
      case IfTAC(t1, goto) => assembleIf(t1, goto)
      case GOTO(label) => assembleGOTO(label)
      case CreatePairElem(pairElemType, pairPos, ptrReg, pairElemReg) => assemblePairElem(pairElemType, pairPos, ptrReg, pairElemReg)
      case CreatePair(fstType, sndType, fstReg, sndReg, srcReg, ptrReg, dstReg) => assemblePair(fstType, sndType, fstReg, sndReg, srcReg, ptrReg, dstReg)
      case GetPairElem(datatype, pairReg, pairPos, dstReg) => assembleGetPairElem(datatype, pairReg, pairPos, dstReg)
      case StorePairElem(datatype, pairReg, pairPos, srcReg) => assembleStorePairElem(datatype, pairReg, pairPos, srcReg)
      case InitialiseArray(arrLen, dstReg) => assembleArrayInit(arrLen, dstReg)
      case CreateArrayElem(arrayElemType, elemPos, arrReg, elemReg) => assembleArrayElem(arrayElemType, elemPos, arrReg, elemReg)
      case CreateArray(arrayElemType, elemsReg, dstReg) => assembleArray(arrayElemType, elemsReg, dstReg)
      case LoadArrayElem(datatype, arrReg, arrPos, dstReg) => assembleLoadArrayElem(datatype, arrReg, arrPos, dstReg)
      case StoreArrayElem(datatype, arrReg, arrPos, srcReg) => assembleStoreArrayElem(datatype, arrReg, arrPos, srcReg)
      case UnaryOpTAC(op, t1, res) => assembleUnaryOp(op, t1, res)
      case CallTAC(lbl, args, dstReg) => assembleCall(lbl, args, dstReg)
      case PopParamTAC(datatype, treg, index) => assemblePopParam(datatype, treg, index)
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

  def getLdrInstructionType(decType: DeclarationType): String = {
    decType match {
      case BaseType(BaseT.Char_T) => "sb"
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

  def assemblePair(fstType: DeclarationType, sndType: DeclarationType, fstReg: TRegister, sndReg: TRegister, srcReg: TRegister, ptrReg: TRegister, dstReg: TRegister): AssemblerState = {
    // r12: pointer to pair r8: pointer to pairElem
    translatePop("", List(translateRegister(fstReg))) ::
      translatePop("", List(translateRegister(sndReg))) ::
      translateMove("", r0, new ImmediateInt(2 * POINTER_BYTE_SIZE)) ::
      translateBranchLink("", new BranchString("malloc")) ::
      translateMove("", translateRegister(ptrReg), r0) ::
      translateStr("", translateRegister(fstReg), translateRegister(ptrReg), new ImmediateInt(POINTER_BYTE_SIZE)) ::
      translateStr("", translateRegister(sndReg), translateRegister(ptrReg), new ImmediateInt(0)) ::
      translateMove("", translateRegister(dstReg), translateRegister(ptrReg))
  }

  def assemblePairElem(pairElemType: DeclarationType, pairPos: PairElemT.Elem, ptrReg: TRegister, pairElem: TRegister): AssemblerState = {
    translateMove("", r0, new ImmediateInt(getTypeSize(pairElemType))) ::
      translateBranchLink("", new BranchString("malloc")) ::
      translateMove("", translateRegister(ptrReg), r0) ::
      translateStr(getInstructionType(pairElemType), translateRegister(pairElem), translateRegister(ptrReg), new ImmediateInt(0)) ::
      translateMove("", translateRegister(pairElem), translateRegister(ptrReg)) ::
      translatePush("", List(translateRegister(pairElem)))
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
        // ldr res, [t1, #-4]
        // t1 must necessarily be a TRegister
        translateLdr("", translateRegister(res), translateRegister(t1.asInstanceOf[TRegister]), new ImmediateInt(-4))
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
    //var output = translatePush("", List(r0, r1, r2, r3))
    var output = translateMove("", r0, r0)
    // move all the args in to arg registers
    args.slice(0, args.length.min(argRegs.length)).zip(argRegs).foreach {
      case (arg, reg) => output = output ++ translateMove("", reg, translateRegister(arg))
    }
    // push extra args into memory
    if (args.length > argRegs.length) {
      args.slice(4, args.length).reverse.foreach(reg => {
        output = output ++ translateStrPre("", translateRegister(reg), sp, ImmediateInt(-4))
      })
    }
    output = output ++ (translateBranchLink("", new BranchString(lbl.name)))

    /* Decrement the stack pointer for each argument pushed to the stack */
    if (args.length > argRegs.length)
      output = output ++ translateSub("", None(), sp, sp, ImmediateInt(4 * (argRegs.length - args.length)))

    // move the result into dst before r0 is popped back
    output = output ++ (translateMove("", translateRegister(dstReg), r0))
    // get previous registers from stack
    output //++ (translatePop("", regs))
  }

  // GetPairElem
  // Check Null
  // ldr dstReg [pairReg, pairPos], where (pairPos == fst) ? #0 : #4
  // push pairReg
  // mov pairReg dstReg
  // ldr(type) dstReg [pairReg, 0]
  // pop pairReg
  def assembleGetPairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, dstReg: TRegister): AssemblerState = {
    // TODO: Check Null
    addEndFunc("_errNull", new HardcodeFunctions().translate_errNull())
    addEndFunc("_prints", new HardcodeFunctions().translate_prints())

    translateCompare("", translateRegister(pairReg), new ImmediateInt(0)) ::
      translateBranchLink("eq", new BranchString("_errNull")) ::
      translateLdr("", translateRegister(dstReg), translateRegister(pairReg), new ImmediateInt(if (pairPos == PairElemT.Fst) 0 else 4)) ::
      translatePush("", List(translateRegister(pairReg))) ::
      translateMove("", translateRegister(pairReg), translateRegister(dstReg)) ::
      translateLdr(getLdrInstructionType(datatype), translateRegister(dstReg), translateRegister(pairReg), new ImmediateInt(0)) ::
      translatePop("", List(translateRegister(pairReg)))
  }

  // StorePairElem
  // push pairReg
  // ldr pairReg [pairReg, pairPos], where (pairPos == fst) ? #0 : #4
  // str srcReg [pairReg, 0]
  // pop pairReg
  def assembleStorePairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, srcReg: TRegister): AssemblerState = {
    translatePush("", List(translateRegister(pairReg))) ::
    translateLdr("", translateRegister(pairReg), translateRegister(pairReg), new ImmediateInt(if (pairPos == PairElemT.Fst) 0 else 4)) ::
    translateStr(getInstructionType(datatype), translateRegister(srcReg), translateRegister(pairReg), new ImmediateInt(0)) ::
    translatePop("", List(translateRegister(pairReg)))
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

  def assemblePopParam(dataType: DeclarationType, treg: TRegister, index: Int): AssemblerState = {
    val cRegs = List(r0, r1, r2, r3)
    val funcStackFrameSize = 8 // Stack frame consists of {fp, lr}.

    if (index < cRegs.length) {
      // Populate from registers in r0-
      val callReg = cRegs.take(index + 1).last
      translateMove("", translateRegister(treg), callReg)
    } else {
      // Populate from stack
      translateLdr("", translateRegister(treg), fp, ImmediateInt(funcStackFrameSize + (4 * (index - cRegs.size))))
    }
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
        addEndFunc("_prints", new HardcodeFunctions().translate_print("_prints"))
        translateMove("", r0, translateOperand(op1)) ::
          translateMove("", r1, translateOperand(op2)) ::
          translateCompare("", r1, new ImmediateInt(0)) ::
          translateBranchLink("eq", new BranchString("_errDivZero")) ::
          translateBranchLink("", new BranchString("__aeabi_idivmod")) ::
          translateMove("", translateRegister(res), r0)
      }
      case BinaryOpType.Mod => {
        addEndFunc("_errDivZero", new HardcodeFunctions().translate_errDivZero())
        addEndFunc("_prints", new HardcodeFunctions().translate_print("_prints"))
        translateMove("", r0, translateOperand(op1)) ::
          translateMove("", r1, translateOperand(op2)) ::
          translateCompare("", r1, new ImmediateInt(0)) ::
          translateBranchLink("eq", new BranchString("_errDivZero")) ::
          translateBranchLink("", new BranchString("__aeabi_idivmod")) ::
          translateMove("", translateRegister(res), r1)
      }
      case BinaryOpType.Eq => {
        translateCompare("", translateOperand(op1), translateOperand(op2)) ::
          translateMove("eq", translateRegister(res), new ImmediateInt(1)) ::
          translateMove("ne", translateRegister(res), new ImmediateInt(0))
      }
      case BinaryOpType.Neq => {
        translateCompare("", translateOperand(op1), translateOperand(op2)) ::
          translateMove("ne", translateRegister(res), new ImmediateInt(1)) ::
          translateMove("eq", translateRegister(res), new ImmediateInt(0))
      }
      case BinaryOpType.Lt => {
        translateCompare("", translateOperand(op1), translateOperand(op2)) ::
          translateMove("lt", translateRegister(res), new ImmediateInt(1)) ::
          translateMove("ge", translateRegister(res), new ImmediateInt(0))
      }
      case BinaryOpType.Gt => {
        translateCompare("", translateOperand(op1), translateOperand(op2)) ::
          translateMove("gt", translateRegister(res), new ImmediateInt(1)) ::
          translateMove("le", translateRegister(res), new ImmediateInt(0))
      }
      case BinaryOpType.Lte => {
        translateCompare("", translateOperand(op1), translateOperand(op2)) ::
          translateMove("le", translateRegister(res), new ImmediateInt(1)) ::
          translateMove("gt", translateRegister(res), new ImmediateInt(0))
      }
      case BinaryOpType.Gte => {
        translateCompare("", translateOperand(op1), translateOperand(op2)) ::
          translateMove("ge", translateRegister(res), new ImmediateInt(1)) ::
          translateMove("lt", translateRegister(res), new ImmediateInt(0))
      }
      case BinaryOpType.And => {
        translateCompare("", translateOperand(op1), new ImmediateInt(1)) ::
          translateCompare("eq", translateOperand(op2), new ImmediateInt(1)) ::
          translateMove("ne", translateRegister(res), new ImmediateInt(0)) ::
          translateMove("eq", translateRegister(res), new ImmediateInt(1))
      }
      case BinaryOpType.Or => {
        translateCompare("", translateOperand(op1), new ImmediateInt(1)) ::
          translateMove("eq", translateRegister(res), ImmediateInt(1)) ::
          translateCompare("ne", translateOperand(op2), new ImmediateInt(1)) ::
          translateMove("ne", translateRegister(res), ImmediateInt(0)) ::
          translateMove("eq", translateRegister(res), ImmediateInt(1))
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
    case '"' => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _ => {
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
      //translatePush("", List(r8, r10, r12)) ::
      translateMove("", fp, sp)
  }

  def assembleEndFunc(): AssemblerState = {
    (translateMove("", r0, new ImmediateInt(0)) ::
      //translatePop("", List(r8, r10, r12)) ::
      translatePop("", List(fp, pc)))
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
        state.deleteFunctionScope
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
          case NestedPair() | PairType(_, _) => "_printp"
          // Character arrays should be printed as strings, but all others should be printed as a pointer
          case ArrayType(t, _) if !(t is BaseType(Char_T)) => "_printp"

          // TODO: This may not work yet:
          case ArrayType(t, _) if t is BaseType(Char_T) => "prints"
        }
        addEndFunc(bl, new HardcodeFunctions().translate_print(bl))
        if (cmd == CmdT.PrintLn) {
          addEndFunc("_println", new HardcodeFunctions().translate_print("_println"))
          translateMove("", r0, translateOperand(operand)) ::
            translateBranchLink("", new BranchString(bl)) ::
            translateBranchLink("", new BranchString("_println"))
        }
        else {
          translateMove("", r0, translateOperand(operand)) ::
            translateBranchLink("", new BranchString(bl))
        }
      }
      case CmdT.Ret => {
        state.exitFunction
        translateMove("", r0, translateOperand(operand)) ::
          translateMove("", sp, fp) ::
          //translatePop("", List(r8, r10, r12)) ::
          translatePop("", List(fp, pc)) ::
          ".ltorg"
      }

      case _ => List("Command not implemented")
    }

  }

  // Assume r8 not used
  // TODO n-D arrays
  def assembleArrayInit(arrLen: Int, dstReg: TRegister): AssemblerState = {
    // println("ini", translateRegister(dstReg))
    translateMove("", r0, new ImmediateInt(POINTER_BYTE_SIZE * (arrLen + 1))) ::
      translateMove("", translateRegister(dstReg), r0) ::
      translateAdd("", Status(), translateRegister(dstReg), translateRegister(dstReg), new ImmediateInt(4)) ::
      translateMove("", r8, new ImmediateInt(arrLen)) ::
      translateStr("", r8, translateRegister(dstReg), new ImmediateInt(-POINTER_BYTE_SIZE)) ::
      translateBranchLink("", new BranchString("malloc"))
  }

  // Can be removed
  def assembleArray(arrayElemType: DeclarationType, elemsReg: List[TRegister], dstReg: TRegister): AssemblerState = {
    // elemsReg.foreach(e => println("asm_e", translateRegister(e)))
    // println("asm", translateRegister(dstReg))
    Nil
    // translateMove("", translateRegister(dstReg), r12)
  }

  def assembleArrayElem(arrayElemType: DeclarationType, elemPos: Int, arrReg: TRegister, elemReg: TRegister): AssemblerState = {
    // println(elemPos, translateRegister(elemReg))
    translateBranchLink("", new BranchString("malloc")) ::
      translateStr(getInstructionType(arrayElemType), translateRegister(elemReg), translateRegister(arrReg), new ImmediateInt(POINTER_BYTE_SIZE * elemPos))
  }
  
  // LoadArrayElem
  // Check Null?
  // mov r10 arrPos
  // mov r3 arrReg
  // bl _arrLoad
  def assembleLoadArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: List[TRegister], dstReg: TRegister): AssemblerState = {
    addEndFunc("_arrLoad", new HardcodeFunctions().translate_arrLoad("_arrLoad"))
    addEndFunc("_boundsCheck", new HardcodeFunctions().translate_boundsCheck())
    // println("ld", translateRegister(arrReg), translateRegister(dstReg))
    arrPos match {
      case _ if (arrPos.isEmpty) => Nil
      case _ => {
        translateMove("", r10, translateRegister(arrPos.head)) :: // TODO n-D arrays (again)
        translateMove("", r3, translateRegister(arrReg)) :: // arrLoad uses r3 = r3[r10]
        translateBranchLink("", new BranchString("_arrLoad")) ::
        assembleLoadArrayElem(datatype, arrReg, arrPos.drop(1), dstReg)
      }
    }
  }
  
  // StorePairElem
  // mov r10 arrPos
  // mov r8 srcReg
  // mov r3 arrReg
  // bl _arrStore
  def assembleStoreArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: List[(List[TAC], TRegister)], srcReg: TRegister): AssemblerState = {
    addEndFunc("_arrStore", new HardcodeFunctions().translate_arrStore("_arrStore"))
    addEndFunc("_boundsCheck", new HardcodeFunctions().translate_boundsCheck())
    // TODO translate tac of each index
    // val index = arrayPos.head
    // checkIndexTAC(index) ::
    // println("st", translateRegister(arrReg), translateRegister(srcReg))
    arrPos match {
      case _ if (arrPos.isEmpty) => Nil
      case _ => {
        translateMove("", r10, translateRegister(arrPos.head._2)) ::
        translateMove("", r8, translateRegister(srcReg)) ::
        translateMove("", r3, translateRegister(arrReg)) :: // arrStore uses r3[r10] = r8
        translateBranchLink("", new BranchString("_arrStore"))
        assembleStoreArrayElem(datatype, arrReg, arrPos.drop(1), srcReg)
      }
    }
  }

  // def checkIndexTAC(arrayPos: (List[TAC], TRegister)): AssemblerState = { // TODO translate tac of each index
  //   arrayPos match {
  //     case _ if (!arrayPos._1.isEmpty) => translateTAC(arrayPos._1.head)
  //     case _ => Nil
  //   }
  // }
}