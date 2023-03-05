package wacc

import wacc.AbstractSyntaxTree.BaseT.Char_T
import wacc.AbstractSyntaxTree._
import wacc.AssemblerTypes._
import wacc.RegisterAllocator._
import wacc.TAC._

import scala.collection.mutable.ListBuffer

import wacc.HelperFunctions
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

  def assemblePush(condition: String, registers: List[Register]): String = {
    "push" + pushPopAssist(condition, registers)
  }

  def assembleLdr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): String = {
    //Incomplete
    "ldr" + ldrStrAssist(condition, destinationRegister, sourceRegister, operand)
  }

  def assembleStr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): String = {
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

  def assembleAdd(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): String = {
    "add" + addSubMulAssist(condition, setflag, destinationRegister, sourceRegister, operand)
    
  }

  def assembleSub(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): String = {
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

  def getRealReg(t: TRegister) = state.getRegister(t)

  implicit private[this] def updateState(str: String): AssemblerState = {
    state.addInstruction(str)
  }

  implicit private[this] def updateState(strs: List[String]): AssemblerState = {
    state.addInstructions(strs)
  }

  // Assembly translation functions

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

  def assemblePush(condition: String, registers: List[Register]): AssemblerState = {
    return "push" + pushPopAssist(condition, registers)
  }

  def assemblePop(condition: String, registers: List[Register]): AssemblerState = {
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

  def assembleLdr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): AssemblerState = {
    return "ldr" + ldrStrAssist(condition, destinationRegister, sourceRegister, operand)
  }

  def assembleStr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): AssemblerState = {
    return "str" + ldrStrAssist(condition, destinationRegister, sourceRegister, operand)
  }

  def assembleStrPre(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): AssemblerState = {
    "str" + ldrStrAssist(condition, destinationRegister, sourceRegister, operand).toString + "!".toString()
  }

  // Get a unique label for branches/loops
  def generateLabel(): Label = {
    labelCount += 1
    new Label(".La" + labelCount.toString())
  }

  // Add predefined function to end of assembly code (.e.g _prints)
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
    addEndFunc("_errOverflow", new HelperFunctions().assemble_errOverflow())
    addEndFunc("_prints", new HelperFunctions().assemble_prints())
    return condition + setflag + " " + destinationRegister + ", " + sourceRegister + ", " + operand + "\nblvs _errOverflow"
  }

  def assembleAdd(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): AssemblerState = {
    return "add" + addSubMulAssist(condition, setflag, destinationRegister, sourceRegister, operand)
    
  }

  def assembleSub(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): AssemblerState = {
    return "sub" + addSubMulAssist(condition, setflag, destinationRegister, sourceRegister, operand)
  }

  def assembleRsb(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): AssemblerState = {
    return "rsb" + addSubMulAssist(condition, setflag, destinationRegister, sourceRegister, operand)
  }

  def assembleMul(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, sourceRegisterTwo: LHSop): AssemblerState = {
    return "mul" + addSubMulAssist(condition, setflag, destinationRegister, sourceRegister, sourceRegisterTwo)
  }

  def fourMulAssist(condition: String, setflag: Suffi, destinationLow: LHSop, destinationHigh: LHSop,
                    sourceRegister: LHSop, operand: LHSop): String = {
    addEndFunc("_errOverflow", new HelperFunctions().assemble_errOverflow())
    addEndFunc("_prints", new HelperFunctions().assemble_prints())

    var str = condition + setflag + " " + destinationLow + "," + " " + destinationHigh + "," + " " + sourceRegister +
      "," + " " + operand +
      "\ncmp " + destinationHigh + ", " + destinationLow + ", asr #31" +
      "\nbne _errOverflow"
    return str
  }

  def assembleMla(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: Register, operand1: Register, operand2: Register): AssemblerState = {
    return "mla" + fourMulAssist(condition, setflag, destinationRegister, sourceRegister, operand1, operand2)
  }

  def assembleUmull(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: Register, operand1: Register, operand2: Register): AssemblerState = {
    return "umull" + fourMulAssist(condition, setflag, destinationRegister, sourceRegister, operand1, operand2)
  }

  def assembleUmlal(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: Register, operand1: Register, operand2: Register): AssemblerState = {
    return "umlal" + fourMulAssist(condition, setflag, destinationRegister, sourceRegister, operand1, operand2)
  }

  def assembleSmull(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand1: LHSop, operand2: LHSop): AssemblerState = {
    return "smull" + fourMulAssist(condition, setflag, destinationRegister, sourceRegister, operand1, operand2)
  }

  def assembleSmlal(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: Register, operand1: Register, operand2: Register): AssemblerState = {
    return "smlal" + fourMulAssist(condition, None(), destinationRegister, sourceRegister, operand1, operand2)
  }

  def assembleCompare(condition: String, register1: LHSop, operand: LHSop): AssemblerState = {
    return "cmp" + condition + " " + register1.toString + ", " + operand.toString
  }

  def assembleCompareNeg(condition: String, register1: Register, operand: LHSop): AssemblerState = {
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

  def assembleMove(condition: String, dst: Register, operand: LHSop): AssemblerState = {
    operand match {
      case ImmediateInt(i) if !checkMovCases(i) => "ldr " + condition + " " + dst.toString() + ", =" + i
      case _ => "mov" + condition + " " + dst.toString + ", " + operand.toString()
    }
  }

  def assembleBranch(condition: String, operand: String): AssemblerState = {
    state.enterBranch
    return "b" + condition + " " + operand
  }

  def assembleBranchLink(condition: String, operand: LHSop): AssemblerState = {
    state.enterBranch
    return "bl" + condition + " " + operand
  }

  val OperandToLiteral = Map[TAC.Operand, Either[String, Either[Register, Int]]]()

  def getOperand2(operand: TAC.Operand): Either[String, Either[Register, Int]] = {
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

  // Get correct operand type from TAC
  def getOperand(op: Operand): LHSop = {
    op match {
      case reg: TRegister => getRealReg(reg)
      case IntLiteralTAC(value) => new ImmediateInt(value)
      case CharLiteralTAC(chr) => new ImmediateInt(chr.toInt)
      case BoolLiteralTAC(b) => new ImmediateInt(b.compare(true) + 1)
      case Label(name) => new LabelString(name)
      case PairLiteralTAC() => new ImmediateInt(0)
      case ArrayOp(_) => ImmediateInt(0)
      case a => println("getOperand fail: " + a); null 
    }
  }

  // Convert TAC into List[ARM Code]
  def assembleTAC(tripleAddressCode: TAC): AssemblerState = {
    println("Translating: " + TAC)
    println(endFuncs.keys)
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
      case InitialiseArray(arrLen, lenReg, dstReg) => assembleArrayInit(arrLen, lenReg, dstReg)
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
    assemblePop("", List(getRealReg(fstReg))) ::
      assemblePop("", List(getRealReg(sndReg))) ::
      assemblePush("", List(r0)) :: 
      assembleMove("", r0, new ImmediateInt(2 * POINTER_BYTE_SIZE)) ::
      assembleBranchLink("", new BranchString("malloc")) ::
      assembleMove("", getRealReg(ptrReg), r0) ::
      assemblePop("", List(r0)) :: 
      assembleStr("", getRealReg(fstReg), getRealReg(ptrReg), new ImmediateInt(POINTER_BYTE_SIZE)) ::
      assembleStr("", getRealReg(sndReg), getRealReg(ptrReg), new ImmediateInt(0)) ::
      assembleMove("", getRealReg(dstReg), getRealReg(ptrReg))
  }
  // Push r0
  // mov r0 #(size)
  // bl malloc
  // push ptrReg (remove?)
  // mov ptrReg r0
  // str pairElemReg, [ptrReg, #0]
  // mov r0 pairElemReg
  // mov pairElemReg ptrReg
  // pop ptrReg (remove?)
  // Pop r0
  // push pairElemReg
  // mov pairElemReg r0
  def assemblePairElem(pairElemType: DeclarationType, pairPos: PairElemT.Elem, ptrReg: TRegister, pairElem: TRegister): AssemblerState = {
    assembleMove("", r0, new ImmediateInt(getTypeSize(pairElemType))) ::
      assembleBranchLink("", new BranchString("malloc")) ::
      assemblePush("", List(getRealReg((ptrReg)))) :: // TODO: Remove?
      assembleMove("", getRealReg(ptrReg), r0) ::
      assembleStr(getInstructionType(pairElemType), getRealReg(pairElem), getRealReg(ptrReg), new ImmediateInt(0)) ::
      assembleMove("", r0, getRealReg(pairElem)) ::
      assembleMove("", getRealReg(pairElem), getRealReg(ptrReg)) ::
      assemblePop("", List(getRealReg(ptrReg))) :: // TODO: Remove?
      assemblePush("", List(getRealReg(pairElem))) ::
      assembleMove("", getRealReg(pairElem), r0)
  }

  def assembleUnaryOp(op: UnaryOpType.UnOp, t1: Operand, res: TRegister): AssemblerState = {
    op match {
      case UnaryOpType.Neg => {
        assembleRsb("", Status(), getRealReg(res), getOperand(t1), new ImmediateInt(0))
      }
      case UnaryOpType.Not => {
        assembleCompare("", getOperand(t1), new ImmediateInt(1)) ::
          assembleMove("ne", getRealReg(res), new ImmediateInt(1)) ::
          assembleMove("eq", getRealReg(res), new ImmediateInt(0))
      }
      case UnaryOpType.Chr | UnaryOpType.Ord => {
        assembleMove("", getRealReg(res), getOperand(t1))
      }
      case UnaryOpType.Len => {
        assembleLdr("", getRealReg(res), getRealReg(t1.asInstanceOf[TRegister]), new ImmediateInt(-4))
      }
    }
  }

  def assembleRead(datatype: DeclarationType, readReg: TRegister): AssemblerState = {
    val bl = datatype match {
      case BaseType(baseType) => {
        baseType match {
          case BaseT.Int_T =>"_readi"
          case BaseT.Char_T => "_readc"
          case BaseT.String_T => "_reads"
          case BaseT.Bool_T => "_readb"
          case _ => "_readi"
        }
      }
      case _ => "_readi"
    }
    addEndFunc("_errOverflow", new HelperFunctions().assemble_errOverflow())
    addEndFunc("_prints", new HelperFunctions().assemble_prints())//.assemble_errOverflow())
    addEndFunc(bl, new HelperFunctions().assemble_read(bl))
    assembleBranchLink("", new BranchString(bl))
    assembleMove("", getRealReg(readReg), r0)
  }

  def assembleCall(lbl: Label, args: List[TRegister], dstReg: TRegister): AssemblerState = {
    // save arg registers on the stack
    //var output = assemblePush("", List(r0, r1, r2, r3))
    var output = assembleMove("", r0, r0)
    // move all the args in to arg registers
    args.slice(0, args.length.min(argRegs.length)).zip(argRegs).foreach {
      case (arg, reg) => output = output ++ assembleMove("", reg, getRealReg(arg))
    }
    // push extra args into memory
    if (args.length > argRegs.length) {
      args.slice(4, args.length).reverse.foreach(reg => {
        output = output ++ assembleStrPre("", getRealReg(reg), sp, ImmediateInt(-4))
      })
    }
    output = output ++ (assembleBranchLink("", new BranchString(lbl.name)))

    /* Decrement the stack pointer for each argument pushed to the stack */
    if (args.length > argRegs.length)
      output = output ++ assembleSub("", None(), sp, sp, ImmediateInt(4 * (argRegs.length - args.length)))

    // move the result into dst before r0 is popped back
    output = output ++ (assembleMove("", getRealReg(dstReg), r0))
    // get previous registers from stack
    output //++ (assemblePop("", regs))
  }

  // GetPairElem
  // Check Null
  // ldr dstReg [pairReg, pairPos], where (pairPos == fst) ? #0 : #4
  // push pairReg
  // mov pairReg dstReg
  // ldr(type) dstReg [pairReg, 0]
  // pop pairReg
  def assembleGetPairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, dstReg: TRegister): AssemblerState = {
    addEndFunc("_errNull", new HelperFunctions().assemble_errNull())
    addEndFunc("_prints", new HelperFunctions().assemble_prints())

    assembleCompare("", getRealReg(pairReg), new ImmediateInt(0)) ::
      assembleBranchLink("eq", new BranchString("_errNull")) ::
      assembleLdr("", getRealReg(dstReg), getRealReg(pairReg), new ImmediateInt(if (pairPos == PairElemT.Fst) 0 else 4)) ::
      assemblePush("", List(getRealReg(pairReg))) ::
      assembleMove("", getRealReg(pairReg), getRealReg(dstReg)) ::
      assembleLdr(getLdrInstructionType(datatype), getRealReg(dstReg), getRealReg(pairReg), new ImmediateInt(0)) ::
      assemblePop("", List(getRealReg(pairReg)))
  }

  // StorePairElem
  // Check null
  // push pairReg
  // ldr pairReg [pairReg, pairPos], where (pairPos == fst) ? #0 : #4
  // str srcReg [pairReg, 0]
  // pop pairReg
  def assembleStorePairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, srcReg: TRegister): AssemblerState = {
    addEndFunc("_errNull", new HelperFunctions().assemble_errNull())
    addEndFunc("_prints", new HelperFunctions().assemble_prints())

    assembleCompare("", getRealReg(pairReg), new ImmediateInt(0)) ::
    assembleBranchLink("eq", new BranchString("_errNull")) ::
    assemblePush("", List(getRealReg(pairReg))) ::
    assembleLdr("", getRealReg(pairReg), getRealReg(pairReg), new ImmediateInt(if (pairPos == PairElemT.Fst) 0 else 4)) ::
    assembleStr(getInstructionType(datatype), getRealReg(srcReg), getRealReg(pairReg), new ImmediateInt(0)) ::
    assemblePop("", List(getRealReg(pairReg)))
  }

  def assembleProgram(tacList: List[TAC]): String = {
    tacList.map(assembleTAC)
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
    assembleBranch("", label.name)
  }

  def assembleIf(t1: Operand, goto: Label): AssemblerState = {
    assembleCompare("", getOperand(t1), new ImmediateInt(1)) ::
      assembleBranch("eq", goto.name)
  }

  def assemblePopParam(dataType: DeclarationType, treg: TRegister, index: Int): AssemblerState = {
    val cRegs = List(r0, r1, r2, r3)
    val funcStackFrameSize = 8 // Stack frame consists of {fp, lr}.

    if (index < cRegs.length) {
      // Populate from registers in r0-
      val callReg = cRegs.take(index + 1).last
      assembleMove("", getRealReg(treg), callReg)
    } else {
      // Populate from stack
      assembleLdr("", getRealReg(treg), fp, ImmediateInt(funcStackFrameSize + (4 * (index - cRegs.size))))
    }
  }

  def assembleBinOp(operation: BinaryOpType.BinOp, op1: Operand, op2: Operand, res: TRegister): AssemblerState = {
    operation match {
      case BinaryOpType.Add => {
        assembleAdd("", Status(), getRealReg(res), getOperand(op1), getOperand(op2))
      }
      case BinaryOpType.Sub => {
        assembleSub("", Status(), getRealReg(res), getOperand(op1), getOperand(op2))
      }
      case BinaryOpType.Mul => {
        assembleSmull("", Status(), getRealReg(res), getOperand(op2), getOperand(op1), getOperand(op2))
      }
      case BinaryOpType.Div => {
        addEndFunc("_errDivZero", new HelperFunctions().assemble_errDivZero())
        addEndFunc("_prints", new HelperFunctions().assemble_print("_prints"))
        assembleMove("", r0, getOperand(op1)) ::
          assembleMove("", r1, getOperand(op2)) ::
          assembleCompare("", r1, new ImmediateInt(0)) ::
          assembleBranchLink("eq", new BranchString("_errDivZero")) ::
          assembleBranchLink("", new BranchString("__aeabi_idivmod")) ::
          assembleMove("", getRealReg(res), r0)
      }
      case BinaryOpType.Mod => {
        addEndFunc("_errDivZero", new HelperFunctions().assemble_errDivZero())
        addEndFunc("_prints", new HelperFunctions().assemble_print("_prints"))
        assembleMove("", r0, getOperand(op1)) ::
          assembleMove("", r1, getOperand(op2)) ::
          assembleCompare("", r1, new ImmediateInt(0)) ::
          assembleBranchLink("eq", new BranchString("_errDivZero")) ::
          assembleBranchLink("", new BranchString("__aeabi_idivmod")) ::
          assembleMove("", getRealReg(res), r1)
      }
      case BinaryOpType.Eq => {
        assembleCompare("", getOperand(op1), getOperand(op2)) ::
          assembleMove("eq", getRealReg(res), new ImmediateInt(1)) ::
          assembleMove("ne", getRealReg(res), new ImmediateInt(0))
      }
      case BinaryOpType.Neq => {
        assembleCompare("", getOperand(op1), getOperand(op2)) ::
          assembleMove("ne", getRealReg(res), new ImmediateInt(1)) ::
          assembleMove("eq", getRealReg(res), new ImmediateInt(0))
      }
      case BinaryOpType.Lt => {
        assembleCompare("", getOperand(op1), getOperand(op2)) ::
          assembleMove("lt", getRealReg(res), new ImmediateInt(1)) ::
          assembleMove("ge", getRealReg(res), new ImmediateInt(0))
      }
      case BinaryOpType.Gt => {
        assembleCompare("", getOperand(op1), getOperand(op2)) ::
          assembleMove("gt", getRealReg(res), new ImmediateInt(1)) ::
          assembleMove("le", getRealReg(res), new ImmediateInt(0))
      }
      case BinaryOpType.Lte => {
        assembleCompare("", getOperand(op1), getOperand(op2)) ::
          assembleMove("le", getRealReg(res), new ImmediateInt(1)) ::
          assembleMove("gt", getRealReg(res), new ImmediateInt(0))
      }
      case BinaryOpType.Gte => {
        assembleCompare("", getOperand(op1), getOperand(op2)) ::
          assembleMove("ge", getRealReg(res), new ImmediateInt(1)) ::
          assembleMove("lt", getRealReg(res), new ImmediateInt(0))
      }
      case BinaryOpType.And => {
        assembleCompare("", getOperand(op1), new ImmediateInt(1)) ::
          assembleCompare("eq", getOperand(op2), new ImmediateInt(1)) ::
          assembleMove("ne", getRealReg(res), new ImmediateInt(0)) ::
          assembleMove("eq", getRealReg(res), new ImmediateInt(1))
      }
      case BinaryOpType.Or => {
        assembleCompare("", getOperand(op1), new ImmediateInt(1)) ::
          assembleMove("eq", getRealReg(res), ImmediateInt(1)) ::
          assembleCompare("ne", getOperand(op2), new ImmediateInt(1)) ::
          assembleMove("ne", getRealReg(res), ImmediateInt(0)) ::
          assembleMove("eq", getRealReg(res), ImmediateInt(1))
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
    assembleTAC(lbl) ++
      List(".asciz " + escape(str))
  }

  def assembleBeginFunc() = {
    assemblePush("", List(fp, lr)) ::
      //assemblePush("", List(r8, r10, r12)) ::
      assembleMove("", fp, sp)
  }

  def assembleEndFunc(): AssemblerState = {
    (assembleMove("", r0, new ImmediateInt(0)) ::
      //assemblePop("", List(r8, r10, r12)) ::
      assemblePop("", List(fp, pc)))
  }

  def assembleAssignment(operand: Operand, reg: TRegister) = {
    operand match {
      case Label(name) => assembleLdr("", getRealReg(reg), r0, getOperand(operand))
      case _ => assembleMove("", getRealReg(reg), getOperand(operand))
    }

  }

  def assembleCommand(cmd: CmdT.Cmd, operand: Operand, opType: DeclarationType): AssemblerState = {
    cmd match {
      case CmdT.Exit => {
        assembleMove("", r0, getOperand(operand)) ::
          assembleBranchLink("", new BranchString("exit"))
        state.deleteFunctionScope
      }

      case CmdT.Print | CmdT.PrintLn => {
        val bl = opType match {
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

          case ArrayType(t, _) if t is BaseType(Char_T) => "_prints"
        }
        addEndFunc(bl, new HelperFunctions().assemble_print(bl))
        if (cmd == CmdT.PrintLn) {
          addEndFunc("_println", new HelperFunctions().assemble_print("_println"))
          assembleMove("", r0, getOperand(operand)) ::
            assembleBranchLink("", new BranchString(bl)) ::
            assembleBranchLink("", new BranchString("_println"))
        }
        else {
          assembleMove("", r0, getOperand(operand)) ::
            assembleBranchLink("", new BranchString(bl))
        }
      }

      case CmdT.Ret => {
        state.exitFunction
        assembleMove("", r0, getOperand(operand)) ::
          assembleMove("", sp, fp) ::
          //assemblePop("", List(r8, r10, r12)) ::
          assemblePop("", List(fp, pc)) ::
          ".ltorg"
      }

      case CmdT.Free => {
      opType match {
        case ArrayType(dataType, length) => {
          assembleSub("", Status(), r8, r4, new ImmediateInt(4)) ::
            assemblePush("", List(r8)) ::
            assemblePop("", List(r8)) ::
          assembleMove("", r8, r8) ::
            assembleMove("", r0, r8) ::
            assembleBranchLink("", new BranchString("free"))


        }
        case PairType(fstType, sndType) => {
          addEndFunc("_freepair", new HelperFunctions().assemble_freepair())
          addEndFunc("_errNull", new HelperFunctions().assemble_errNull())
          addEndFunc("_prints", new HelperFunctions().assemble_prints())

            assembleMove("", r0, getOperand(operand)) ::
            assembleBranchLink("", new BranchString("_freepair"))
        }

      }
    }

      case _ => List("Command not implemented")
    }

  }

  def assembleArrayInit(arrLen: Int, lenReg: TRegister, dstReg: TRegister): AssemblerState = {
    assemblePush("", List(r0)) ::
      assembleMove("", r0, new ImmediateInt(POINTER_BYTE_SIZE * (arrLen + 1))) ::
      assembleBranchLink("", new BranchString("malloc")) ::
      assembleMove("", getRealReg(dstReg), r0) ::
      assemblePop("", List(r0)) ::
      assembleAdd("", Status(), getRealReg(dstReg), getRealReg(dstReg), new ImmediateInt(POINTER_BYTE_SIZE)) ::
      assembleMove("", getRealReg(lenReg), new ImmediateInt(arrLen)) ::
      assembleStr("", getRealReg(lenReg), getRealReg(dstReg), new ImmediateInt(-POINTER_BYTE_SIZE))
  }

  def assembleArray(arrayElemType: DeclarationType, elemsReg: List[TRegister], dstReg: TRegister): AssemblerState = {
    // elemsReg.foreach(e => println("asm_e", getRealReg(e)))
    // println("asm", getRealReg(dstReg))
    Nil
    // assembleMove("", getRealReg(dstReg), r12)
  }

  def assembleArrayElem(arrayElemType: DeclarationType, elemPos: Int, arrReg: TRegister, elemReg: TRegister): AssemblerState = {
    assemblePush("", List(getRealReg(elemReg))) ::
      assemblePush("", List(r0)) ::
      assembleMove("", r0, new ImmediateInt(getTypeSize(arrayElemType))) ::
      assembleBranchLink("", new BranchString("malloc")) ::
      assembleStr("", getRealReg(elemReg), r0, new ImmediateInt(0))
      assembleMove("", getRealReg(elemReg), r0)
      assemblePop("", List(r0)) ::
      assembleStr(getInstructionType(arrayElemType), getRealReg(elemReg), getRealReg(arrReg), new ImmediateInt(POINTER_BYTE_SIZE * elemPos)) ::
      assemblePop("", List(getRealReg(elemReg)))
  }
  
  // LoadArrayElem
  // Check Null?
  // push r0
  // mov r0 arrPos
  // mov r3 arrReg
  // bl _arrLoad
  // RECURSE LoadArrayElem
  // pop r0
  def assembleLoadArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: List[TRegister], dstReg: TRegister): AssemblerState = {
    addEndFunc("_arrLoad", new HelperFunctions().assemble_arrLoad())
    addEndFunc("_boundsCheck", new HelperFunctions().assemble_boundsCheck())
    var regs = List(getRealReg(arrReg), getRealReg(dstReg))
    regs = regs ++ arrPos.map(a => getRealReg(a))
    assemblePush("", regs.sortWith((s, t) => s < t)) ::
    assemblePush("", List(r0, r1, r2, r3))
    arrPos.foreach(a => {
      assembleMove("", r0, getRealReg(a)) ::
      assembleMove("", r3, getRealReg(arrReg)) :: // arrLoad uses r0 = r3[r2]
      assembleBranchLink("", new BranchString("_arrLoad")) ::
      assembleLdr("", r2, r2, new ImmediateInt(0)) ::
      assembleMove("", getRealReg(dstReg), r2)
    })
    // loadArrayElemHelper(assembleRegister(arrReg), arrPos, assembleRegister(dstReg)) ::
    assemblePop("", List(r0, r1, r2, r3)) ::
    assemblePop("", regs.sortWith((s, t) => s < t))
  }


  // StoreArrayElem
  // push r0, r2, r3
  // mov r0 arrPos
  // mov r2 srcReg
  // mov r3 arrReg
  // bl _arrStore
  // RECURSE StoreArrayElem
  // pop r0, r2, r3
  def assembleStoreArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: List[(List[TAC], TRegister)], srcReg: TRegister): AssemblerState = {
    addEndFunc("_arrStore", new HelperFunctions().assemble_arrStore())
    addEndFunc("_boundsCheck", new HelperFunctions().assemble_boundsCheck())
    // TODO assemble tac of each index
    // val index = arrayPos.head
    // checkIndexTAC(index) ::
    arrPos match {
      case _ if (arrPos.isEmpty) => Nil
      case _ => {
        assemblePush("", List(r0, r2, r3)) ::
        assembleMove("", r0, getRealReg(arrPos.head._2)) ::
        assembleMove("", r2, getRealReg(srcReg)) ::
        assembleMove("", r3, getRealReg(arrReg)) :: // arrStore uses r3[r0] = r2
        assembleBranchLink("", new BranchString("_arrStore")) ::
        assemblePop("", List(r0, r2, r3)) ::
        assembleStoreArrayElem(datatype, arrReg, arrPos.drop(1), srcReg)
      }
    }
  }

  // def checkIndexTAC(arrayPos: (List[TAC], TRegister)): AssemblerState = { 
  //   arrayPos match {
  //     case _ if (!arrayPos._1.isEmpty) => assembleTAC(arrayPos._1.head)
  //     case _ => Nil
  //   }
  // }
}