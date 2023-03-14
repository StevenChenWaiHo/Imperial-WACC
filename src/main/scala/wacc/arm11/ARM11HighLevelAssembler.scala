package wacc

import wacc.AbstractSyntaxTree.BaseT.Char_T
import wacc.AbstractSyntaxTree._
import wacc.AssemblerTypes._
import wacc.FinalIR.FinalIR
import wacc.RegisterAllocator._
import wacc.TAC._
import wacc.cfgutils.{Colouring, RegisterAllocator}

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

  def assemblePush(condition: String, registers: List[Register]): FinalIR = {
    FinalIR.Push(condition, registers)
  }

  def assembleLdr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): FinalIR = {
    FinalIR.Ldr(condition, sourceRegister, operand, destinationRegister)
  }

  def assembleStr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): FinalIR = {
    FinalIR.Str(condition, sourceRegister, operand, destinationRegister)
  }

  def assembleAdd(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: LHSop, operand: LHSop): FinalIR = {
    FinalIR.Add(condition, setflag, sourceRegister, operand, destinationRegister)
  }

  def assembleSub(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: LHSop, operand: LHSop): FinalIR = {
    FinalIR.Sub(condition, setflag, sourceRegister, operand, destinationRegister)
  }

}

class ARM11HighLevelAssembler(allocationScheme: RegisterAllocator[Register]) {
  var colouring: Colouring[Register] = null
  private[this] val state = new AssemblerState(ListBuffer(r4, r5, r6, r7, r8, r10))
  val endFuncs = collection.mutable.Map[String, List[FinalIR]]()
  var labelCount = 0
  val argRegs = List(r0, r1, r2, r3)
  val POINTER_BYTE_SIZE = 4

  // Add predefined function to end of assembly code (.e.g _prints)
  def addEndFunc(name: String, code: List[FinalIR]): Unit = {
    if (!endFuncs.contains(name)) {
      endFuncs.addOne(name, code)
    }
  }

  def getRealReg(t: TRegister) = colouring.coloured(t)

  implicit private[this] def updateState(instr: FinalIR): AssemblerState = {
    state.addInstruction(instr)
  }

  implicit private[this] def updateState(instrs: List[FinalIR]): AssemblerState = {
    state.addInstructions(instrs)
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

  // Convert TAC into next IR
  def assembleTAC(tripleAddressCode: TAC): List[FinalIR] = {
    tripleAddressCode match {
      case Label(name) => {
        if (name == "main") {
          List(FinalIR.Global(name), FinalIR.Lbl(name))
        } else {
          List(FinalIR.Lbl(name))
        }
      }
      case Comments(str) => List(FinalIR.Comment(str))
      case DataSegmentTAC() => List(FinalIR.DataSeg())
      case TextSegmentTAC() => List(FinalIR.TextSeg())
      case StringLengthDefinitionTAC(len, _) => List(FinalIR.Word(len))
      case StringDefinitionTAC(str, lbl) => assembleStringDef(str, lbl)
      case BeginFuncTAC() => {
        assembleBeginFunc()
      }
      case EndFuncTAC() => {
        assembleEndFunc()
      }
      case AssignmentTAC(operand, reg) => assembleAssignment(operand, reg)
      case CommandTAC(cmd, operand, opType) => assembleCommand(cmd, operand, opType)
      case BinaryOpTAC(operation, op1, op2, res) => assembleBinOp(operation, op1, op2, res)
      case IfTAC(t1, goto) => assembleIf(t1, goto)
      case GOTO(label) => assembleJump(label)
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
      case ReservedPushTAC(reg, location, _) => List[FinalIR](FinalIR.Str("", fp, ImmediateInt((location + 2) * POINTER_BYTE_SIZE), getRealReg(reg)))
      case ReservedPopTAC(location, reg, _) => List[FinalIR](FinalIR.Ldr("", fp, ImmediateInt((location + 2) * POINTER_BYTE_SIZE), getRealReg(reg)))
      // ^Add 2 more to account for the fp and lr, which are pushed to the stack at the start of functions
      case AllocateStackTAC(size) => List(FinalIR.Sub("", AssemblerTypes.None(), sp, ImmediateInt((size + 1) * POINTER_BYTE_SIZE), sp))
      // ^Since 'size' is zero-indexed, add 1 to it to prevent overwriting the bottom of the stack.
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

  def assemblePair(fstType: DeclarationType, sndType: DeclarationType, fstReg: TRegister, sndReg: TRegister, srcReg: TRegister, ptrReg: TRegister, dstReg: TRegister): List[FinalIR] = {
    FinalIR.Pop("", List(getRealReg(fstReg))) ::
      FinalIR.Pop("", List(getRealReg(sndReg))) ::
      FinalIR.Push("", List(r0)) ::
      FinalIR.Mov("", ImmediateInt(2 * POINTER_BYTE_SIZE), r0) ::
      FinalIR.BranchLink("", new BranchString("malloc")) ::
      FinalIR.Mov("", r0, getRealReg(ptrReg)) ::
      FinalIR.Pop("", List(r0)) ::
      FinalIR.Str("", getRealReg(ptrReg), ImmediateInt(POINTER_BYTE_SIZE), getRealReg(fstReg)) ::
      FinalIR.Str("", getRealReg(ptrReg), ImmediateInt(0), getRealReg(sndReg)) ::
      FinalIR.Mov("", getRealReg(ptrReg), getRealReg(dstReg)) :: List()
  }

  def assemblePairElem(pairElemType: DeclarationType, pairPos: PairElemT.Elem, ptrReg: TRegister, pairElem: TRegister): List[FinalIR] = {
    FinalIR.Mov("", ImmediateInt(getTypeSize(pairElemType)), r0) ::
    FinalIR.BranchLink("", new BranchString("malloc")) ::
    FinalIR.Push("", List(getRealReg((ptrReg)))) ::
    FinalIR.Mov("", r0, getRealReg(ptrReg)) ::
    FinalIR.Str(getInstructionType(pairElemType), getRealReg(ptrReg), ImmediateInt(0), getRealReg(pairElem)) ::
    FinalIR.Mov("", getRealReg(pairElem), r0) ::
    FinalIR.Mov("", getRealReg(ptrReg), getRealReg(pairElem)) ::
    FinalIR.Pop("", List(getRealReg(ptrReg))) ::
    FinalIR.Push("", List(getRealReg(pairElem))) ::
    FinalIR.Mov("", r0, getRealReg(pairElem)) :: List()
  }

  def assembleUnaryOp(op: UnaryOpType.UnOp, t1: Operand, res: TRegister): List[FinalIR] = {
    op match {
      case UnaryOpType.Neg => {
        List(FinalIR.Rsb("", Status(), getOperand(t1), ImmediateInt(0), getRealReg(res)))
      }
      case UnaryOpType.Not => {
        FinalIR.Cmp("", getOperand(t1), new ImmediateInt(1)) ::
          FinalIR.Mov("ne", ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Mov("eq", ImmediateInt(0), getRealReg(res)) :: List()
      }
      case UnaryOpType.Chr | UnaryOpType.Ord => {
        List(FinalIR.Mov("", getOperand(t1), getRealReg(res)))
      }
      case UnaryOpType.Len => {
        List(FinalIR.Ldr("", getRealReg(t1.asInstanceOf[TRegister]), ImmediateInt(-POINTER_BYTE_SIZE), getRealReg(res)))
      }
    }
  }

  def assembleRead(datatype: DeclarationType, readReg: TRegister): List[FinalIR] = {
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
    addEndFunc("_errOverflow", new HelperFunctions().assemble_errOverflow())
    addEndFunc("_prints", new HelperFunctions().assemble_prints())
    addEndFunc(bl, new HelperFunctions().assemble_read(bl))
    FinalIR.BranchLink("", new BranchString(bl)) ::
      FinalIR.Mov("", r0, getRealReg(readReg)) :: List()
  }

  def assembleCall(lbl: Label, args: List[TRegister], dstReg: TRegister): List[FinalIR] = {
    var output = List[FinalIR]()
    // move all the args in to arg registers
    args.slice(0, args.length.min(argRegs.length)).zip(argRegs).foreach {
      case (arg, reg) => output = output ++ List(FinalIR.Mov("", getRealReg(arg), reg))
    }
    // push extra args into memory
    if (args.length > argRegs.length) {
      args.slice(POINTER_BYTE_SIZE, args.length).reverse.foreach(reg => {
        output = output ++ List(FinalIR.StrPre("", sp, ImmediateInt(-POINTER_BYTE_SIZE), getRealReg(reg)))
      })
    }
    output = output ++ List(FinalIR.BranchLink("", BranchString(lbl.name)))

    /* Decrement the stack pointer for each argument pushed to the stack */
    if (args.length > argRegs.length)
      output = output ++ List(FinalIR.Sub("", None(), sp, ImmediateInt(POINTER_BYTE_SIZE * (argRegs.length - args.length)), sp))

    // move the result into dst before r0 is popped back
    output ++ List(FinalIR.Mov("", r0, getRealReg(dstReg)))
  }

  def assembleGetPairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, dstReg: TRegister): List[FinalIR] = {
    addEndFunc("_errNull", new HelperFunctions().assemble_errNull())
    addEndFunc("_prints", new HelperFunctions().assemble_prints())

    FinalIR.Cmp("", getRealReg(pairReg), ImmediateInt(0)) ::
      FinalIR.BranchLink("eq", new BranchString("_errNull")) ::
      FinalIR.Ldr("", getRealReg(pairReg), ImmediateInt(if (pairPos == PairElemT.Fst) 0 else POINTER_BYTE_SIZE), getRealReg(dstReg)) ::
      FinalIR.Push("", List(getRealReg(pairReg))) ::
      FinalIR.Mov("", getRealReg(dstReg), getRealReg(pairReg)) ::
      FinalIR.Ldr(getLdrInstructionType(datatype), getRealReg(pairReg), ImmediateInt(0), getRealReg(dstReg)) ::
      FinalIR.Pop("", List(getRealReg(pairReg))) :: List()
  }

  def assembleStorePairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, srcReg: TRegister): List[FinalIR] = {
    addEndFunc("_errNull", new HelperFunctions().assemble_errNull())
    addEndFunc("_prints", new HelperFunctions().assemble_prints())

    FinalIR.Cmp("", getRealReg(pairReg), ImmediateInt(0)) ::
      FinalIR.BranchLink("eq", BranchString("_errNull")) ::
      FinalIR.Push("", List(getRealReg(pairReg))) ::
      FinalIR.Ldr("", getRealReg(pairReg), ImmediateInt(if (pairPos == PairElemT.Fst) 0 else POINTER_BYTE_SIZE), getRealReg(pairReg)) ::
      FinalIR.Str(getInstructionType(datatype), getRealReg(pairReg), ImmediateInt(0), getRealReg(srcReg)) ::
      FinalIR.Pop("", List(getRealReg(pairReg))) :: List()
  }

  // Returns tuple containing the main program and helper functions
  def assembleProgram(tacList: List[TAC]): (List[FinalIR], collection.mutable.Map[String, List[FinalIR]]) = {
    val (spilledCode, colouring) = allocationScheme.allocateRegisters
    this.colouring = colouring
    val finalCode = cfgutils.StackAssignment(spilledCode.toList)

    (state.code.toList, endFuncs.map(elem => elem match {
      case (name, state) => (name, state.code.toList)
    }))

    (finalCode.map(assembleTAC).flatten, endFuncs)
  }

  def assembleJump(label: Label): List[FinalIR] = {
    List(FinalIR.Branch("", label.name))
  }

  def assembleIf(t1: Operand, goto: Label): List[FinalIR] = {
    FinalIR.Cmp("", getOperand(t1), new ImmediateInt(1)) ::
      FinalIR.Branch("eq", goto.name) :: List()
  }

  def assemblePopParam(dataType: DeclarationType, treg: TRegister, index: Int): List[FinalIR] = {
    val cRegs = List(r0, r1, r2, r3)
    val funcStackFrameSize = 8 // Stack frame consists of {fp, lr}.

    if (index < cRegs.length) {
      // Populate from registers in r0-
      val callReg = cRegs.take(index + 1).last
      List(FinalIR.Mov("", getRealReg(treg), callReg))
    } else {
      // Populate from stack
      List(FinalIR.Ldr("", fp, ImmediateInt(funcStackFrameSize + (POINTER_BYTE_SIZE * (index - cRegs.size))), getRealReg(treg)))
    }
  }

  def assembleBinOp(operation: BinaryOpType.BinOp, op1: Operand, op2: Operand, res: TRegister): List[FinalIR] = {
    operation match {
      case BinaryOpType.Add => {
        List(FinalIR.Add("", Status(), getOperand(op1), getOperand(op2), getRealReg(res)))
      }
      case BinaryOpType.Sub => {
        List(FinalIR.Sub("", Status(), getOperand(op1), getOperand(op2), getRealReg(res)))
      }
      case BinaryOpType.Mul => {
        List(FinalIR.Smull("", Status(), getRealReg(res), getOperand(op1), getOperand(op1), getOperand(op2)))
      }
      case BinaryOpType.Div => {
        addEndFunc("_errDivZero", new HelperFunctions().assemble_errDivZero())
        addEndFunc("_prints", new HelperFunctions().assemble_print("_prints"))
        FinalIR.Mov("", getOperand(op1), r0) ::
          FinalIR.Mov("", getOperand(op2), r1) ::
          FinalIR.Cmp("", r1, new ImmediateInt(0)) ::
          FinalIR.BranchLink("eq", new BranchString("_errDivZero")) ::
          FinalIR.BranchLink("", new BranchString("__aeabi_idivmod")) ::
          FinalIR.Mov("", r0, getRealReg(res)) :: List()
      }
      case BinaryOpType.Mod => {
        addEndFunc("_errDivZero", new HelperFunctions().assemble_errDivZero())
        addEndFunc("_prints", new HelperFunctions().assemble_print("_prints"))
        FinalIR.Mov("", getOperand(op1), r0) ::
          FinalIR.Mov("", getOperand(op2), r1) ::
          FinalIR.Cmp("", r1, ImmediateInt(0)) ::
          FinalIR.BranchLink("eq", BranchString("_errDivZero")) ::
          FinalIR.BranchLink("", BranchString("__aeabi_idivmod")) ::
          FinalIR.Mov("", r1, getRealReg(res)) :: List()
      }
      case BinaryOpType.Eq => {
        FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
          FinalIR.Mov("eq", ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Mov("ne", ImmediateInt(0), getRealReg(res)) :: List()
      }
      case BinaryOpType.Neq => {
        FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
          FinalIR.Mov("ne", ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Mov("eq", ImmediateInt(0), getRealReg(res)) :: List()
      }
      case BinaryOpType.Lt => {
        FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
          FinalIR.Mov("lt", ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Mov("ge", ImmediateInt(0), getRealReg(res)) :: List()
      }
      case BinaryOpType.Gt => {
        FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
          FinalIR.Mov("gt", ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Mov("le", ImmediateInt(0), getRealReg(res)) :: List()
      }
      case BinaryOpType.Lte => {
        FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
          FinalIR.Mov("le", ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Mov("gt", ImmediateInt(0), getRealReg(res)) :: List()
      }
      case BinaryOpType.Gte => {
        FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
          FinalIR.Mov("ge", ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Mov("lt", ImmediateInt(0), getRealReg(res)) :: List()
      }
      case BinaryOpType.And => {
        FinalIR.Cmp("", getOperand(op1), new ImmediateInt(1)) ::
          FinalIR.Cmp("eq", getOperand(op2), new ImmediateInt(1)) ::
          FinalIR.Mov("ne", ImmediateInt(0), getRealReg(res)) ::
          FinalIR.Mov("eq", ImmediateInt(1), getRealReg(res)) :: List()
      }
      case BinaryOpType.Or => {
        FinalIR.Cmp("", getOperand(op1), new ImmediateInt(1)) ::
          FinalIR.Mov("eq", ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Cmp("ne", getOperand(op2), new ImmediateInt(1)) ::
          FinalIR.Mov("ne", ImmediateInt(0), getRealReg(res)) ::
          FinalIR.Mov("eq", ImmediateInt(1), getRealReg(res)) :: List()
      }
    }
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

  def assembleStringDef(str: String, lbl: Label): List[FinalIR] = {
    assembleTAC(lbl) ++
      (FinalIR.AsciiZ(escape(str)) :: List())
  }

  def assembleBeginFunc(): List[FinalIR] = {
    FinalIR.Push("", List(fp, lr)) ::
      FinalIR.Mov("", sp, fp) :: List()
  }

  def assembleEndFunc(): List[FinalIR] = {
    FinalIR.Mov("", new ImmediateInt(0), r0) ::
      FinalIR.Mov("", fp, sp) ::
      FinalIR.Pop("", List(fp, pc)) ::
      FinalIR.Special(".ltorg") :: List() 
  }

  def assembleAssignment(operand: Operand, reg: TRegister): List[FinalIR] = {
    operand match {
      case Label(name) => List(FinalIR.Ldr("", r0, getOperand(operand), getRealReg(reg)))
      case _ => List(FinalIR.Mov("", getOperand(operand), getRealReg(reg)))
    }
  }

  def assembleCommand(cmd: CmdT.Cmd, operand: Operand, opType: DeclarationType): List[FinalIR] = {
    cmd match {
      case CmdT.Exit => {
        FinalIR.Mov("", getOperand(operand), r0) ::
          FinalIR.BranchLink("", new BranchString("exit")) :: List()
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
          FinalIR.Mov("", getOperand(operand), r0) ::
            FinalIR.BranchLink("", new BranchString(bl)) ::
            FinalIR.BranchLink("", new BranchString("_println")) :: List()
        }
        else {
          FinalIR.Mov("", getOperand(operand), r0) ::
            FinalIR.BranchLink("", new BranchString(bl)) :: List()
        }
      }
      case CmdT.Ret => {
        FinalIR.Mov("", getOperand(operand), r0) ::
          FinalIR.Mov("", fp, sp) ::
          FinalIR.Pop("", List(fp, pc)) :: List()
      }
      case CmdT.Free => {
        opType match {
          case ArrayType(dataType, length) => {
            FinalIR.Sub("", Status(), r4, new ImmediateInt(POINTER_BYTE_SIZE), r8) ::
              FinalIR.Push("", List(r8)) ::
              FinalIR.Pop("", List(r8)) ::
              FinalIR.Mov("", r8, r8) ::
              FinalIR.Mov("", r8, r0) ::
              FinalIR.BranchLink("", new BranchString("free")) :: List()
          }
          case PairType(fstType, sndType) => {
            addEndFunc("_freepair", new HelperFunctions().assemble_freepair())
            addEndFunc("_errNull", new HelperFunctions().assemble_errNull())
            addEndFunc("_prints", new HelperFunctions().assemble_prints())

            FinalIR.Mov("", getOperand(operand), r0) ::
              FinalIR.BranchLink("", new BranchString("_freepair")) :: List()
          }

        }
      }
    }
  }

  def assembleArrayInit(arrLen: Int, lenReg: TRegister, dstReg: TRegister): List[FinalIR] = {
    FinalIR.Push("", List(r0)) ::
      FinalIR.Mov("", new ImmediateInt(POINTER_BYTE_SIZE * (arrLen + 1)), r0) ::
      FinalIR.BranchLink("", new BranchString("malloc")) ::
      FinalIR.Mov("", r0, getRealReg(dstReg)) ::
      FinalIR.Pop("", List(r0)) ::
      FinalIR.Add("", Status(), getRealReg(dstReg), new ImmediateInt(POINTER_BYTE_SIZE), getRealReg(dstReg)) ::
      FinalIR.Mov("", new ImmediateInt(arrLen), getRealReg(lenReg)) ::
      FinalIR.Str("", getRealReg(lenReg), new ImmediateInt(-POINTER_BYTE_SIZE), getRealReg(dstReg)) :: List()
  }

  def assembleArray(arrayElemType: DeclarationType, elemsReg: List[TRegister], dstReg: TRegister): List[FinalIR] = {
    List[FinalIR]()
  }

  def assembleArrayElem(arrayElemType: DeclarationType, elemPos: Int, arrReg: TRegister, elemReg: TRegister): List[FinalIR] = {
    FinalIR.Push("", List(getRealReg(elemReg))) ::
      FinalIR.Push("", List(r0)) ::
      FinalIR.Mov("", ImmediateInt(getTypeSize(arrayElemType)), r0) ::
      FinalIR.BranchLink("", new BranchString("malloc")) ::
      FinalIR.Str("", r0, ImmediateInt(0), getRealReg(elemReg)) ::
      FinalIR.Str(getInstructionType(arrayElemType), getRealReg(arrReg), new ImmediateInt(POINTER_BYTE_SIZE * elemPos), getRealReg(elemReg)) ::
      FinalIR.Mov("", r0, getRealReg(elemReg)) ::
      FinalIR.Pop("", List(r0)) ::
      FinalIR.Pop("", List(getRealReg(elemReg))) :: List()
  }

  def assembleLoadArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: List[TRegister], dstReg: TRegister): List[FinalIR] = {
    addEndFunc("_arrLoad", new HelperFunctions().assemble_arrLoad())
    addEndFunc("_boundsCheck", new HelperFunctions().assemble_boundsCheck())
    var regs = List(getRealReg(arrReg), getRealReg(dstReg))
    regs = (regs ++ arrPos.map(a => getRealReg(a))).distinct.sortWith((s, t) => s < t)
    var output = List[FinalIR]()
    output = output ++
      (FinalIR.Push("", regs) ::
        FinalIR.Push("", List(r0, r1, r2, r3)) :: List())
    arrPos.foreach(a => {
      output = output ++
        (FinalIR.Mov("", r2, getRealReg(a)) ::
          FinalIR.Mov("", r3, getRealReg(arrReg)) :: // arrLoad uses r0 = r3[r2]
          FinalIR.BranchLink("", new BranchString("_arrLoad")) ::
          FinalIR.Mov("", getRealReg(dstReg), r0) :: List())
    })
    output ++
      (FinalIR.Pop("", List(r0, r1, r2, r3)) ::
        FinalIR.Pop("", regs) :: List())
  }

  def assembleStoreArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: List[TRegister], srcReg: TRegister): List[FinalIR] = {
    addEndFunc("_arrStore", new HelperFunctions().assemble_arrStore())
    addEndFunc("_boundsCheck", new HelperFunctions().assemble_boundsCheck())

    var regs = List(getRealReg(arrReg), getRealReg(srcReg))
    regs = (regs ++ arrPos.map(a => getRealReg(a))).distinct.sortWith((s, t) => s < t)
    var output = List[FinalIR]()
    output = output ++
      (FinalIR.Push("", regs) ::
        FinalIR.Push("", List(r0, r1, r2, r3)) :: List())
    arrPos.foreach(a => {
      output = output ++
        (FinalIR.Mov("", r2, getRealReg(srcReg)) ::
          FinalIR.Mov("", r0, getRealReg(a)) ::
          FinalIR.Mov("", r3, getRealReg(arrReg)) :: // arrStore uses r3[r0] = r2
          FinalIR.BranchLink("", new BranchString("_arrStore")) :: List())
    })
    output ++
      (FinalIR.Pop("", List(r0, r1, r2, r3)) ::
        FinalIR.Pop("", regs) :: List())
  }
}