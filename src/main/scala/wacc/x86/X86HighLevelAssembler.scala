// package wacc

// import wacc.AbstractSyntaxTree.BaseT.Char_T
// import wacc.AbstractSyntaxTree._
// import wacc.X86AssemblerTypes.{X86ImmediateInt}
// import wacc.AssemblerTypes.{Register, LHSop}
// import wacc.RegisterAllocator._
// import wacc.TAC._
// import wacc.cfgutils.{Colouring, RegisterAllocator}
// import wacc.FinalIR.FinalIR // TODO: change this to not import everything
// import wacc.X86HelperFunctions

// import scala.collection.mutable.ListBuffer

// //TODO Change all to x86_64 Architecture

// object X86StatelessAssembler {
//   def pushPopAssist(condition: String, registers: List[Register]): String = {
//     var str = condition + " {"
//     for (register <- registers) {
//       if (register != registers.last) {
//         str = str + register.toString + ", "
//       } else {
//         str = str + register.toString
//       }
//     }
//     str + "}"
//   }

//   def assemblePush(condition: String, registers: List[Register]): FinalIR = {
//     FinalIR.Push(condition, registers)
//   }

//   def assembleLdr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): FinalIR = {
//     FinalIR.Ldr(condition, sourceRegister, operand, destinationRegister)
//   }

//   def assembleStr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): FinalIR = {
//     FinalIR.Str(condition, sourceRegister, operand, destinationRegister)
//   }

//   def assembleAdd(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: LHSop, operand: LHSop): FinalIR = {
//     FinalIR.Add(condition, setflag, sourceRegister, operand, destinationRegister)
//   }

//   def assembleSub(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: LHSop, operand: LHSop): FinalIR = {
//     FinalIR.Sub(condition, setflag, sourceRegister, operand, destinationRegister)
//   }

// }

// class x86HighLevelAssembler(allocationScheme: RegisterAllocator[Register]) {
//   var colouring: Colouring[Register] = null
//   private[this] val state = new AssemblerState(ListBuffer(rcx, r8, r9, r10, r11, r12, r13, r14, r15))
//   val endFuncs = collection.mutable.Map[String, List[FinalIR]]()
//   var labelCount = 0
//   val argRegs = List(rax, rdi, rsi, rdx)
//   val POINTER_BYTE_SIZE = 4

//   // Add predefined function to end of assembly code (.e.g _prints)
//   def addEndFunc(name: String, code: List[FinalIR]): Unit = {
//     if (!endFuncs.contains(name)) {
//       endFuncs.addOne(name, code)
//     }
//   }

//   def getRealReg(t: TRegister) = colouring.coloured(t)

//   implicit private[this] def updateState(instr: FinalIR): AssemblerState = {
//     state.addInstruction(instr)
//   }

//   implicit private[this] def updateState(instrs: List[FinalIR]): AssemblerState = {
//     state.addInstructions(instrs)
//   }

//   def determineLdr(x: Int): Boolean = {
//     if (x <= 255) {
//       return false
//     } else {
//       for (i <- 1 to 16) {
//         if ((x >> i) << i == x) {
//           return false
//         }
//       }
//       return true
//     }
//   }

//   // Get correct operand type from TAC
//   def getOperand(op: Operand): LHSop = {
//     op match {
//       case reg: TRegister => getRealReg(reg)
//       case IntLiteralTAC(value) => new X86ImmediateInt(value)
//       case CharLiteralTAC(chr) => new X86ImmediateInt(chr.toInt)
//       case BoolLiteralTAC(b) => new X86ImmediateInt(b.compare(true) + 1)
//       case Label(name) => X86LabelString(name)
//       case PairLiteralTAC() => new X86ImmediateInt(0)
//       case ArrayOp(_) => new X86ImmediateInt(0)
//       case a => println("getOperand fail: " + a); null 
//     }
//   }

//   // Convert TAC into next IR
//   def assembleTAC(tripleAddressCode: TAC): List[FinalIR] = {
//     tripleAddressCode match {
//       case Global(name) => {
//         List(FinalIR.Global(name))
//       }
//       case Label(name) => {
//         state.enterBranch // TODO: check state here
//         List(FinalIR.Lbl(name))
//       }
//       case Comments(str) => FinalIR.Comment(str)
//       case DataSegmentTAC() => FinalIR.DataSeg()
//       case TextSegmentTAC() => FinalIR.TextSeg()
//       case StringLengthDefinitionTAC(len, _) => FinalIR.Word(len)
//       case StringDefinitionTAC(str, lbl) => assembleStringDef(str, lbl)
//       case BeginFuncTAC() => {
//         assembleBeginFunc()
//       }
//       case EndFuncTAC() => {
//         assembleEndFunc()
//       }
//       case AssignmentTAC(operand, reg) => assembleAssignment(operand, reg)
//       case CommandTAC(cmd, operand, opType) => assembleCommand(cmd, operand, opType)
//       case BinaryOpTAC(operation, op1, op2, res) => assembleBinOp(operation, op1, op2, res)
//       case IfTAC(t1, goto) => assembleIf(t1, goto)
//       case GOTO(label) => assembleJump(label)
//       case CreatePairElem(pairElemType, pairPos, ptrReg, pairElemReg) => assemblePairElem(pairElemType, pairPos, ptrReg, pairElemReg)
//       case CreatePair(fstType, sndType, fstReg, sndReg, srcReg, ptrReg, dstReg) => assemblePair(fstType, sndType, fstReg, sndReg, srcReg, ptrReg, dstReg)
//       case GetPairElem(datatype, pairReg, pairPos, dstReg) => assembleGetPairElem(datatype, pairReg, pairPos, dstReg)
//       case StorePairElem(datatype, pairReg, pairPos, srcReg) => assembleStorePairElem(datatype, pairReg, pairPos, srcReg)
//       case InitialiseArray(arrLen, lenReg, dstReg) => assembleArrayInit(arrLen, lenReg, dstReg)
//       case CreateArrayElem(arrayElemType, elemPos, arrReg, elemReg) => assembleArrayElem(arrayElemType, elemPos, arrReg, elemReg)
//       case CreateArray(arrayElemType, elemsReg, dstReg) => assembleArray(arrayElemType, elemsReg, dstReg)
//       case LoadArrayElem(datatype, arrReg, arrPos, dstReg) => assembleLoadArrayElem(datatype, arrReg, arrPos, dstReg)
//       case StoreArrayElem(datatype, arrReg, arrPos, srcReg) => assembleStoreArrayElem(datatype, arrReg, arrPos, srcReg)
//       case UnaryOpTAC(op, t1, res) => assembleUnaryOp(op, t1, res)
//       case CallTAC(lbl, args, dstReg) => assembleCall(lbl, args, dstReg)
//       case PopParamTAC(datatype, treg, index) => assemblePopParam(datatype, treg, index)
//       case PushParamTAC(op) => null
//       case ReadTAC(dataType, readReg) => assembleRead(dataType, readReg)
//       case ReservedPushTAC(reg, location, _) => List(FinalIR.Str("", getRealReg(reg), ImmediateInt(location * POINTER_BYTE_SIZE), fp))
//       case ReservedPopTAC(location, reg, _) => List(FinalIR.Ldr("", getRealReg(reg), ImmediateInt(location * POINTER_BYTE_SIZE), fp))
//       case AllocateStackTAC(size) => List(FinalIR.Sub("", AssemblerTypes.None(), sp, ImmediateInt(size), sp))
//     }
//   }

//   def getInstructionType(decType: DeclarationType): String = {
//     decType match {
//       case BaseType(BaseT.Char_T) => "b"
//       case _ => ""
//     }
//   }

//   def getLdrInstructionType(decType: DeclarationType): String = {
//     decType match {
//       case BaseType(BaseT.Char_T) => "sb"
//       case _ => ""
//     }
//   }

//   def getTypeSize(decType: DeclarationType): Int = {
//     decType match {
//       case BaseType(BaseT.Int_T) => 4
//       case BaseType(BaseT.Char_T) => 1
//       case _ => 4
//     }
//   }

//   def assemblePair(fstType: DeclarationType, sndType: DeclarationType, fstReg: TRegister, sndReg: TRegister, srcReg: TRegister, ptrReg: TRegister, dstReg: TRegister): List[FinalIR] = {
//     FinalIR.Pop("", List(getRealReg(fstReg))) ::
//       FinalIR.Pop("", List(getRealReg(sndReg))) ::
//       FinalIR.Push("", List(rax)) :: 
//       FinalIR.Mov("", new X86ImmediateInt(2 * POINTER_BYTE_SIZE), rax) ::
//       FinalIR.BranchLink("", X86BranchString("malloc")) ::
//       FinalIR.Mov("", rax, getRealReg(ptrReg)) ::
//       FinalIR.Pop("", List(rax)) :: 
//       FinalIR.Str("", getRealReg(ptrReg), new X86ImmediateInt(POINTER_BYTE_SIZE), getRealReg(fstReg)) ::
//       FinalIR.Str("", getRealReg(ptrReg), new X86ImmediateInt(0), getRealReg(sndReg)) ::
//       FinalIR.Mov("", getRealReg(ptrReg), getRealReg(dstReg)) :: List()
//   }

//   def assemblePairElem(pairElemType: DeclarationType, pairPos: PairElemT.Elem, ptrReg: TRegister, pairElem: TRegister): List[FinalIR] = {
//     FinalIR.Mov("", new X86ImmediateInt(getTypeSize(pairElemType)), rax) ::
//       FinalIR.BranchLink("", X86BranchString("malloc")) ::
//       FinalIR.Push("", List(getRealReg((ptrReg)))) ::
//       FinalIR.Mov("", getRealReg(ptrReg), rax) ::
//       FinalIR.Str(getInstructionType(pairElemType), getRealReg(ptrReg), new X86ImmediateInt(0), getRealReg(pairElem)) ::
//       FinalIR.Mov("", getRealReg(pairElem), rax) ::
//       FinalIR.Mov("", getRealReg(ptrReg), getRealReg(pairElem)) ::
//       FinalIR.Pop("", List(getRealReg(ptrReg))) ::
//       FinalIR.Push("", List(getRealReg(pairElem))) ::
//       FinalIR.Mov("", rax, getRealReg(pairElem)) :: List()
//   }

//   def assembleUnaryOp(op: UnaryOpType.UnOp, t1: Operand, res: TRegister): List[FinalIR] = {
//     op match {
//       case UnaryOpType.Neg => {
//         FinalIR.Rsb("", X86Status(), getOperand(t1), new X86ImmediateInt(0), getRealReg(res))
//       }
//       case UnaryOpType.Not => {
//         FinalIR.Cmp("", getOperand(t1), new X86ImmediateInt(1)) ::
//           FinalIR.Mov("ne", new X86ImmediateInt(1), getRealReg(res)) ::
//           FinalIR.Mov("e", new X86ImmediateInt(0), getRealReg(res)) :: List()
//       }
//       case UnaryOpType.Chr | UnaryOpType.Ord => {
//         FinalIR.Mov("", getOperand(t1), getRealReg(res))
//       }
//       case UnaryOpType.Len => {
//         FinalIR.Ldr("", getRealReg(t1.asInstanceOf[TRegister]), new X86ImmediateInt(-POINTER_BYTE_SIZE), getRealReg(res))
//       }
//     }
//   }

//   def assembleRead(datatype: DeclarationType, readReg: TRegister): List[FinalIR] = {
//     val bl = datatype match {
//       case BaseType(baseType) => {
//         baseType match {
//           case BaseT.Int_T => "_readi"
//           case BaseT.Char_T => "_readc"
//           case BaseT.String_T => "_reads"
//           case BaseT.Bool_T => "_readb"
//           case _ => "_readi"
//         }
//       }
//       case _ => "_readi"
//     }
//     addEndFunc("_errOverflow", new X86HelperFunctions().assemble_errOverflow())
//     addEndFunc("_prints", new X86HelperFunctions().assemble_prints())
//     addEndFunc(bl, new X86HelperFunctions().assemble_read(bl))
//     FinalIR.BranchLink("", X86BranchString(bl)) ::
//       FinalIR.Mov("", rax, getRealReg(readReg)) :: List()
//   }

//   def assembleCall(lbl: Label, args: List[TRegister], dstReg: TRegister): List[FinalIR] = {
//     var output = List[FinalIR]()
//     // move all the args in to arg registers
//     args.slice(0, args.length.min(argRegs.length)).zip(argRegs).foreach {
//       case (arg, reg) => output = output ++ FinalIR.Mov("", getRealReg(arg), reg)
//     }
//     // push extra args into memory
//     if (args.length > argRegs.length) {
//       args.slice(POINTER_BYTE_SIZE, args.length).reverse.foreach(reg => {
//         output = output ++ FinalIR.StrPre("", rsp, new X86ImmediateInt(-POINTER_BYTE_SIZE), getRealReg(reg))
//       })
//     }
//     output = output ++ FinalIR.BranchLink("", X86BranchString(lbl.name))

//     /* Decrement the stack pointer for each argument pushed to the stack */
//     if (args.length > argRegs.length)
//       output = output ++ FinalIR.Sub("", X86None(), rsp, new X86ImmediateInt(POINTER_BYTE_SIZE * (argRegs.length - args.length)), rsp)

//     // move the result into dst before rax is popped back
//     output ++ FinalIR.Mov("", rax, getRealReg(dstReg))
//   }

//   def assembleGetPairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, dstReg: TRegister): List[FinalIR] = {
//     addEndFunc("_errNull", new X86HelperFunctions().assemble_errNull())
//     addEndFunc("_prints", new X86HelperFunctions().assemble_prints())

//     FinalIR.Cmp("", getRealReg(pairReg), new X86ImmediateInt(0)) ::
//       FinalIR.BranchLink("", X86BranchString("_errNull")) ::
//       FinalIR.Ldr("", getRealReg(pairReg), new X86ImmediateInt(if (pairPos == PairElemT.Fst) 0 else POINTER_BYTE_SIZE),  getRealReg(dstReg)) ::
//       FinalIR.Push("", List(getRealReg(pairReg))) ::
//       FinalIR.Mov("", getRealReg(dstReg), getRealReg(pairReg)) ::
//       FinalIR.Ldr(getLdrInstructionType(datatype), getRealReg(pairReg), new X86ImmediateInt(0), getRealReg(dstReg)) ::
//       FinalIR.Pop("", List(getRealReg(pairReg))) :: List()
//   }

//   def assembleStorePairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, srcReg: TRegister): List[FinalIR] = {
//     addEndFunc("_errNull", new X86HelperFunctions().assemble_errNull())
//     addEndFunc("_prints", new X86HelperFunctions().assemble_prints())

//     FinalIR.Cmp("", getRealReg(pairReg), new X86ImmediateInt(0)) ::
//       FinalIR.BranchLink("", X86BranchString("_errNull")) ::
//       FinalIR.Push("", List(getRealReg(pairReg))) ::
//       FinalIR.Ldr("", getRealReg(pairReg), new X86ImmediateInt(if (pairPos == PairElemT.Fst) 0 else POINTER_BYTE_SIZE),  getRealReg(pairReg)) ::
//       FinalIR.Str(getInstructionType(datatype), getRealReg(pairReg), new X86ImmediateInt(0), getRealReg(srcReg)) ::
//       FinalIR.Pop("", List(getRealReg(pairReg))) :: List()
//   }

//   // Returns tuple containing the main program and helper functions
//   def assembleProgram(tacList: List[TAC]): (List[FinalIR], collection.mutable.Map[String, List[FinalIR]]) = {
//     val (spilledCode, colouring) = allocationScheme.allocateRegisters
//     this.colouring = colouring
//     val finalCode = Global("main") :: cfgutils.StackAssignment(spilledCode.toList)

//     finalCode.map(assembleTAC)
//     (state.code.toList, endFuncs.map(elem => elem match {
//       case (name, state) => (name, state.code.toList)
//     }))
//   }

//   def assembleJump(label: Label): List[FinalIR] = {
//     FinalIR.Branch("", label.name)
//   }

//   def assembleIf(t1: Operand, goto: Label): List[FinalIR] = {
//     FinalIR.Cmp("", getOperand(t1), new X86ImmediateInt(1)) ::
//       FinalIR.Branch("e", goto.name) :: List()
//   }

//   def assemblePopParam(dataType: DeclarationType, treg: TRegister, index: Int): List[FinalIR] = {
//     val cRegs = List(rax, rdi, rsi, rdx)
//     val funcStackFrameSize = 8 // Stack frame consists of {rbp, rbx}.

//     if (index < cRegs.length) {
//       // Populate from registers in rax-
//       val callReg = cRegs.take(index + 1).last
//       FinalIR.Mov("", getRealReg(treg), callReg)
//     } else {
//       // Populate from stack
//       FinalIR.Ldr("", rbp, new X86ImmediateInt(funcStackFrameSize + (POINTER_BYTE_SIZE * (index - cRegs.size))), getRealReg(treg))
//     }
//   }

//   def assembleBinOp(operation: BinaryOpType.BinOp, op1: Operand, op2: Operand, res: TRegister): List[FinalIR] = {
//     operation match {
//       case BinaryOpType.Add => {
//         List(FinalIR.Add("", X86Status(), getOperand(op1), getOperand(op2), getRealReg(res)))
//       }
//       case BinaryOpType.Sub => {
//         List(FinalIR.Sub("", X86Status(), getOperand(op1), getOperand(op2),  getRealReg(res)))
//       }
//       case BinaryOpType.Mul => {
//         List(FinalIR.Smull("", X86Status(), getOperand(res), getOperand(op1), getOperand(op1), getRealReg(op2)))
//       }
//       case BinaryOpType.Div => {
//         addEndFunc("_errDivZero", new X86HelperFunctions().assemble_errDivZero())
//         addEndFunc("_prints", new X86HelperFunctions().assemble_print("_prints"))
//         FinalIR.Mov("", getOperand(op1), rax) ::
//           FinalIR.Mov("", getOperand(op2), rdi) ::
//           FinalIR.Cmp("", rdi, new X86ImmediateInt(0)) ::
//           FinalIR.BranchLink("e", X86BranchString("_errDivZero")) ::
//           FinalIR.BranchLink("", X86BranchString("__aeabi_idivmod")) ::
//           FinalIR.Mov("", rax, getRealReg(res)) :: List()
//         }
//       case BinaryOpType.Mod => {
//         addEndFunc("_errDivZero", new X86HelperFunctions().assemble_errDivZero())
//         addEndFunc("_prints", new X86HelperFunctions().assemble_print("_prints"))
//         FinalIR.Mov("", getOperand(op1), rax) ::
//           FinalIR.Mov("", getOperand(op2), rdi) ::
//           FinalIR.Cmp("", rdi, new X86ImmediateInt(0)) ::
//           FinalIR.BranchLink("e", X86BranchString("_errDivZero")) ::
//           FinalIR.BranchLink("", X86BranchString("__aeabi_idivmod")) ::
//           FinalIR.Mov("", rdi, getRealReg(res)) :: List()
//       }
//       case BinaryOpType.Eq => {
//         FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
//           FinalIR.Mov("e", new X86ImmediateInt(1), getRealReg(res)) ::
//           FinalIR.Mov("ne", new X86ImmediateInt(0), getRealReg(res)) :: List()
//       }
//       case BinaryOpType.Neq => {
//         FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
//           FinalIR.Mov("ne", new X86ImmediateInt(1), getRealReg(res)) ::
//           FinalIR.Mov("e", new X86ImmediateInt(0), getRealReg(res)) :: List()
//       }
//       case BinaryOpType.Lt => {
//         FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
//           FinalIR.Mov("lt", new X86ImmediateInt(1), getRealReg(res)) ::
//           FinalIR.Mov("ge", new X86ImmediateInt(0), getRealReg(res)) :: List()
//       }
//       case BinaryOpType.Gt => {
//         FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
//           FinalIR.Mov("gt", new X86ImmediateInt(1), getRealReg(res)) ::
//           FinalIR.Mov("le", new X86ImmediateInt(0), getRealReg(res)) :: List()
//       }
//       case BinaryOpType.Lte => {
//         FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
//           FinalIR.Mov("le", new X86ImmediateInt(1), getRealReg(res)) ::
//           FinalIR.Mov("gt", new X86ImmediateInt(0), getRealReg(res)) :: List()
//       }
//       case BinaryOpType.Gte => {
//         FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
//           FinalIR.Mov("ge", new X86ImmediateInt(1), getRealReg(res)) ::
//           FinalIR.Mov("lt", new X86ImmediateInt(0), getRealReg(res)) :: List()
//       }
//       case BinaryOpType.And => {
//         FinalIR.Cmp("", getOperand(op1), new X86ImmediateInt(1)) ::
//           FinalIR.Cmp("", getOperand(op2), new X86ImmediateInt(1)) ::
//           FinalIR.Mov("ne", new X86ImmediateInt(0), getRealReg(res)) ::
//           FinalIR.Mov("e", new X86ImmediateInt(1), getRealReg(res)) :: List()
//       }
//       case BinaryOpType.Or => {
//         FinalIR.Cmp("", getOperand(op1), new X86ImmediateInt(1)) ::
//           FinalIR.Mov("e", new X86ImmediateInt(1), getRealReg(res)) ::
//           FinalIR.Cmp("", getOperand(op2), new X86ImmediateInt(1)) ::
//           FinalIR.Mov("ne", new X86ImmediateInt(0), getRealReg(res)) ::
//           FinalIR.Mov("e", new X86ImmediateInt(1), getRealReg(res)) :: List()
//       }
//     }
//   }

//   def escape(s: String): String = "\"" + s.flatMap(escapeChar) + "\""

//   def escapeChar(ch: Char): String = ch match {
//     case '\b' => "\\b"
//     case '\t' => "\\t"
//     case '\n' => "\\n"
//     case '\f' => "\\f"
//     case '\r' => "\\r"
//     case '"' => "\\\""
//     case '\'' => "\\\'"
//     case '\\' => "\\\\"
//     case _ => {
//       if (ch.isControl) {
//         "\\0" + Integer.toOctalString(ch.toInt)
//       } else {
//         String.valueOf(ch)
//       }
//     }
//   }

//   def assembleStringDef(str: String, lbl: Label): List[FinalIR] = {
//     assembleTAC(lbl) ++
//       (FinalIR.AsciiZ(escape(str)))
//   }

//   def assembleBeginFunc(): List[FinalIR] = {
//     FinalIR.Push("", List(rbp, rbx)) ::
//       FinalIR.Mov("", rsp, rbp) :: List()
//   }

//   def assembleEndFunc(): List[FinalIR] = {
//     FinalIR.Mov("", new X86ImmediateInt(0), rax) ::
//       FinalIR.Pop("", List(rbp, rbx)) ::
//       FinalIR.Ret() :: List()
//   }

//   def assembleAssignment(operand: Operand, reg: TRegister): List[FinalIR] = {
//     operand match {
//       case Label(name) => FinalIR.Ldr("", rax, getOperand(operand), getRealReg(reg))
//       case _ => FinalIR.Mov("", getOperand(operand), getRealReg(reg))
//     }
//   }

//   def assembleCommand(cmd: CmdT.Cmd, operand: Operand, opType: DeclarationType): List[FinalIR] = {
//     cmd match {
//       case CmdT.Exit => {
//         state.deleteFunctionScope
//         FinalIR.Mov("", getOperand(operand), rax) ::
//           FinalIR.BranchLink("", X86BranchString("exit")) :: List()
//       }
//       case CmdT.Print | CmdT.PrintLn => {
//         val bl = opType match {
//           case BaseType(baseType) => baseType match {
//             case BaseT.String_T => "_prints"
//             case BaseT.Char_T => "_printc"
//             case BaseT.Bool_T => "_printb"
//             case BaseT.Int_T => "_printi"
//             case _ => "_printi"
//           }
//           case NestedPair() | PairType(_, _) => "_printp"
//           // Character arrays should be printed as strings, but all others should be printed as a pointer
//           case ArrayType(t, _) if !(t is BaseType(Char_T)) => "_printp"

//           case ArrayType(t, _) if t is BaseType(Char_T) => "_prints"
//         }
//         addEndFunc(bl, new X86HelperFunctions().assemble_print(bl))
//         if (cmd == CmdT.PrintLn) {
//           addEndFunc("_println", new X86HelperFunctions().assemble_print("_println"))
//           FinalIR.Mov("", getOperand(operand), rax) ::
//             FinalIR.BranchLink("", X86BranchString(bl)) ::
//             FinalIR.BranchLink("", X86BranchString("_println")) :: List()
//         }
//         else {
//           FinalIR.Mov("", getOperand(operand), rax) ::
//             FinalIR.BranchLink("", X86BranchString(bl)) :: List()
//         }
//       }
//       case CmdT.Ret => {
//         state.exitFunction
//         FinalIR.Mov("", getOperand(operand), rax) ::
//           FinalIR.Mov("", rbp, rsp) ::
//           FinalIR.Pop("", List(rbp, rbx)) ::
//           FinalIR.Special(".ltorg") :: List() // TODO: examine use of special
//       }
//       case CmdT.Free => {
//         opType match {
//           case ArrayType(dataType, length) => {
//             // why is r4/rcx here
//             FinalIR.Sub("", X86Status(), rcx, new X86ImmediateInt(POINTER_BYTE_SIZE), r8) ::
//               FinalIR.Push("", List(r8)) ::
//               FinalIR.Pop("", List(r8)) ::
//               FinalIR.Mov("", r8, r8) ::
//               FinalIR.Mov("", r8, rax) ::
//               FinalIR.BranchLink("", X86BranchString("free")) :: List()
//           }
//           case PairType(fstType, sndType) => {
//             addEndFunc("_freepair", new X86HelperFunctions().assemble_freepair())
//             addEndFunc("_errNull", new X86HelperFunctions().assemble_errNull())
//             addEndFunc("_prints", new X86HelperFunctions().assemble_prints())

//             FinalIR.Mov("", getOperand(operand), rax) ::
//               FinalIR.BranchLink("", X86BranchString("_freepair")) :: List()
//           }

//         }
//       }
//     }
//   }

//   def assembleArrayInit(arrLen: Int, lenReg: TRegister, dstReg: TRegister): List[FinalIR] = {
//     addEndFunc("_malloc", new X86HelperFunctions().assemble_malloc())
//     FinalIR.Push("", List(rax)) ::
//       FinalIR.Mov("", new X86ImmediateInt(POINTER_BYTE_SIZE * (arrLen + 1)), rax) ::
//       FinalIR.BranchLink("", X86BranchString("_malloc")) ::
//       FinalIR.Mov("", rax, getRealReg(dstReg)) ::
//       FinalIR.Pop("", List(rax)) ::
//       FinalIR.Add("", X86Status(), getRealReg(dstReg), new X86ImmediateInt(POINTER_BYTE_SIZE), getRealReg(dstReg)) ::
//       FinalIR.Mov("", new X86ImmediateInt(arrLen), getRealReg(lenReg)) ::
//       FinalIR.Str("", getRealReg(lenReg), new X86ImmediateInt(-POINTER_BYTE_SIZE), getRealReg(dstReg)) :: List()
//   }

//   def assembleArray(arrayElemType: DeclarationType, elemsReg: List[TRegister], dstReg: TRegister): List[FinalIR] = {
//     List[FinalIR]()
//   }

//   def assembleArrayElem(arrayElemType: DeclarationType, elemPos: Int, arrReg: TRegister, elemReg: TRegister): List[FinalIR] = {
//     FinalIR.Push("", List(getRealReg(elemReg))) ::
//       FinalIR.Push("", List(rax)) ::
//       FinalIR.Mov("", new X86ImmediateInt(getTypeSize(arrayElemType)), rax) ::
//       FinalIR.BranchLink("", X86BranchString("malloc")) ::
//       FinalIR.Str("", rax, new X86ImmediateInt(0), getRealReg(elemReg)) ::
//       FinalIR.Str(getInstructionType(arrayElemType), getRealReg(arrReg), new X86ImmediateInt(POINTER_BYTE_SIZE * elemPos), getRealReg(elemReg)) ::
//       FinalIR.Mov("", rax, getRealReg(elemReg)) ::
//       FinalIR.Pop("", List(rax)) ::
//       FinalIR.Pop("", List(getRealReg(elemReg)) :: List())
//   }

//   def assembleLoadArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: List[TRegister], dstReg: TRegister): List[FinalIR] = {
//     addEndFunc("_arrLoad", new X86HelperFunctions().assemble_arrLoad())
//     addEndFunc("_boundsCheck", new X86HelperFunctions().assemble_boundsCheck())
//     var regs = List(getRealReg(arrReg), getRealReg(dstReg))
//     regs = (regs ++ arrPos.map(a => getRealReg(a))).distinct.sortWith((s, t) => s < t)
//     var output = List[FinalIR]()
//     output = output ++
//       (FinalIR.Push("", regs) ::
//         FinalIR.Push("", List(rax, rdi, rsi, rdx)) :: List())
//     arrPos.foreach(a => {
//       output = output ++ 
//       (FinalIR.Mov("", rsi, getRealReg(a)) ::
//         FinalIR.Mov("", rdx, getRealReg(arrReg)) :: // arrLoad uses rax = rdx[rsi]
//         FinalIR.BranchLink("", X86BranchString("_arrLoad")) ::
//         FinalIR.Mov("", getRealReg(dstReg), rax) :: List())
//     })
//     output ++
//     (FinalIR.Pop("", List(rax, rdi, rsi, rdx)) ::
//       FinalIR.Pop("", regs) :: List())
//   }

//   def assembleStoreArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: List[TRegister], srcReg: TRegister): List[FinalIR] = {
//     addEndFunc("_arrStore", new X86HelperFunctions().assemble_arrStore())
//     addEndFunc("_boundsCheck", new X86HelperFunctions().assemble_boundsCheck())
    
//     var regs = List(getRealReg(arrReg), getRealReg(srcReg))
//     regs = (regs ++ arrPos.map(a => getRealReg(a))).distinct.sortWith((s, t) => s < t)
//     var output = List[FinalIR]()
//     output = output ++
//       (FinalIR.Push("", regs) ::
//         FinalIR.Push("", List(rax, rdi, rsi, rdx)))
//     arrPos.foreach(a => {
//       output = output ++
//       (FinalIR.Mov("", rsi, getRealReg(srcReg)) ::
//         FinalIR.Mov("", rax, getRealReg(a)) ::
//         FinalIR.Mov("", rdx, getRealReg(arrReg)) :: // arrStore uses rdx[rax] = rsi
//         FinalIR.BranchLink("", X86BranchString("_arrStore")) :: List())
//     })
//     output ++
//     (FinalIR.Pop("", List(rax, rdi, rsi, rdx)) ::
//       FinalIR.Pop("", regs) :: List())
//   }
// }