package wacc

import wacc.AbstractSyntaxTree.BaseT.Char_T
import wacc.AbstractSyntaxTree._
import wacc.Assembler._
import wacc.AssemblerTypes._
import wacc.RegisterAllocator._
import wacc.TAC._
import wacc.cfgutils.{Colouring, RegisterAllocator}
import wacc.FinalIR.FinalIR // TODO: change this to not import everything
import wacc.X86AssemblerTypes._
import wacc.X86HelperFunctions._

import scala.collection.mutable.ListBuffer

//TODO Change all to x86_64 Architecture

object X86HighLevelAssembler {
  // Get correct operand type from TAC
  def getOperand(op: Operand): LHSop = {
    op match {
      case reg: TRegister => Assembler.getRealReg(reg)
      case IntLiteralTAC(value) => new X86ImmediateInt(value)
      case CharLiteralTAC(chr) => new X86ImmediateInt(chr.toInt)
      case BoolLiteralTAC(b) => new X86ImmediateInt(b.compare(true) + 1)
      case Label(name) => new X86LabelString(name)
      case PairLiteralTAC() => new X86ImmediateInt(0)
      case ArrayOp(_) => new X86ImmediateInt(0)
      case a => println("getOperand fail: " + a); null 
    }
  }

  // Convert TAC into next IR (Implemented in Assembler.wacc)
  def assembleX86TAC(tripleAddressCode: TAC): List[FinalIR] = {
    tripleAddressCode match {
      case Global(name) => List(FinalIR.Global(name))
      case Label(name) => List(FinalIR.Lbl(name))
      case Comments(str) => List(FinalIR.Comment(str))
      case DataSegmentTAC() => List(FinalIR.DataSeg())
      case TextSegmentTAC() => List(FinalIR.TextSeg())
      case StringLengthDefinitionTAC(len, _) => List(FinalIR.Word(len))
      case StringDefinitionTAC(str, lbl) => assembleStringDef(str, lbl)
      case BeginFuncTAC() => assembleBeginFunc()
      case EndFuncTAC() => assembleEndFunc()
      case AssignmentTAC(operand, reg) => assembleAssignment(operand, reg)
      case CommandTAC(cmd, operand, opType) => assembleCommand(cmd, operand, opType)
      case UnaryOpTAC(op, t1, res) => assembleUnaryOp(op, t1, res)
      case BinaryOpTAC(operation, op1, op2, res) => assembleBinOp(operation, op1, op2, res)
      case IfTAC(t1, goto) => assembleIf(t1, goto)
      case GOTO(label) => assembleJump(label)

      // Pair
      case CreatePairElem(pairElemType, pairPos, pairElemReg) => assemblePairElem(pairElemType, pairPos, pairElemReg)
      case CreatePair(dstReg) => assemblePair(dstReg)
      case GetPairElem(datatype, pairReg, pairPos, dstReg) => assembleGetPairElem(datatype, pairReg, pairPos, dstReg)
      case StorePairElem(datatype, pairReg, pairPos, srcReg) => assembleStorePairElem(datatype, pairReg, pairPos, srcReg)
      
      // Array
      // case InitialiseArray(arrLen, lenReg, dstReg) => assembleArrayInit(arrLen, lenReg, dstReg)
      // case CreateArrayElem(arrayElemType, elemPos, arrReg, elemReg) => assembleArrayElem(arrayElemType, elemPos, arrReg, elemReg)
      // case CreateArray(arrayElemType, elemsReg, dstReg) => assembleArray(arrayElemType, elemsReg, dstReg)
      // case LoadArrayElem(datatype, arrReg, arrPos, dstReg) => assembleLoadArrayElem(datatype, arrReg, arrPos, dstReg)
      // case StoreArrayElem(datatype, arrReg, arrPos, srcReg) => assembleStoreArrayElem(datatype, arrReg, arrPos, srcReg)
      
      case CallTAC(lbl, args, dstReg) => assembleCall(lbl, args, dstReg)
      case PopParamTAC(datatype, treg, index) => assemblePopParam(datatype, treg, index)
      case PushParamTAC(op) => List()
      case ReadTAC(dataType, readReg) => assembleRead(dataType, readReg)
      case ReservedPushTAC(reg, location, _) => List[FinalIR](FinalIR.Str("", rbp, X86ImmediateInt((location + 2) * POINTER_BYTE_SIZE), getRealReg(reg)))
      case ReservedPopTAC(location, reg, _) => List[FinalIR](FinalIR.Ldr("", rbp, X86ImmediateInt((location + 2) * POINTER_BYTE_SIZE), getRealReg(reg)))
      // ^Add 2 more to account for the fp and lr, which are pushed to the stack at the start of functions
      case AllocateStackTAC(size) => if (size > 0) List(FinalIR.Sub("", X86None(), rsp, X86ImmediateInt(size * POINTER_BYTE_SIZE), rsp)) else List()
      case PushTAC(tReg) => List(FinalIR.Push("", List(getRealReg(tReg))))
      case PopTAC(tReg) => List(FinalIR.Pop("", List(getRealReg(tReg))))
    }
  }

  def assembleUnaryOp(op: UnaryOpType.UnOp, t1: Operand, res: TRegister): List[FinalIR] = {
    op match {
      case UnaryOpType.Neg => {
        List(FinalIR.Rsb("", new X86Status(), getOperand(t1), new X86ImmediateInt(0), getRealReg(res)))
      }
      case UnaryOpType.Not => {
        FinalIR.Cmp("", getOperand(t1), new X86ImmediateInt(1)) ::
          FinalIR.Mov("ne", new X86ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Mov("e", new X86ImmediateInt(0), getRealReg(res)) :: List()
      }
      case UnaryOpType.Chr | UnaryOpType.Ord => {
        List(FinalIR.Mov("", getOperand(t1), getRealReg(res)))
      }
      case UnaryOpType.Len => {
        List(FinalIR.Ldr("", getRealReg(t1.asInstanceOf[TRegister]), new X86ImmediateInt(-POINTER_BYTE_SIZE), getRealReg(res)))
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
    addEndFunc("_errOverflow", X86HelperFunctions.assemble_errOverflow())
    addEndFunc("_prints", X86HelperFunctions.assemble_prints())
    addEndFunc(bl, X86HelperFunctions.assemble_read(bl))
    FinalIR.BranchLink("", new X86BranchString(bl)) ::
      FinalIR.Mov("", rax, getRealReg(readReg)) :: List()
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
        output = output ++ List(FinalIR.StrPre("", rsp, new X86ImmediateInt(-POINTER_BYTE_SIZE), getRealReg(reg)))
      })
    }
    output = output ++ List(FinalIR.BranchLink("", new X86BranchString(lbl.name)))

    /* Decrement the stack pointer for each argument pushed to the stack */
    if (args.length > argRegs.length)
      output = output ++ List(FinalIR.Sub("", new X86None(), rsp, new X86ImmediateInt(POINTER_BYTE_SIZE * (argRegs.length - args.length)), rsp))

    // move the result into dst before rax is popped back
    output ++ List(FinalIR.Mov("", rax, getRealReg(dstReg)))
  }    

  def checkNull(pairReg: TRegister) : List[FinalIR] = {
    addEndFunc("_errNull", X86HelperFunctions.assemble_errNull())
    addEndFunc("_prints", X86HelperFunctions.assemble_prints())
    
    FinalIR.Cmp("", getRealReg(pairReg), X86ImmediateInt(0)) ::
      FinalIR.Branch("e", "_errNull") :: List()
  }

  def assemblePair(dstReg: TRegister): List[FinalIR] = {
    addEndFunc("_malloc", X86HelperFunctions.assemble_malloc())
    
    FinalIR.Mov("", X86ImmediateInt(4 * POINTER_BYTE_SIZE), rdi) :: //asu
      FinalIR.BranchLink("", new X86BranchString("_malloc")) ::
      FinalIR.Mov("", rdi, getRealReg(dstReg)) ::
      FinalIR.Pop("", List(rax)) ::
      FinalIR.StrPre("", rax, X86ImmediateInt(2 * POINTER_BYTE_SIZE), getRealReg(dstReg)) ::
      FinalIR.Pop("", List(rax)) ::
      FinalIR.StrPre("", rax, X86ImmediateInt(0), getRealReg(dstReg)) :: List()
  }

  def assemblePairElem(pairElemType: DeclarationType, pairPos: PairElemT.Elem, pairElem: TRegister): List[FinalIR] = {
    addEndFunc("_malloc", X86HelperFunctions.assemble_malloc())
    
    FinalIR.Mov("", X86ImmediateInt(getTypeSize(pairElemType)), rdi) ::
      FinalIR.BranchLink("", new X86BranchString("_malloc")) ::
      FinalIR.StrPre("", rax, X86ImmediateInt(0), getRealReg(pairElem)) ::
      FinalIR.Push("", List(getRealReg(pairElem))):: List()
  }

  def assembleGetPairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, dstReg: TRegister): List[FinalIR] = {
    checkNull(pairReg) ++
    (FinalIR.Ldr("", getRealReg(pairReg), X86ImmediateInt(if (pairPos == PairElemT.Fst) 0 else 2 * POINTER_BYTE_SIZE), rax) ::
      FinalIR.Ldr("", rax, X86ImmediateInt(0), rax) :: List())
  }

  def assembleStorePairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, srcReg: TRegister): List[FinalIR] = {
    checkNull(pairReg) ++
    (FinalIR.Ldr("", getRealReg(pairReg), X86ImmediateInt(if (pairPos == PairElemT.Fst) 0 else 2 * POINTER_BYTE_SIZE), rax) ::
      FinalIR.StrPre("", rax, X86ImmediateInt(0), getRealReg(srcReg)) :: List())
  }

  def assembleJump(label: Label): List[FinalIR] = {
    List(FinalIR.Branch("", label.name))
  }

  def assembleIf(t1: Operand, goto: Label): List[FinalIR] = {
    FinalIR.Cmp("", getOperand(t1), new X86ImmediateInt(1)) ::
      FinalIR.Branch("e", goto.name) :: List()
  }

  def assemblePopParam(dataType: DeclarationType, treg: TRegister, index: Int): List[FinalIR] = {
    val cRegs = List(rax, rdi, rsi, rdx)
    val funcStackFrameSize = 8 // Stack frame consists of {rbp, rbx}.

    if (index < cRegs.length) {
      // Populate from registers in rax-
      val callReg = cRegs.take(index + 1).last
      List(FinalIR.Mov("", callReg, getRealReg(treg)))
    } else {
      // Populate from stack
      List(FinalIR.Ldr("", rbp, new X86ImmediateInt(funcStackFrameSize + (POINTER_BYTE_SIZE * (index - cRegs.size))), getRealReg(treg)))
    }
  }

  def assembleBinOp(operation: BinaryOpType.BinOp, op1: Operand, op2: Operand, res: TRegister): List[FinalIR] = {
    operation match {
      case BinaryOpType.Add => {
        List(FinalIR.Add("", new X86Status(), getOperand(op1), getOperand(op2), getRealReg(res)))
      }
      case BinaryOpType.Sub => {
        List(FinalIR.Sub("", new X86Status(), getOperand(op1), getOperand(op2),  getRealReg(res)))
      }
      case BinaryOpType.Mul => {
        List(FinalIR.Smull("", new X86Status(), getRealReg(res), rax, getOperand(op1), getOperand(op2)))
      }
      case BinaryOpType.Div => {
        addEndFunc("_errDivZero", X86HelperFunctions.assemble_errDivZero())
        addEndFunc("_prints", X86HelperFunctions.assemble_print("_prints"))
        FinalIR.Mov("", getOperand(op1), rax) ::
          FinalIR.Mov("", getOperand(op2), rdi) ::
          FinalIR.Cmp("", rdi, new X86ImmediateInt(0)) ::
          FinalIR.BranchLink("e", new X86BranchString("_errDivZero")) ::
          FinalIR.BranchLink("", new X86BranchString("__aeabi_idivmod")) ::
          FinalIR.Mov("", rax, getRealReg(res)) :: List()
        }
      case BinaryOpType.Mod => {
        addEndFunc("_errDivZero", X86HelperFunctions.assemble_errDivZero())
        addEndFunc("_prints", X86HelperFunctions.assemble_print("_prints"))
        FinalIR.Mov("", getOperand(op1), rax) ::
          FinalIR.Mov("", getOperand(op2), rdi) ::
          FinalIR.Cmp("", rdi, new X86ImmediateInt(0)) ::
          FinalIR.BranchLink("e", new X86BranchString("_errDivZero")) ::
          FinalIR.BranchLink("", new X86BranchString("__aeabi_idivmod")) ::
          FinalIR.Mov("", rdi, getRealReg(res)) :: List()
      }
      case BinaryOpType.Eq => {
        FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
          FinalIR.Mov("e", new X86ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Mov("ne", new X86ImmediateInt(0), getRealReg(res)) :: List()
      }
      case BinaryOpType.Neq => {
        FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
          FinalIR.Mov("ne", new X86ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Mov("e", new X86ImmediateInt(0), getRealReg(res)) :: List()
      }
      case BinaryOpType.Lt => {
        FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
          FinalIR.Mov("lt", new X86ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Mov("ge", new X86ImmediateInt(0), getRealReg(res)) :: List()
      }
      case BinaryOpType.Gt => {
        FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
          FinalIR.Mov("gt", new X86ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Mov("le", new X86ImmediateInt(0), getRealReg(res)) :: List()
      }
      case BinaryOpType.Lte => {
        FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
          FinalIR.Mov("le", new X86ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Mov("gt", new X86ImmediateInt(0), getRealReg(res)) :: List()
      }
      case BinaryOpType.Gte => {
        FinalIR.Cmp("", getOperand(op1), getOperand(op2)) ::
          FinalIR.Mov("ge", new X86ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Mov("lt", new X86ImmediateInt(0), getRealReg(res)) :: List()
      }
      case BinaryOpType.And => {
        FinalIR.Cmp("", getOperand(op1), new X86ImmediateInt(1)) ::
          FinalIR.Cmp("", getOperand(op2), new X86ImmediateInt(1)) ::
          FinalIR.Mov("ne", new X86ImmediateInt(0), getRealReg(res)) ::
          FinalIR.Mov("e", new X86ImmediateInt(1), getRealReg(res)) :: List()
      }
      case BinaryOpType.Or => {
        FinalIR.Cmp("", getOperand(op1), new X86ImmediateInt(1)) ::
          FinalIR.Mov("e", new X86ImmediateInt(1), getRealReg(res)) ::
          FinalIR.Cmp("", getOperand(op2), new X86ImmediateInt(1)) ::
          FinalIR.Mov("ne", new X86ImmediateInt(0), getRealReg(res)) ::
          FinalIR.Mov("e", new X86ImmediateInt(1), getRealReg(res)) :: List()
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
    FinalIR.Lbl(lbl.name) ::
      FinalIR.AsciiZ(escape(str)) :: List()
  }

  def assembleBeginFunc(): List[FinalIR] = {
    FinalIR.Push("", List(rbp)) ::
      FinalIR.Mov("", rsp, rbp) :: List()
  }

  def assembleEndFunc(): List[FinalIR] = {
    FinalIR.Mov("", new X86ImmediateInt(0), rax) ::
      FinalIR.Pop("", List(rbp)) ::
      FinalIR.Ret() :: List()
  }

  def assembleAssignment(operand: Operand, reg: TRegister): List[FinalIR] = {
    operand match {
      case Label(name) => List(FinalIR.Ldr("", rax, getOperand(operand), getRealReg(reg)))
      case _ => List(FinalIR.Mov("", getOperand(operand), getRealReg(reg)))
    }
  }

  def assembleCommand(cmd: CmdT.Cmd, operand: Operand, opType: DeclarationType): List[FinalIR] = {
    cmd match {
      case CmdT.Exit => {
        FinalIR.Mov("", getOperand(operand), rax) ::
          FinalIR.BranchLink("", new X86BranchString("exit")) :: List()
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
        addEndFunc(bl, X86HelperFunctions.assemble_print(bl))
        if (cmd == CmdT.PrintLn) {
          addEndFunc("_println", X86HelperFunctions.assemble_print("_println"))
          FinalIR.Mov("", getOperand(operand), rdi) ::
            FinalIR.BranchLink("", new X86BranchString(bl)) ::
            FinalIR.BranchLink("", new X86BranchString("_println")) :: List()
        }
        else {
          FinalIR.Mov("", getOperand(operand), rdi) ::
            FinalIR.BranchLink("", new X86BranchString(bl)) :: List()
        }
      }
      case CmdT.Ret => {
        FinalIR.Mov("", getOperand(operand), rax) ::
          FinalIR.Mov("", rbp, rsp) ::
          FinalIR.Pop("", List(rbp)) :: List() // TODO: examine use of special
      }
      case CmdT.Free => {
        opType match {
          case ArrayType(dataType, length) => {
            // why is r4/rcx here
            FinalIR.Sub("", new X86Status(), rcx, new X86ImmediateInt(POINTER_BYTE_SIZE), X86AssemblerTypes.r8) ::
              FinalIR.Push("", List(X86AssemblerTypes.r8)) ::
              FinalIR.Pop("", List(X86AssemblerTypes.r8)) ::
              FinalIR.Mov("", X86AssemblerTypes.r8, X86AssemblerTypes.r8) ::
              FinalIR.Mov("", X86AssemblerTypes.r8, rax) ::
              FinalIR.BranchLink("", new X86BranchString("free")) :: List()
          }
          case PairType(fstType, sndType) => {
            addEndFunc("_freepair", X86HelperFunctions.assemble_freepair())
            addEndFunc("_errNull", X86HelperFunctions.assemble_errNull())
            addEndFunc("_prints", X86HelperFunctions.assemble_prints())

            FinalIR.Mov("", getOperand(operand), rax) ::
              FinalIR.BranchLink("", new X86BranchString("_freepair")) :: List()
          }

        }
      }
    }
  }

  def assembleArrayInit(arrLen: Int, lenReg: TRegister, dstReg: TRegister): List[FinalIR] = {
    addEndFunc("_malloc", X86HelperFunctions.assemble_malloc())
    FinalIR.Push("", List(rax)) ::
      FinalIR.Mov("", new X86ImmediateInt(POINTER_BYTE_SIZE * (arrLen + 1)), rax) ::
      FinalIR.BranchLink("", new X86BranchString("_malloc")) ::
      FinalIR.Mov("", rax, getRealReg(dstReg)) ::
      FinalIR.Pop("", List(rax)) ::
      FinalIR.Add("", new X86Status(), getRealReg(dstReg), new X86ImmediateInt(POINTER_BYTE_SIZE), getRealReg(dstReg)) ::
      FinalIR.Mov("", new X86ImmediateInt(arrLen), getRealReg(lenReg)) ::
      FinalIR.Str("", getRealReg(lenReg), new X86ImmediateInt(-POINTER_BYTE_SIZE), getRealReg(dstReg)) :: List()
  }

  def assembleArray(arrayElemType: DeclarationType, elemsReg: List[TRegister], dstReg: TRegister): List[FinalIR] = {
    List[FinalIR]()
  }

  def assembleArrayElem(arrayElemType: DeclarationType, elemPos: Int, arrReg: TRegister, elemReg: TRegister): List[FinalIR] = {
    FinalIR.Push("", List(getRealReg(elemReg))) ::
      FinalIR.Push("", List(rax)) ::
      FinalIR.Mov("", new X86ImmediateInt(getTypeSize(arrayElemType)), rax) ::
      FinalIR.BranchLink("", new X86BranchString("malloc")) ::
      FinalIR.Str("", rax, new X86ImmediateInt(0), getRealReg(elemReg)) ::
      FinalIR.Str(getInstructionType(arrayElemType), getRealReg(arrReg), new X86ImmediateInt(POINTER_BYTE_SIZE * elemPos), getRealReg(elemReg)) ::
      FinalIR.Mov("", rax, getRealReg(elemReg)) ::
      FinalIR.Pop("", List(rax)) ::
      FinalIR.Pop("", List(getRealReg(elemReg))) :: List()
  }

  def assembleLoadArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: List[TRegister], dstReg: TRegister): List[FinalIR] = {
    addEndFunc("_arrLoad", X86HelperFunctions.assemble_arrLoad())
    addEndFunc("_boundsCheck", X86HelperFunctions.assemble_boundsCheck())
    var regs = List(getRealReg(arrReg), getRealReg(dstReg))
    regs = (regs ++ arrPos.map(a => getRealReg(a))).distinct.sortWith((s, t) => s < t)
    var output = List[FinalIR]()
    output = output ++
      (FinalIR.Push("", regs) ::
        FinalIR.Push("", List(rax, rdi, rsi, rdx)) :: List())
    arrPos.foreach(a => {
      output = output ++ 
      (FinalIR.Mov("", rsi, getRealReg(a)) ::
        FinalIR.Mov("", rdx, getRealReg(arrReg)) :: // arrLoad uses rax = rdx[rsi]
        FinalIR.BranchLink("", new X86BranchString("_arrLoad")) ::
        FinalIR.Mov("", getRealReg(dstReg), rax) :: List())
    })
    output ++
    (FinalIR.Pop("", List(rax, rdi, rsi, rdx)) ::
      FinalIR.Pop("", regs) :: List())
  }

  def assembleStoreArrayElem(datatype: DeclarationType, arrReg: TRegister, arrPos: List[TRegister], srcReg: TRegister): List[FinalIR] = {
    addEndFunc("_arrStore", X86HelperFunctions.assemble_arrStore())
    addEndFunc("_boundsCheck", X86HelperFunctions.assemble_boundsCheck())
    
    var regs = List(getRealReg(arrReg), getRealReg(srcReg))
    regs = (regs ++ arrPos.map(a => getRealReg(a))).distinct.sortWith((s, t) => s < t)
    var output = List[FinalIR]()
    output = output ++
      (FinalIR.Push("", regs) ::
        FinalIR.Push("", List(rax, rdi, rsi, rdx)) :: List())
    arrPos.foreach(a => {
      output = output ++
      (FinalIR.Mov("", rsi, getRealReg(srcReg)) ::
        FinalIR.Mov("", rax, getRealReg(a)) ::
        FinalIR.Mov("", rdx, getRealReg(arrReg)) :: // arrStore uses rdx[rax] = rsi
        FinalIR.BranchLink("", new X86BranchString("_arrStore")) :: List())
    })
    output ++
    (FinalIR.Pop("", List(rax, rdi, rsi, rdx)) ::
      FinalIR.Pop("", regs) :: List())
  }
}