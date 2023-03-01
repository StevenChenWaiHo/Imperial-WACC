package wacc

import wacc.TAC._
import wacc.AbstractSyntaxTree._
import wacc.RegisterAllocator._
import wacc.HardcodeFunctions._

object Assembler {

  val endFuncs = collection.mutable.Map[String, List[String]]()
  var labelCount = 0

  def generateLabel(): Label = {
    labelCount += 1
    new Label(".La" + labelCount.toString())
  }

  def addEndFunc(name: String, code: List[String]) = {
    if (!endFuncs.contains(name)) {
      endFuncs.addOne(name, "" :: code)
    }
  }

  def endFuncsToList(): List[String] = {
    endFuncs.toList.map(entry => entry match {
      case (name, code) => code
    }).flatten
  }

  sealed trait Operand2
  case class ImmediateValueOrRegister(operand: Either[Register, Int]) extends Operand2 {
    @Override
    override def toString: String = {
      operand match {
        case Left(value) => {
          value.toString
        }
        case Right(value) => {
          "#" + value
        }
      }
    }
  }

  case class LogicalShiftLeft(sourceRegister: Register, operand: Either[Register, Int]) extends Operand2 {
    override def toString: String = {
      operand match {
        case Left(x) => {
          sourceRegister + ", " + "LSL " + x
        }
        case Right(value) => {
          sourceRegister + ", " + "LSL " + "#" + value
        }
      }
    }
  }

  case class LogicalShiftRight(sourceRegister: Register, operand: Either[Register, Int]) extends Operand2 {
    override def toString: String = {
      operand match {
        case Left(x) => {
          sourceRegister + ", " + "LSR " + x
        }
        case Right(value) => {
          sourceRegister + ", " + "LSR " + " " + "#" + value
        }
      }
    }
  }

  case class ArithmeticShiftRight(sourceRegister: Register, operand: Either[Register, Int]) extends Operand2 {
    override def toString: String = {
      operand match {
        case Left(x) => {
          sourceRegister + ", " + "ASR " + x
        }
        case Right(value) => {
          sourceRegister + ", " + "ASR " + "#" + value
        }
      }
    }
  }

  case class RotateRight(sourceRegister: Register, operand: Either[Register, Int]) extends Operand2 {
    override def toString: String = {
      operand match {
        case Left(x) => {
          sourceRegister + ", " + "ROR " + x
        }
        case Right(value) => {
          sourceRegister + ", " + "ROR " + "#" + value
        }
      }
    }
  }

  case class RotateRightExtended(sourceRegister: Register) extends Operand2 {
    override def toString: String = {
      sourceRegister + ", " + "RRX"
    }
  }

  sealed trait Suffi

  case class Control() extends Suffi {
    override def toString: String = {
      "c"
    }
  }

  case class Extension() extends Suffi {
    override def toString: String = {
      "x"
    }
  }

  case class Status() extends Suffi {
    override def toString: String = {
      "s"
    }
  }

  case class Flags() extends Suffi {
    override def toString: String = {
      "f"
    }
  }

  case class None() extends Suffi {
    override def toString: String = {
      ""
    }
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

  def translatePush(condition: String, registers: List[Register]): String = {
    return "push" + pushPopAssist(condition, registers)
  }

  def translatePop(condition: String, registers: List[Register]): String = {
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
    }
    return str
  }

  def translateLdr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): String = {
    //Incomplete
    return "ldr" + ldrStrAssist(condition, destinationRegister, sourceRegister, operand)
  }

  def translateStr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): String = {
    //Incomplete
    return "str" + ldrStrAssist(condition, destinationRegister, sourceRegister, operand)
  }

  def addSubMulAssist(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): String = {
    return condition + setflag + " " + destinationRegister + ", " + sourceRegister + ", " + operand
  }

  //Incomplete, no condition
  def translateAdd(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): List[String] = {
    "add" + addSubMulAssist(condition, setflag, destinationRegister, sourceRegister, operand) ::
    translateBranchLink("vs", new BranchString("_errOverflow")) :: List()
  }

  def translateSub(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): List[String] = {
    return "sub" + addSubMulAssist(condition, setflag, destinationRegister, sourceRegister, operand) :: List()
  }

  def translateRsb(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, operand: LHSop): String = {
    return "rsb" + addSubMulAssist(condition, setflag, destinationRegister, sourceRegister, operand)
  }

  def translateMul(condition: String, setflag: Suffi, destinationRegister: LHSop, sourceRegister: LHSop, sourceRegisterTwo: LHSop): String = {
    return "mul" + addSubMulAssist(condition, setflag, destinationRegister, sourceRegister, sourceRegisterTwo)
  }

  def fourMulAssist(condition: String, setflag: Suffi, destinationLow: Register, destinationHigh: Register,
                    sourceRegister: Register, operand: Register): String = {
    var str = condition + setflag + " " + destinationLow + "," + " " + destinationHigh + "," + " " + sourceRegister +
      "," + " " + operand
    return str
  }

  def translateMla(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: Register, operand1: Register, operand2: Register): String = {
    return "mla" + fourMulAssist(condition, setflag, destinationRegister, sourceRegister, operand1, operand2)
  }

  def translateUmull(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: Register, operand1: Register, operand2: Register): String = {
    return "umull" + fourMulAssist(condition, setflag, destinationRegister, sourceRegister, operand1, operand2)
  }

  def translateUmlal(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: Register, operand1: Register, operand2: Register): String = {
    return "umlal" + fourMulAssist(condition, setflag, destinationRegister, sourceRegister, operand1, operand2)
  }

  def translateSmull(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: Register, operand1: Register, operand2: Register): String = {
    return "smull" + fourMulAssist(condition, setflag, destinationRegister, sourceRegister, operand1, operand2)
  }

  def translateSmlal(condition: String, setflag: Suffi, destinationRegister: Register, sourceRegister: Register, operand1: Register, operand2: Register): String = {
    return "smlal" + fourMulAssist(condition, setflag, destinationRegister, sourceRegister, operand1, operand2)
  }

  def CompareAssist(condition: String, register1: LHSop, operand: LHSop): String = {
    return condition + " " + register1.toString + ", " + operand.toString
  }

  def translateCompare(condition: String, register1: LHSop, operand: LHSop): String = {
    return "cmp" + CompareAssist(condition, register1, operand)
  }

  def translateCompareNeg(condition: String, register1: Register, operand: LHSop): String = {
    return "cmn" + CompareAssist(condition, register1, operand)
  }

  def translateMove(condition: String, dst: LHSop, operand: LHSop): String = {
    "mov " + dst.toString + ", " + operand.toString()
  }

  def translateBranch(condition: String, operand: String): String = {
    return "b" + condition + " " + operand
  }

  def translateBranchLink(condition: String, operand: LHSop): String = {
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

  sealed trait LHSop
  class Register extends LHSop
  case class StackOffset(offset: Int) extends LHSop{
    override def toString(): String = "STACK" + offset.toString()
  }
  case class ImmediateInt(i: Int) extends LHSop {
    override def toString(): String = "#" + i.toString()
  }
  case class LabelString(name: String) extends LHSop {
    override def toString(): String = "=" + name
  }
  case class BranchString(name: String) extends LHSop {
    override def toString(): String = name
  }
  
  def translateOperand(op: Operand): LHSop = {
    op match {
      case reg: TRegister => translateRegister(reg)
      case IntLiteralTAC(value) => new ImmediateInt(value)
      case CharLiteralTAC(chr) => new ImmediateInt(chr.toInt)
      case BoolLiteralTAC(b) => new ImmediateInt(b.compare(true))
      case Label(name) => new LabelString(name)
      case PairLiteralTAC() => new ImmediateInt(0)
      case a => println("translateOperand fail: " + a); null // TODO: this should not match
    }
  }

  def translateTAC(tripleAddressCode: TAC): List[String] = {
    //Need to figure out how registers work
    //Push and pop might not be in right place
    //Algorithm for determining if ldr is needed
    tripleAddressCode match {
      case Label(name) => assembleLabel(name)
      case Comments(str) => List("@ " + str)
      case DataSegmentTAC() => List(".data")
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

  def getTypeSize(decType: DeclarationType): Integer = {
    decType match {
      case BaseType(BaseT.Int_T) => 4
      case BaseType(BaseT.Char_T) => 1
      case _ => 4 
    }
  }

  val POINTER_BYTE_SIZE = 4

  def assemblePair(fstType: DeclarationType, sndType: DeclarationType, dstReg: TRegister): List[String] = {
    // Assume r8 and r12 not used
    // r12: pointer to pair r8: pointer to pairElem
    translateMove("", r0, new ImmediateInt(2 * POINTER_BYTE_SIZE)) ::
    translateBranchLink("", new BranchString("malloc")) ::
    translateMove("", r12, r0) ::
    translatePop("", List(r8)) ::
    translateStr("", r8, r12, new ImmediateInt(POINTER_BYTE_SIZE)) ::
    translatePop("", List(r8)) ::
    translateStr("", r8, r12, new ImmediateInt(0)) ::
    translateMove("", translateRegister(dstReg), r12) :: List()
  }

  def assemblePairElem(pairElemType: DeclarationType, pairPos: PairElemT.Elem, srcReg: TRegister): List[String] = {
    translatePush("", List(r8, r12)) ::
    translateMove("", r0, new ImmediateInt(getTypeSize(pairElemType))) ::
    translateBranchLink("", new BranchString("malloc")) ::
    translateMove("", r8, translateRegister(srcReg)) ::
    translateMove("", r12, r0) ::
    translateStr("", r8, r12, new ImmediateInt(if (pairPos == PairElemT.Fst) 0 else 4)) ::
    translatePush("", List(r12)) :: List()
  }

  def assembleUnaryOp(op: UnaryOpType.UnOp, t1: Operand, res: TRegister): List[String] = {
    op match {
      case UnaryOpType.Neg => {
        translateRsb("", Status(), translateRegister(res), translateOperand(t1), new ImmediateInt(0)) :: List()
      }
      case UnaryOpType.Not => {
        translateCompare("", translateOperand(t1), new ImmediateInt(1)) ::
        translateMove("ne", translateRegister(res), new ImmediateInt(1)) ::
        translateMove("eq", translateRegister(res), new ImmediateInt(0)) :: List()
      }
      case UnaryOpType.Chr | UnaryOpType.Ord => {
        translateMove("", translateRegister(res), translateOperand(t1)) :: List()
      }
      case UnaryOpType.Len => {
        // ldr r8, [r4, #-4]
        translateLdr("", translateRegister(res), r0, translateOperand(t1)) :: List()
      }
    }
  }

  def assembleRead(datatype: DeclarationType, readReg: TRegister): List[String] = {
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
    addEndFunc(bl, translate_read(bl))
    List(translateBranchLink("", new BranchString(bl)))
  }

  def assembleCall(lbl: Label, args: List[TRegister], dstReg: TRegister): List[String] = {
    var output = List[String]()
    // save arg registers on the stack
    output = output ++ List(translatePush("", List(r0, r1, r2, r3)))
    // move all the args in to arg registers
    val regs = List(r0, r1, r2, r3)
    args.slice(0, 4).zip(regs).foreach(elem => {
      elem match {
        case (arg, reg) => output = output ++ List(translateMove("", reg, translateRegister(arg)))
      }
    })
    output = output ++ List(translateBranchLink("", new BranchString(lbl.name)))
    // move the result into r12 before r0 is popped back
    output = output ++ List(translateMove("", r12, r0))
    // get previous registers from stack
    output ++ List(translatePop("", List(r0, r1, r2, r3)))
  }

  def assembleGetPairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, dstReg: TRegister): List[String] = {
    translateLdr("", translateRegister(dstReg), translateRegister(pairReg), new ImmediateInt(if (pairPos == PairElemT.Fst) 0 else 4)) :: List()
  }

  def assembleStorePairElem(datatype: DeclarationType, pairReg: TRegister, pairPos: PairElemT.Elem, srcReg: TRegister): List[String] = {
    translateStr("", translateRegister(srcReg), translateRegister(pairReg), new ImmediateInt(if (pairPos == PairElemT.Fst) 0 else 4)) :: List()
  }

  def assembleProgram(tacList: List[TAC]): List[String] = {
    var output = List[String]()
     tacList.foreach(tac => {
      output = output ++ translateTAC(tac)
    })
    output ++ endFuncsToList()
  }

  def assembleLabel(name: String): List[String] = {
    if (name == "main") {
      List(".global main", name + ":")
    } else {
      List(name + ":")
    }
  }

  def assembleGOTO(label: Label): List[String] = {
    translateBranch("", label.name) :: List()
  }

  def assembleIf(t1: Operand, goto: Label): List[String] = {
    translateCompare("", translateOperand(t1), new ImmediateInt(1)) ::
    translateBranch("eq", goto.name) :: List()
  }

  def assembleBinOp(operation: BinaryOpType.BinOp, op1: Operand, op2: Operand, res: TRegister) = {
    operation match {
      case BinaryOpType.Add => {
        translateAdd("", Status(), translateRegister(res), translateOperand(op1), translateOperand(op2))
      }
      case BinaryOpType.Sub => {
        translateSub("", Status(), translateRegister(res), translateOperand(op1), translateOperand(op2))
      }
      case BinaryOpType.Mul => {
        translateMul("", Status(), translateRegister(res), translateOperand(op1), translateOperand(op2)) :: List()
      }
      case BinaryOpType.Div => {
        addEndFunc("_errDivZero", translate_errDivZero())
        translateMove("", r4, translateOperand(op1)) ::
        translateMove("", r1, translateOperand(op2)) ::
        translateCompare("", r1, new ImmediateInt(0)) ::
        translateBranchLink("eq", new BranchString("_errDivZero")) ::
        translateBranchLink("", new BranchString("__aeabi_idivmod")) :: List()
      }
      case BinaryOpType.Eq => {
        translateMove("", translateRegister(res), new ImmediateInt(0)) ::
        translateCompare("", translateOperand(op1), translateOperand(op2)) :: 
        translateMove("eq", translateRegister(res), new ImmediateInt(1)) :: List()
      }
      case BinaryOpType.Neq => {
        translateMove("", translateRegister(res), new ImmediateInt(0)) ::
        translateCompare("", translateOperand(op1), translateOperand(op2)) :: 
        translateMove("ne", translateRegister(res), new ImmediateInt(1)) :: List()
      }
      case BinaryOpType.Lt => {
        translateMove("", translateRegister(res), new ImmediateInt(0)) ::
        translateCompare("", translateOperand(op1), translateOperand(op2)) :: 
        translateMove("lt", translateRegister(res), new ImmediateInt(1)) :: List()
      }
      case BinaryOpType.Gt => {
        translateMove("", translateRegister(res), new ImmediateInt(0)) ::
        translateCompare("", translateOperand(op1), translateOperand(op2)) :: 
        translateMove("gt", translateRegister(res), new ImmediateInt(1)) :: List()
      }
      case BinaryOpType.Lte => {
        translateMove("", translateRegister(res), new ImmediateInt(0)) ::
        translateCompare("", translateOperand(op1), translateOperand(op2)) :: 
        translateMove("le", translateRegister(res), new ImmediateInt(1)) :: List()
      }
      case BinaryOpType.Gte => {
        translateMove("", translateRegister(res), new ImmediateInt(0)) ::
        translateCompare("", translateOperand(op1), translateOperand(op2)) :: 
        translateMove("ge", translateRegister(res), new ImmediateInt(1)) :: List()
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
    }
  }

  def assembleStringLengthDef(len: Int, lbl: Label) = {
    List(".word " + len.toString())
  }

  def assembleStringDef(str: String, lbl: Label) = {
    translateTAC(lbl) ++
    List(".asciz \"" + str + "\"")
  }
  
  def assembleBeginFunc() = {
    translatePush("", List(fp, lr)) ::
    translatePush("", List(r8, r10, r12)) ::
    translateMove("", fp, sp) :: List()
  }

  def assembleEndFunc() = {
    translateMove("", r0, new ImmediateInt(0)) ::
    translatePop("", List(r8, r10, r12)) ::
    translatePop("", List(fp, pc)) :: List()
  }

  def assembleAssignment(operand: Operand, reg: TRegister) = {
    operand match {
      case Label(name) => translateLdr("", translateRegister(reg), r0, translateOperand(operand)) :: List()
      case _=> translateMove("", translateRegister(reg), translateOperand(operand)) :: List()
    }
    
  }

  def assembleCommand(cmd: CmdT.Cmd, operand: Operand, opType: DeclarationType) = {
    if (cmd == CmdT.Exit) {
      translateMove("", r0, translateOperand(operand)) ::
      translateBranchLink("", new BranchString("exit")) :: List() 
    } else if (cmd == CmdT.Print || cmd == CmdT.PrintLn) {
      // TODO: change print behaviour of arrays and pairs
      val bl = opType match {
        case ArrayType(dataType, length) => "_prints"
        case BaseType(baseType) => baseType match {
          case BaseT.String_T => "_prints"
          case BaseT.Char_T => "_printc"
          case BaseT.Bool_T => "_printb"
          case BaseT.Int_T => "_printi"
          case _ => "_printi"
        }
        case NestedPair() => "_printi"
        case PairType(fstType, sndType) => "_printi"
      }
      addEndFunc(bl, translate_print(bl))
      var listEnd = List[String]()
      if (cmd == CmdT.PrintLn) {
        addEndFunc("_println", translate_print("_println"))
        listEnd = translateBranchLink("", new BranchString("_println")) :: List() 
      }
      translateMove("", r0, translateOperand(operand)) ::
      translateBranchLink("", new BranchString(bl)) :: listEnd
    } else {
      List("Command not implemented")
    }
  }
}
