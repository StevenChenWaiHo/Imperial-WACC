package wacc

import wacc.TAC._
import wacc.AbstractSyntaxTree._
import wacc.RegisterAllocator._

object Assembler {

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
          sourceRegister + ", " + "LSL " + x
        }
        case Right(value) => {
          sourceRegister + ", " + "LSL " + "#" + value
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

  def ldrStrAssist(condition: String, destinationRegister: Register, sourceRegister: Register, operand: Either[LHSop, String]): String = {
    var str = condition + " " + destinationRegister.toString + ", "
    operand match {
      case Left(x) => {
        str = str + "[" + sourceRegister.toString + ", " + x + "]"
      }
      case Right(x) => {
        str = str + "=" + x
      }
    }
    return str
  }

  def translateLdr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): String = {
    //Incomplete
    return "ldr" + ldrStrAssist(condition, destinationRegister, sourceRegister, Left(operand)) // TODO: change
  }

  def translateStr(condition: String, destinationRegister: Register, sourceRegister: Register, operand: LHSop): String = {
    //Incomplete
    return "str" + ldrStrAssist(condition, destinationRegister, sourceRegister, Left(operand))
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

  def CompareAssist(condition: String, register1: Register, operand: LHSop): String = {
    return condition + " " + register1.toString + ", " + operand.toString
  }

  def translateCompare(condition: String, register1: Register, operand: LHSop): String = {
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

  def translate_errOverflow(): List[String] = {
    translateLdr("", r0, r0, new LabelString(".L._errOverflow_str0")) :: 
    translateBranchLink("", new BranchString("_prints")) ::
    translateMove("", r0, new ImmediateInt(255)) ::
    translateBranchLink("", new BranchString("exit")) :: List()
  }

  def translate_arrStoreB(): List[String] = {
    translatePush("", List(lr)) :: 
    translateCompare("", r10, new ImmediateInt(0)) ::
    translateMove("", r1 , r10) ::
    translateBranchLink("lt", new BranchString("_boundsCheck")) :: 
    translateLdr("", lr, r3, new ImmediateInt(-4)) :: 
    translateCompare("eq", r10, lr) ::
    translateMove("ge", r1, r10) :: 
    translateBranchLink("ge", new BranchString("_boundsCheck")) :: 
    //translateStrb() :: 
    translatePop("", List(pc)) :: List()
  }



  def translate_boundsCheck(): List[String] = {
    translateLdr("", r0, r0, new LabelString(".L._boundsCheck_str_0")) :: 
    translateBranchLink("", new BranchString("printf")) :: 
    translateMove("", r0, new ImmediateInt(255)) :: 
    translateBranchLink("", new BranchString("fflush")) :: 
    translateMove("", r0, new ImmediateInt(255)) :: 
    translateBranchLink("", new BranchString("exit")) :: List()
  }

  def translate_prints(): List[String] = {
    translatePush("", List(lr)) ::
    translateMove("", r2, r0) ::
    translateLdr("", r1, r0, new ImmediateInt(-4)) ::
    translateLdr("", r0, r0, new LabelString(".L.prints_str_0")) ::
    translateBranchLink("", new BranchString("printf")) ::
    translateMove("", r0, new ImmediateInt(0)) ::
    translateBranchLink("", new BranchString("fflush")) ::
    translatePop("", List(pc)) :: List()
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
  class StackOffset(offset: Int) extends LHSop{
    override def toString(): String = "STACK" + offset.toString()
  }
  class ImmediateInt(i: Int) extends LHSop {
    override def toString(): String = "#" + i.toString()
  }
  class LabelString(name: String) extends LHSop {
    override def toString(): String = "=" + name
  }
  class BranchString(name: String) extends LHSop {
    override def toString(): String = name
  }
  
  def translateOperand(op: Operand): LHSop = {
    op match {
      case reg: TRegister => translateRegister(reg)
      case IntLiteralTAC(value) => new ImmediateInt(value)
      case CharLiteralTAC(chr) => new ImmediateInt(chr.toInt)
      case BoolLiteralTAC(b) => new ImmediateInt(b.compare(true))
      case Label(name) => new LabelString(name)
      case a => println("translateOperand fail: " + a); null // TODO: this should not match
    }
  }

  def translateTAC(tripleAddressCode: TAC): List[String] = {
    //Need to figure out how registers work
    //Push and pop might not be in right place
    //Algorithm for determining if ldr is needed
    tripleAddressCode match {
      // case BinaryOpTAC(op, t1, t2, res) => {
      //   op match {
          // case BinaryOpType.Add => {
          //   val destinationRegister: Register= translateOperand(res).left.getOrElse(r0)
          //   var t1t: Either[Register, Int] = translateOperand(t1)
          //   var t2t: Either[Register, Int] = translateOperand(t2)
          //   strList = strList ++ List(translatePush("", List(r8)))
          //   strList = strList ++ List(translatePop("", List(r8)))
          //   strList = strList ++ List(translateMove("", r8, ImmediateValueOrRegister(Left(r8))))
          //   strList = strList ++ List(translateMove("", r8, ImmediateValueOrRegister(Left(r8))))
          //   t1t match {
          //     case Left(x) => {
          //       t2t match {
          //         case Right(x) => {
          //           if (determineLdr(x)) {
          //             strList = strList ++ List(translateLdr("", r9, r0, Right("=" + x)))
          //             t2t = Left(r9)
          //           }
          //         }
          //       }
          //       strList = strList ++ List(translateAdd("", Status(), destinationRegister, x, ImmediateValueOrRegister(t2t)))
          //     }
          //     case Right(x) => {
          //       if (determineLdr(x)) {
          //         strList = strList ++ List(translateLdr("", r8, r0, Right("=" + x)))
          //       } else {
          //         strList = strList ++ List(translateMove("", r8, ImmediateValueOrRegister(Right(x))))
          //       }
          //       strList = strList ++ List(translateAdd("", Status(), destinationRegister, r8, ImmediateValueOrRegister(t2t)))
          //     }
          //   }
          //   strList = strList ++ List(translateBranchLink("vs", "_errOverflow"))
          //   return strList
          // }
          /*
          case BinaryOpType.Sub => {
          val destinationRegister: Register = translateOperand(res)
            var t1t: Either[Register, Int] = translateOperand(t1)
            var t2t: Either[Register, Int] = translateOperand(t2)
            strList = strList ++ List(translatePush("", List(r8)))
            strList = strList ++ List(translatePop("", List(r8)))
            strList = strList ++ List(translateMove("", r8, ImmediateValueOrRegister(Left(r8))))
            strList = strList ++ List(translateMove("", r8, translate))
            t1t match {
              case Left(x) => {
                t2t match {
                  case Right(x) => {
                    if (determineLdr(x)) {
                      strList = strList ++ List(translateLdr("", r9, r0, Right("=" + x)))
                      t2t = Left(r9)
                    }
                  }
                }
                strList = strList ++ List(translateSub("", Status(), destinationRegister, x, ImmediateValueOrRegister(t2t)))
              }
              case Right(x) => {
                if (determineLdr(x)) {
                  strList = strList ++ List(translateLdr("", r8, r0, Right("=" + x)))
                }
                strList = strList ++ List(translateRsb("", Status(), destinationRegister, r8, ImmediateValueOrRegister(t2t)))
              }
            }
            strList = strList ++ List(translateBranchLink("vs", "_errOverflow"))
            return strList
            strList = strList ++ List(translateSub(translateOperand(res), translateOperand(t1), translateOperand(t2)))
          }
          case BinaryOpType.Mul => {
            strList ++ List(translateSmull(r8, translateOperand(res), translateOperand(t1), translateOperand(t2)))
            strList ++ List(translateCompare("", r9, ArithmeticShiftRight(r8, Right(31))))
            strList ++ List(translateBranchLink("ne", "_errOverflow"))
          }
          case BinaryOpType.Div => {
            strList ++ List(translateMove("", r8, ImmediateValueOrRegister(Left(r8))))
            strList ++ List(translateMove("", translateOperand(res), ImmediateValueOrRegister(Left(r8))))
            strList ++ List(translateMove("", r0, ImmediateRegister(translateOperand(t1))))
            strList ++ List(translateMove("", r1, ImmediateRegister(translateOperand(t2))))
            strList ++ List(translateCompare("", r1, ImmediateValueOrRegister(Right(0))))
            strList ++ List(translateBranchLink("eq", "_errDivZero"))
            strList ++ List(translateBranchLink("", "__aeabi_idivmod"))
            strList ++ List(translateMove("", r12, ImmediateValueOrRegister(Left(r0))))
            strList ++ List(translateMove("", r8, ImmediateValueOrRegister(Left(r12))))
            strList ++ List(translatePush("", List(r8)))
          }
          */
      //   }
      // }
      // case AssignmentTAC(t1, res) => {
      //   t1 match {
      //     case TRegisterNum(Num) => {
      //       strList = strList ++ List(translateMove("", translateOperand(res), ImmediateValueOrRegister(Right(Num))))
      //     }
      //     case IdentLiteralTAC(name) => {
      //       strList = strList ++ List(translateMove("", r8, nameToAddress(name)))
      //       strList = strList ++ List(translateMove("", translateOperand(res), ImmediateValueOrRegister(Left(r8))))
      //     }
      //     case IntLiteralTAC(value) => {
      //       strList = strList ++ List(translateMove("", translateOperand(res), ImmediateValueOrRegister(value)))
      //     }
      //     case StringLiteralTAC(string) => {
      //       strList = strList ++ List(translateLdr("", translateOperand(res), nameToLabel(string)))
      //     }
      //   }
      // }
      // case TAC.Label(name) => {
      //   str = labelToCodeTable()
      // }
      case Label(name) => List(name + ":")
      case Comments(str) => List("@ " + str)
      case DataSegmentTAC() => List(".data")
      case TextSegmentTAC() => List(".text")
      case StringLengthDefinitionTAC(len, lbl) => translateStringLengthDef(len, lbl)
      case StringDefinitionTAC(str, lbl) => translateStringDef(str, lbl)
      case BeginFuncTAC() => translateBeginFunc()
      case EndFuncTAC() => translateEndFunc()
      case AssignmentTAC(operand, reg) => translateAssignment(operand, reg)
      case CommandTAC(cmd, operand, opType) => translateCommand(cmd, operand, opType)
      case BinaryOpTAC(operation, op1, op2, res) => translateBinOp(operation, op1, op2, res)
    }
  }

  def translateProgram(tacList: List[TAC]): List[String] = {
    var output = List[String]()
     tacList.foreach(tac => {
      output = output ++ translateTAC(tac)
    })
    output
  }

  def translateBinOp(operation: BinaryOpType.BinOp, op1: Operand, op2: Operand, res: TRegister) = {
    operation match {
      case BinaryOpType.Add => {
        translateAdd("", None(), translateRegister(res), translateOperand(op1), translateOperand(op2))
      }
      case BinaryOpType.Sub => {
        translateSub("", None(), translateRegister(res), translateOperand(op1), translateOperand(op2))
      }
    }
  }

  def translateStringLengthDef(len: Int, lbl: Label) = {
    List(".word " + len.toString())
  }

  def translateStringDef(str: String, lbl: Label) = {
    translateTAC(lbl) ++
    List(".asciz \"" + str + "\"")
  }
  
  def translateBeginFunc() = {
    translatePush("", List(fp, lr)) ::
    translatePush("", List(r8, r10, r12)) ::
    translateMove("", fp, sp) :: List()
  }

  def translateEndFunc() = {
    translateMove("", r0, new ImmediateInt(0)) ::
    translatePop("", List(r8, r10, r12)) ::
    translatePop("", List(fp, pc)) :: List()
  }

  def translateAssignment(operand: Operand, reg: TRegister) = {
    translateMove("", translateRegister(reg), translateOperand(operand)) :: List()
  }

  def translateCommand(cmd: CmdT.Cmd, operand: Operand, opType: DeclarationType) = {
    if (cmd == CmdT.Exit) {
      translateMove("", r0, translateOperand(operand)) ::
      translateBranchLink("", new BranchString("exit")) :: List() 
    } else if (cmd == CmdT.Print || cmd == CmdT.PrintLn) {
      // TODO: change print behaviour of arrays and pairs
      val bl = opType match {
        case ArrayType(dataType, length) => "_printi"
        case BaseType(baseType) => baseType match {
          case BaseT.String_T => "_prints"
          case BaseT.Char_T => "_printc"
          case BaseT.Bool_T => "_printb" // ???
          case BaseT.Int_T => "_printi"
          case _ => "_printi"
        }
        case NestedPair() => "_printi"
        case PairType(fstType, sndType) => "_printi"
      }
      var listEnd = List[String]()
      if (cmd == CmdT.PrintLn) {
        listEnd = translateBranchLink("", new BranchString("_println")) :: List() 
      }
      // TODO: Add the _prints function in at the end
      translateMove("", r0, translateOperand(operand)) ::
      translateBranchLink("", new BranchString(bl)) :: listEnd
    } else {
      List("Command not implemented")
    }
  }
}
