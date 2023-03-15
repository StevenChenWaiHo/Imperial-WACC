package wacc

import wacc.AssemblerTypes._

object X86AssemblerTypes {
  //TODO change these to x86 format
  //TODO change all calls to LHSop instances in ALL files
  case class X86StackOffset(offset: Int) extends StackOffset {
    override def toString(): String = "STACK" + offset.toString()
  }

  case class X86ImmediateInt(i: Int) extends ImmediateInt {
    override def toString(): String = "#" + i.toString()
  }

  case class X86LabelString(name: String) extends LabelString {
    override def toString(): String = "=" + name
  }

  case class X86BranchString(name: String) extends BranchString {
    override def toString(): String = name
  }

  sealed trait X86Register extends Register {
    import scala.math.Ordered.orderingToOrdered
    def compare(that: X86Register): Int = listOfRegisters.get(this) compare listOfRegisters.get(that)
  }

  object r0 extends X86Register {
    override def toString(): String = "r0"
  }

  object r1 extends X86Register {
    override def toString(): String = "r1"
  }

  object r2 extends X86Register {
    override def toString(): String = "r2"
  }

  object r3 extends X86Register {
    override def toString(): String = "r3"
  }

  object r4 extends X86Register {
    override def toString(): String = "r4"
  }

  object r5 extends X86Register {
    override def toString(): String = "r5"
  }

  object r6 extends X86Register {
    override def toString(): String = "r6"
  }

  object r7 extends X86Register {
    override def toString(): String = "r7"
  }

  object r8 extends X86Register {
    override def toString(): String = "r8"
  }

  object r9 extends X86Register {
    override def toString(): String = "r9"
  }

  object r10 extends X86Register {
    override def toString(): String = "r10"
  }

  object r11 extends X86Register {
    override def toString(): String = "r11"
  }

  object r12 extends X86Register {
    override def toString(): String = "r12"
  }

  object r13 extends X86Register {
    override def toString(): String = "r13"
  }

  object r14 extends X86Register {
    override def toString(): String = "r14"
  }

  object fp extends X86Register {
    override def toString(): String = "fp"
  }

  object lr extends X86Register {
    override def toString(): String = "lr"
  }

  object pc extends X86Register {
    override def toString(): String = "pc"
  }

  object sp extends X86Register {
    override def toString(): String = "sp"
  }

  val listOfRegisters = Map[X86Register, Int](r0 -> 0, r1 -> 1, r2 -> 2, r3 -> 3, r4 -> 4, r5 -> 5, r6 -> 6,
    r7 -> 7, r8 -> 8, r9 -> 9, r10 -> 10, r11 -> 11, r12 -> 12, r13 -> 13, r14 -> 14)

  case class ImmediateValueOrRegister(operand: Either[X86Register, Int]) extends LHSop {
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

  case class LogicalShiftLeft(sourceRegister: X86Register, operand: Either[X86Register, Int]) extends LHSop {
    override def toString: String = {
      operand match {
        case Left(x) => {
          sourceRegister + ", " + "lsl " + x
        }
        case Right(value) => {
          sourceRegister + ", " + "lsl " + "#" + value
        }
      }
    }
  }

  case class LogicalShiftRight(sourceRegister: X86Register, operand: Either[X86Register, Int]) extends LHSop {
    override def toString: String = {
      operand match {
        case Left(x) => {
          sourceRegister + ", " + "lsr " + x
        }
        case Right(value) => {
          sourceRegister + ", " + "lsr " + " " + "#" + value
        }
      }
    }
  }

  case class ArithmeticShiftRight(sourceRegister: X86Register, operand: Either[X86Register, Int]) extends LHSop {
    override def toString: String = {
      operand match {
        case Left(x) => {
          sourceRegister + ", " + "asr " + x
        }
        case Right(value) => {
          sourceRegister + ", " + "asr " + "#" + value
        }
      }
    }
  }

  case class RotateRight(sourceRegister: X86Register, operand: Either[X86Register, Int]) extends LHSop {
    override def toString: String = {
      operand match {
        case Left(x) => {
          sourceRegister + ", " + "ror " + x
        }
        case Right(value) => {
          sourceRegister + ", " + "ror " + "#" + value
        }
      }
    }
  }

  case class RotateRightExtended(sourceRegister: X86Register) extends LHSop {
    override def toString: String = {
      sourceRegister + ", " + "rrx"
    }
  }

  sealed trait X86Suffi extends Suffi
  
  case class Control() extends X86Suffi {
    override def toString: String = {
      "c"
    }
  }

  case class Extension() extends X86Suffi {
    override def toString: String = {
      "x"
    }
  }

  case class Status() extends X86Suffi {
    override def toString: String = {
      "s"
    }
  }

  case class Flags() extends X86Suffi {
    override def toString: String = {
      "f"
    }
  }

  case class None() extends X86Suffi {
    override def toString: String = {
      ""
    }
  }
}
