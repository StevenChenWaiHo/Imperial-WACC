package wacc

import wacc.AssemblerTypes._

object X86AssemblerTypes {
  //TODO change these to x86 format
  //TODO change all calls to LHSop instances in ALL files
  class X86StackOffset(offset: Int) extends StackOffset(offset: Int) {
    override def toString(): String = "STACK" + offset.toString()
  }

  class X86ImmediateInt(i: Int) extends ImmediateInt(i: Int) {
    override def toString(): String = i.toString()
  }

  class X86LabelString(name: String) extends LabelString(name: String) {
    override def toString(): String = "=" + name
  }

  class X86BranchString(name: String) extends BranchString(name: String) {
    override def toString(): String = name
  }

  sealed trait X86Register extends Register {
    import scala.math.Ordered.orderingToOrdered
    def compare(that: X86Register): Int = listOfRegisters.get(this) compare listOfRegisters.get(that)
    // rbx/rsp/rbp should not be here
    val listOfRegisters = Map[Register, Int](rax -> 0, rcx -> 1, rdx -> 2, rsi -> 6, rdi -> 7,
    r8 -> 8, r9 -> 9, r10 -> 10, r11 -> 11, r12 -> 12, r13 -> 13, r14 -> 14, r15 -> 15)
  }

  object rax extends X86Register {
    override def toString(): String = "rax" // r0 equiv.
  }

  object rcx extends X86Register {
    override def toString(): String = "rcx" // 4th arg (assume r4 equiv.)
  }

  object rdx extends X86Register {
    override def toString(): String = "rdx" // r3 equiv.
  }

  object rbx extends Register {
    override def toString(): String = "rbx" // base pointer? (lr/pc) PRESERVED
  }

  object rsp extends Register {
    override def toString(): String = "rsp" // sp PRESERVED
  }

  object rbp extends Register {
    override def toString(): String = "rbp" // fp PRESERVED
  }

  object rsi extends X86Register {
    override def toString(): String = "rsi" // r2 equiv.
  }

  object rdi extends X86Register {
    override def toString(): String = "rdi" // r1 equiv.
  }

  object r8 extends X86Register {
    override def toString(): String = "r8" // 5th arg
  }

  object r9 extends X86Register {
    override def toString(): String = "r9" // 6th arg
  }

  object r10 extends X86Register {
    override def toString(): String = "r10" // temp
  }

  object r11 extends X86Register {
    override def toString(): String = "r11" // temp
  }

  object r12 extends X86Register {
    override def toString(): String = "r12" // PRESERVED
  }

  object r13 extends X86Register {
    override def toString(): String = "r13" // PRESERVED
  }

  object r14 extends X86Register {
    override def toString(): String = "r14" // PRESERVED
  }

  object r15 extends X86Register {
    override def toString(): String = "r15" // PRESERVED
  }

  // object lr extends X86Register {
  //   override def toString(): String = "lr"
  // }

  // object pc extends X86Register {
  //   override def toString(): String = "pc"
  // }

  sealed trait LHSop

  case class ImmediateValueOrRegister(operand: Either[X86Register, Int]) extends LHSop {
    @Override
    override def toString: String = {
      operand match {
        case Left(value) => {
          value.toString
        }
        case Right(value) => {
          value.toString
        }
      }
    }
  }

  case class LogicalShiftLeft(sourceRegister: X86Register, operand: Either[X86Register, Int]) extends LHSop {
    override def toString: String = {
      operand match {
        case Left(x) => {
          sourceRegister + ", " + "shl " + x
        }
        case Right(value) => {
          sourceRegister + ", " + "shl " + value
        }
      }
    }
  }

  case class LogicalShiftRight(sourceRegister: X86Register, operand: Either[X86Register, Int]) extends LHSop {
    override def toString: String = {
      operand match {
        case Left(x) => {
          sourceRegister + ", " + "shr " + x
        }
        case Right(value) => {
          sourceRegister + ", " + "shr " + " " + value
        }
      }
    }
  }

  case class ArithmeticShiftRight(sourceRegister: X86Register, operand: Either[X86Register, Int]) extends LHSop {
    override def toString: String = {
      operand match {
        case Left(x) => {
          sourceRegister + ", " + "sar " + x
        }
        case Right(value) => {
          sourceRegister + ", " + "sar " + value
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
          sourceRegister + ", " + "ror " + value
        }
      }
    }
  }

  // dont think its used
  class X86Control() extends Control {
    override def toString: String = {
      ""
    }
  }

  class X86Extension() extends Extension {
    override def toString: String = {
      ""
    }
  }

  class X86Status() extends Status {
    override def toString: String = {
      ""
    }
  }

  class X86Flags() extends Flags {
    override def toString: String = {
      ""
    }
  }

  class X86None() extends None {
    override def toString: String = {
      ""
    }
  }
}