package wacc

object AssemblerTypes {
  sealed trait LHSop

  case class StackOffset(offset: Int) extends LHSop {
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

  sealed trait Register extends LHSop

  object r0 extends Register {
    override def toString(): String = "r0"
  }

  object r1 extends Register {
    override def toString(): String = "r1"
  }

  object r2 extends Register {
    override def toString(): String = "r2"
  }

  object r3 extends Register {
    override def toString(): String = "r3"
  }

  object r4 extends Register {
    override def toString(): String = "r4"
  }

  object r5 extends Register {
    override def toString(): String = "r5"
  }

  object r6 extends Register {
    override def toString(): String = "r6"
  }

  object r7 extends Register {
    override def toString(): String = "r7"
  }

  object r8 extends Register {
    override def toString(): String = "r8"
  }

  object r9 extends Register {
    override def toString(): String = "r9"
  }

  object r10 extends Register {
    override def toString(): String = "r10"
  }

  object r11 extends Register {
    override def toString(): String = "r11"
  }

  object r12 extends Register {
    override def toString(): String = "r12"
  }

  object r13 extends Register {
    override def toString(): String = "r13"
  }

  object r14 extends Register {
    override def toString(): String = "r14"
  }

  object fp extends Register {
    override def toString(): String = "fp"
  }

  object lr extends Register {
    override def toString(): String = "lr"
  }

  object pc extends Register {
    override def toString(): String = "pc"
  }

  object sp extends Register {
    override def toString(): String = "sp"
  }

  val listOfRegisters = Map[Register, Int](r0 -> 0, r1 -> 0, r2 -> 0, r3 -> 0, r4 -> 0, r5 -> 0, r6 -> 0,
    r7 -> 0, r8 -> 0, r9 -> 0, r10 -> 0, r11 -> 0, r12 -> 0, r13 -> 0, r14 -> 0)


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

}
