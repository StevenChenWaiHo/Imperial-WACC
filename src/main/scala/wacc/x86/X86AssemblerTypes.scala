

//TODO Change all to x86_64 Architecture

object x86AssemblerTypes {
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

  sealed trait Register extends LHSop with Ordered[Register] {
      import scala.math.Ordered.orderingToOrdered
    def compare(that: Register): Int =  listOfRegisters.get(this) compare listOfRegisters.get(that)
  }

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

  val listOfRegisters = Map[Register, Int](r0 -> 0, r1 -> 1, r2 -> 2, r3 -> 3, r4 -> 4, r5 -> 5, r6 -> 6,
    r7 -> 7, r8 -> 8, r9 -> 9, r10 -> 10, r11 -> 11, r12 -> 12, r13 -> 13, r14 -> 14)


  sealed trait Operand2 //is this needed?

  case class ImmediateValueOrRegister(operand: Either[Register, Int]) extends LHSop {
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

  case class LogicalShiftLeft(sourceRegister: Register, operand: Either[Register, Int]) extends LHSop {
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

  case class LogicalShiftRight(sourceRegister: Register, operand: Either[Register, Int]) extends LHSop {
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

  case class ArithmeticShiftRight(sourceRegister: Register, operand: Either[Register, Int]) extends LHSop {
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

  case class RotateRight(sourceRegister: Register, operand: Either[Register, Int]) extends LHSop {
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

  case class RotateRightExtended(sourceRegister: Register) extends LHSop {
    override def toString: String = {
      sourceRegister + ", " + "rrx"
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
