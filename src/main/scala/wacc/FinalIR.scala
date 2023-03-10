package wacc

import wacc.AssemblerTypes._

object FinalIR {
  sealed trait FinalIR

  case class Str(condition: String, src: Register, operand: LHSop, dst: Register)
  case class StrPre(condition: String, src: Register, operand: LHSop, dst: Register)
  case class Ldr(condition: String, src: Register, operand: LHSop, dst: Register)

  case class Push(condition: String, regs: List[Register])
  case class Pop(condition: String, regs: List[Register])
  
  case class Add(condition: String, setFlag: Suffi, op1: Register, op2: Register, dst: Register)
  case class Sub(condition: String, setFlag: Suffi, op1: Register, op2: Register, dst: Register)
  case class Rsb(condition: String, setFlag: Suffi, op1: Register, op2: Register, dst: Register)
  case class Mul(condition: String, setFlag: Suffi, op1: Register, op2: Register, dst: Register)

  case class Move(condition: String, src: Register, dst: Register)

  case class Branch(condition: String, name: String)
  case class BranchLink(condition: String, name: LHSop)
}
