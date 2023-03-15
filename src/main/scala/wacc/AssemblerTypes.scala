package wacc

object AssemblerTypes {
  trait LHSop

  trait StackOffset extends LHSop {
    def offset: Int
  }

  trait ImmediateInt extends LHSop {
    def i: Int
  }

  trait LabelString extends LHSop {
    def name: String
  }

  trait BranchString extends LHSop {
    def name: String
  }

  trait Register extends LHSop with Ordered[Register] {
    import scala.math.Ordered.orderingToOrdered
    def compare(that: Register): Int
  }

  // ARM/X86
  trait GeneralRegister extends Register // r0-14?/other regs
  trait SPRegister extends Register // sp/rsp
  trait FPRegister extends Register // fp/rbp
  // are the below needed
  trait LinkRegister extends Register // lr/??
  trait PCRegister extends Register // pc/??
  //rbx as basepointer?

  //registers should be defined here? (in ARM format, more registers)

  trait Suffi

  trait Control extends Suffi
  trait Extension extends Suffi
  trait Status extends Suffi
  trait Flags extends Suffi
  trait None extends Suffi
}