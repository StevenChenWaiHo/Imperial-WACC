package wacc

import wacc.AssemblerTypes._
  trait LHSop

  class StackOffset(offset: Int) extends LHSop
  class ImmediateInt(i: Int) extends LHSop
  class LabelString(name: String) extends LHSop
  class BranchString(name: String) extends LHSop

  trait Register extends LHSop

  // ARM/X86
  trait GeneralRegister extends Register with Ordered[GeneralRegister] {
    import scala.math.Ordered.orderingToOrdered
    def compare(that: GeneralRegister): Int = listOfRegisters.get(this) compare listOfRegisters.get(that)
    val listOfRegisters: Map[GeneralRegister, Int]
  } // r0-14?/other regs
  class SPRegister extends Register // sp/rsp
  class FPRegister extends Register // fp/rbp
  // are the below needed
  trait LinkRegister extends Register // lr/??
  trait PCRegister extends Register // pc/??
  //rbx as basepointer?

  trait Suffi

  class Control extends Suffi
  class Extension extends Suffi
  class Status extends Suffi
  class Flags extends Suffi
  class None extends Suffi
}
