package wacc

object AssemblerTypes {
  class LHSop

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

  class Register extends LHSop with Ordered[Register] {
    import scala.math.Ordered.orderingToOrdered
    def compare(that: Register): Int
  }

  //registers should be defined here? (in ARM format, more registers)

  class Suffi
}