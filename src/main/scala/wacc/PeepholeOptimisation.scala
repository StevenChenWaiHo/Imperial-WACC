package wacc

import wacc.FinalIR._
import wacc.AssemblerTypes._

object  PeepholeOptimisation {
  def PeepholeOptimise(code: List[FinalIR]): List[FinalIR] = {
    // Loop through code applying optimisations
    code.filterNot(isNullOp)
  }

  // Convert higher processing time instructions to lower cost ones
  def strengthReduction(instr: FinalIR): FinalIR = {
    instr match {
        // src*(2^x) => src << x
        case Mul(_, _, op1, op2, dst)
            if (op1 equals dst) && (op2 equals ImmediateInt(2)) => true
    }
    instr
  }

  // Identify if the instruction can be removed entirely
  def isNullOp(instr: FinalIR): Boolean = {
    instr match {
        // Mov src->src
        case Mov(_, src, dst)
            if src equals dst => true
        // Str/Ldr src->src
        case Str(condition, src, operand, dst) => false // TODO: implement correctly
        case StrPre(condition, src, operand, dst) => false
        case Ldr(condition, src, operand, dst) => false
        // Add/Sub with 0
        case Add(_, _, op1, ImmediateInt(0), dst) if (op1 equals dst) => true
        case Add(_, _, ImmediateInt(0), op2, dst) if (op2 equals dst) => true
        case Sub(_, _, op1, ImmediateInt(0), dst) if (op1 equals dst) => true
        case Sub(_, _, ImmediateInt(0), op2, dst) if (op2 equals dst) => true
        // Mul with 1
        case Mul(_, _, op1, ImmediateInt(1), dst) if (op1 equals dst) => true
        case Mul(_, _, ImmediateInt(1), op2, dst) if (op2 equals dst) => true
        case Smull(_, _, op1, ImmediateInt(1), dst, dst1) if (op1 equals dst) => false // TODO: implement correctly
        case Smull(_, _, ImmediateInt(1), op2, dst, dst1) if (op2 equals dst) => false
        // Empty push/pop
        case Push(_, List()) => true
        case Pop(_, List()) => true
        case _ => false
    }
  }
}
