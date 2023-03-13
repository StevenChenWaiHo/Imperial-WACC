package wacc

import wacc.FinalIR._
import wacc.AssemblerTypes._

object  PeepholeOptimisation {
  def PeepholeOptimise(code: List[FinalIR]): List[FinalIR] = {
    // Loop through code applying optimisations
    code.filterNot(isNullOp)
  }

  // Identify if the operation can be removed entirely
  def isNullOp(code: FinalIR): Boolean = {
    code match {
        // Mov src->src
        case Mov(_, src, dst) 
            if src equals dst => true
        // Str/Ldr src->src
        case Str(condition, src, operand, dst) => false // TODO: implement correctly
        case StrPre(condition, src, operand, dst) => false
        case Ldr(condition, src, operand, dst) => false
        // Add/Sub with 0
        case Add(_, _, op1, op2, dst) 
            if (op1 equals dst) && (op2 equals ImmediateInt(0)) => true
        case Add(_, _, op1, op2, dst) 
            if (op2 equals dst) && (op1 equals ImmediateInt(0)) => true
        case Sub(_, _, op1, op2, dst) 
            if (op1 equals dst) && (op2 equals ImmediateInt(0)) => true
        case Sub(_, _, op1, op2, dst) 
            if (op2 equals dst) && (op1 equals ImmediateInt(0)) => true
        // Mul with 1
        case Mul(_, _, op1, op2, dst)
            if (op1 equals dst) && (op2 equals ImmediateInt(1)) => true
        case Mul(_, _, op1, op2, dst)
            if (op2 equals dst) && (op1 equals ImmediateInt(1)) => true
        case Smull(_, _, op1, op2, dst, dst1)
            if (op1 equals dst) && (op2 equals ImmediateInt(1)) => false // TODO: implement correctly
        case Smull(_, _, op1, op2, dst, dst1)
            if (op2 equals dst) && (op1 equals ImmediateInt(1)) => false
        // Empty push/pop
        case Push(_, regs) if regs.isEmpty => true
        case Pop(_, regs) if regs.isEmpty => true
        case _ => false
    }
  }
}
