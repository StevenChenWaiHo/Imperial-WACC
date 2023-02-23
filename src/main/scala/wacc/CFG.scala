package wacc

import scala.collection.mutable.ListBuffer
import wacc.AbstractSyntaxTree._
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.TAC._
import wacc.Translator._

object CFG {
  sealed trait CFGReg
  class DReg() extends CFGReg
  class TReg() extends CFGReg

  case class CFGNode(id: Int, instr: TAC, uses: ListBuffer[TRegister],
                     defs: ListBuffer[TRegister], succs: ListBuffer[Int], preds: ListBuffer[Int])
  
  case class LiveIn(id: Int, regs: ListBuffer[TRegister])
  case class LiveOut(id: Int, regs: ListBuffer[TRegister])
  
  def buildCFG(instrs: List[TAC], treg: TRegister) = {
    for ()
  }
}
