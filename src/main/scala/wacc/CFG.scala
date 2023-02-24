package wacc

import scala.collection.mutable.ListBuffer
import wacc.AbstractSyntaxTree._
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.TAC._
import wacc.Translator._

object CFG {
  private val labelMap = collection.mutable.Map[Label, Id]()

  type Id = Int

  sealed trait CFGReg
  class DReg(x: Int) extends CFGReg
  class TReg(x: Int) extends CFGReg

  case class CFGNode(id: Id, instr: TAC, uses: ListBuffer[CFGReg],
                     defs: ListBuffer[CFGReg], succs: ListBuffer[Id], preds: ListBuffer[Id])
  
  case class LiveIn(id: Id, regs: ListBuffer[CFGReg])
  case class LiveOut(id: Id, regs: ListBuffer[CFGReg])

  def buildCFGNode(instr: TAC, id: Id) : CFGNode = {
    instr match {
        case BinaryOpTAC(op, t1, t2, res) => CFGNode(instr, List(t1, t2), List(res), List(id + 1), List())
        case UnaryOpTAC(op, t1, res) => CFGNode(instr, List(t1), List(res), List(id + 1), List())
        case AssignmentTAC(t1, res) => CFGNode(instr, List(t1), List(res), List(id + 1), List())
        case IfTAC(t1, goto) => CFGNode(instr, List(t1), List(), List(id + 1, labelMap.get(goto)), List())
        case CommandTAC(cmd, t1) => CFGNode(instr, List(t1), List(), List(id + 1), List())
        case PushParamTAC(t1) => CFGNode(instr, List(t1), List(), List(id + 1), List())
        case PopParamTAC(t1) => CFGNode(instr, List(t1), List(), List(id + 1), List())
        case CallTAC(f) => CFGNode(instr, List(), List(), List(label.get(f)), List())
        case GOTO(label) => CFGNode(instr, List(), List(), List(label.get(label)), List())
        case Label(name) => CFGNode(instr, List(), List(), List(id + 1), List())
        case _: TAC => 
    }
  }
  
  def buildCFGList(instrs: List[TAC]) : List[CFGNode] = {
    // Record Label Id
    instrs.zipWithIndex.foreach{case (Label(name), i) => labelMap.addOne(name, i)}

    instrs.zipWithIndex.map(buildCFGNode)
  }
}
