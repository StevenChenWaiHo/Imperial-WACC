package wacc

import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.CmdT.Cmd

object TAC {
    sealed trait TAC
  
  case class BinaryOpTAC(op: BinOp, t1: Operand, t2: Operand, res: TRegister) extends TAC
  case class UnaryOpTAC(op: UnOp, t1: Operand, res: TRegister) extends TAC
  case class AssignmentTAC(t1: Operand, res: TRegister) extends TAC
  case class IfTAC(t1: Operand, goto: Label) extends TAC
  case class CommandTAC(cmd: Cmd, t1: Operand) extends TAC
  case class PushParamTAC(t1: Operand) extends TAC
  case class PopParamTAC(t1: TRegister) extends TAC
  case class CallTAC(f: Label) extends TAC
  case class BeginFuncTAC() extends TAC
  case class EndFuncTAC() extends TAC
  case class GOTO(label: Label) extends TAC
  case class Label(name: String = "label") extends TAC

  sealed trait Operand
  class TRegister(num: Int) extends Operand with TAC
  class Literal() extends Operand
  
}