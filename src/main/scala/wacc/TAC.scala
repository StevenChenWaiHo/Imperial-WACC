package wacc

import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.CmdT.Cmd

class TAC {

  case class BinaryOp(op: BinOp, t1: Operand, t2: Operand, res: TRegister) extends TAC
  case class UnaryOp(op: UnOp, t1: Operand, res: TRegister) extends TAC
  case class Assignment(t1: Operand, res: TRegister) extends TAC
  case class IfStat(t1: Operand, goto: Label) extends TAC
  case class Command(cmd: Cmd, t1: Operand) extends TAC
  case class PushParam(t1: Operand) extends TAC
  case class PopParam(t1: TRegister) extends TAC
  case class Call(f: Label) extends TAC
  case class BeginFunc() extends TAC
  case class EndFunc() extends TAC

  sealed trait Operand
  class TRegister() extends Operand
  class Literal() extends Operand

  class Label()
}

