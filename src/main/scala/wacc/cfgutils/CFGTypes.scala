package wacc.cfgutils

import wacc.TAC.{TAC, TRegister}
import wacc.cfgutils.CFG.Id

trait CFGNodeInfo {
  /** Given a TAC, return:
   * (the set of tRegisters it uses, the set of tRegisters it defines, the indices of its successor TACs). */
  def getInfo(tac: TAC, program: Vector[TAC]): (Set[TRegister], Set[TRegister], Set[Id])
}

case class Colouring[A](coloured: Map[TRegister, A], uncoloured: Set[TRegister])
abstract class RegisterAllocator[A] {
  def allocateRegisters: (Vector[TAC], Colouring[A])
}