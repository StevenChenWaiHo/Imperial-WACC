package wacc.cfgutils

import wacc.TAC.{TAC, TRegister}

trait CFGNodeInfo {
  /** Given a TAC, return:
   * (the set of tRegisters it uses, the set of tRegisters it defines, the indices of its successor TACs). */
  def getInfo(tac: TAC, program: Vector[TAC]): (Set[TRegister], Set[TRegister], Set[Id])
}

abstract class RegisterAllocator[A] {
  def allocateRegisters(regs: List[A], cfg: CFG): (Vector[TAC], Map[TRegister, A], List[Id])
}