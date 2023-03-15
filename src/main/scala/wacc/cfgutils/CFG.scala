package wacc.cfgutils

import wacc.TAC._

import scala.annotation.tailrec
import scala.language.implicitConversions

object CFG {
  type Id = Integer

  class CFGBuilder(nodeInfo: CFGNodeInfo) {
    def build(tacs: Vector[TAC]): CFG = new CFG(tacs, nodeInfo)
  }

  case class CFGNode(val id: Id, instr: TAC, uses: Set[TRegister], defs: Set[TRegister],
                     succs: Set[Id], liveIn: Set[TRegister] = Set(), liveOut: Set[TRegister] = Set(), depth: Int = 0) {
    def withLiveIns(newLiveIns: Set[TRegister]): CFGNode = this.copy(liveIn = liveIn union newLiveIns)

    def withLiveOuts(newLiveOuts: Set[TRegister]): CFGNode = this.copy(liveOut = liveOut union newLiveOuts)
  }

  class CFG(val tacs: Vector[TAC], val nodeInfo: CFGNodeInfo) {

    val nodes: Vector[CFGNode] = {
      val initialNodes = tacs.zipWithIndex.map(x => {
        val (instr, id) = x
        val (uses, defs, succs) = nodeInfo.getInfo(instr, tacs)
        CFGNode(id, instr, uses, defs, succs)
      })
      computeGraph(initialNodes)
    }

    val tRegisterCount = nodes.foldRight(Set[TRegister]()) {
      (node, set) => (node.uses union node.defs) union set
    }.size

    def getSuccs(node: CFGNode): Set[CFGNode] = node.succs.map(nodes(_))

    private def updateLiveIns(node: CFGNode): CFGNode = node withLiveIns (node.uses union (node.liveIn diff node.defs))

    private def updateLiveOuts(node: CFGNode): CFGNode = node withLiveOuts getSuccs(node).flatMap(_.liveIn)

    @tailrec
    private def computeGraph(current: Vector[CFGNode]): Vector[CFGNode] = {
      val next: Vector[CFGNode] = current.map(x => updateLiveOuts(updateLiveIns(x)))
      if (current != next) computeGraph(next)
      else current
    }

    def getInfo(tac: TAC): (Set[TRegister], Set[TRegister], Set[Id]) = nodeInfo.getInfo(tac, tacs)
  }
}
