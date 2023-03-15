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

    private var currentNodes: Vector[CFGNode] = tacs.zipWithIndex.map(x => {
      val (instr, id) = x
      val (uses, defs, succs) = nodeInfo.getInfo(instr, tacs, id)
      CFGNode(id, instr, uses, defs, succs)
    })

    val nodes: Vector[CFGNode] = computeGraph()

    val tRegisterCount: Int = nodes.foldRight(Set[TRegister]()) {
      (node, set) => (node.uses union node.defs) union set
    }.size

    def getSuccs(node: CFGNode): Set[CFGNode] = node.succs.map(currentNodes(_))

    private def updateLiveIns(node: CFGNode): CFGNode = node withLiveIns (node.uses union (node.liveOut diff node.defs))

    private def updateLiveOuts(node: CFGNode): CFGNode = node withLiveOuts getSuccs(node).flatMap(_.liveIn)

    @tailrec
    private def computeGraph(): Vector[CFGNode] = {
      val next: Vector[CFGNode] = currentNodes.map(x => updateLiveOuts(updateLiveIns(x)))
      println("\nUpdate graph:")
      println(currentNodes)
      println(next)
      if (currentNodes != next) {
        currentNodes = next
        computeGraph()
      }
      else currentNodes
    }

    def getInfo(tac: TAC, id: Id): (Set[TRegister], Set[TRegister], Set[Id]) = nodeInfo.getInfo(tac, tacs, id)

    override def toString: String = nodes.foldLeft("") {
      (s, x) => s + x.toString + "\n"
    }
  }

}
