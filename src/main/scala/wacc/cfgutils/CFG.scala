package wacc.cfgutils

import wacc.TAC._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions

type Id = Int

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

object CFG {

  class CFG[A](instrs: Vector[TAC], regLimit: Int) {
    var nodes: Vector[CFGNode] = instrs.zipWithIndex.map(x => makeNode(x._1, x._2))
    nodes = iterate(nodes)
    val interferences: InterferenceGraph[A] = buildInterferenceGraph

    implicit def toCFGReg(tReg: TRegister): TReg = TReg(tReg.num)

    implicit def toUsedRegisters(ops: List[Operand]): Set[CFGReg] = ops.collect {
      case TRegister(num) => TReg(num)
    }.toSet

    def getId(l: Label): Id = instrs.indexOf(l)


    def makeNode(instr: TAC, id: Id): CFGNode = {
      var uses: Set[CFGReg] = Set()
      var defs: Set[CFGReg] = Set()
      var succs: List[Id] = List(id + 1)

      instr match {
        case BinaryOpTAC(_, t1, t2, res) => {
          uses = List(t1, t2)
          defs = List(res)
        }
        case UnaryOpTAC(_, t1, res) => {
          uses = List(t1)
          defs = List(res)
        }
        case AssignmentTAC(t1, res) => {
          uses = List(t1)
          defs = List(res)
        }
        case IfTAC(t1, lbl) => {
          uses = List(t1)
          succs = List(id + 1, getId(lbl))
        }
        case EndFuncTAC() => {
          succs = Nil
        }
        case CommandTAC(_, t1, _) =>
          uses = List(t1)
        case PushParamTAC(t1) =>
          uses = List(t1)
        case PopParamTAC(_, t1, _) =>
          uses = List(t1)
        case CallTAC(lbl, args, dstReg) =>
          uses = args //TODO: uses could probably be left empty here
          defs = List(dstReg)
          succs = List(id + 1, getId(lbl)) //TODO: does this work?
        case GOTO(lbl) =>
          succs = List(getId(lbl))
        case CreatePairElem(_, _, ptr, value) => //TODO: These pair-related ones could be wrong:
          uses = List(value)
          defs = List(ptr)
        case CreatePair(_, _, fstReg, sndReg, srcReg, ptrReg, dstReg) => //TODO
          print("CreatePair not yet translated in CFG.scala")
        case _ =>

        //TODO
      }
      CFGNode(id, instr, uses, defs, succs.toSet)
    }

    def getNodes(succs: Set[Id]): Set[CFGNode] = succs.map(nodes(_))

    def updateLiveIns(node: CFGNode): CFGNode =
      node.addLiveIns(node.uses union (node.liveOut diff node.defs))

    def updateLiveOuts(node: CFGNode): CFGNode =
      node.addLiveOuts(getNodes(node.succs).flatMap(_.liveIn))

    private def iterate(nodes: Vector[CFGNode]): Vector[CFGNode] = {
      var before: Vector[CFGNode] = null
      var after: Vector[CFGNode] = nodes
      while (before != after) {
        before = after
        after = after.map(x => {
          updateLiveOuts(updateLiveIns(x))
        })
      }
      after
    }

    private def buildInterferenceGraph: InterferenceGraph[A] = {
      var interferes = scala.collection.mutable.Map[CFGReg, Set[CFGReg]]()
      nodes.foreach {
        case CFGNode(_, _, _, _, _, _, liveOut) =>
          liveOut.foreach(t => interferes.update(t, interferes(t) union liveOut))
      }
      new InterferenceGraph(interferes.toMap)
    }

    /* Returns either a mapping or the furthest register it got to before no mapping was possible.
    * Note: regs should be comparable using ==. */
    def findColouring(regs: List[A]): Option[Map[CFGReg, A]] = {
      // Extremely inefficient:
      def colour(rRegs: List[A], ints: InterferenceGraph[A]): Option[Map[CFGReg, A]] = {
        if (ints.tRegisters.isEmpty) return Some(Map())
        for (t <- ints.tRegisters) {
          for (r <- rRegs) {
            if (ints.canAssign(t, RReg(r))) {
              val result = colour(rRegs, ints.assign(t, RReg(r))).map(_.updated(t, r))
              if (result.isDefined) return result
            }
          }
        }
        None
      }

      colour(regs, interferences)
    }
  }

  class InterferenceGraph[A](interferences: Map[CFGReg, Set[CFGReg]]) {
    def tRegisters = interferences.keys

    // no interference when assigning r to t
    def canAssign(t: CFGReg, r: CFGReg): Boolean = !(interferences(t).excl(t) contains r)

    // Assign reg2 to reg1
    def assign(reg1: CFGReg, reg2: CFGReg): InterferenceGraph[A] = {
      val newInterferences: mutable.Map[CFGReg, Set[CFGReg]] = interferences.to(mutable.Map)
      for (r <- interferences.keys) {
        if (interferences(r) contains reg1) {
          newInterferences.update(r, interferences(r).excl(r).incl(reg2))
        }
      }
      newInterferences.remove(reg1)
      new InterferenceGraph[A](newInterferences.toMap)
    }
  }
  // def buildCFGNode(instr: TAC, id: Id) : CFGNode = {
  //   instr match {
  //       case BinaryOpTAC(op, t1, t2, res) => CFGNode(instr, List(t1, t2), List(res), List(id + 1), List())
  //       case UnaryOpTAC(op, t1, res) => CFGNode(instr, List(t1), List(res), List(id + 1), List())
  //       case AssignmentTAC(t1, res) => CFGNode(instr, List(t1), List(res), List(id + 1), List())
  //       case IfTAC(t1, goto) => CFGNode(instr, List(t1), List(), List(id + 1, labelMap.get(goto)), List())
  //       case CommandTAC(cmd, t1) => CFGNode(instr, List(t1), List(), List(id + 1), List())
  //       case PushParamTAC(t1) => CFGNode(instr, List(t1), List(), List(id + 1), List())
  //       case PopParamTAC(t1) => CFGNode(instr, List(t1), List(), List(id + 1), List())
  //       case CallTAC(f) => CFGNode(instr, List(), List(), List(label.get(f)), List())
  //       case GOTO(label) => CFGNode(instr, List(), List(), List(label.get(label)), List())
  //       case Label(name) => CFGNode(instr, List(), List(), List(id + 1), List())
  //       case _: TAC =>
  //   }
  // }

  //def buildCFGList(instrs: List[TAC]) : List[CFGNode] = {
  // Record Label Id
  //instrs.zipWithIndex.foreach{case (Label(name), i) => labelMap.addOne(name, i)}

  //instrs.zipWithIndex.map(buildCFGNode)
  //}
}
