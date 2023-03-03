package wacc

import wacc.TAC._

import scala.language.implicitConversions

object CFG {
  private val labelMap = collection.mutable.Map[Label, Id]()

  type Id = Int

  sealed trait CFGReg

  case class RReg(x: Int) extends CFGReg

  case class TReg(x: Int) extends CFGReg

  implicit def toCFGReg(tReg: TRegister): TReg = TReg(tReg.num)

  implicit def toUsedRegisters(ops: Set[Operand]): Set[CFGReg] = ops.collect {
    case TRegister(num) => TReg(num)
  }.toSet

  class CFG(instrs: Vector[TAC]) {
    var nodes: Vector[CFGNode] = instrs.zipWithIndex.map(x => makeNode(x._1, x._2))

    def makeNode(instr: TAC, id: Id): CFGNode = {
      var uses: Set[CFGReg] = Set()
      var defs: Set[CFGReg] = Set()
      var succs: Set[Id] = Set()

      instr match {
        case BinaryOpTAC(_, t1, t2, res) => {
          uses = Set(t1, t2)
          defs = Set(res)
          succs = Set(id + 1)
        }
        //TODO
      }
      new CFGNode(id, instr, uses, defs, succs)
    }

    case class CFGNode(val id: Id, instr: TAC, uses: Set[CFGReg], defs: Set[CFGReg],
                       succs: Set[Id], liveIn: Set[CFGReg] = Set(), liveOut: Set[CFGReg] = Set()) {
      def addLiveIns(regs: Set[CFGReg]): CFGNode = CFGNode(id, instr, uses, defs, succs, liveIn + regs, liveOut)

      def addLiveOuts(regs: Set[CFGReg]): CFGNode = CFGNode(id, instr, uses, defs, succs, liveIn, liveOut + regs)
    }

    def getNodes(succs: Set[Id]): Set[CFGNode] = succs.map(nodes(_))

    def updateLiveIns(node: CFGNode): CFGNode =
      node.addLiveIns(node.uses union (node.liveOut diff node.defs))

    def updateLiveOuts(node: CFGNode): CFGNode =
      node.addLiveOuts(getNodes(node.succs).flatMap(_.liveIn))

    private def iterate: Unit = {
      var result: Vector[CFGNode] = null
      while (result != nodes) {
        result = nodes
        nodes.map(x => {
          updateLiveOuts(updateLiveIns(x))
        })
      }
    }

    private def buildInterferenceGraph: Map[CFGReg, Set[CFGReg]] = {
      var interferes = scala.collection.mutable.Map[CFGReg, Set[CFGReg]]()
      nodes.foreach {
        case CFGNode(_, _, _, _, _, _, liveOut) =>
          liveOut.foreach(t => interferes.update(t, interferes(t) union liveOut))
      }
      interferes.toMap
    }

    private def canAssign(t: TReg, r: RReg): Boolean = {
      
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
