package wacc

import wacc.TAC._

import scala.collection.mutable
import scala.language.implicitConversions

object CFG {
  type Id = Int

  sealed trait CFGReg

  // Real Register
  case class RReg[A](x: A) extends CFGReg

  // Temporary Register
  case class TReg(x: Int) extends CFGReg

  implicit def toCFGReg(tReg: TRegister): TReg = TReg(tReg.num)

  implicit def toUsedRegisters(ops: Set[Operand]): Set[CFGReg] = ops.collect {
    case TRegister(num) => TReg(num)
  }.toSet

  case class CFGNode(val id: Id, instr: TAC, uses: Set[CFGReg], defs: Set[CFGReg],
                     succs: Set[Id], liveIn: Set[CFGReg] = Set(), liveOut: Set[CFGReg] = Set()) {
    def addLiveIns(regs: Set[CFGReg]): CFGNode = CFGNode(id, instr, uses, defs, succs, liveIn + regs, liveOut)

    def addLiveOuts(regs: Set[CFGReg]): CFGNode = CFGNode(id, instr, uses, defs, succs, liveIn, liveOut + regs)
  }

  class CFG[A](instrs: Vector[TAC], regLimit: Int) {
    val nodes: Vector[CFGNode] = iterate(instrs.zipWithIndex.map(x => makeNode(x._1, x._2)))
    val interferences: InterferenceGraph[A] = buildInterferenceGraph

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
      CFGNode(id, instr, uses, defs, succs)
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
    def findColouring(regs: List[A]): Option[Map[TReg, A]] = {
      // Extremely inefficient:
      def colour(rRegs: List[A], ints: InterferenceGraph[A]): Option[Map[TReg, A]] = {
        for(t <- ints.tRegisters) {
          for(r <- rRegs) {
            if(ints.canAssign(t, RReg(r))) {
              val result = colour(rRegs, ints.assign(t, RReg(r)))
              if(result.isDefined) return result
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
