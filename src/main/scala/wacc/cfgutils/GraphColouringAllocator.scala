package wacc.cfgutils

import wacc.TAC._
import wacc.cfgutils.CFG.{CFG, CFGBuilder, CFGNode, Id}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.{immutable, mutable}
import scala.util.control.Breaks.break

class GraphColouringAllocator[A](regs: List[A], tacs: Vector[TAC], cfgBuilder: CFGBuilder) extends RegisterAllocator[A] {
  /* State */
  private var cfg: CFG = null
  private var interferenceGraph: InterferenceGraph = null
  private var colourer: GraphColourer[A] = null
  private var colouring: Colouring[A] = null

  /* nextTacs determines the next state at each iteration */
  private var nextTacs: Vector[TAC] = tacs
  recolour()

  override def allocateRegisters: (Vector[TAC], Colouring[A]) = {
    while (colouring.uncoloured.nonEmpty) {
      spill(colouring.uncoloured)
    }
    (nextTacs, colouring)
  }

  private def recolour(): Unit = {
    cfg = cfgBuilder.build(nextTacs)
    interferenceGraph = new InterferenceGraph(cfg)
    colourer = new GraphColourer[A](regs, interferenceGraph)
    colouring = colourer.attemptColouring
    println("\n--- Recolour: ---")
    println(colouring)
    println
    println(interferenceGraph.interferences)
  }

  private def spill(uncoloured: Set[TRegister]): Unit = {
    //TODO: Currently doesn't use the interference graph
    /* Simple strategy: spill the uncolourable node with the most interferences */
    val target = uncoloured.maxBy(x => interferenceGraph.interferences(x).size)

    /* Add a 'push' after each definition, and a 'pop' after each use. */
    @tailrec
    def modifyGraph(initial: Vector[CFGNode], result: Vector[TAC]): Vector[TAC] = initial match {
      case n +: ns if n.defs contains target =>
        modifyGraph(ns, n.instr +: (ReservedPushTAC(target) +: result))
      case n +: ns if n.uses contains target =>
        modifyGraph(ns, Vector(ReservedPopTAC(target), n.instr, ReservedPopTAC(target)) ++ result)
      case n +: ns =>
        modifyGraph(ns, n.instr +: result)
      case Vector() => result
    }

    nextTacs = modifyGraph(cfg.nodes, Vector[TAC]())
  }
}

class GraphColourer[A](val regs: List[A], interferenceGraph: InterferenceGraph) {
  private type IGNode = (TRegister, Set[TRegister])

  def attemptColouring: Colouring[A] = {
    val stack = mutable.Stack[(TRegister, Set[TRegister])]()
    val interferences = interferenceGraph.interferences.to(mutable.Map)

    if (interferences.isEmpty) {
      println("WARNING: Attempted to allocate registers to an empty program! \n")
      return Colouring(Map(), Set())
    }

    pushAllToStack(interferences, stack)

    // Any interferences not in the stack are uncolourable
    var colouring = Colouring(Map[TRegister, A](), interferences.keySet.toSet)

    /* Put as many nodes as possible back into the graph, assigning each one a colour */
    while (stack.nonEmpty) {
      val (tReg, neighbours) = stack.pop()
      val existingNeighbours = neighbours.filter(colouring.coloured.contains(_))
      val existingColours: Set[A] = existingNeighbours.map(x => colouring.coloured(x))
      val validColours = regs.toSet diff existingColours
      if (validColours.isEmpty)
        colouring = colouring.copy(uncoloured = colouring.uncoloured union Set(tReg))
      else
        colouring = colouring.copy(coloured = colouring.coloured.updated(tReg, validColours.head))
    }

    colouring
  }

  private def remainingArcs(node: IGNode, remainingInters: mutable.Map[TRegister, Set[TRegister]]): Int =
    node._2.count(x => remainingInters.contains(x))

  private def pushAllToStack(interferences: mutable.Map[TRegister, Set[TRegister]], stack: mutable.Stack[(TRegister, Set[TRegister])]): Unit = {
  /* Repeatedly push the smallest-order colourable node to the stack. */
    do {
      val fewestInterferences = interferences.minBy(x => remainingArcs(x, interferences))
      if (fewestInterferences._2.size > regs.size) return
      interferences.subtractOne(fewestInterferences._1)
      stack.push(fewestInterferences)
    }
    while (interferences.nonEmpty)
  }
}

private class InterferenceGraph(cfg: CFG) {
  val interferences: immutable.Map[TRegister, Set[TRegister]] = {
    val inters = mutable.Map[TRegister, Set[TRegister]]()
    cfg.nodes.foreach {
      node: CFGNode =>
        node.liveOut.foreach(t => inters.update(t, inters.getOrElse(t, Set()) union node.liveOut excl t))
        // Ensure that every tRegister gets a node (even if it doesn't get used)
        node.defs.foreach(t => if(!(inters contains t)) inters.update(t, Set()))
    }
    inters.toMap
  }

  /* The index of the end of a block */
  val blocks: Vector[(Id, Id)] = {
    val tempEnds: ListBuffer[(Id, Id)] = ListBuffer()
    var start = cfg.nodes.head.id
    cfg.nodes.zipWithIndex.foreach {
      case (node, index) =>
        val (_, _, succs) = cfg.getInfo(node.instr, index)
        node.instr match {
          case Label(_) =>
            tempEnds.addOne(start, index)
            start = index + 1
          case _ =>
        }
        // succs.size > 1: the instruction is a branch.
        if (succs.size > 1) {
          tempEnds.addOne(start, index)
          start = index + 1
        }
    }
    // Adding the final block
    println(tempEnds)
    val lastBlockStart = tempEnds.last._2 + 1
    if (lastBlockStart < cfg.nodes.length) // This should always be true
      tempEnds.addOne((lastBlockStart, cfg.nodes.length - 1))
    tempEnds.toVector
  }

  def block(id: Id): (Id, Id) = blocks.find {
    b => {
      val blockEnd = b._2
      blockEnd >= id
    }
  }.get
}