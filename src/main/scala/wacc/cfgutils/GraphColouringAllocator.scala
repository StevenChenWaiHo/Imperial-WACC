package wacc.cfgutils

import wacc.TAC._

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

  recolour

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
      case Vector() => result
    }

    nextTacs = modifyGraph(cfg.nodes, Vector[TAC]())
  }

}

class GraphColourer[A](val regs: List[A], interferenceGraph: InterferenceGraph) {
  type IGNode = (TRegister, Set[TRegister])

  def attemptColouring: Colouring[A] = {
    val stack = mutable.Stack[(TRegister, Set[TRegister])]()
    val interferences = interferenceGraph.interferences.to(mutable.Map)

    if (interferences.isEmpty) {
      println("Empty program passed to graph colouring allocator! \n")
      return Colouring(Map(), Set())
    }

    /* Repeatedly push the smallest-order colourable node to the stack. */
    do {
      val fewestInterferences = interferences.minBy(x => remainingArcs(x, interferences))
      if (fewestInterferences._2.size > regs.size) break
      interferences.subtractOne(fewestInterferences._1)
      stack.push(fewestInterferences)
    }
    while (interferences.nonEmpty)

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

}

private class InterferenceGraph(cfg: CFG) {
  val interferences: immutable.Map[TRegister, Set[TRegister]] = {
    val inters = mutable.Map[TRegister, Set[TRegister]]()
    cfg.nodes.foreach {
      node: CFGNode =>
        node.liveOut.foreach(t => inters.update(t, inters(t) union node.liveOut))
    }
    inters.toMap
  }

  /* The index of the end of a block */
  val blocks: Vector[(Id, Id)] = {
    val tempEnds: ListBuffer[(Id, Id)] = ListBuffer()
    var start = cfg.nodes.head.id
    cfg.nodes.zipWithIndex.foreach {
      case (node, index) =>
        val (_, _, succs) = cfg.getInfo(node.instr)
        node.instr match {
          // succs.size > 1: the instruction is a branch.
          case Label(_) | succs.size > 1 =>
            tempEnds.addOne(start, index)
            start = index + 1
        }
    }
    // Adding the final block
    val lastBlockStart = tempEnds(cfg.nodes.last.id)._2 + 1
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

trait AdjacencyMatrix {
  def apply(x: Int, y: Int): Boolean

  def addOne(x: Int, y: Int): Unit
}

/* Adjacency matrix using a bitset. A hashmap based approach might be a good way to save space. */
class BitSetAdjacencyMatrix(width: Int, height: Int) extends mutable.BitSet with AdjacencyMatrix {
  override def apply(x: Int, y: Int): Boolean = this (x * width + y)

  override def addOne(x: Int, y: Int): Unit = this.addOne(x * width + y)
}


/**
 * Optional: Implements register 'splitting'.
 * Uses the approach described here: https://link.springer.com/chapter/10.1007/BFb0026430
 * */
private class ContainmentGraph(cfg: CFG, interferenceGraph: InterferenceGraph) {

  val adjacencies: AdjacencyMatrix = new BitSetAdjacencyMatrix(cfg.tRegisterCount, cfg.tRegisterCount)
  val storeCosts: mutable.Map[TRegister, Int] = mutable.Map()
  val loadCosts: mutable.Map[TRegister, Int] = mutable.Map()
  val EstimatedLoopIterations = 10

  /* Construct graph */
  for ((start, end) <- interferenceGraph.blocks) {
    val block = cfg.nodes.slice(start, end + 1)
    var live = block.last.liveOut

    /* Update load costs for splitting values that 'die' between blocks. */
    for (succ <- block.last.succs.map(cfg.nodes(_))) {
      val deaths = block.last.liveOut diff succ.liveIn
      for (d <- deaths) {
        loadCosts(d) = loadCosts(d) + Math.ceil(Math.pow(EstimatedLoopIterations, succ.depth) / deaths.size).toInt
        //TODO: this division is not in the paper. It seems like it makes sense but I probably misunderstood something.
      }
    }

    for (id <- Range.inclusive(end, start)) {
      val (uses, defs, _) = cfg.getInfo(cfg.nodes(id).instr)

      /* Update split costs */
      val weight = Math.pow(EstimatedLoopIterations, cfg.nodes(id).depth).toInt
      defs.foreach(x => storeCosts(x) = storeCosts(x) + weight)
      uses.foreach(x => if (!live.contains(x)) loadCosts(x) = loadCosts(x) + weight)

      /* Update the adjacency matrix */
      updateAdjacencies(defs, live)
      live = (live diff defs) union uses
      updateAdjacencies(uses, live)
    }
  }


  private def updateAdjacencies(used: Set[TRegister], exists: Set[TRegister]): Unit = {
    for (u <- used; e <- exists)
      adjacencies.addOne(u.num, e.num)
  }
}
