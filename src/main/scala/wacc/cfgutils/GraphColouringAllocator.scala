package wacc.cfgutils

import wacc.TAC._

import scala.collection.mutable.ListBuffer
import scala.collection.{immutable, mutable}

class GraphColouringAllocator[A] extends RegisterAllocator[A] {
  override def allocateRegisters(regs: List[A], cfg: CFG): (Vector[TAC], Map[TRegister, A], List[Id]) = ???
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


/** Uses the approach described here: https://link.springer.com/chapter/10.1007/BFb0026430
 * Note: I didn't use the methods described for improving space efficiency in the matrix. */
private class ContainmentGraph(cfg: CFG, interferenceGraph: InterferenceGraph) {

  val adjacencies: AdjacencyMatrix = new BitSetAdjacencyMatrix(cfg.tRegisterCount, cfg.tRegisterCount)
  val storeCosts: mutable.Map[TRegister, Int] = mutable.Map()
  val loadCosts: mutable.Map[TRegister, Int] = mutable.Map()
  val EstimatedLoopIterations = 10

  /* Construct graph */
  for ((start, end) <- interferenceGraph.blocks) {
    val block = cfg.nodes.slice(start, end + 1)
    var live = block.last.liveOut

    /* Update load costs for values that 'die' between blocks */
    for (succ <- block.last.succs.map(cfg.nodes(_))) {
      val deaths = block.last.liveOut diff succ.liveIn
      for (d <- deaths) {
        loadCosts(d) = loadCosts(d) + Math.ceil(Math.pow(EstimatedLoopIterations, succ.depth)).toInt
      }
    }

    for (id <- Range.inclusive(end, start)) {
      val (uses, defs, _) = cfg.getInfo(cfg.nodes(id).instr)

      /* Update store costs */
      val weight = Math.pow(EstimatedLoopIterations, cfg.nodes(id).depth).toInt
      defs.foreach(x => storeCosts(x) = storeCosts(x) + weight)
      uses.foreach(x => if (!live.contains(x)) loadCosts(x) = loadCosts(x) + weight)
      
      /* Update the adjacency matrix */
      updateAdjacencies(defs, live)
      live = (live diff defs) union uses
      updateAdjacencies(uses, live)

    }
  }

  /* Compute split costs */

  private def updateAdjacencies(used: Set[TRegister], exists: Set[TRegister]): Unit = {
    for (u <- used; e <- exists)
      adjacencies.addOne(u.num, e.num)
  }
}
