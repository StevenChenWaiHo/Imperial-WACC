package wacc.cfgutils

import wacc.TAC.TRegister
import wacc.cfgutils.CFG.CFG

import scala.collection.mutable

class RegisterSplittingAllocator {
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
        val (uses, defs, _) = cfg.getInfo(cfg.nodes(id).instr, id)

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

  trait AdjacencyMatrix {
    def apply(x: Int, y: Int): Boolean

    def addOne(x: Int, y: Int): Unit
  }

  /* Adjacency matrix using a bitset. A hashmap based approach might be a good way to save space. */
  class BitSetAdjacencyMatrix(width: Int, height: Int) extends mutable.BitSet with AdjacencyMatrix {
    override def apply(x: Int, y: Int): Boolean = this (x * width + y)

    override def addOne(x: Int, y: Int): Unit = this.addOne(x * width + y)
  }
}
