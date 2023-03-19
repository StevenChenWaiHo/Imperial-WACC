package wacc.cfgutils

import wacc.TAC._
import wacc.cfgutils.CFG.{CFG, CFGBuilder, CFGNode, Id}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.{immutable, mutable}

class GraphColouringAllocator[A](regs: List[A], tacs: Vector[TAC], cfgBuilder: CFGBuilder) extends RegisterAllocator[A] {
  /* State */
  private var cfg: CFG = null
  private var interferenceGraph: InterferenceGraph = null
  private var colouring: Colouring[A] = null
  private var spilled: Set[TRegister] = Set()

  /* nextTacs determines the next state at each iteration */
  private var nextTacs: Vector[TAC] = tacs

  recolour()

  /** Allocates registers, returning:
   * (Modified TACs, Colouring). */
  override def allocateRegisters: (Vector[TAC], Colouring[A]) = {
    while (colouring.uncoloured.nonEmpty) {
      spill(colouring)
    }


    (StackAssignment(pushBeforeCalls), colouring)
  }

  private def recolour(): Unit = {
    cfg = cfgBuilder.build(nextTacs)
    interferenceGraph = new InterferenceGraph(cfg)
    colouring = new GraphColourer[A](regs, interferenceGraph).attemptColouring
  }

  private def spill(colouring: Colouring[A]): Unit = {
    //TODO: Currently doesn't use the interference graph
    /* Simple strategy: spill the node that interferes with the most uncolourable nodes. */
    val Colouring(coloured, uncoloured) = colouring
    // Map of tRegisters to how often they interfere with uncoloured nodes.
    val interferenceCounts = uncoloured.toVector.flatMap(t => interferenceGraph.interferences(t).toVector)
      .groupBy(identity).transform((_, t) => t.size).removedAll(spilled)
    var allTRegs = coloured.keySet union uncoloured
    // Choose most frequently interfering register. If that doesn't work, pick any register.
    val target = if (interferenceCounts.nonEmpty) interferenceCounts.maxBy(_._2)._1
    else (allTRegs diff spilled).head
    spilled = spilled incl target
    println("TARGET:")
    println(target)

    def nextTReg: TRegister = {
      val highestTReg = allTRegs.maxBy(_.num)
      highestTReg.copy(num = highestTReg.num + 1)
    }

    /* Add a 'push' after each definition, and a 'pop' after each use. */
    @tailrec
    def modifyGraph(initial: Vector[CFGNode], result: Vector[TAC], targetReg: TRegister): Vector[TAC] = {
      // Renumber the target register:
      val newInstr = if (initial.nonEmpty) TACLiveRange.mapTAC(initial.head.instr, (target -> targetReg)) else null
      val next = nextTReg
      // If the instruction uses or defines the target register, add a pop and/or a push as appropriate.
      initial match {
        case n +: ns if n.defs contains target =>
          spilled = spilled incl next
          allTRegs = allTRegs incl next
          modifyGraph(ns, result ++ Vector(newInstr, ReservedPushTAC(targetReg, 0, target)), next)
        case n +: ns if n.uses contains target =>
          spilled = spilled incl next
          allTRegs = allTRegs incl next
          modifyGraph(ns, result ++ Vector(ReservedPopTAC(0, targetReg, target), newInstr, ReservedPushTAC(targetReg, 0, target)), next)
        case n +: ns =>
          modifyGraph(ns, result :+ newInstr, targetReg)
        case Vector() => result
      }
    }

    nextTacs = modifyGraph(cfg.nodes, Vector[TAC](), target)
    recolour()
  }


  // Push the minimal amount of registers possible before each function call.
  // This can only be done after the graph is coloured.
  private def pushBeforeCalls: Vector[TAC] = {
    /* Construct register overwrite graph */
    /* Get function register definitions */
    var funcDefs = mutable.Map[Label, Set[A]]()
    val funcCalls = mutable.Map[Label, Set[Label]]()
    var lastLabel: Label = null
    var currentFunc: Label = null
    var currentDefs = Set[A]()
    for (node <- cfg.nodes) node.instr match {
      case l: Label => lastLabel = l
      case BeginFuncTAC() => {
        currentFunc = lastLabel
        currentDefs = Set()
      }
      case CallTAC(lbl, _, _) =>
        currentDefs = currentDefs union node.defs.map(t => colouring.coloured(t))
        if(funcCalls contains currentFunc) funcCalls.update(currentFunc, funcCalls(currentFunc) incl lbl)
        else funcCalls.addOne(currentFunc -> Set(lbl))
      case EndFuncTAC() => funcDefs.addOne(currentFunc -> currentDefs)
      case _ => currentDefs = currentDefs union node.defs.map(t => colouring.coloured(t))
    }

    /* Iteratively recalculate the registers overwritten by each function */
    var currentFuncDefs = mutable.Map[Label, Set[A]]()

    do {
      currentFuncDefs = funcDefs.clone()
      println
      println
      println("hihihi")
      currentFuncDefs.map(println)
      println
      println
      funcDefs = funcDefs.map {
        kv =>
          val (func, defs) = kv
          val callDefs = funcCalls.getOrElse(func, Set()).flatMap(funcDefs)
          (func, defs union callDefs)
      }
    } while (currentFuncDefs != funcDefs)

    /* Apply these definitions to calls */
    @tailrec
    def addPushes(initial: Vector[CFGNode], result: Vector[TAC]): Vector[TAC] =
      if (initial.isEmpty) result
      else {
        val node = initial.head
        node.instr match {
          case n@CallTAC(lbl, _, _) =>
            val survivingTRegs = (node.liveOut diff node.defs)
            val wipedRegs = funcDefs(lbl)
            val pushTRegs: Vector[TRegister] = survivingTRegs.collect {
              case t if wipedRegs contains colouring.coloured(t) => t
            }.toVector
            addPushes(initial.tail, result ++ pushTRegs.map(PushTAC) ++ (n +: pushTRegs.reverse.map(PopTAC)))
          case n => addPushes(initial.tail, result :+ n)
        }
      }

    addPushes(cfg.nodes, Vector[TAC]())
  }
}

private class GraphColourer[A](val regs: List[A], interferenceGraph: InterferenceGraph) {
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
        node.defs.foreach(t => if (!(inters contains t)) inters.update(t, Set()))
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
    //    println(tempEnds)
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