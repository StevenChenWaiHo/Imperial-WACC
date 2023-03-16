package wacc.cfgutils

import wacc.TAC._

import java.security.InvalidParameterException
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object StackAssignment {
  def apply(tacs: List[TAC]): List[TAC] = {
    var newTacs = tacs.to(ListBuffer)
    for (i <- 0 to tacs.length) {
      newTacs(i) match {
        case _: BeginFuncTAC =>
          val height = funcStackSize(newTacs.slice(i, newTacs.length), 0, 0)
          newTacs.insert(i + 1, AllocateStackTAC(height))
      }
    }
    newTacs.toList
  }

  @tailrec
  def funcStackSize(instrs: List[(TAC, Int)], height: Int, maxHeight: Int): Int = {
    if (instrs.isEmpty) throw new InvalidParameterException("Unterminated function in TACs.")
    instrs.head._1 match {
      case _: PushParamTAC => funcStackSize(instrs.tail, height + 1, Math.max(height, maxHeight))
      case _: PopParamTAC => funcStackSize(instrs.tail, height - 1, Math.max(height, maxHeight))
      case _: EndFuncTAC => height
    }
  }
}
