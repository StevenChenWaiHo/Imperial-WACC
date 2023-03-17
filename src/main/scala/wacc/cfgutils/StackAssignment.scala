package wacc.cfgutils

import wacc.TAC._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object StackAssignment {
  def apply(tacs: List[TAC]): List[TAC] = {
    val newTacs = tacs.to(ListBuffer)
    var height = 0
    var lastLabel = 0
    var lastFuncStart = 0
    var i = 0
    val stackMap = mutable.Map[TRegister, Int]()
    while (i < newTacs.length) {
      newTacs(i) match {
        case _: Label => lastLabel = i + 1
        case _: BeginFuncTAC => lastFuncStart = lastLabel
        case _: EndFuncTAC =>
          newTacs.insert(lastFuncStart, AllocateStackTAC(height))
          stackMap.clear()
          i += 1 // We just added an instruction; return i to where it was.
          height = 0
        case ReservedPushTAC(alias, _, original) =>
          val location = if (stackMap.contains(original)) {
            stackMap(original)
          } else {
            height += 1
            stackMap.addOne(original, height)
            height
          }
          newTacs(i) = ReservedPushTAC(alias, location, original)
        case ReservedPopTAC(_, alias, original) =>
          newTacs(i) = ReservedPopTAC(stackMap(original), alias, original)
        case _ =>
      }
      i += 1
    }
    newTacs.foreach(println)
    newTacs.toList
  }
}
