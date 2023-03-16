import org.scalatest.flatspec.AnyFlatSpec

import wacc.FinalIR._
import wacc.PeepholeOptimisation._
import wacc.AssemblerTypes._

class PeepholeOptimserSpec extends AnyFlatSpec {
  def testNullOpIdentify() = {
    assert(isNullOp(Mul("", None(), r0, ImmediateInt(1), r0)))
    assert(isNullOp(Add("", None(), r2, ImmediateInt(0), r2)))
    assert(isNullOp(Mov("", r0, r0)))
    assert(!isNullOp(Mul("", None(), r1, ImmediateInt(3), r1)))
    assert(!isNullOp(Mov("", r4, r2)))
  }

  def testRemoveNullOp() = {
    val testCode = List(
      DataSeg(),
      TextSeg(),
      Global("main"),
      Lbl("main"),
      Push("", List(fp, lr)),
      Mov("", sp, fp),
      Mov("", ImmediateInt(3), r1),
      Add("", None(), r1, ImmediateInt(0), r1),
      Mov("", r1, r0),
      Pop("", List(fp, pc)),
    )

    val testOutput = List(
      DataSeg(),
      TextSeg(),
      Global("main"),
      Lbl("main"),
      Push("", List(fp, lr)),
      Mov("", sp, fp),
      Mov("", ImmediateInt(3), r1),
      Add("", None(), r1, ImmediateInt(0), r1),
      Mov("", r1, r0),
      Pop("", List(fp, pc)),
    )

    assert(PeepholeOptimise(testCode) == testOutput)
  }

  "Peephole Optimiser" can "identify null operations" in {
    testNullOpIdentify()
  }

  "Peephole Optimiser" should "remove null operations" in {
    testRemoveNullOp()
  }
}
