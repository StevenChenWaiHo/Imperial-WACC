package wacc

import org.scalatest.flatspec.AnyFlatSpec

import wacc.FinalIR._
import wacc.Optimisations.PeepholeOptimisation._
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
      Pop("", List(fp, pc))
    )

    val testOutput = List(
      DataSeg(),
      TextSeg(),
      Global("main"),
      Lbl("main"),
      Push("", List(fp, lr)),
      Mov("", sp, fp),
      Mov("", ImmediateInt(3), r1),
      Mov("", r1, r0),
      Pop("", List(fp, pc))
    )

    assert(PeepholeOptimise(testCode) == testOutput)
  }

  def testRedundantIdentify() = {
    assert(isRedundant(Push("", List(r0, r1)), Pop("", List(r0, r1))))
    assert(isRedundant(Push("eq", List(r0, r1)), Pop("eq", List(r0, r1))))
    assert(!isRedundant(Push("", List(r1)), Pop("", List(r0, r1))))
    assert(!isRedundant(Push("eq", List(r0, r1)), Pop("", List(r0, r1))))
    assert(isRedundant(Mov("", r0, r1), Mov("", r1, r0)))
  }
  
  def testRemoveRedundant() = {
    val testCode = List(
      DataSeg(),
      TextSeg(),
      Global("main"),
      Lbl("main"),
      Push("", List(fp, lr)),
      Mov("", sp, fp),
      Push("", List(r1, r0)),
      Pop("", List(r1, r0)),
      Mov("", ImmediateInt(0), r0),
      Pop("", List(fp, pc))
    )

    val testOutput = List(
      DataSeg(),
      TextSeg(),
      Global("main"),
      Lbl("main"),
      Push("", List(fp, lr)),
      Mov("", sp, fp),
      Mov("", ImmediateInt(0), r0),
      Pop("", List(fp, pc))
    )

    assert(PeepholeOptimise(testCode) == testOutput)
  }

  def testReduceStrength() = {
    assert(strengthReduction(
      Mul("", None(), r1, ImmediateInt(4), r1))
      == Mov("", LogicalShiftLeft(r1, Right(2)), r1))
  }

  def testRemoveDeadCode() = {
    val testCode = List(
      Lbl("wacc_f"),
      Push("", List(fp, lr)),
      Mov("", sp, fp),
      Mov("", ImmediateInt(1), r1),
      Mov("", r1, r0),
      Mov("", fp, sp),
      Pop("", List(fp, pc)),
      Mov("", new ImmediateInt(0), r0),
      Mov("", fp, sp),
      Pop("", List(fp, pc)),
      Special(".ltorg")
    )

    val testOutput = List(
      Lbl("wacc_f"),
      Push("", List(fp, lr)),
      Mov("", sp, fp),
      Mov("", ImmediateInt(1), r1),
      Mov("", r1, r0),
      Mov("", fp, sp),
      Pop("", List(fp, pc)),
      Special(".ltorg")
    )

    assert(testCode.removeDeadCode() == testOutput)
    
  }

  "Peephole Optimiser" can "identify null operations" in {
    testNullOpIdentify()
  }

  "Peephole Optimiser" should "remove null operations" in {
    testRemoveNullOp()
  }

  "Peephole Optimiser" can "identify redundant code" in {
    testRedundantIdentify()
  }

  "Peephole Optimiser" should "remove redundant code" in {
    testRemoveRedundant()
  }

  "Peephole Optimiser" can "reduce instruction strength" in {
    testReduceStrength()
  }

  "Peephole Optimiser" should "remove dead code at end of function" in {
    testRemoveDeadCode()
  }
}
