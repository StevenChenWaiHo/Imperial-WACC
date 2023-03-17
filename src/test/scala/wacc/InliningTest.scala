

import org.scalatest.flatspec.AnyFlatSpec

import wacc.AbstractSyntaxTree._
import wacc.FinalIR._
import wacc.Optimisations.Inlining._
import wacc.AssemblerTypes._
import wacc.Parser.ProgramParser.program
import wacc.TAC._

class InliningTest extends AnyFlatSpec {
  def lastLineReturn() = {
    val ast = program.parse(
      """begin
          int f() is
            return 0
          end
          int x = call f() ;
          println x
        end""")
    assert(inline_delegateASTNode(ast.get)._1.exists( x => x match {case l: Label => true case _ => false} ))
  }

  def checkCandidateFunction() = {
    val candidateTacList = List(
      Label("Candidate"),
      CommandTAC(CmdT.Ret, TRegister(0), BaseType(BaseT.Int_T)))
    assert(checkInlineFunc(candidateTacList))

    val candidateWithPrintAfterReturnTacList = List(
      Label("Candidate"),
      CommandTAC(CmdT.Ret, TRegister(0), BaseType(BaseT.Int_T)),
      CommandTAC(CmdT.PrintLn, TRegister(0), BaseType(BaseT.Int_T)))
    assert(checkInlineFunc(candidateWithPrintAfterReturnTacList))

    val notCandidateTacList = List(
      Label("NotCandidate"),
      CallTAC(Label("Helper"), List(), TRegister(0)))
    assert(!checkInlineFunc(notCandidateTacList))
  }

  "Inlining Optimiser" can "replace call for last line return function" in {
    lastLineReturn()
  }

  "Inlining Optimiser" can "detect recursive or call-chained functions" in {
    checkCandidateFunction()
  }
}
