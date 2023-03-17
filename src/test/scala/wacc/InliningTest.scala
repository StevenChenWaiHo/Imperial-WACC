

import org.scalatest.flatspec.AnyFlatSpec

import wacc.AbstractSyntaxTree._
import wacc.FinalIR._
import wacc.Inlining._
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

  "Inlining Optimiser" can "replace call for last line return function" in {
    lastLineReturn()
  }
}
