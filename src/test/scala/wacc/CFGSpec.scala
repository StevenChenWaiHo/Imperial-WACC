package wacc

import org.scalatest.flatspec.AnyFlatSpec
import wacc.Parser.ProgramParser.program
import wacc.Translator.delegateASTNode
import wacc.cfgutils.CFG
import wacc.cfgutils.CFG.CFGBuilder

import scala.language.postfixOps

class CFGSpec extends AnyFlatSpec{
  private val testProgram =
    """
      |begin
      |int a = 10
      |int b = 11
      |int c = 12
      |int x = 1
      |int y = 2
      |int z = 3
      |x = y + z
      |""".stripMargin
  private val testTACs = delegateASTNode(program.parse(testProgram).get)

  "CFGBuilder" can "Build a cfg from a simple test program" in {
    println("--- TACS ---\n")
    println(testTACs)
    println("\n\n--- CFG ---\n")
    println(new CFGBuilder())
  }
}
