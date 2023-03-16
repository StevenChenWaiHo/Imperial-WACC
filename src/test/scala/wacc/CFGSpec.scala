package wacc

import org.scalatest.flatspec.AnyFlatSpec
import wacc.AssemblerTypes.ImmediateInt
import wacc.Parser.ProgramParser.program
import wacc.TAC.{BinaryOpTAC, TRegister}
import wacc.Translator.delegateASTNode
import wacc.cfgutils.CFG.CFGBuilder
import wacc.cfgutils.{CfgTacInfo, GraphColouringAllocator}

import scala.language.postfixOps
import scala.sys.exit

class CFGSpec extends AnyFlatSpec {
  private val testProgram =
    """
      |begin
      |int x = 1;
      |int y = 2;
      |int z = 3;
      |int k = 4;
      |x = y + z;
      |y = x + z;
      |z = x + y;
      |z = z + k
      |end
      |""".stripMargin
  private val testTACs = delegateASTNode(program.parse(testProgram).get)._1.toVector

  "CFGBuilder" can "Build a cfg from a simple test program" in {
    println("--- TACS ---\n")
    println(testTACs)
    println("\n\n--- CFG ---\n")
    println(new CFGBuilder(CfgTacInfo).build(testTACs))

    println("\n\n\n\n--- Colouring ---\n")
    val (ts, cs) = (new GraphColouringAllocator[String](List("aaa", "bbb", "ccc", "ddd"), testTACs, new CFGBuilder(CfgTacInfo)).allocateRegisters)
    println(cs)
    println
    ts.foreach(println)
  }
}
