package wacc

import org.scalatest.flatspec.AnyFlatSpec
import parsley.Success
import wacc.AbstractSyntaxTree._
import ParseExamples._
import wacc.Parser.FunctionParser.func
import wacc.Parser.ProgramParser.program

class ProgramParserSpec extends AnyFlatSpec {
  def testFunctions2(progFormat: String, funcs: Set[(String, Func)]) = {
    for (func1 <- funcs; func2 <- funcs; stat <- statExamples) {
      var parseString = progFormat.stripMargin.format(func1._1, func2._1, stat._1)
      assert(program.parse(parseString) == Success(Program(List(func1._2, func2._2), stat._2)))
    }
  }
  def testFunctions1(progFormat: String, funcs: Set[(String, Func)]) = {
    for(func <- funcs; stat <- statExamples) {
      var parseString = progFormat.stripMargin.format(func._1, stat._1)
      assert(program.parse(parseString) == Success(Program(List(func._2), stat._2)))
    }
  }
  def testFunctions(progFormat: String) = {
    for (stat <- statExamples) {
      var parseString = progFormat.format(stat._1)
      assert(program.parse(parseString) == Success(Program(List(), stat._2)))
    }
  }

  def testFunctionParsing = {
    for (function <- funcExamples) {
      assert(func.parse(function._1) == Success(function._2))
    }
  }

  val whitespaceFormat =
    """
      |
      |begin
      |# a program
      | %s
      | %s
      |end
      |
      |""".stripMargin


  val format2 =
    """begin
      | %s
      | %s
      | %s
      |end""".stripMargin

  val format1 =
    """begin
      | %s
      | %s
      |end""".stripMargin

  val format =
    """begin
      | %s
      |end""".stripMargin

  "Function Parser" can "parse individual functions correctly" in {
    testFunctionParsing
  }

  "Program Parser" can "parse functions with multiple inputs" in {
    testFunctions1(format1, funcExamples)
  }

  "Program Parser" can "parse functions with no inputs" in {
    testFunctions1(format1, procExamples)
  }

  "Program Parser" should "ignore comments and whitespace" in {
    testFunctions1(whitespaceFormat, procExamples union funcExamples)
  }

  "Program Parser" can "parse programs without functions" in {
    testFunctions(format)
  }

  "Program Parser" can "parse programs with multiple functions" in {
    testFunctions2(format2, procExamples union funcExamples)
  }
}
