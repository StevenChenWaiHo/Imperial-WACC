package wacc

import org.scalatest.flatspec.AnyFlatSpec
import parsley.Success
import wacc.AbstractSyntaxTree._
import wacc.Parser.StatementParser.statement

class StatementParserSpec extends AnyFlatSpec {

  // Generating these could be automated from the BNF,
  // but implementing that would be almost as much work as the parser itself.
  // It would be quite interesting to build a reverse parsley for test generation, though
  private var rValExamples = Set(
    "3 + exp" -> BinaryOp(BinaryOpType.Add, IntLiteral(3), IdentLiteral("exp")),
    "[3 + exp, 12]" -> ArrayLiteral(List(BinaryOp(BinaryOpType.Add, IntLiteral(3), IdentLiteral("exp")), IntLiteral(12))),
    "newpair (3 + exp, 12)" -> PairValue(BinaryOp(BinaryOpType.Add, IntLiteral(3), IdentLiteral("exp")), IntLiteral(12)),
    "fst fst_var" -> PairElement(PairElemT.Fst, IdentLiteral("fst_var")),
    "call call_ident" -> Call(IdentLiteral("call_ident"), List())
  )

  private var lValExamples = Set(
    "fst_var" -> IdentLiteral("fst_var"),
    "fst_var[12][13]" -> ArrayElem("fst_var", List(IntLiteral(12), IntLiteral(13))),
    "fst fst_var[12][13]" -> PairElement(PairElemT.Fst, ArrayElem("fst_var", List(IntLiteral(12), IntLiteral(13))))
  )

  private var exprExamples = Set(
    "3 * 3 " -> BinaryOp(BinaryOpType.Mul, IntLiteral(3), IntLiteral(3))
  )

  private var statExamples = Set(
    "skip" -> SkipStat(),
    "return 12;\nreturn 13" -> StatList(Command(CmdT.Ret, IntLiteral(12)), Command(CmdT.Ret, IntLiteral(13)))
  )


  "Statement Parser" can "parse skip statements" in {
    assert(statement.parse("skip") == Success(SkipStat()))
  }

  "Statement Parser" can "parse variable declarations" in {
    for (rval <- rValExamples) {
      assert(statement.parse("int int_declaration = " + rval._1)
        == Success(Declaration(BaseT.Int_T, IdentLiteral("int_declaration"), rval._2)))
    }
  }

  "Statement Parser" can "parse assignments" in {
    for (rval <- rValExamples; lval <- lValExamples) {
      var parseString =
      """skip;
      |%s = %s;
      |int skip_int = 3
      |""".stripMargin.format(lval._1, rval._1)
      var result = statement.parse(parseString)
      assert(result == Success(StatList(
        SkipStat(),
        StatList(Assignment(lval._2, rval._2),
          Declaration(BaseT.Int_T, IdentLiteral("skip_int"), IntLiteral(3))
        )
      )))

    }
  }

  "Statement Parser" can "parse commands" in {
    import AbstractSyntaxTree.CmdT._
    for (cmd <- Set("free" -> Free, "return" -> Ret, "exit" -> Exit, "print" -> Print, "println" -> PrintLn)) {
      var result = statement.parse(
        """skip;
          |%s 2;
          |int skip_int = 3
          |""".stripMargin.format(cmd._1))
      assert(result == Success(StatList(SkipStat(),
        StatList(Command(cmd._2, IntLiteral(2)),
          Declaration(BaseT.Int_T, IdentLiteral("skip_int"), IntLiteral(3))))))
    }
  }

  "Statement Parser" can "parse if statements" in {
    for (stat1 <- statExamples; stat2 <- statExamples; expr <- exprExamples) {
      var parseString =
        """skip;
          |if %s
          |then
          |  %s
          |else
          |  %s
          |fi;
          |int skip_int = 3
          |""".stripMargin.format(expr._1, stat1._1, stat2._1)
      var result = statement.parse(parseString)
      assert(result == Success(StatList(SkipStat(),
        StatList(IfStat(expr._2, stat1._2, stat2._2),
          Declaration(BaseT.Int_T, IdentLiteral("skip_int"), IntLiteral(3))))))
    }
  }

  "Statement Parser" can "parse while loops" in {
    for (stat <- statExamples; expr <- exprExamples) {
      var parseString =
        """skip;
          |while %s
          |do
          |  %s
          |done;
          |int skip_int = 3
          |""".stripMargin.format(expr._1, stat._1)
      var result = statement.parse(parseString)
      assert(result == Success(StatList(SkipStat(),
        StatList(WhileLoop(expr._2, stat._2),
          Declaration(BaseT.Int_T, IdentLiteral("skip_int"), IntLiteral(3))))))
    }
  }

  "Statement Parser" can "parse statements" in {
    for (stat <- statExamples){
      var parseString =
        """begin
          |  %s
          |end""".stripMargin.format(stat._1)
      var result = statement.parse(parseString)
      assert(result == Success(Program(stat._2)))
    }
  }
}
