package wacc

import org.scalatest.flatspec.AnyFlatSpec
import parsley.Success
import wacc.AbstractSyntaxTree._
import wacc.Parser.StatementParser.statement

class StatementParserSpec extends AnyFlatSpec {
  import wacc.ParseExamples._


  "Statement Parser" can "parse skip statements" in {
    assert(statement.parse("skip") == Success(SkipStat()))
  }

  "Statement Parser" can "parse variable declarations" in {
    for (rval <- rValExamples) {
      assert(statement.parse("int int_declaration = " + rval._1)
        == Success(Declaration(BaseType(BaseT.Int_T), IdentLiteral("int_declaration"), rval._2)))
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
          Declaration(BaseType(BaseT.Int_T), IdentLiteral("skip_int"), IntLiteral(3))
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
          Declaration(BaseType(BaseT.Int_T), IdentLiteral("skip_int"), IntLiteral(3))))))
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
          Declaration(BaseType(BaseT.Int_T), IdentLiteral("skip_int"), IntLiteral(3))))))
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
          Declaration(BaseType(BaseT.Int_T), IdentLiteral("skip_int"), IntLiteral(3))))))
    }
  }

  "Statement Parser" can "parse statements" in {
    for (stat <- statExamples){
      var parseString =
        """begin
          |  %s
          |end""".stripMargin.format(stat._1)
      var result = statement.parse(parseString)
      assert(result == Success(BeginEndStat(stat._2)))
    }
  }
}
