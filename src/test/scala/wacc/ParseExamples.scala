package wacc

import wacc.AbstractSyntaxTree.BaseT._
import wacc.AbstractSyntaxTree._

object ParseExamples {

  // Generating these could be automated from the BNF,
  // but implementing that would be almost as much work as the parser itself.
  // It would be quite interesting to build a reverse parsley for test generation, though
  var rValExamples = Set(
    "3 + exp" -> BinaryOp(BinaryOpType.Add, IntLiteral(3), IdentLiteral("exp")),
    "[3 + exp, 12]" -> ArrayLiteral(List(BinaryOp(BinaryOpType.Add, IntLiteral(3), IdentLiteral("exp")), IntLiteral(12))),
    "newpair (3 + exp, 12)" -> PairValue(BinaryOp(BinaryOpType.Add, IntLiteral(3), IdentLiteral("exp")), IntLiteral(12)),
    "fst fst_var" -> PairElement(PairElemT.Fst, IdentLiteral("fst_var")),
    "call call_ident" -> Call(IdentLiteral("call_ident"), List())
  )

  var lValExamples = Set(
    "fst_var" -> IdentLiteral("fst_var"),
    "fst_var[12][13]" -> ArrayElem("fst_var", List(IntLiteral(12), IntLiteral(13))),
    "fst fst_var[12][13]" -> PairElement(PairElemT.Fst, ArrayElem("fst_var", List(IntLiteral(12), IntLiteral(13))))
  )

  var exprExamples = Set(
    "3 * 3 " -> BinaryOp(BinaryOpType.Mul, IntLiteral(3), IntLiteral(3))
  )

  var statExamples = Set(
    "skip" -> SkipStat(),
    "return 12;\nreturn 13" -> List(Command(CmdT.Ret, IntLiteral(12)), Command(CmdT.Ret, IntLiteral(13)))
  )

  private var declarationTypeExamples = Set(
    "int x " -> (BaseType(Int_T), IdentLiteral("x")),
    "char begin_char " -> (BaseType(Char_T), IdentLiteral("begin_char"))
  )

  var funcExamples: Set[(String, Func)] = {
    var set = Set[(String, Func)]()
    for (decT1 <- declarationTypeExamples;
         decT2 <- declarationTypeExamples;
         decT3 <- declarationTypeExamples;
         stat <- statExamples) {
      set = set.union(
        Set(
          """%s(%s, %s) is
            | %s
            |end
            |""".stripMargin.format(decT1._1, decT2._1, decT3._1, stat._1)
            -> Func(decT1._2._1, decT1._2._2, List(decT2._2, decT3._2), stat._2)
        )
      )
    }
    set
  }

  // Functions without arguments:
  var procExamples: Set[(String, Func)] = {
    var set = Set[(String, Func)]()
    for (decT1 <- declarationTypeExamples; stat <- statExamples) {
      set = set.union(
        Set(
          """%s() is
            | %s
            |end
            |""".stripMargin.format(decT1._1, stat._1)
            -> Func(decT1._2._1, decT1._2._2, List(), stat._2)
        )
      )
    }
    set
  }

}
