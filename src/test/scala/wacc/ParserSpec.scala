package wacc
import org.scalatest.flatspec.AnyFlatSpec
import parsley.{Failure, Success}
import Parser.Expression.parseExpr
import wacc.AbstractSyntaxTree._
class ParserSpec extends AnyFlatSpec {

  "Expression Parser" can "evaluate positive integers" in {
    assert(parseExpr.parse("+123") == Success(IntLiteral(123)))
  }

  "Expression Parser" can "evaluate negative integers" in {
    assert(parseExpr.parse("-123") == Success(IntLiteral(-123)))
  }

  "Expression Parser" can "evaluate 'true'" in {
    assert(parseExpr.parse("true") == Success(BoolLiteral(true)))
  }

  "Expression Parser" can "evaluate 'false'" in {
    assert(parseExpr.parse("false") == Success(BoolLiteral(false)))
  }

  "Expression Parser" can "evaluate a character" in {
    assert(parseExpr.parse("\'a\'") == Success(CharLiteral('a')))
  }

  "Expression Parser" can "evaluate a string" in {
    assert(parseExpr.parse("\"+ - ' true false\"") == Success(StringLiteral("+ - ' true false")))
  }

  "Expression Parser" can "parse null" in {
    assert(parseExpr.parse("null") == Success(PairLiteral()))
  }

  "Expression Parser" can "parse identifiers" in {
    assert(parseExpr.parse("nully") == Success(IdentLiteral("nully")))
  }

  "Expression Parser" can "parse identifiers containing numbers and uppercase letters, and no others" in {
    assert(parseExpr.parse("_literal_123") == Success(IdentLiteral("_literal_123")))
    assert(parseExpr.parse("_literal_(123") != Success(IdentLiteral("_literal_(123")))
  }

  "Expression Parser" should "not confuse literals and unary operations" in {
    assert(parseExpr.parse("len_var") == Success(IdentLiteral("len_var")))
  }

  /*TODO: This test tests for behaviour which is allowed by the syntax of the language (as defined in the spec) -
     namely array indices with arbitrary expressions inside - but which is not allowed by the semantics of the language.
      This should raise an exception during semantic analysis, but it might be pretty easy to modify the parser so that these errors
       already get detected at this stage.*/
  "Expression Parser" can "parse array identifiers" in {
    assert(parseExpr.parse("_literal_123[123][true][_literal_321_[321]]") == Success(ArrayElem(List(IntLiteral(123),
      BoolLiteral(true), ArrayElem(List(IntLiteral(321)))("_literal_321_")))("_literal_123")))
  }

  "Expression Parser" can "fail when array indices don't match" in {
    assert(parseExpr.parse("_literal_123[123][true][_literal_321_[321]").isFailure)
  }

  "Expression Parser" should "parse binary operations greedily" in {
    assert(parseExpr.parse ("a<=b") == Success(BinaryOp(BinaryOpType.Lte, IdentLiteral("a"), IdentLiteral("b"))))
  }

}
