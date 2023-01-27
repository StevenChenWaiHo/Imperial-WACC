package wacc
import org.scalatest.flatspec.AnyFlatSpec
import parsley.Success
import Parser.ExpressionParser.expression
import wacc.AbstractSyntaxTree._
import wacc.AbstractSyntaxTree.UnaryOpType._
import wacc.AbstractSyntaxTree.BinaryOpType._
class ParserSpec extends AnyFlatSpec {

  "Expression Parser" can "parse positive integers" in {
    assert(expression.parse("123") == Success(IntLiteral(123)))
  }
  "Expression Parser" can "parse negative integers" in {
    assert(expression.parse("-123") == Success(IntLiteral(-123)))
  }

  "Expression Parser" can "parse 'true'" in {
    assert(expression.parse("true") == Success(BoolLiteral(true)))
  }

  "Expression Parser" can "parse 'false'" in {
    assert(expression.parse("false") == Success(BoolLiteral(false)))
  }

  "Expression Parser" can "parse a character" in {
    assert(expression.parse("\'a\'") == Success(CharLiteral('a')))
  }

  "Expression Parser" can "parse a string" in {
    assert(expression.parse("\"+ - ' true false\"") == Success(StringLiteral("+ - ' true false")))
  }

  "Expression Parser" can "parse null" in {
    assert(expression.parse("null") == Success(PairLiteral()))
  }

  "Expression Parser" can "parse identifiers" in {
    assert(expression.parse("nully") == Success(IdentLiteral("nully")))
  }

  "Expression Parser" can "parse identifiers containing numbers and uppercase letters, and no others" in {
    assert(expression.parse("_literal_123") == Success(IdentLiteral("_literal_123")))
    assert(expression.parse("_literal_(123") != Success(IdentLiteral("_literal_(123")))
  }

  "Expression Parser" should "not confuse literals and unary operations" in {
    assert(expression.parse("lenvar") == Success(IdentLiteral("lenvar")))
  }

  "Expression Parser" should "use correct binding when parsing prefix ops" in {
    assert(expression.parse("!b + 1") ==
      Success(BinaryOp(BinaryOpType.Add)((UnaryOp(Not)(IdentLiteral("b"))), IntLiteral(1))))
  }

  /*TODO: This test tests for behaviour which is allowed by the syntax of the language (as defined in the spec) -
     namely array indices with arbitrary expressions inside - but which is not allowed by the semantics of the language.
      This should raise an exception during semantic analysis, but it might be pretty easy to modify the parser so that these errors
       already get detected at this stage.*/
  "Expression Parser" can "parse array identifiers" in {
    assert(expression.parse("_literal_123[123][true][_literal_321_[321]]") ==
      Success(ArrayElem(List(IntLiteral(123),
        BoolLiteral(true), ArrayElem(List(IntLiteral(321)))("_literal_321_")))("_literal_123")))
  }

  "Expression Parser" can "fail when array indices don't match" in {
    assert(expression.parse("_literal_123[123][true][_literal_321_[321]").isFailure)
  }

  "Expression Parser" should "parse binary operations greedily" in {
    assert(expression.parse("a<=b") == Success(BinaryOp(Lte, IdentLiteral("a"), IdentLiteral("b"))))
  }

  "Expression Parser" can "parse unary operations" in {
    val ops = Set("chr" -> Chr, "len" -> Len, "ord" -> Ord, "-" -> Neg, "!" -> Not)
    for (op <- ops) {
      assert(expression.parse("a + " + op._1 + " b") == Success(BinaryOp(Add)(IdentLiteral("a"), UnaryOp(op._2)(IdentLiteral("b")))))
    }
  }

  "Expression Parser" should "parse bracketed expressions correctly" in {
    assert(expression.parse("4*(a<=b)") == Success(BinaryOp(Mul)(IntLiteral(4), BinaryOp(BinaryOpType.Lte)(IdentLiteral("a"), IdentLiteral("b")))))
  }

  "Expression Parser" should "respect precedence of operations" in {
    assert(expression.parse("1 * 2 + len x") == Success(BinaryOp(Add)(BinaryOp(Mul)(IntLiteral(1), IntLiteral(2)), UnaryOp(Len)(IdentLiteral("x")))))
  }
}