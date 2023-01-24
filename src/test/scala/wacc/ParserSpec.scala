package wacc
import org.scalatest.flatspec.AnyFlatSpec
import parsley.Success
import Parser.Expression.parseExp
import wacc.AbstractSyntaxTree.{BoolLiteral, CharLiteral, IntLiteral, StringLiteral, PairLiteral}
class ParserSpec extends AnyFlatSpec {

  "Expression Parser" can "evaluate positive integers" in {
    assert(parseExp.parse("+123") == Success(IntLiteral(123)))
  }

  "Expression Parser" can "evaluate negative integers" in {
    assert(parseExp.parse("-123") == Success(IntLiteral(-123)))
  }

  "Expression Parser" can "evaluate 'true'" in {
    assert(parseExp.parse("true") == Success(BoolLiteral(true)))
  }

  "Expression Parser" can "evaluate 'false'" in {
    assert(parseExp.parse("false") == Success(BoolLiteral(false)))
  }

  "Expression Parser" can "evaluate a character" in {
    assert(parseExp.parse("\'a\'") == Success(CharLiteral('a')))
  }

  "Expression Parser" can "evaluate a string" in {
    assert(parseExp.parse("\"+ - ' true false\"") == Success(StringLiteral("+ - ' true false")))
  }

  "Expression Parser" can "parse null" in {
    assert(parseExp.parse("null") == Success(PairLiteral()))
  }
}
