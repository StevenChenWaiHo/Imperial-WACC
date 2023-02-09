package wacc

import org.scalatest.flatspec.AnyFlatSpec
import parsley.Success
import wacc.AbstractSyntaxTree.BaseT._
import wacc.AbstractSyntaxTree._
import wacc.Parser.StatementParser.statement
import wacc.TypeValidator._


class TypeValidatorSpec extends AnyFlatSpec {

  import wacc.ValidateExamples._

  implicit val emptyScope = new ScopeContext()

  "Type Validator" can "validate base-type literals" in {
    for (base <- BaseLiteralExamples) {
      assert(returnType(base._1) == base._2)
    }
  }

  "DeclarationTypes" can "be compared" in {
    val valids = Set(
      BaseType(Int_T) -> BaseType(Any_T),
      BaseType(Any_T) -> BaseType(Int_T),
      BaseType(Int_T) -> BaseType(Int_T)
    )
    for(pair <- valids) assert(pair._1 is pair._2)
  }
}
/*
  // returnType is a placeholder function below

  "Type Validator" can "validate ident-type literals" in {
    for (ident <- IdentLiteralExamples) {
      var result = statement.parse(ident._1) // should be IdentLiteral("spam")
      assert(returnType(result) == ident._2)
    }
  }

  "Type Validator" can "validate array-type literals" in {
    for (array <- ArrayTypeExamples) {
      assert(returnType(array._1) == array._2)
    }
  }

  "Type Validator" can "validate pair-type literals" in {
    for (pair <- PairTypeExamples) {
      assert(returnType(pair._1) == pair._2)
    }
  }
}

 */