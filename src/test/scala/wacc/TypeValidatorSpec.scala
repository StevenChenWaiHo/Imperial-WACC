package wacc

import org.scalatest.flatspec.AnyFlatSpec
import parsley.Success
import wacc.AbstractSyntaxTree.BaseT._
import wacc.AbstractSyntaxTree._
import wacc.Parser.StatementParser
import wacc.Parser.StatementParser.statement
import wacc.TypeValidator._


class TypeValidatorSpec extends AnyFlatSpec {

  import wacc.ValidateExamples._

  implicit val emptyScope = new ScopeContext()

  implicit val emptyContext = new ScopeContext()

  "Type Validator" can "validate base literals" in {
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

  
  // "Type Validator" can "validate ident-type literals" in {
  //   for (ident <- IdentLiteralExamples) {
  //     assert(returnType(ident._1) == ident._2)
  //   }
  // }

  // "Type Validator" can "validate array-type literals" in {
  //   for (array <- ArrayTypeExamples) {
  //     assert(returnType(array._1) == array._2)
  //   }
  // }

  // "Type Validator" can "validate pair-type literals" in {
  //   for (pair <- PairTypeExamples) {
  //     assert(returnType(pair._1) == pair._2)
  //   }
  // }

    "Type Validator" can "validate unaryOps" in {
    for (uop <- UnaryOpExamples) {
      assert(returnType(uop._1) == uop._2)
    }
  }

    "Type Validator" can "validate binaryOps" in {
    for (bop <- BinaryOpExamples) {
      assert(returnType(bop._1) == bop._2)
    }
  }
}

  "Type Validator" can "validate pair-type literals" in {
    for (pair <- PairTypeExamples) {
      assert(returnType(pair._1) == pair._2)
    }
  }
}

 */