package wacc

import wacc.AbstractSyntaxTree.BaseT._
import wacc.AbstractSyntaxTree._

object ValidateExamples {
  var BaseLiteralExamples = Set(
    IntLiteral(2) -> Right(BaseType(Int_T)),
    BoolLiteral(false) -> Right(BaseType(Bool_T)),
    CharLiteral('q') -> Right(BaseType(Char_T)),
    StringLiteral("spam") -> Right(BaseType(String_T))
  )

  var IdentLiteralExamples = Set(
    "int spam = 3" -> Right(BaseType(Int_T)),
    "bool spam = true" -> Right(BaseType(Bool_T)),
    "char spam = 'q'" -> Right(BaseType(Char_T)),
    "string spam = \"spam\"" -> Right(BaseType(String_T)),
    "int[] spam = [1, 2]" -> Right(ArrayType(BaseType(Int_T))),
    "pair(int, int) spam = newpair(2, 3)" -> Right(PairType(BaseType(Int_T), BaseType(Int_T)))
  )

  var ArrayTypeExamples = Set(
    ArrayLiteral(List(IntLiteral(12), IntLiteral(24))) -> Right(ArrayType(BaseType(Int_T))),
    ArrayLiteral(List(BoolLiteral(true), BoolLiteral(false))) -> Right(ArrayType(BaseType(Bool_T))),
    ArrayLiteral(List(CharLiteral('a'), CharLiteral('z'))) -> Right(ArrayType(BaseType(Char_T))),
    ArrayLiteral(List(StringLiteral("hey"), StringLiteral("33"))) -> Right(ArrayType(BaseType(String_T))),
    // ArrayLiteral(List()) -> Right(ArrayType(ArrayType())), // 2D array
    // ArrayLiteral(List(PairLiteral())) -> Right(ArrayType(PairType(BaseType(Int_T), BaseType(Int_T)))) // WIP
  )

  var PairTypeExamples = Set(
    
  )
}
