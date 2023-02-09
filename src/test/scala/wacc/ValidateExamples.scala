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

  var UnaryOpExamples = Set(
    UnaryOp(UnaryOpType.Not, BoolLiteral(true)) -> Right(BaseType(Bool_T)),
    UnaryOp(UnaryOpType.Neg, IntLiteral(3)) -> Right(BaseType(Int_T)),
    // UnaryOp(UnaryOpType.Len, ArrayElem("arr", List(IntLiteral(3)))) -> Right(BaseType(Int_T)),
    UnaryOp(UnaryOpType.Ord, CharLiteral('c')) -> Right(BaseType(Int_T)),
    UnaryOp(UnaryOpType.Chr, IntLiteral(3)) -> Right(BaseType(Char_T))
  )

  var BinaryOpExamples = Set(
    BinaryOp(BinaryOpType.Mul, IntLiteral(3), IntLiteral(3)) -> Right(BaseType(Int_T)),
    BinaryOp(BinaryOpType.Div, IntLiteral(3), IntLiteral(3)) -> Right(BaseType(Int_T)),
    BinaryOp(BinaryOpType.Mod, IntLiteral(3), IntLiteral(3)) -> Right(BaseType(Int_T)),
    BinaryOp(BinaryOpType.Add, IntLiteral(3), IntLiteral(3)) -> Right(BaseType(Int_T)),
    BinaryOp(BinaryOpType.Sub, IntLiteral(3), IntLiteral(3)) -> Right(BaseType(Int_T)),

    BinaryOp(BinaryOpType.Gt, IntLiteral(3), IntLiteral(3)) -> Right(BaseType(Bool_T)),
    BinaryOp(BinaryOpType.Gte, CharLiteral('c'), CharLiteral('c')) -> Right(BaseType(Bool_T)),
    BinaryOp(BinaryOpType.Lt, CharLiteral('c'), CharLiteral('c')) -> Right(BaseType(Bool_T)),
    BinaryOp(BinaryOpType.Lte, IntLiteral(3), IntLiteral(3)) -> Right(BaseType(Bool_T)),
    BinaryOp(BinaryOpType.Eq, IntLiteral(3), IntLiteral(3)) -> Right(BaseType(Bool_T)),
    BinaryOp(BinaryOpType.Neq, IntLiteral(3), IntLiteral(3)) -> Right(BaseType(Bool_T)),
    BinaryOp(BinaryOpType.And, BoolLiteral(true), BoolLiteral(false)) -> Right(BaseType(Bool_T)),
    BinaryOp(BinaryOpType.Or, BoolLiteral(true), BoolLiteral(false)) -> Right(BaseType(Bool_T))
  )
}
