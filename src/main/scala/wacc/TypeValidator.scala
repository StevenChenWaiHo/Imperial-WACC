package wacc

import wacc.AbstractSyntaxTree.BaseT._
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.UnaryOpType._
import wacc.AbstractSyntaxTree._

object TypeValidator {
  implicit def declarationTypeToReturnType(t: DeclarationType): ReturnType = Right(t)

  implicit def makeBaseType(t: BaseTypeType): BaseType = BaseType(t)

  implicit def decTypeToList(x: DeclarationType): List[DeclarationType] = List(x)

  implicit def retTypeToList(x: ReturnType): List[ReturnType] = List(x)

  def returnType(expr: Expr)(implicit context: Map[String, DeclarationType]): ReturnType = expr match {
    case IntLiteral(_) => BaseType(Int_T)
    case BoolLiteral(_) => BaseType(Bool_T)
    case CharLiteral(_) => BaseType(Char_T)
    case StringLiteral(_) => BaseType(String_T)
    case IdentLiteral(name) => context(name)
    case UnaryOp(op, x) => UnaryOpExpectations(op) matchedWith returnType(x)
    case BinaryOp(op, x1, x2) => BinaryOpExpectations(op) matchedWith List(returnType(x1), returnType(x2))
  }

  private val UnaryOpExpectations = Map[UnOp, Expectation](
    Not -> TypeProcessor.simple(List(Bool_T) -> Bool_T),
    Neg -> TypeProcessor.simple(List(Int_T) -> Int_T),
    Len -> TypeProcessor.simple(List(ArrayType(AnyType())) -> Int_T),
    Ord -> TypeProcessor.simple(List(Char_T) -> Int_T),
    Chr -> TypeProcessor.simple(List(Int_T) -> Char_T)
  )

  private val boolComparisonTypes =
    TypeProcessor.conditional(List(List(Int_T, Int_T) -> Bool_T, List(Char_T, Char_T) -> Bool_T))

  private val identicalTypes = new Expectation((inputs: List[DeclarationType]) => {
    if(inputs(0) == inputs(1)) Right(BaseType(Bool_T))
    else Left(List("Only matching types may be compared using == and !="))
  })

  private val BinaryOpExpectations = Map[BinOp, Expectation](
    BinaryOpType.Mul -> TypeProcessor.simple(List(Int_T, Int_T) -> Int_T),
    BinaryOpType.Div -> TypeProcessor.simple(List(Int_T, Int_T) -> Int_T),
    BinaryOpType.Mod -> TypeProcessor.simple(List(Int_T, Int_T) -> Int_T),
    BinaryOpType.Add -> TypeProcessor.simple(List(Int_T, Int_T) -> Int_T),
    BinaryOpType.Sub -> TypeProcessor.simple(List(Int_T, Int_T) -> Int_T),
    BinaryOpType.Gt -> boolComparisonTypes,
    BinaryOpType.Gte -> boolComparisonTypes,
    BinaryOpType.Lt -> boolComparisonTypes,
    BinaryOpType.Lte -> boolComparisonTypes,
    BinaryOpType.Eq -> identicalTypes,
    BinaryOpType.Neq -> identicalTypes,
    BinaryOpType.And -> TypeProcessor.simple(List(Bool_T, Bool_T) -> Bool_T),
    BinaryOpType.Or -> TypeProcessor.simple(List(Bool_T, Bool_T) -> Bool_T)
  )
}
