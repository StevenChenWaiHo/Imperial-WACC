package wacc

import wacc.AbstractSyntaxTree._
import wacc.AbstractSyntaxTree.BaseT._
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp

object TypeValidator {
  /* The input and output type an AST node is expecting. */
  final case class Expectation (input: List[DeclarationType], output: DeclarationType)

  private def TypeGenerator(declarationType: DeclarationType) = new Expectation(Nil, declarationType)
  private def TypeProcessor(types: (List[DeclarationType], DeclarationType)) = new Expectation(types._1, types._2)

  /* Contains functions for getting an expectation for any type of AST node. */
  object Expectation {
    implicit def baseTypeDeclaration(t: BaseT.BaseTypeType): DeclarationType = BaseType(t)

    object Expression {
      import wacc.AbstractSyntaxTree.UnaryOpType._

      def apply(expr: Expr): Expectation = expr match {
          case IntLiteral(_) => TypeGenerator(Int_T)
          case BoolLiteral(_) => TypeGenerator(Bool_T)
          case CharLiteral(_) => TypeGenerator(Char_T)
          case StringLiteral(_) => TypeGenerator(String_T)
          case UnaryOp(op, _) => TypeProcessor(UnaryOpExpectations(op))
        }


      private val UnaryOpExpectations = Map[UnOp, (List[DeclarationType], DeclarationType)](
        Not -> (List(Bool_T)  , Bool_T),
        Neg -> (List(Int_T)    , Int_T),
        Len -> (List(String_T) , String_T),
        Ord -> (List(String_T) ,Int_T),
        Chr -> (List(Int_T)    , String_T)
      )

      private val BinaryOpExpectations = Map[BinOp, (List[DeclarationType], DeclarationType)](
        BinaryOpType.Mul -> (List(Int_T), Int_T),
        BinaryOpType.Div -> (List(Int_T), Int_T),
        BinaryOpType.Mod -> (List(Int_T), Int_T),
        BinaryOpType.Add -> (List(Int_T, Char_T), Int_T),
        BinaryOpType.Sub -> (List(Int_T, Char_T), Int_T),
        BinaryOpType.Gt -> (List(Int_T, Char_T), Bool_T),
        BinaryOpType.Gte -> (List(Int_T, Char_T), Bool_T),
        BinaryOpType.Lt -> (List(Int_T, Char_T), Bool_T),
        BinaryOpType.Lte -> (List(Int_T, Char_T), Bool_T),
        BinaryOpType.Eq -> (List(Int_T, Char_T, Bool_T, String_T), Bool_T),
        BinaryOpType.Neq -> (List(Int_T, Char_T, Bool_T, String_T), Bool_T),
        BinaryOpType.And -> (List(Bool_T), Bool_T),
        BinaryOpType.Or -> (List(Bool_T), Bool_T)
      ) // for add to neq need to verify all input are of same type
    }
  }

}
