package wacc

import wacc.AbstractSyntaxTree._
import wacc.AbstractSyntaxTree.BaseT._

object TypeValidator {
  /* The input and output type an AST node is expecting. */
  final case class Expectation (input: List[DeclarationType], output: DeclarationType)

  private def TypeGenerator(declarationType: DeclarationType) = new Expectation(Nil, declarationType)
  private def TypeProcessor(types: (List[DeclarationType], DeclarationType)) = new Expectation(types._1, types._2)

  /* Contains functions for getting an expectation for any type of AST node. */
  object Expectation {

    object Expression {
      import wacc.AbstractSyntaxTree.UnaryOpType._

      def apply(expr: Expr) {
        expr match {
          case IntLiteral(_) => TypeGenerator(BaseType(Int_T))
          case BoolLiteral(_) => TypeGenerator(BaseType(Bool_T))
          case CharLiteral(_) => TypeGenerator(BaseType(Char_T))
          case StringLiteral(_) => TypeGenerator(BaseType(String_T))
          case UnaryOp(op, _) => TypeProcessor(UnaryOpExpectations.get(op).get)//Not the best way)
        }
      }

      private val UnaryOpExpectations = Map[UnOp, (List[DeclarationType], DeclarationType)](
        (Not -> (List[DeclarationType](BaseType(Bool_T)), BaseType(Bool_T))),
        (Neg -> (List[DeclarationType](BaseType(Int_T)), BaseType(Int_T))),
        (Len -> (List[DeclarationType](BaseType(String_T)), BaseType(String_T))),
        (Ord -> (List[DeclarationType](BaseType(String_T)), BaseType(Int_T))),
        (Chr -> (List[DeclarationType](BaseType(Int_T)), BaseType(String_T)))
      )
    }
  }

}
