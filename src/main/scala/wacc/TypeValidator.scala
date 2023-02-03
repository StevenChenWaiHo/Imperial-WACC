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
          case UnaryOp(op, _) => TypeProcessor()
        }
      }

      private val UnaryOpExpectations = Map[UnOp, (List[DeclarationType], DeclarationType)](
        (Not -> ()),
        (Neg ->),
        (Len ->),
        (Ord ->),
        (Chr ->)
      )
    }

  }

}
