package wacc

object AbstractSyntaxTree {
  sealed trait Expr
  case class IntLiteral(x: Int) extends Expr
  case class BoolLiteral(x: Boolean) extends Expr
  case class CharLiteral(x: Char) extends Expr
  case class StringLiteral(x: String) extends Expr
  case class PairLiteral() extends Expr
  case class IdentLiteral(name: String) extends Expr
  case class ArrayElem(types: List[Expr])(name: String) extends Expr
}
