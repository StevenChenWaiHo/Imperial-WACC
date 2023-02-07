import wacc.AbstractSyntaxTree.DeclarationType

package object wacc {
  type TypeMatcher = List[Either[List[String], DeclarationType]] => Either[List[String], DeclarationType]
}
