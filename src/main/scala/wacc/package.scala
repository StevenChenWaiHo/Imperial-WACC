import wacc.AbstractSyntaxTree.DeclarationType

package object wacc {
  type ReturnType = Either[List[String], DeclarationType]
  type ReturnTypeMatcher = List[ReturnType] => ReturnType 
}
