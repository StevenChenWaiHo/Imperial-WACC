package wacc

import wacc.AbstractSyntaxTree._;
import wacc.Expectation

object SemanticAnalyser {



  /*
  case class FunctionType(ExpectedInput: List[DeclarationType], ExpectedOutput: DeclarationType)
  private def validateType(n:ASTNode, context: Map[IdentLiteral, FunctionType]): Boolean = {

  }
  private def verifyStat(stat: AbstractSyntaxTree.Stat, expectedFunctionType: FunctionType): List[Errors] = {
    val errorsList= List[Errors]
    stat match {
      case skipStat => {/*Pass by default*/}
      case Declaration(dataType, ident, rvalue) => {
        /*if ident is keyword then return an error*/
        dataType match{
          case NestedPair() => {}
          case BaseType(baseType) => {}
        }/*and make sure dataType and rvalue have same type*/}
      case Assignment(lvalue, rvalue) => {/*if lvalue type == rvalue type then true otherwise error*/}
      case Read(lvalue) => {/*Not sure what this is*/}
      case Command(command, input) => {/*Not sure what this is*/}
      case IfStat(cond, stat1, stat2) => {/*Make sure cond is boolean, verify stat1 and stat2 and make sure there is fi*/}
      case WhileLoop(cond, stat) => {/*Make sure cond is boolean, verify stat*/}
      case BeginEndStat(stat) => {/*verify stat (What is this?)*/}
      case StatList(statList) => {/*verify stat in list*/}
    }
  }
  sealed trait Errors
  case class DeclarationError(errorMessage: String) extends Errors
  case class AssignmentError(errorMessage: String) extends Errors
  case class ReadError(errorMessage: String) extends Errors
  case class CommandError(errorMessage: String) extends Errors
  case class ConditionError(errorMessage: String) extends Errors
  case class ArrayError(errorMessage: String) extends Errors
  */
}
