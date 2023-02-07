package wacc

import wacc.AbstractSyntaxTree._


import scala.util.control.Breaks.break

object SemanticAnalyser {

  def verifyProgram(program: Program) = {
    val topLevelScope = new ScopeContext()
    for (func <- program.funcs) {
      verifyFunc(topLevelScope, func)
    }
    verifyStat(topLevelScope, program.stats)
  }

  private def verifyFunc(context: ScopeContext, func: Func) = {
    // TODO: Change this to properly deal with functions
    verifyStat(context, func.code)
  }

  private def verifyStat(context: ScopeContext, stat: Stat): Either[List[String], ScopeContext] = {
    val listOfErrors = List(): List[String];
    val hardKeywords = Set(
      "true", "false", "null", "len", "ord", "chr", "skip", "read", "free", "return", "exit",
      "print", "println", "if", "then", "else", "fi", "while", "do", "done", "begin", "end",
      "int", "bool", "char", "string", "fst", "snd", "call", "pair", "newpair", "is"
    ): Set[String]
    val hardOperators = Set("!", "*", "/", "%", "+", "-", ">=", ">", "<", "<=", "==", "!=", "&&", "||", ";") : Set[String]
    stat match {
      case skipStat => context
      case Declaration(dataType, ident, rvalue) => {
        if (hardKeywords.contains(ident.name) || ident.name.contains(hardOperators)) {
          listOfErrors.appended("Variable names cannot be identifiers or contain operator\n")
        }
        /*if ident is keyword then return an error*/
        if (context.findVar(ident.name).nonEmpty) {
          listOfErrors.appended("Variable exists in this scope already\n")
        }
        dataType match {
          case NestedPair() => {}
          case BaseType(baseType) => {baseType.equals(/*evaluated expectation of rval*/)}
          case PairType(fstType, sndType) => {
            rvalue match {
              case PairValue(exp1, exp2) => {
                if (exp1 != fstType || exp2 != sndType) {
                  listOfErrors.appended("Pair values do not match\n")
                }
              }
              case _ => {
                listOfErrors.appended("Rhs not a pair\n")
              }

            }
          }
          case ArrayType(dataType) => {
            rvalue match {
              case ArrayLiteral(elements) => {
                for (element <- elements) {
                  if (element != dataType) {
                    listOfErrors.appended("Invalid array typing")
                    break
                  }
                }
              }
            }
          }
        }
        /*and make sure dataType and rvalue have same type*/
      }
      case Assignment(lvalue, rvalue) => {
        if (context.findVar(lvalue).isEmpty) {
          listOfErrors.appended("Variable doesn't exist in this scope currently")
        }
        /*if lvalue type == rvalue type then true otherwise error*/
        Left(List("Not Yet Implemented"))
      }
      case Read(lvalue) => {
        /*Not sure what this is*/
        Left(List("Not Yet Implemented"))
      }
      case Command(command, input) => {
        /*Not sure what this is*/
        Left(List("Not Yet Implemented"))
      }
      case IfStat(cond, stat1, stat2) => {
        cond match {
          case BoolLiteral(x) => context
          case UnaryOp(UnaryOpType(),)
        }
        /*Make sure cond is boolean, verify stat1 and stat2 and make sure there is fi*/
        Left(List("Not Yet Implemented"))
      }
      case WhileLoop(cond, stat) => {
        /*Make sure cond is boolean, verify stat*/
        verifyStat(context, stat)
      }
      case BeginEndStat(stat) => {
        /*verify stat (What is this?)*/
        verifyStat(context, stat)
      }
      case StatList(statList) => {
        /*verify stat in list*/
        var newContext = context
        for (stat <- statList) {
          //newContext = verifyStat(newContext, stat)
        }
        Right(newContext)
      }
    }
  }
}

/*

object SemanticAnalyser {
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
}

*/
