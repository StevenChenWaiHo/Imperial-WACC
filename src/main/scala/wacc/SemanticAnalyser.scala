package wacc

import wacc.AbstractSyntaxTree._
import wacc.AbstractSyntaxTree.BaseT._
import wacc.TypeValidator.returnType

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
      case SkipStat() => Right(context)
      case Declaration(dataType, ident, rvalue) => {
        /* Ensure identifier does not include keywords */
        if (hardKeywords.contains(ident.name) || ident.name.contains(hardOperators)) {
          Left(List("Variable names cannot be identifiers or contain operator\n"))
        }
        if (context.findVar(ident.name).nonEmpty) {
          listOfErrors.appended("Variable " + ident.name + " exists in this scope already\n")
        }
        // TODO: add variable to context if no errors
        //context.addVar(ident.name, BaseType(ident)) ??
        dataType match {
          case NestedPair() => Left(List("Not Yet Implemented\n"))
          case BaseType(baseType) => {
            /*
            baseType.equals(/*evaluated expectation of rval*/)
            */
            Left(List("Not Yet Implemented\n"))
          }
          case PairType(fstType, sndType) => {
            rvalue match {
              case PairValue(exp1, exp2) => {
                if (exp1 != fstType || exp2 != sndType) {
                  Left(List("Pair values do not match\n"))
                } else {
                  Left(List("Not Yet Implemented\n"))
                }
              }
              case _ => {
                Left(List("Rhs not a pair\n"))
              }
            }
          }
          case ArrayType(dataType) => {
            rvalue match {
              case ArrayLiteral(elements) => {
                for (element <- elements) {
                  if (element != dataType) {
                    return Left(List("Invalid array typing\n"))
                  }
                }
                return Left(List("Not Yet Implemented\n"))
              }
            }
          }
        }
        /*and make sure dataType and rvalue have same type*/
      }
      case Assignment(lvalue, rvalue) => {
        /* Check if LHS is in scope */
        val name = lvalue match {
          case IdentLiteral(name) => name
          case ArrayElem(name, indicicies) => name
        }
        if (context.findVar(name).isEmpty) {
          return Left(List("Identifier " + name + " not in scope\n"))
        }
        /* If lvalue type == rvalue type then true */
        /*
        if (returnType(lvalue) == returnType(rvalue)) {
          ???
        }
        */

        Left(List("Not Yet Implemented: lvalue/rvalue type checking\n"))
      }
      case Read(lvalue) => {
        /*Not sure what this is*/
        Left(List("Not Yet Implemented\n"))
      }
      case Command(command, input) => {
        /*Not sure what this is*/
        Left(List("Not Yet Implemented\n"))
      }
      case IfStat(cond, stat1, stat2) => {
        /*Make sure cond is boolean, verify stat1 and stat2 and make sure there is fi*/

        if (returnType(cond)(context) == BaseType(Bool_T)) {
          verifyStat(context, stat1) match {
            case Left(err) => return Left(err)
            case Right(_) => return verifyStat(context, stat2)
          }
        }
        Left(List("Semantic Error: Condition is not of type Bool\n"))
        // TODO: verify there is a fi
      }
      case WhileLoop(cond, stat) => {
        /*Make sure cond is boolean, verify stat*/
        if (returnType(cond)(context) == BaseType(Bool_T)) {
          return verifyStat(context, stat)
        }
        Left(List("Semantic Error: Condition is not of type Bool\n"))
      }
      case BeginEndStat(stat) => {
        /*verify stat (What is this?)*/
        verifyStat(context, stat)
      }
      case StatList(statList) => {
        /*verify stat in list*/
        var newContext = context
        for (stat <- statList) {
          verifyStat(newContext, stat) match {
            case Left(err) => return Left(err)
            case Right(c) => newContext = c
          }
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
