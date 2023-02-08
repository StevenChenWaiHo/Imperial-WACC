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
          Left(List("Variable names cannot be identifiers or contain operator"))
        }
        if (context.findVar(ident.name).nonEmpty) {
          Left(List("Variable " + ident.name + " exists in this scope already"))
        }
        // TODO: add variable to context if no errors
        //context.addVar(ident.name, BaseType(ident)) ??
        dataType match {
          case NestedPair() => Left(List("Not Yet Implemented"))
           /*
            baseType.equals(/*evaluated expectation of rval*/)
            */
          case BaseType(baseType) => {
            // int i = 0
            rvalue match {
              case IntLiteral(x) => {
                if (!dataType.equals(BaseType(Int_T))) {
                  // TODO: extract this error message into function
                  return Left(List("RHS has incorrect type"))
                }
                context.addVar(ident.name, BaseType(Int_T))
                Right(context)
              }
              // int i = i + 1
              case binOp@BinaryOp(op, expr1, expr2) => {
                returnType(binOp)(context) match {
                  case Left(err) => Left(err)
                  case Right(opType) => {
                    if (dataType.equals(opType)) {
                      context.addVar(ident.name, opType)
                      return Right(context)
                    }
                    Left(List("RHS has incorrect type")) 
                  }
                }
              }
              // int i = ord 'a'
              case unOp@UnaryOp(op, expr) => {
                returnType(unOp)(context) match {
                  case Left(err) => Left(err)
                  case Right(opType) => {
                    context.addVar(ident.name, opType)
                    Right(context)
                  }
                }
              }
              // int i = f()
              case call@Call(ident, args) => {
                // TODO: implement
                Right(context)
              }
              case any => Left(List("rvalue %s not implemented".format(any)))
            }
          }
          case PairType(fstType, sndType) => {
            rvalue match {
              case PairValue(exp1, exp2) => {
                if (exp1 != fstType || exp2 != sndType) {
                  Left(List("Pair values do not match"))
                } else {
                  Left(List("Good Pair Value Not Yet Implemented"))
                }
              }
              case _ => {
                Left(List("Rhs not a pair"))
              }
            }
          }
          case ArrayType(dataType) => {
            rvalue match {
              case ArrayLiteral(elements) => {
                for (element <- elements) {
                  if (element != dataType) {
                    return Left(List("Invalid array typing"))
                  }
                }
                return Left(List("ArryType Not Yet Implemented"))
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
          return Left(List("Identifier " + name + " not in scope"))
        }
        /* If lvalue type == rvalue type then true */
        /*
        if (returnType(lvalue) == returnType(rvalue)) {
          ???
        }
        */

        Left(List("Not Yet Implemented: lvalue/rvalue type checking"))
      }
      case Read(lvalue) => {
        /*Not sure what this is*/
        // TODO: ensure lvalue is int or char
        Right(context)
      }
      case Command(command, input) => {
        /*Not sure what this is*/
        Right(context)
      }
      case IfStat(cond, stat1, stat2) => {
        /*Make sure cond is boolean, verify stat1 and stat2 and make sure there is fi*/
        returnType(cond)(context) match {
          case Left(err) => Left(err)
          case Right(sType) => {
            if (!sType.equals(BaseType(Bool_T))) {
              return Left(List("Semantic Error: if condition is not of type Bool"))
            }
            verifyStat(context, stat1) match {
              case Left(err) => return Left(err)
              case Right(_) => return verifyStat(context, stat2)
            }
          }
        }
        // TODO: verify there is a fi
      }
      case WhileLoop(cond, stat) => {
        /*Make sure cond is boolean, verify stat*/
        returnType(cond)(context) match {
          case Left(err) => Left(err)
          case Right(sType) => {
            if (!sType.equals(BaseType(Bool_T))) {
              return Left(List("Semantic Error: while condition is not of type Bool"))
            }
            verifyStat(context, stat)
          }
        }
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
            // TODO: does this exit correctly on error?
            case Left(err) => Left(err)
            case Right(c) => newContext = c
          }
        }
        Right(newContext)
      }
    }
  }
}

/*
  sealed trait Errors
  case class DeclarationError(errorMessage: String) extends Errors
  case class AssignmentError(errorMessage: String) extends Errors
  case class ReadError(errorMessage: String) extends Errors
  case class CommandError(errorMessage: String) extends Errors
  case class ConditionError(errorMessage: String) extends Errors
  case class ArrayError(errorMessage: String) extends Errors
*/
