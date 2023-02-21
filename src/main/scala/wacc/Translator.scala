package wacc

import wacc.AbstractSyntaxTree._
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.TAC._

class Translator {

  private val map = collection.mutable.Map[ASTNode, TRegister]()
  private val regList = collection.mutable.ListBuffer[TRegister]()

  def nextRegister(): TRegister = {
    val next = new TRegister(regList.length)
    regList += next
    next
  }
  
  def delegateASTNode(node: ASTNode) : (List[TAC], TRegister) = {
    // Check if ASTNode has already been calculated
    map.get(node) match {
      case Some(reg) => (List(reg), reg)
      case None => {
        node match {
          case BinaryOp(op, expr1, expr2) => translateBinOp(op, expr1, expr2)
          case UnaryOp(op, expr) => translateUnOp(op, expr)
          case IfStat(cond, stat1, stat2) => translateIfStat(cond, stat1, stat2)
          case Assignment(lvalue, rvalue) => translateAssignment(lvalue, rvalue)
          case BeginEndStat(stat) => translateBeginEnd(stat)
          case SkipStat() => translateSkip()
          case Program(funcs, stats) => translateProgram(funcs, stats)
          case StatList(stats) => translateStatList(stats)
          case _ => (List(new Label("Not Implemented")), null)
        }
      }
    }
  }

  def translateBinOp(op: BinOp, exp1: Expr, exp2: Expr) = {
    delegateASTNode(exp1) match {
      case (tacList1, reg1) => {
        delegateASTNode(exp2) match {
          case (tacList2, reg2) => {
            val nextReg = nextRegister()
            (tacList1 ++ tacList2 ++ List(BinaryOpTAC(op, reg1, reg2, nextReg)),
              nextReg)
          }
        }
      }
    }
  }

  def translateUnOp(op: UnOp, exp: Expr): (List[TAC], TRegister) = {
    delegateASTNode(exp) match {
      case (tacList, reg) => {
        val nextReg = nextRegister()
        (tacList ++ List(UnaryOpTAC(op, reg, nextReg)),
          nextReg)
      }
    }
  }

  def translateIfStat(cond: Expr, stat1: Stat, stat2: Stat): (List[TAC], TRegister) = {
    delegateASTNode(cond) match {
      case (condList, reg1) => {
          delegateASTNode(stat1) match {
            case (trueList, reg2) => {
              delegateASTNode(stat2) match {
                case (falseList, reg3) => {
                  val l1 = new Label()
                  val l2 = new Label()
                  (condList ++ List(IfTAC(reg1, l1)) ++ falseList ++ List(GOTO(l2), l1) ++ trueList ++ List(l2),
                    null)
                }
              }
            }
          }
      }
    }
  }

  def translateAssignment(lvalue: LVal, rvalue: RVal): (List[TAC], TRegister) = {
    delegateASTNode(lvalue) match {
      case (lList, lReg) => {
        delegateASTNode(rvalue) match {
          case (rList, rReg) => {
            (lList ++ rList ++ List(AssignmentTAC(rReg, lReg)), lReg) // TODO: Check this
          }
        }
      }
    }
  }

  def translateProgram(l: List[Func], s: Stat): (List[TAC], TRegister) = {
    // TODO: translate funcs
    println(delegateASTNode(s))
    delegateASTNode(s)
  }

  def translateStatList(stats: List[Stat]): (List[TAC], TRegister) = {
    // TODO: Change to not use mutable list?
    val TAClist = collection.mutable.ListBuffer[TAC]()
    stats.foreach(s => {
      TAClist ++ delegateASTNode(s)._1
    })
    (TAClist.toList, null)
  }

  def translateBeginEnd(stat : Stat): (List[TAC], TRegister) = {
    delegateASTNode(stat) match {
      case (sList, sReg) => {
        (List(new Label()) ++ sList, null)
      }
    } 
  }

  def translateSkip(): (List[TAC], TRegister) = {
    (List(), null)
  }

  def translateCommand(cmd : AbstractSyntaxTree.CmdT.Cmd, expr : AbstractSyntaxTree.Expr) : (List[TAC], TRegister) = {
    delegateASTNode(expr) match {
      case (eList, eReg) => {
        (eList ++ List(CommandTAC(cmd, eReg)), null)
      }
    }
  }

  def translateFunction(returnType : AbstractSyntaxTree.DeclarationType, 
                          ident : AbstractSyntaxTree.IdentLiteral, 
                          types : List[(AbstractSyntaxTree.DeclarationType, 
                            AbstractSyntaxTree.IdentLiteral)], 
                          code : Stat) : List[TAC] = {
    List()
  }
}
