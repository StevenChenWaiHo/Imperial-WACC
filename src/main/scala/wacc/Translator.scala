package wacc

import wacc.AbstractSyntaxTree.{ASTNode, Stat, SkipStat, BeginEndStat, 
  Command, Program, Func, Expr, BinaryOp, UnaryOp}
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.TAC._
import wacc.AbstractSyntaxTree.IfStat
import wacc.AbstractSyntaxTree.Assignment

class Translator {

  private val map = collection.mutable.Map[ASTNode, TRegister]()

  def nextRegister(): TRegister = {
    new TRegister(0)
  }
  
  def delegateASTNode(node: ASTNode, context : ScopeContext) : (List[TAC], TRegister) = {
    // Check if ASTNode has already been calculated
    map.get(node) match {
      case Some(reg) => (List(reg), reg)
      case None => {
        node match {
          case BinaryOp(op, expr1, expr2) => translateBinOp(op, expr1, expr2)
          case UnaryOp(op, expr) => translateUnOp(op, expr)
          case IfStat(cond, stat1, stat2) => translateIfStat(cond, stat1, stat2)
          case _ => (List(), null)
        }
      }
    }
  }

  def translateBinOp(op: BinOp, exp1: Expr, exp2: Expr) = {
    delegateASTNode(exp1, null) match {
      case (tacList1, reg1) => {
        delegateASTNode(exp2, null) match {
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
    delegateASTNode(exp, null) match {
      case (tacList, reg) => {
        val nextReg = nextRegister()
        (tacList ++ List(UnaryOpTAC(op, reg, nextReg)),
          nextReg)
      }
    }
  }

  def translateIfStat(cond: Expr, stat1: Stat, stat2: Stat): (List[TAC], TRegister) = {
    delegateASTNode(cond, null) match {
      case (condList, reg1) => {
          delegateASTNode(stat1, null) match {
            case (trueList, reg2) => {
              delegateASTNode(stat2, null) match {
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

  def translateProgram(l: List[Func], s: Stat, context: ScopeContext): List[TAC] = {
    List()
  }

  def translateBeginEnd(stat : Stat) : List[TAC] = {
    List()
  }

  def translateSkip() : List[TAC] = {
    List()
  }

  def translateCommand(cmd : AbstractSyntaxTree.CmdT.Cmd, expr : AbstractSyntaxTree.Expr) : List[TAC] = {
    List()
  }

  def translateFunction(returnType : AbstractSyntaxTree.DeclarationType, 
                          ident : AbstractSyntaxTree.IdentLiteral, 
                          types : List[(AbstractSyntaxTree.DeclarationType, 
                            AbstractSyntaxTree.IdentLiteral)], 
                          code : Stat) : List[TAC] = {
    List()
  }
}
