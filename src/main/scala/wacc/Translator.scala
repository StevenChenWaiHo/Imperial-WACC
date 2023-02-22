package wacc

import wacc.AbstractSyntaxTree._
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.TAC._

object Translator {

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
      case Some(reg) => (List(), reg)
      case None => {
        val tac = node match {
          case BinaryOp(op, expr1, expr2) => translateBinOp(op, expr1, expr2)
          case UnaryOp(op, expr) => translateUnOp(op, expr)
          case IfStat(cond, stat1, stat2) => translateIfStat(cond, stat1, stat2)
          case Declaration(dataType, ident, rvalue) => translateAssignment(ident, rvalue)
          case Assignment(lvalue, rvalue) => translateAssignment(lvalue, rvalue)
          case BeginEndStat(stat) => translateBeginEnd(stat)
          case SkipStat() => translateSkip()
          case Program(funcs, stats) => translateProgram(funcs, stats)
          case StatList(stats) => translateStatList(stats)
          case Command(command, input) => translateCommand(command, input)
          case lit: Literal => translateLiteral(lit)
          // TODO: check this if can be included in translate literal
          case ArrayLiteral(elements) => translateArrayLiteral(elements)
          case na => (List(new Label("Not Implemented " + na)), null)
        }
        map.addOne(node, tac._2)
        tac
      }
    }
  }

  def translateLiteral(lit: Literal): (List[TAC], TRegister) = {
    val next = nextRegister()
    val lhs = lit match {
      case BoolLiteral(x) => new BoolLiteralTAC(x)
      case CharLiteral(x) => new CharLiteralTAC(x)
      case IntLiteral(x) => new IntLiteralTAC(x)
      case StringLiteral(x) => new StringLiteralTAC(x)
      case IdentLiteral(x) => new IdentLiteralTAC(x)
      //case PairLiteral(x) => new PairLiteralTAC(x)
      //case ArrayLiteral(x) => translateArrayLiteral(x)
    }
    (List(AssignmentTAC(lhs, next)), next)
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
            (lList ++ rList ++ List(new AssignmentTAC(rReg, lReg)), lReg) // TODO: Check this
          }
        }
      }
    }
  }

  def translateProgram(l: List[Func], s: Stat): (List[TAC], TRegister) = {
    // TODO: translate funcs
    delegateASTNode(s) match {
      case (tacList, reg) => {
        (List(Label("main"), BeginFuncTAC()) ++ tacList ++ List(EndFuncTAC()), reg)
      }
    }
  }

  def translateStatList(stats: List[Stat]): (List[TAC], TRegister) = {
    // TODO: Change to not use mutable list?
    val TAClist = collection.mutable.ListBuffer[TAC]()
    stats.foreach(s => {
      TAClist.addAll(delegateASTNode(s)._1)
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

  def translateArrayLiteral(elems: List[Expr]): (List[TAC], TRegister) = {
    val instrs = collection.mutable.ListBuffer[TAC]()
    val regs = collection.mutable.ListBuffer[TRegister]()
    elems.foreach(e => {
      val node = delegateASTNode(e)
      node match {
        case (instr, reg) => {
          instrs.addAll(instr)
          regs += reg
        }
      }
    })
    val next = nextRegister()
    (instrs.toList ++ List(AssignmentTAC(new ArrayOp(regs.toList), next)),
      next)
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
