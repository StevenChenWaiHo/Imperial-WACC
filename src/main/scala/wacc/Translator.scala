package wacc

import scala.collection.mutable._
import wacc.AbstractSyntaxTree._
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.TAC._
import wacc.AbstractSyntaxTree.BaseT

object Translator {

  private val scopes = ListBuffer[Map[ASTNode, TRegister]]()
  private val typeMaps = ListBuffer[Map[String, DeclarationType]]()
  private val regCountStack = Stack[Int]()
  private val regList = ListBuffer[TRegister]()
  private val strings = Map[String, Label]()
  private val dataList = ListBuffer[TAC]()

  def newMap(): Map[ASTNode, TRegister] = { 
    // Push scope on to stack when entering new context
    val map = Map[ASTNode, TRegister]()
    scopes.addOne(map)
    // Do the same for the map of ident->type
    val typeMap = Map[String, DeclarationType]()
    typeMaps.addOne(typeMap)
    // Push the current highest register for use later
    regCountStack.push(regList.length)
    map
  }

  def popMap(): Int = {
    // Pop scope off the stack when exiting a context
    scopes.remove(scopes.length - 1)
    typeMaps.remove(typeMaps.length - 1)
    // Remove the registers only used within that context
    val oldLength = regCountStack.pop()
    regList.remove(oldLength, regList.length - oldLength)
    // Return scope depth
    scopes.length
  }

  def findNode(node: ASTNode): Option[TRegister] = {
    // Returns register the value of node is stored in
    scopes.reverse.foreach(m => {
      m.get(node) match {
        case Some(x) => return Some(x)
        case _ =>
      }
    })
    None
  }

  def findType(expr: ASTNode): Option[DeclarationType] = {
    expr match {
      case IdentLiteral(name) => {
        // Returns type of ident
        typeMaps.reverse.foreach(m => {
          m.get(name) match {
            case Some(x) => return Some(x)
            case _ =>
          }
        })
        None
      }
      case ArrayElem(name, indices) => {
        if (indices.length == 0) {
          Some(ArrayType(BaseType(BaseT.Any_T)))
        } else {
          findType(indices.head)
        }
      }
      case UnaryOp(op, expr) => {
        // match based on op
        op match {
          case UnaryOpType.Chr => {
            Some(BaseType(BaseT.Char_T))
          }
          case UnaryOpType.Len | UnaryOpType.Neg | UnaryOpType.Ord => {
            Some(BaseType(BaseT.Int_T))
          }
          case UnaryOpType.Not => {
            // Assuming only booleans can be notted
            Some(BaseType(BaseT.Bool_T))
          }
        }
      }
      case BinaryOp(op, expr1, expr2) => {
        op match {
          case BinaryOpType.Add | BinaryOpType.Div | 
            BinaryOpType.Mod | BinaryOpType.Mul | BinaryOpType.Sub => {
              Some(BaseType(BaseT.Int_T))
          }
          case _ => {
            Some(BaseType(BaseT.Bool_T))
          }
        }
      }
      // TODO: unsure if pair type is correct
      case PairLiteral() => Some(BaseType(BaseT.None_T))
      case StringLiteral(x) => Some(BaseType(BaseT.String_T))
      case BoolLiteral(x) => Some(BaseType(BaseT.Bool_T))
      case CharLiteral(x) => Some(BaseType(BaseT.Char_T))
      case IntLiteral(x) => Some(BaseType(BaseT.Int_T))
      case t: PairType => Some(t)
      case _ => None
    }
  }

  def addNode(node: ASTNode, reg: TRegister) = {
    scopes.last.addOne(node, reg)
  }
  
  def addType(ident: IdentLiteral, bType: DeclarationType) = {
    typeMaps.last.addOne(ident.name, bType)
  }

  def nextRegister(): TRegister = {
    val next = new TRegister(regList.length)
    regList += next
    next
  }
  
  def delegateASTNode(node: ASTNode) : (List[TAC], TRegister) = {
    // Check if ASTNode has already been calculated
    findNode(node) match {
      case Some(reg) => (List(), reg)
      case None => {
        val tac = node match {
          case BinaryOp(op, expr1, expr2) => translateBinOp(op, expr1, expr2)
          case UnaryOp(op, expr) => translateUnOp(op, expr)
          case IfStat(cond, stat1, stat2) => translateIfStat(cond, stat1, stat2)
          case Declaration(dataType, ident, rvalue) => translateDeclaration(dataType, ident, rvalue)
          case Assignment(lvalue, rvalue) => translateAssignment(lvalue, rvalue)
          case BeginEndStat(stat) => translateBeginEnd(stat)
          case SkipStat() => translateSkip()
          case Program(funcs, stats) => (translateProgram(funcs, stats), null)
          case StatList(stats) => translateStatList(stats)
          case Command(command, input) => translateCommand(command, input)
          case lit: Literal => translateLiteral(lit)
          case ArrayLiteral(elements) => translateArrayLiteral(elements)
          case ArrayElem(name, indices) => translateArrayElem(name, indices)
          case WhileLoop(expr, stat) => translateWhileLoop(expr, stat)
          case Call(ident, args) => translateCall(ident, args)
          case Read(lval) => translateRead(lval)
          case na => (List(new Label("Not Implemented " + na)), null)
        }
        // Only add literal assignments/declarations to the scope
        node match {
          case x: Literal => addNode(node, tac._2)
          case _ =>
        }
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
      case StringLiteral(str) => {
        strings.getOrElse(str, {
          val lbl = new Label("L.str" + strings.size.toString)
          strings.addOne((str, lbl))
          dataList.addOne(Comments("length of " + lbl.toString()))
          dataList.addOne(StringLengthDefinitionTAC(str.length(), lbl))
          dataList.addOne(StringDefinitionTAC(str, lbl))
          lbl
        })
      }
      case IdentLiteral(x) => return (List(), next)
      //case PairLiteral(x) => new PairLiteralTAC(x)
      //case ArrayLiteral(x) => translateArrayLiteral(x)
    }
    (List(AssignmentTAC(lhs, next)), next)
  } 

  def translateRead(lval: LVal): (List[TAC], TRegister) = {
    delegateASTNode(lval) match {
      case (tacList, outReg) => {
        (tacList ++ List(AssignmentTAC(new TRegister(998), outReg)), outReg)
      }
    }
  }

  def translateArrayElem(name: String, indices: List[Expr]): (List[TAC], TRegister) = {
    delegateASTNode(IdentLiteral(name)) match {
      // Hopefully find the identifier in the map already
      case (_, aReg) => {
        val is = ListBuffer[TAC]()
        val rs = ListBuffer[TRegister]()
        indices.foreach(i => delegateASTNode(i) match {
          case (iList, iReg) => {
            is.addAll(iList)
            rs.addOne(iReg)
          }
        })
        val next = nextRegister()
        (is.toList ++ List(AssignmentTAC(new ArrayElemTAC(aReg, rs.toList), next)), next)
      }
    }
  }

  def translateWhileLoop(expr: Expr, stat: Stat): (List[TAC], TRegister) = {
    delegateASTNode(expr) match {
      case (expList, expReg) => {
        newMap()
        delegateASTNode(stat) match {
          case (statList, statReg) => {
            popMap()
            val startLabel = new Label("start")
            val bodyLabel = new Label("body")
            val endLabel = new Label("end")
            (List(startLabel) ++ expList 
            ++ List(IfTAC(expReg, bodyLabel), GOTO(endLabel), bodyLabel)
            ++ statList ++ List(GOTO(startLabel), endLabel), statReg)
          }
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
          newMap()
          delegateASTNode(stat2) match {
            case (falseList, reg3) => {
              popMap()
              newMap()
              delegateASTNode(stat1) match {
                case (trueList, reg2) => {
                  popMap()
                  val l1 = new Label("true")
                  val l2 = new Label("false")
                  (condList ++ List(IfTAC(reg1, l1)) ++ falseList ++ List(GOTO(l2), l1) ++ trueList ++ List(l2),
                    null)
                }
              }
            }
          }
      }
    }
  }

  def translateDeclaration(dataType: DeclarationType, ident: IdentLiteral, rvalue: RVal): (List[TAC], TRegister) = {
    addType(ident, dataType)
    dataType match {
      case BaseType(baseType) => translateBaseDeclaration(baseType, ident, rvalue)
      case PairType(fstType, sndType) => translatePairDeclaration(fstType, sndType, ident, rvalue)
      case ArrayType(dataType, length) => translateArrayDeclaration(dataType, length, ident, rvalue)
    }
  }

  def translateArrayDeclaration(dataType: DeclarationType, length: Integer, ident: IdentLiteral, rvalue: RVal): (List[TAC], TRegister) = {
    rvalue match {
      case ArrayLiteral(elements) => {
        val tacs = ListBuffer[TAC]()
        val tRegs = ListBuffer[TRegister]() //required?
        elements.foreach(e => {
          val (elemTacs, reg) = delegateASTNode(e)
          addNode(e, reg)
          tacs ++= elemTacs
          tacs += CreateArrayElem(dataType, reg)
          tRegs += reg
        })
        val arrReg = nextRegister()
        addNode(ident, arrReg)
        (List(Comments("Array Declaration Start")) ++ tacs.toList ++ List(CreateArray(tRegs.toList, arrReg),
         Comments("Array Declaration End")), arrReg)
      }
      case _ => (List(new Label("Array Type not Matched")), null)
    }
  }

  def translatePairDeclaration(fstType: DeclarationType, sndType: DeclarationType, ident: IdentLiteral, pairValue: RVal): (List[TAC], TRegister) = {
     pairValue match {
      case PairValue(exp1, exp2) => {
        val (exp1List, fstReg) = delegateASTNode(exp1)
        addNode(exp1, fstReg)
        val (exp2List, sndReg) = delegateASTNode(exp2)
        addNode(exp2, sndReg)
        val pairReg = nextRegister()
        addNode(ident, pairReg)
        (List(Comments("Pair Declaration Start")) ++ exp1List ++ List(CreatePairFstElem(fstType, fstReg)) ++ 
        exp2List ++ List(CreatePairSndElem(sndType, sndReg), 
        CreatePair(fstReg, sndReg, pairReg),Comments("Pair Declaration Ends")), pairReg)
      }
      case _ => (List(new Label("Pair Type not Matched")), null) 
    }
  }

  def translateBaseDeclaration(baseType: BaseT.BaseTypeType, ident: IdentLiteral, rvalue: RVal): (List[TAC], TRegister) = {
    delegateASTNode(rvalue) match {
      case (rList, rReg) => {
        addNode(ident, rReg)
        (rList, rReg)
      }
    }
  }

  def translateAssignment(lvalue: LVal, rvalue: RVal): (List[TAC], TRegister) = {
    lvalue match {
      case _: IdentLit => translateIdentAssignment(lvalue, rvalue)
      case _: PairElem => translatePairElemAssignment(lvalue, rvalue)
      case _: ArrayE =>  translateArrayElemAssignment(lvalue, rvalue)
    }
  }

  def translatePairElemAssignment(lvalue: LVal, rvalue: RVal): (List[TAC], TRegister) = {
    (List(), null)
  }

  def translateArrayElemAssignment(lvalue: LVal, rvalue: RVal): (List[TAC], TRegister) = {
    (List(), null)
  }

  def translateIdentAssignment(lvalue: LVal, rvalue: RVal): (List[TAC], TRegister) = {
    delegateASTNode(lvalue) match {
      case (lList, lReg) => {
        delegateASTNode(rvalue) match {
          case (rList, rReg) => {
            (lList ++ rList ++ List(new AssignmentTAC(rReg, lReg)), lReg)
          }
        }
      }
    }
  }

  def translateProgram(funcs: List[Func], s: Stat): List[TAC] = {
    // Initialise the .data segment
    dataList.addOne(DataSegmentTAC())
    // Start the code segment with the functions first
    val funcTAClist = ListBuffer[TAC](TextSegmentTAC())
    funcs.foreach(f => {
      funcTAClist.addAll(translateFunction(f))
    })
    newMap()
    delegateASTNode(s) match {
      case (tacList, reg) => {
        dataList.toList ++ funcTAClist.toList ++ List(Label("main"), BeginFuncTAC()) ++ tacList ++ List(EndFuncTAC())
      }
    }
  }

  def translateStatList(stats: List[Stat]): (List[TAC], TRegister) = {
    var TAClist = List[TAC]()
    stats.foreach(s => {
      TAClist = TAClist ++ delegateASTNode(s)._1
    })
    (TAClist, null)
  }

  def translateBeginEnd(stat : Stat): (List[TAC], TRegister) = {
    newMap()
    delegateASTNode(stat) match {
      case (sList, sReg) => {
        popMap()
        (List(new Label("newScope")) ++ sList, sReg)
      }
    } 
  }

  def translateArrayLiteral(elems: List[Expr]): (List[TAC], TRegister) = {
    val instrs = ListBuffer[TAC]()
    val regs = ListBuffer[TRegister]()
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
        (eList ++ List(CommandTAC(cmd, eReg, findType(expr).getOrElse(BaseType(BaseT.Any_T)))), null)
      }
    }
  }

  def translateCall(ident: IdentLiteral, args: List[Expr]): (List[TAC], TRegister) = {
    var argOutList = List[TRegister]()
    var argTacList = List[TAC]()
    args.foreach(a => {
      delegateASTNode(a) match {
        case (tacList, outReg) => {
          argTacList = argTacList ++ tacList
          argOutList = argOutList ++ List(outReg)
        }
      }
    })
    (argTacList ++ List(CallTAC(new Label(ident.name), argOutList)), new TRegister(999))
  }


  def translateFunction(func: Func) : List[TAC] = {
    newMap()
    func match {
        case Func(returnType, ident, types, code) => {
          delegateASTNode(code) match {
            case (tacList, outReg) => {
              // Remove the map from scope
              popMap()
              // Clear the register list 
              regList.clear()
              List(new Label(ident.name), BeginFuncTAC()) ++ tacList ++ List(EndFuncTAC())
            }
          }
        }
      }
  }
}
