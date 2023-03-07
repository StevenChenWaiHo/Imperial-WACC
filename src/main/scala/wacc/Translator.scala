package wacc

import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.AbstractSyntaxTree._
import wacc.TAC._

import scala.collection.mutable._


object Translator {

  private val scopes = ListBuffer[Map[ASTNode, TRegister]]()
  private val typeMaps = ListBuffer[Map[String, DeclarationType]]()
  private val regCountStack = Stack[Int]()
  private val regList = ListBuffer[TRegister]()
  private val strings = Map[String, Label]()
  private val dataList = ListBuffer[TAC]()
  private var labelCount = 0

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
      case PairLiteral() => Some(PairType(NestedPair(), NestedPair()))
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

  def generateLabel(): Label = {
    labelCount += 1
    Label(".L" + labelCount.toString())
  }

  def delegateASTNode(node: ASTNode): (List[TAC], TRegister) = {
    // Check if ASTNode has already been calculated
    println(node)
    println(findNode(node))
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
          case PairElement(elem, lvalue) => translatePairElem(elem, lvalue)
          case ArrayLiteral(elements) => translateArrayLiteral(elements)
          case ArrayElem(ident, indices) => translateArrayElem(ident.name, indices)
          case WhileLoop(expr, stat) => translateWhileLoop(expr, stat)
          case Call(ident, args) => translateCall(ident, args)
          case Read(lval) => translateRead(lval)
          case na => (List(new Label("Not Implemented " + na)), null)
        }
        // Only add literal assignments/declarations to the scope
        node match {
          case node: IdentLiteral => addNode(node, tac._2)
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
          val lbl = new Label(".L.str" + strings.size.toString)
          strings.addOne((str, lbl))
          dataList.addOne(Comments("length of " + lbl.toString()))
          dataList.addOne(StringLengthDefinitionTAC(str.length(), lbl))
          dataList.addOne(StringDefinitionTAC(str, lbl))
          lbl
        })
      }
      case IdentLiteral(x) => return (List(), next)
      case PairLiteral() => new PairLiteralTAC()
      //case ArrayLiteral(x) => translateArrayLiteral(x)
    }
    (List(AssignmentTAC(lhs, next)), next)
  }

  def translateRead(lval: LVal): (List[TAC], TRegister) = {
    val (tacList, outReg) = delegateASTNode(lval)
    val dataType = findType(lval).getOrElse(BaseType(BaseT.Any_T))
    (tacList ++ List(ReadTAC(dataType, outReg)), outReg)
  }

  def translatePairElem(elem: PairElemT.Elem, lvalue: LVal): (List[TAC], TRegister) = {
    val (pairRegList, pairReg) = delegateASTNode(lvalue)
    val elemType = lvalue match {
      case ArrayElem(name, indices) => findType(name).get // TODO null
      case _ => {
        val (fstType, sndType) = findType(lvalue) match {
          case Some(PairType(fstType, sndType)) => (fstType, sndType)
          case None => (BaseType(BaseT.Any_T), BaseType(BaseT.Any_T))
        }
        if (elem == PairElemT.Fst) fstType else sndType
      }
    }
    // Should not add this register to Map as it might update
    val dstReg = nextRegister()
    (List(Comments("Getting Pair Elem")) ++ pairRegList ++ List(GetPairElem(elemType, pairReg, elem, dstReg)) ++ List(Comments("Got Pair Elem")), dstReg)
  }

  // def translateArrayElem(name: String, indices: List[Expr]): (List[TAC], TRegister) = {
  //   val (arrRegList, arrReg) = delegateASTNode(IdentLiteral(name))
  //   val dstReg = nextRegister()
  //   (arrRegList ++ List(GetArrayElem(null, arrReg, indices, dstReg)), dstReg)
  // }

  def translateArrayElem(name: String, indices: List[Expr]): (List[TAC], TRegister) = {
    delegateASTNode(IdentLiteral(name)) match {
      // Hopefully find the identifier in the map already
      case (_, arrReg) => {
        val is = ListBuffer[TAC]()
        val rs = ListBuffer[TRegister]()
        indices.foreach(i => delegateASTNode(i) match {
          case (iList, iReg) => {
            is.addAll(iList)
            rs.addOne(iReg)
          }
        })
        val dstReg = nextRegister()
        (List(Comments("Load Array Elem")) ++ is.toList ++ List(LoadArrayElem(findType(indices.head).get, arrReg, rs.toList, dstReg)) ++ List(Comments("Finish loading Array Elem")), dstReg)
      }
      case _ => (List(Label("Not translating ArrayElem")), null)
    }
  }

  def translateWhileLoop(expr: Expr, stat: Stat): (List[TAC], TRegister) = {
    val (expList, expReg) = delegateASTNode(expr)
    newMap()
    val (statList, statReg) = delegateASTNode(stat)
    popMap()
    val startLabel = generateLabel()
    val bodyLabel = generateLabel()
    val endLabel = generateLabel()
    (List(startLabel) ++ expList
      ++ List(IfTAC(expReg, bodyLabel), GOTO(endLabel), bodyLabel)
      ++ statList ++ List(GOTO(startLabel), endLabel), statReg)
  }


  def translateBinOp(op: BinOp, exp1: Expr, exp2: Expr) = {
    val (tacList1, reg1) = delegateASTNode(exp1)
    val (tacList2, reg2) =  delegateASTNode(exp2)
    val nextReg = nextRegister()
    (tacList1 ++ tacList2 ++ List(BinaryOpTAC(op, reg1, reg2, nextReg)),
      nextReg)
  }

  def translateUnOp(op: UnOp, exp: Expr): (List[TAC], TRegister) = {
    val (tacList, reg) = delegateASTNode(exp)
    val nextReg = nextRegister()
    (tacList ++ List(UnaryOpTAC(op, reg, nextReg)),
      nextReg)
  }

  def translateIfStat(cond: Expr, stat1: Stat, stat2: Stat): (List[TAC], TRegister) = {
    val (condList, reg1) = delegateASTNode(cond)
    newMap()
    val (falseList, reg3) = delegateASTNode(stat2)
    popMap()
    newMap()
    val (trueList, reg2) = delegateASTNode(stat1)
    popMap()
    val l1 = generateLabel()
    val l2 = generateLabel()
    (condList ++ List(IfTAC(reg1, l1)) ++ falseList ++ List(GOTO(l2), l1) ++ trueList ++ List(l2),
      null)
  }

  def translateDeclaration(dataType: DeclarationType, ident: IdentLiteral, rvalue: RVal): (List[TAC], TRegister) = {
    addType(ident, dataType)
    dataType match {
      case BaseType(baseType) => translateBaseDeclaration(baseType, ident, rvalue)
      case PairType(fstType, sndType) => translatePairDeclaration(fstType, sndType, ident, rvalue)
      case ArrayType(dataType, length) => translateArrayDeclaration(dataType, length, ident, rvalue)
    }
  }

  def translateArrayDeclaration(dataType: DeclarationType, length: Int, ident: IdentLiteral, rvalue: RVal): (List[TAC], TRegister) = {
    rvalue match {
      case ArrayLiteral(elements) => {
        val tacs = ListBuffer[TAC]()
        val tRegs = ListBuffer[TRegister]() //required?
        val lenReg = nextRegister()
        val dstReg = nextRegister()
        addNode(ident, dstReg)
        val elemType = dataType match {
          case ArrayType(dType, length) => dType
          case _ => dataType
        }
        for (i <- 0 to elements.length - 1) {
          val (elemTacs, reg) = delegateASTNode(elements(i))
          addNode(elements(i), reg)
          tacs ++= elemTacs
          tRegs += reg
          tacs ++= List(Comments("ArrayElem Declaration Start"), CreateArrayElem(elemType, i, dstReg, reg), Comments("ArrayElem Declaration End"))
        }
        (List(Comments("Array Declaration Start"), InitialiseArray(elements.length, lenReg, dstReg)) ++ tacs.toList ++ List(CreateArray(elemType, tRegs.toList, dstReg),
         Comments("Array Declaration End")), dstReg)
      }
      case _ => (List(new Label("Array Type not Matched")), null)
    }
  }

  def translatePairValue(fstType: DeclarationType, sndType: DeclarationType, pairValue: RVal): (List[TAC], TRegister) = {
    pairValue match {
      case PairValue(exp1, exp2) => {
        findNode(pairValue) match {
          case Some(value) => (List(), value)
          case None => {
            val (exp1List, fstReg) = delegateASTNode(exp1)
            val (exp2List, sndReg) = delegateASTNode(exp2)
            val pairReg = nextRegister()
            val srcReg = nextRegister()
            val ptrReg = nextRegister()

             // TODO: Remove?
            val fstReg2 = nextRegister()
            val sndReg2 = nextRegister()
             // TODO: Remove?
             
            // addNode(pairValue, pairReg)
            (List(Comments("Creating newpair")) ++
              exp1List ++ List(CreatePairElem(fstType, PairElemT.Fst, ptrReg, fstReg)) ++
              exp2List ++ List(CreatePairElem(sndType, PairElemT.Snd, ptrReg, sndReg),
              CreatePair(fstType, sndType, fstReg2, sndReg2, srcReg, ptrReg, pairReg), Comments("Created newpair")), pairReg)
          }
        }

      }
      case PairLiteral() => {
        val reg = nextRegister()
        (List(AssignmentTAC(PairLiteralTAC(), reg)), reg)
      }
      
      case PairElement(elem, lvalue) => translatePairElem(elem, lvalue)
      // PairLiteral or IdentLiteral
      case lit@(_: Literal | _: Call) => {
        val reg = nextRegister()
        val (srcRegList, srcReg) = delegateASTNode(lit)
        (srcRegList ++ List(AssignmentTAC(srcReg, reg)), reg)
      }

      case t => throw new ArithmeticException("Not translating Pair Value"); (List(Label("Not translating Pair Value")), null)
    }
  }

  def translatePairDeclaration(fstType: DeclarationType, sndType: DeclarationType, ident: IdentLiteral, pairValue: RVal): (List[TAC], TRegister) = {
    val (pairList, pairReg) = translatePairValue(fstType, sndType, pairValue)
    addNode(ident, pairReg)
    (pairList, pairReg)
  }

  def translateBaseDeclaration(baseType: BaseT.BaseTypeType, ident: IdentLiteral, rvalue: RVal): (List[TAC], TRegister) = {
    val (rList, rReg) = delegateASTNode(rvalue)
    addNode(ident, rReg)
    (rList, rReg)
  }

  def translateAssignment(lvalue: LVal, rvalue: RVal): (List[TAC], TRegister) = {
    lvalue match {
      case _: IdentLit => translateIdentAssignment(lvalue, rvalue)
      case _: PairElem => translatePairElemAssignment(lvalue, rvalue)
      case _: ArrayE => translateArrayElemAssignment(lvalue, rvalue)
    }
  }

  def translatePairElemAssignment(lvalue: LVal, rvalue: RVal): (List[TAC], TRegister) = {
    val (rvalueList, rvalueReg) = delegateASTNode(rvalue)
    lvalue match {
      case PairElement(elem, lvalue) => {
        val (lvalueList, lvalueReg) = delegateASTNode(lvalue)
        val elemType = lvalue match {
          case ArrayElem(name, indices) => findType(name).get // TODO null
          case _ => {
            val (fstType, sndType) = findType(lvalue) match {
              case Some(PairType(fstType, sndType)) => (fstType, sndType)
              case None => (BaseType(BaseT.Any_T), BaseType(BaseT.Any_T))
            }
            if (elem == PairElemT.Fst) fstType else sndType
          }
        }
        (rvalueList ++ lvalueList ++ 
        List(Comments("Store Pair Elem")) ++ List(StorePairElem(elemType, lvalueReg, elem, rvalueReg)) ++ List(Comments("Finish Storing Pair Elem")), lvalueReg)
      }
      case _ => (List(Label("Not translating PairElem")), null)
    }
  }

  def translateArrayElemAssignment(lvalue: LVal, rvalue: RVal): (List[TAC], TRegister) = {
    val (rvalueList, rvalueReg) = delegateASTNode(rvalue)
    lvalue match {
      case ArrayElem(name, indices) => {
        var indexNodes = ListBuffer[TRegister]()
        indices.foreach(i => {
          indexNodes += delegateASTNode(i)._2
        })
        val lvalueReg = findNode(name).get
        (rvalueList ++ 
        List(Comments("Store Array Elem")) ++ List(StoreArrayElem(null, lvalueReg, indexNodes.toList, rvalueReg)) ++ List(Comments("Finish storing Array Elem")), lvalueReg)
      }
      case _ => (List(Label("Not translating ArrayElem")), null)
    }
  }

  def translateIdentAssignment(lvalue: LVal, rvalue: RVal): (List[TAC], TRegister) = {
    val (lList, lReg) = delegateASTNode(lvalue) // this is the variable: it should have a register assigned to it, but no value. /
    val (rList, rReg) = delegateASTNode(rvalue) // this is the value: it should ALWAYS be assigned to a different node to all variables.
    (lList ++ rList ++ List(new AssignmentTAC(rReg, lReg)), lReg)
  }

  def translateProgram(funcs: List[Func], s: Stat): List[TAC] = {
    newMap()
    // Initialise the main .data segment
    dataList.addOne(DataSegmentTAC())
    // Translate main to TAC
    var (tacList, reg) = delegateASTNode(s)
    // Translate funcs after main to TAC
    val funcTAClist = ListBuffer[TAC]()
    funcs.foreach(f => {
      funcTAClist.addAll(translateFunction(f))
    })
    // Save main .data and .text segment
    dataList.toList ++ 
    List[TAC](TextSegmentTAC()) ++
    List(Label("main"), BeginFuncTAC()) ++ 
    tacList ++ List(EndFuncTAC()) ++ 
    funcTAClist.toList
  }

  def translateStatList(stats: List[Stat]): (List[TAC], TRegister) = {
    var TAClist = List[TAC]()
    stats.foreach(s => {
      TAClist = TAClist ++ delegateASTNode(s)._1
    })
    (TAClist, null)
  }

  def translateBeginEnd(stat: Stat): (List[TAC], TRegister) = {
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
      val (instr, reg) = delegateASTNode(e)
      instrs.addAll(instr)
      regs += reg
    })
    val next = nextRegister()
    (instrs.toList ++ List(AssignmentTAC(new ArrayOp(regs.toList), next)),
      next)
  }

  def translateSkip(): (List[TAC], TRegister) = {
    (List(), null)
  }

  def translateCommand(cmd: AbstractSyntaxTree.CmdT.Cmd, expr: AbstractSyntaxTree.Expr): (List[TAC], TRegister) = {
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
    val returnReg = nextRegister()
    (argTacList ++ List(CallTAC(new Label("wacc_" + ident.name), argOutList, returnReg)), returnReg)
  }

  def translateFunction(func: Func): List[TAC] = {
    newMap()
    var paramList = List[TAC]()
    func match {
      case Func(returnType, ident, types, code) => {
        // PopParam
        types.zipWithIndex.foreach {
          case ((t, paramName), index) => {
            val paramReg = nextRegister()
            addNode(paramName, paramReg)
            addType(paramName, t)
            paramList = paramList ++ List(PopParamTAC(t, paramReg, index))
          }
        }
        delegateASTNode(code) match {
          case (tacList, outReg) => {
            // Remove the map from scope
            popMap()
            // Clear the register list
            regList.clear()
          }
            List(new Label("wacc_" + ident.name), BeginFuncTAC()) ++ paramList.reverse ++ tacList
        }
      }
    }
  }
}
