package wacc

import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.UnaryOpType.UnOp
import wacc.AbstractSyntaxTree._
import wacc.TAC._


object Translator {

  private val scopes = collection.mutable.ListBuffer[collection.mutable.Map[ASTNode, TRegister]]()
  private val regCountStack = collection.mutable.Stack[Int]()
  private val regList = collection.mutable.ListBuffer[TRegister]()
  private val strings = collection.mutable.Map[String, Label]()
  private val dataList = collection.mutable.ListBuffer[TAC]()
  
  def newMap(): collection.mutable.Map[ASTNode, TRegister] = {
    // Push scope on to stack when entering new context
    val map = collection.mutable.Map[ASTNode, TRegister]()
    scopes.addOne(map)
    // Push the current highest register for use later
    regCountStack.push(regList.length)
    map
  }

  def popMap(): Int = {
    // Pop scope off the stack when exiting a context
    scopes.remove(scopes.length - 1)
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

  def addNode(node: ASTNode, reg: TRegister) = {
    scopes.last.addOne(node, reg)
  }

  def nextRegister(): TRegister = {
    val next = new TRegister(regList.length)
    regList += next
    next
  }

  def delegateASTNode(node: ASTNode): (List[TAC], TRegister) = {
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
          // TODO: check this if can be included in translate literal
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
          val lbl = new Label(".L.str" + strings.size.toString)
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
    val (tacList, outReg) = delegateASTNode(lval)
    (tacList ++ List(AssignmentTAC(new TRegister(998), outReg)), outReg)
  }

  def translateArrayElem(name: IdentLiteral, indices: List[Expr]): (List[TAC], TRegister) = {
    // Hopefully find the identifier in the map already
    val (_, aReg) = delegateASTNode(name)
    val is = collection.mutable.ListBuffer[TAC]()
    val rs = collection.mutable.ListBuffer[TRegister]()
    indices.foreach(i => delegateASTNode(i) match {
      case (iList, iReg) => {
        is.addAll(iList)
        rs.addOne(iReg)
      }
    })
    val next = nextRegister()
    (is.toList ++ List(AssignmentTAC(new ArrayElemTAC(aReg, rs.toList), next)), next)


  }

  def translateWhileLoop(expr: Expr, stat: Stat): (List[TAC], TRegister) = {
    val (expList, expReg) = delegateASTNode(expr)
    newMap()
    val (statList, statReg) = delegateASTNode(stat)
    popMap()
    val startLabel = new Label("start")
    val bodyLabel = new Label("body")
    val endLabel = new Label("end")
    (List(startLabel) ++ expList
      ++ List(IfTAC(expReg, bodyLabel), GOTO(endLabel), bodyLabel)
      ++ statList ++ List(GOTO(startLabel), endLabel), statReg)
  }


  def translateBinOp(op: BinOp, exp1: Expr, exp2: Expr) = {
    val (tacList1, reg1) = delegateASTNode(exp1)
    val (tacList2, reg2) = delegateASTNode(exp2)
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
    val l1 = new Label("true")
    val l2 = new Label("false")
    (condList ++ List(IfTAC(reg1, l1)) ++ falseList ++ List(GOTO(l2), l1) ++ trueList ++ List(l2),
      null)
  }

  def translateDeclaration(dataType: DeclarationType, ident: IdentLiteral, rvalue: RVal): (List[TAC], TRegister) = {
    dataType match {
      case BaseType(baseType) => translateBaseDeclaration(baseType, ident, rvalue)
      case PairType(fstType, sndType) => translatePairDeclaration(fstType, sndType, ident, rvalue)
      case ArrayType(dataType, length) => translateArrayDeclaration(dataType, length, ident, rvalue)
    }
  }

  def translateArrayDeclaration(dataType: DeclarationType, length: Integer, ident: IdentLiteral, rvalue: RVal): (List[TAC], TRegister) = {
    (List(), null)
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
          CreatePair(fstReg, sndReg, pairReg), Comments("Pair Declaration Ends")), pairReg)
      }
      case _ => (List(new Label("Pair Type not Matched")), null)
    }
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
    (List(), null)
  }

  def translateArrayElemAssignment(lvalue: LVal, rvalue: RVal): (List[TAC], TRegister) = {
    (List(), null)
  }

  def translateIdentAssignment(lvalue: LVal, rvalue: RVal): (List[TAC], TRegister) = {
    val (lList, lReg) = delegateASTNode(lvalue)
    val (rList, rReg) = delegateASTNode(rvalue)
    (lList ++ rList ++ List(new AssignmentTAC(rReg, lReg)), lReg) // TODO: Check this
  }

  def translateProgram(funcs: List[Func], s: Stat): List[TAC] = {
    // Initialise the .data segment
    dataList.addOne(DataSegmentTAC())
    // Start the code segment with the functions first
    val funcTAClist = collection.mutable.ListBuffer[TAC](TextSegmentTAC())
    funcs.foreach(f => {
      funcTAClist.addAll(translateFunction(f))
    })
    newMap()
    var (tacList, reg) = delegateASTNode(s)
    dataList.toList ++ funcTAClist.toList ++ List(Label("main"), BeginFuncTAC()) ++ tacList ++ List(EndFuncTAC())
  }

  def translateStatList(stats: List[Stat]): (List[TAC], TRegister) = {
    // TODO: Change to not use mutable list?
    val TAClist = collection.mutable.ListBuffer[TAC]()
    stats.foreach(s => {
      TAClist.addAll(delegateASTNode(s)._1)
    })
    (TAClist.toList, null)
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
    val instrs = collection.mutable.ListBuffer[TAC]()
    val regs = collection.mutable.ListBuffer[TRegister]()
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
        (eList ++ List(CommandTAC(cmd, eReg)), null)
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


  def translateFunction(func: Func): List[TAC] = {
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
