
package wacc.Optimisations

import wacc.TAC._
import wacc.AbstractSyntaxTree._

import scala.collection.mutable._
import scala.None

object Inlining{
  import wacc.Translator._

  var inlineLabelCounter = 0

  def nextInlineLabel(): Label = {
    inlineLabelCounter += 1
    Label("InlineFunc" + inlineLabelCounter.toString)
  }

  class InlineFunc(val param: Stack[TRegister], val tacList: List[TAC], val labelFlag: Boolean,  val dstReg: TRegister)

  val INLINE_FUNC_MAX_INSTR = 20
  // funcMap : Map(FunctionName => (FunctionInstr, dstReg))
  private val funcMap = Map[Label, InlineFunc]()
  
  def inline_delegateASTNode(node: ASTNode): (List[TAC], TRegister) = {
    node match{
      case Program(funcs, stats) => (inline_translateProgram(funcs, stats), null)
      case _ => throw new IllegalArgumentException("Translating non-program wacc file")
    }
  }

  def addInlineFuncToMap(funcTac: List[TAC]) : Unit = {
    var inLineFuncTAClist = ListBuffer[TAC]()
    val param = Stack[TRegister]()
    var name: Label = null
    var labelFlag = false
    val dstReg  = nextRegister()
    funcTac.zipWithIndex.foreach{ case (tac, i) => tac match {
        case label@Label(lbl) => {
          if (name == null) {
            name = label
          }
          else{
            inLineFuncTAClist.addOne(label)
          }
        }
        case BeginFuncTAC() => null
        case EndFuncTAC() => null
        case PopParamTAC(datatype, tReg, index) => param.push(tReg)
        case cmd@CommandTAC(CmdT.Ret, tReg, opType) => {
          inLineFuncTAClist.addOne(AssignmentTAC(tReg, dstReg))
          // If return not the last line of the function
          if (i != funcTac.length - 2){
            labelFlag = true
            inLineFuncTAClist.addOne(GOTO(name))
          }
        }
        case t => inLineFuncTAClist.addOne(t)
      }
    }
    funcMap.addOne((name, new InlineFunc(param, inLineFuncTAClist.toList, labelFlag, dstReg)))
  }

  def replaceCall(lbl: Label, args: List[TRegister], dstReg: TRegister, inlineFunc: InlineFunc) : List[TAC] = {
    val tacList = ListBuffer[TAC]()
    tacList.addOne(Comments("Add inline function"))
    for (i <- 0 until args.length){
      tacList.addOne(AssignmentTAC(args(i), inlineFunc.param.toList(i)))
    }
    tacList.addAll(inlineFunc.tacList)
    tacList.toList
  }

  def checkInlineFunc(tacList: List[TAC]) : Boolean = {
    tacList.foreach{tac => tac match{
        case _: CallTAC => return false
        case _ => 
      }
    }
    return true
  }

  def insert_inlineFunc(tacList: List[TAC]) : ListBuffer[TAC] = {
    // Insert inline func in each Call
    val inlinedTacList = ListBuffer[TAC]()
    tacList.foreach(tac => tac match {
      case call@CallTAC(lbl, args, dstReg) => {
        funcMap.get(lbl) match {
          case Some(inlineFunc) => {
            inlinedTacList.addAll(replaceCall(lbl, args, dstReg, inlineFunc))
            if (inlineFunc.labelFlag){
              inlinedTacList.addOne(lbl)
            }
            inlinedTacList.addOne(AssignmentTAC(inlineFunc.dstReg, dstReg))
          }
          case None => inlinedTacList.addOne(call)
        }
      }
      case t => inlinedTacList.addOne(t)
    })
    inlinedTacList
  }

  def inline_translateProgram(funcs: List[Func], s: Stat): List[TAC] = {
  newMap()
  // Initialise the main .data segment
  addOneDataList(DataSegmentTAC())

  // Translate main to TAC
  var (tacList, reg) = delegateASTNode(s)

  // Translate funcs after main to TAC
  var funcTAClist = ListBuffer[TAC]()
  funcs.foreach(f => f match {
    case Func(returnType, ident, types, code) => {
      val funcTac = translateFunction(f)
      // TODO: add checks to check if inline function calls another function
      if (funcTac.length <= INLINE_FUNC_MAX_INSTR && checkInlineFunc(funcTac)){
        addInlineFuncToMap(funcTac)
      }
      else{
        funcTAClist.addAll(funcTac)
      }
    }
  })

  val inlinedTacList = insert_inlineFunc(tacList)
  funcTAClist = insert_inlineFunc(funcTAClist.toList)

  // Save main .data and .text segment
  getDataList().toList ++ 
  List[TAC](TextSegmentTAC()) ++
  List(Label("main"), BeginFuncTAC()) ++ 
  inlinedTacList ++ List(EndFuncTAC()) ++ 
  funcTAClist.toList
  }
}
