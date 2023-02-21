package wacc

import wacc.AbstractSyntaxTree.{ASTNode, Stat, SkipStat, BeginEndStat, Command, Program, Func}

object Translator {
  //TODO: Translate each ASTNode into ARM
  
  def delegateASTNode(node: ASTNode, context : ScopeContext) : List[String] = {
    node match {
      case Program(funcs, stat) => translateProgram(funcs, stat, context)
      case BeginEndStat(stat) => translateBeginEnd(stat)
      case SkipStat() => translateSkip()
      case Command(cmd, expr) => translateCommand(cmd, expr)
      case Func(returnType, ident, types, code) => translateFunction(returnType, ident, types, code)
      case _ => List("")
    }
  }

  def translateProgram(l: List[Func], s: Stat, context: ScopeContext): List[String] = {
    var str = List("")
    for (function: Func <- l) {
      str = str ++ delegateASTNode(function, context) // Not actually sure about the structure of this thing
    }
    str = str ++ delegateASTNode(s, context)
    return str
  }

  def translateBeginEnd(stat : Stat, context: ScopeContext) : List[String] = {
    //Seems like it takes as many variables as it can find in every scope and pushes the corresponding
    //number of registers, instead of just this scope.
    var str: List[String] = List("")
    var defaultRegistersList: List[String]
    if (context.scopeLevel == 0) {
      defaultRegistersList = List("r8", "r10", "r12")
    } else {
      defaultRegistersList = List("r0")
    }
    val registersList: List[String] = List("r6", "r4", "r7", "r5", "r1", "r2")
    if (context.scopeVarSize() >= 4) {
      defaultRegistersList = defaultRegistersList ++ registersList
    } else {
      defaultRegistersList = defaultRegistersList ++ registersList.slice(0, context.scopeVarSize())
    }
    str = str ++ translatePush(List("fp", "lr")) //Maybe not meant to be in BeginEnd
    str = str ++ translatePush(defaultRegistersList) //dependent on context
    str = str ++ delegateASTNode(stat, context)
    str = str ++ translatePop(List(defaultRegistersList)) // dependent on context
    str = str ++ translatePop(List("fp", "pc")) //Maybe meant to be in prog
    return str
  }

  def translateSkip() : List[String] = {
    var str = List("")
    return str
  }

  def translateCommand(cmd : AbstractSyntaxTree.CmdT.Cmd, expr : AbstractSyntaxTree.Expr) : List[String] = {
    List("")
  }

  def translateFunction(returnType : AbstractSyntaxTree.DeclarationType, 
                          ident : AbstractSyntaxTree.IdentLiteral, 
                          types : List[(AbstractSyntaxTree.DeclarationType, 
                            AbstractSyntaxTree.IdentLiteral)], 
                          code : Stat) : List[String] = {
    List("")
  }
  def translateARM(command: String, operand: String, operand2: String = "") : String = {
    //Maybe add check to make sure command is valid
    if (operand2 == "") {
      command ++ " " ++ operand
    }
    command ++ " " ++ operand ++ ", " ++ operand2
  }

  def pushPopAssist(registers: List[String]): String = {
    var str = "{"
    for (register <- registers) {
      if (register != registers.last) {
        str = str ++ register ++ ", "
      } else {
        str = str ++ register
      }
    }
    str = str ++ "}"
    return str
  }

  def translatePush(registers: List[String]): String = {
    return "push " ++ pushPopAssist(registers)
  }

  def translatePop(registers: List[String]): String = {
     return "pop" ++ pushPopAssist(registers)
  }
}
