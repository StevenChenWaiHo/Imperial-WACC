package wacc

import wacc.AbstractSyntaxTree.{ASTNode, Stat, SkipStat, BeginEndStat, Command, Program, Func}

object Translator {
  //TODO: Translate each ASTNode into ARM
  def delegateASTNode(node: ASTNode, context : ScopeContext) : List[String] = {
    node match {
      case Program(funcs, stat) => translateProgram(funcs, stat)
      case BeginEndStat(stat) => translateBeginEnd(stat)
      case SkipStat() => translateSkip()
      case Command(cmd, expr) => translateCommand(cmd, expr)
      case Func(returnType, ident, types, code) => translateFunction(returnType, ident, types, code)
      case _ => List("")
    }
  }

  def translateProgram(funcs : List[ASTNode], stat : Stat) : List[String] = {
    List("")
  }

  def translateBeginEnd(stat : Stat) : List[String] = {
    List("")
  }

  def translateSkip() : List[String] = {
    List("")
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
}
