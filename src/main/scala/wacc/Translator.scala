package wacc

import wacc.AbstractSyntaxTree.{ASTNode, Stat, SkipStat, BeginEndStat, Command, Program, Func}

object Translator {
  //TODO: Translate each ASTNode into ARM
  
  def delegateASTNode(node: ASTNode, context : ScopeContext) : List[TAC] = {
    node match {
      case Program(funcs, stat) => translateProgram(funcs, stat, context)
      case BeginEndStat(stat) => translateBeginEnd(stat)
      case SkipStat() => translateSkip()
      case Command(cmd, expr) => translateCommand(cmd, expr)
      case Func(returnType, ident, types, code) => translateFunction(returnType, ident, types, code)
      case _ => List()
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
