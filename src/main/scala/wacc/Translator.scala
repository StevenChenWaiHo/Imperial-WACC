package wacc

import AbstractSyntaxTree.{Func, Stat}

object Translator {
  //TODO: Translate each ASTNode into ARM
  def translateProgram(l: List[Func], s: Stat, context: ScopeContext): List[String] = {
    val str = List("")
    for (function: Func <- l) {
      str = str ++ delegateASTNode(function, context)//Not actually sure about the structure of this thing
    }
    str = str ++ delegateASTNode(s, context)
    return str
  }
}
