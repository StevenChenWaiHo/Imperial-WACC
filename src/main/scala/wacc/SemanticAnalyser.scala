package wacc

import wacc.AbstractSyntaxTree.BaseT._
import wacc.AbstractSyntaxTree.CmdT._
import wacc.AbstractSyntaxTree._
import wacc.TypeValidator.{isHomogenousList, returnType}

object SemanticAnalyser {

  import TypeValidator.makeBaseType

  private def pairElementType(element: PairElemT.Elem) = simpleExpectation {
    (input) =>
      input match {
        //case PairType(t1, t2) => Right(if (element == Fst) t1 else t2)
        case _ => Left(List("Mismatched type: expected a pair but received: \n".format(input)))
      }
  }

  private def arrayNestedType(array: DeclarationType, indices: Int): Either[List[String], DeclarationType] = array match {
    case ArrayType(innerType) if indices != 0 => arrayNestedType(innerType, indices - 1)
    case someType if indices == 0 => Right(someType)
    case someType if indices != 0 => Left(List("Attempted to dereference non-array type: %s\n".format(someType)))
  }

  def rValType(rVal: RVal)(implicit scopeContext: ScopeContext): Either[List[String], DeclarationType] = rVal match {
    case rVal: Expr => returnType(rVal)
    case ArrayLiteral(exprs) => isHomogenousList(exprs)
    case PairValue(exp1, exp2) => simpleExpectation { (inputs) => Right(PairType(inputs(0), inputs(1))) }
      .matchedWith(List(returnType(exp1), returnType(exp2)))
    case Call(ident, args) => {
      val funcExpectation = scopeContext.findFunc(ident.name)
      funcExpectation.map(_ matchedWith args.map(returnType))
        .getOrElse(Left(List("Function '%s' not defined in scope.")))
    }
    case PairElement(element, pair) => pairElementType(element) matchedWith List(lValType(pair))
  }

  def lValType(lVal: LVal)(implicit scopeContext: ScopeContext): Either[List[String], DeclarationType] = lVal match {
    case PairElement(element, pair) => pairElementType(element) matchedWith List(lValType(pair))
    case IdentLiteral(name) => scopeContext.findVar(name)
    case ArrayElem(name, indices) => {
      if (indices.map(returnType(_)).find((x) => x match {
        case Right(Int_T) => false
        case _ => true
      }).isDefined) Left(List("Non-integer indices in array access."))
      else simpleExpectation(
        (input) => arrayNestedType(input(0), indices.length)) matchedWith List(scopeContext.findVar(name)
      )
    }
  }

  def verifyStatement(statement: Stat)(implicit scopeContext: ScopeContext): Either[List[String], ScopeContext] =
    statement match {
      // Make a new context from 'stat', and feed it to verifyStatement to verify 'stats'
      case StatList(stat :: stats) => verifyStatement(stat).map(verifyStatement(StatList(stats))(_)).joinRight
      case StatList(Nil) => Right(scopeContext)
      case SkipStat() => Right(scopeContext)
      case Declaration(dataType, ident, rValue) => {
        // Make sure rValue and dataType are a pair of (any) matching data types
        (TypeMatcher.identicalTypes(BaseType(Any_T)) matchedWith List(Right(dataType), rValType(rValue)))
          // Drop the return type and replace it with the new context
          .map(_ => scopeContext.addVar(ident.name, dataType)).joinRight
      }
      // Accept any two identical types. Drop the return type and replace it with the (unchanged) context.
      case Assignment(lVal, rVal) => {
        (TypeMatcher.identicalTypes(BaseType(Any_T)) matchedWith List(lValType(lVal), rValType(rVal)))
          .map(_ => scopeContext)
      }
      case Read(lVal) => (simpleExpectation((input) => input.head match {
        case BaseType(Bool_T) => Left(List("Attempted to read into a boolean"))
        case _ => Right(BaseType(None_T))
      }) matchedWith List(lValType(lVal)))
        .map(_ => scopeContext)

      case Command(cmd, input) => {
        val expectation = cmd match {
          case Free => TypeMatcher.oneOf(List(PairType(Any_T, Any_T), ArrayType(Any_T)))
          case Ret => scopeContext.expectedReturn
          case Exit => TypeMatcher.oneOf(List(Int_T))
          case Print => (TypeMatcher.oneOf(List(Any_T)))
          case PrintLn => (TypeMatcher.oneOf(List(Any_T)))
        }
        (expectation matchedWith List(returnType(input)))
          .map(_ => scopeContext)
      }

      case IfStat(cond, stat1, stat2) => {
        val condReturn = TypeMatcher.oneOf(List(String_T)) matchedWith List(returnType(cond))
        val stat1Return = verifyStatement(stat1)(scopeContext.newScope(TypeMatcher.oneOf(List(None_T))))
        val stat2Return = verifyStatement(stat2)(scopeContext.newScope(TypeMatcher.oneOf(List(None_T))))

        val returns = List(condReturn.map(_ => scopeContext), stat1Return, stat2Return)
        returns.find(_.isLeft).getOrElse(Right(scopeContext))
      }

      case WhileLoop(cond, stat) => {
        val condReturn = TypeMatcher.oneOf(List(String_T)) matchedWith List(returnType(cond))
        val statReturn = verifyStatement(stat)(scopeContext.newScope(TypeMatcher.oneOf(List(None_T))))

        val returns = List(condReturn.map(_ => scopeContext), statReturn)
        returns.find(_.isLeft).getOrElse(Right(scopeContext))
      }

      case BeginEndStat(stat) => verifyStatement(stat)(scopeContext.newScope(TypeMatcher.oneOf(List(None_T))))
    }

  def verifyFunction(f: Func)(implicit scopeContext: ScopeContext): Either[List[String], ScopeContext] = {
    val updatedScope = scopeContext.addFunc(
      f.ident.name,
      TypeProcessor.simple(f.types.map(_._1) -> f.returnType))
    val functionScope = f.types.foldLeft(updatedScope) { (scope, arg) =>
      scope match {
        case Left(_) => scope
        case Right(x) => x.addVar(arg._2.name, arg._1)
      }
    }
    if (updatedScope.isLeft) updatedScope
    if (functionScope.isLeft) functionScope

    val success = verifyStatement(f.code)
    if (success.isLeft) success
    else updatedScope
  }

  def verifyProgram(program: Program): Either[List[String], ScopeContext] = {
    val emptyScope: Either[List[String], ScopeContext] = Right(new ScopeContext())

    val initialScope = program.funcs.foldLeft(emptyScope) {
      (scope, func) =>
        scope match {
          case Right(x) => verifyFunction(func)(x)
          case Left(x) => Left(x)
        }
    }

    if (initialScope.isLeft) initialScope
    val mainScope = (initialScope.toOption.get).newScope(TypeMatcher.oneOf(List(None_T)))

    verifyStatement(program.stats)(mainScope)
  }
}