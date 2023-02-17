package wacc

import wacc.AbstractSyntaxTree.BaseT._
import wacc.AbstractSyntaxTree.CmdT._
import wacc.AbstractSyntaxTree.PairElemT._
import wacc.AbstractSyntaxTree._
import wacc.TypeValidator.{isHomogenousList, returnType}

import scala.annotation.tailrec

object SemanticAnalyser {

  import TypeValidator.makeBaseType

  private def pairElementType(element: PairElemT.Elem): Expectation = simpleExpectation {
    (input) =>
      input.head match {
        case PairType(t1, t2) => Right(if (element == Fst) t1 else t2)
        case NestedPair() => Right(Any_T)
        case _ => Left(List("Mismatched type: expected a pair but received: %s"
          .format(if(input.length == 1) input.head else input)))
      }
  }

  @tailrec
  private def arrayNestedType(array: DeclarationType, indices: Int): Either[List[String], DeclarationType] = {
    array match {
      case ArrayType(innerType, _) if indices > 0 => arrayNestedType(innerType, indices - 1)
      case someType if indices == 0 => Right(someType)
      case someType if indices > 0 => Left(List("Attempted to dereference non-array type: %s".format(someType)))
    }
  }

  def rValType(rVal: RVal)(implicit scopeContext: ScopeContext): Either[List[String], DeclarationType] = rVal match {
    case rVal: Expr => returnType(rVal)
    case ArrayLiteral(exprs) => isHomogenousList(exprs).map(ArrayType(_, exprs.length))
    case PairValue(exp1, exp2) => simpleExpectation { (inputs) => Right(PairType(inputs.head, inputs(1))) }
      .matchedWith(List(returnType(exp1), returnType(exp2)))
    case Call(ident, args) => {
      val funcExpectation = scopeContext.findFunc(ident.name)
      funcExpectation.map(_ matchedWith args.map(returnType))
        .getOrElse(Left(List(s"Function '${ident.name}' not defined in scope.")))
    }
    case PairElement(element, pair) => pairElementType(element) matchedWith List(lValType(pair))
  }

  def lValType(lVal: LVal)(implicit scopeContext: ScopeContext): Either[List[String], DeclarationType] = lVal match {
    case PairElement(element, pair) => pairElementType(element) matchedWith List(lValType(pair))
    case IdentLiteral(name) => scopeContext.findVar(name)
    case ArrayElem(name, indices) => {
      val types = indices.map(returnType(_))
      if (types.exists((x) => x match {
        case Right(x) if x is Int_T => false
        case _ => true
      })) Left(List("Non-integer indices in array access."))
      else simpleExpectation(
        (input) => arrayNestedType(input(0), types.length)) matchedWith List(scopeContext.findVar(name)
      )
    }
  }

  def verifyStatement(statement: Stat)(implicit scopeContext: ScopeContext): Either[List[String], ScopeContext] = {
    statement match {
      // Make a new context from 'stat', and feed it to verifyStatement to verify 'stats'
      case StatList(stat :: stats) => verifyStatement(stat).map(verifyStatement(StatList(stats))(_)).joinRight
      case StatList(Nil) => Right(scopeContext)
      case SkipStat() => Right(scopeContext)
      case Declaration(dataType, ident, rValue) => {
        // Make sure rValue and dataType are a pair of (any) matching data types
        val matcher = TypeMatcher.identicalTypes(BaseType(Any_T)) withContext s"In variable declaration for '$ident'"

        (matcher matchedWith List(Right(dataType), rValType(rValue)))
          // Drop the return type and replace it with the new context
          .map(_ => scopeContext.addVar(ident.name, dataType)).joinRight
      }
      // Accept any two identical types. Drop the return type and replace it with the (unchanged) context.
      case Assignment(lVal, rVal) => {
        val lValT = lValType(lVal)
        val rValT = rValType(rVal)
        if(lValT.isRight && rValT.isRight && lValT.toOption.get.isAny && rValT.toOption.get.isAny)
          return Left(List(s"Assignment between two ambiguous types: ${lValT.toOption.get}, ${rValT.toOption.get}"))

        val matcher = TypeMatcher.identicalTypes withContext s"In variable assignment on line: ${0}" //TODO
        (matcher matchedWith List(lValType(lVal), rValType(rVal)))
          .map(_ => scopeContext)
      }
      case Read(lVal) =>
        (simpleExpectation((input) => input.head match {
          case BaseType(x) if x is Bool_T => Left(List("Attempted to read into an invalid type: %s".format(x)))
          case PairType(a, b) if (a is Any_T) && (b is Any_T) => Left(List("Attempted to read into a pair"))
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
        (expectation.withContext(s"Inside command: $cmd") matchedWith List(returnType(input)))
          .map(_ => scopeContext)
      }

      case IfStat(cond, stat1, stat2) => {
        val condReturn = TypeMatcher.oneOf(List(Bool_T)) matchedWith List(returnType(cond))
        val stat1Return = verifyStatement(stat1)(scopeContext.newScope())
        val stat2Return = verifyStatement(stat2)(scopeContext.newScope())

        val returns = List(condReturn.map(_ => scopeContext), stat1Return, stat2Return)
        returns.find(_.isLeft).getOrElse(Right(scopeContext))
      }

      case WhileLoop(cond, stat) => {
        val condReturn = TypeMatcher.oneOf(List(Bool_T)) matchedWith List(returnType(cond))
        val statReturn = verifyStatement(stat)(scopeContext.newScope())

        val returns = List(condReturn.map(_ => scopeContext), statReturn)
        returns.find(_.isLeft).getOrElse(Right(scopeContext))
      }

      case BeginEndStat(stat) => verifyStatement(stat)(scopeContext.newScope()).map(_ => scopeContext)
    }
  }

  def verifyFunction(f: Func)(implicit scopeContext: ScopeContext): Option[List[String]] = {
    val initial: Either[List[String], ScopeContext] = Right(scopeContext.newScope(None_T))
    val functionScope = f.types.foldLeft(initial){ (scope, arg) =>
      scope match {
        case Right(x) => x.addVar(arg._2.name, arg._1)
        case Left(_) => scope
      }
    }
    if (functionScope.isLeft) return functionScope.left.toOption

    val success = verifyStatement(f.code)(functionScope.toOption.get.newScope(f.returnType))
    success.left.toOption
  }

  def declareFunction(scopeContext: ScopeContext, f: Func) = scopeContext.addFunc(
      f.ident.name,
      TypeProcessor.simple(f.types.map(_._1) -> f.returnType))

  def verifyProgram(program: Program): Either[List[String], ScopeContext] = {
    val emptyScope: Either[List[String], ScopeContext] = Right(new ScopeContext())

    val nominalScope = program.funcs.foldLeft(emptyScope)((s, f) => s match {
      case Left(_) => s
      case Right(sc) => declareFunction(sc, f)
    })
    if(nominalScope.isLeft) return nominalScope

    val funcErrors = program.funcs.map(verifyFunction(_)(nominalScope.toOption.get))
    val funcError = funcErrors.find(_.isDefined).flatten
    if(funcError.isDefined) return Left(funcError.get)

    val mainScope = (nominalScope.toOption.get).newScope(None_T)
    verifyStatement(program.stats)(mainScope)
  }
}