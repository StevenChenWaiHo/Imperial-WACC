package wacc

import wacc.AbstractSyntaxTree.BaseT._
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.UnaryOpType._
import wacc.AbstractSyntaxTree._
import wacc.SemanticAnalyser.{lValType, rValType}

import javax.management.InvalidAttributeValueException


case class Scope(val vars: Map[String, DeclarationType],
                 val funcs: Map[String, Expectation],
                 val nextReturn: DeclarationType)

class ScopeContext(scopeStack: List[Scope]) {
  if (scopeStack.isEmpty) throw new InvalidAttributeValueException("Context cannot be empty")

  def this() = this(List(new Scope(Map(), Map(), null)))

  def findVar(name: String): Either[List[String], DeclarationType] = {

    def findVar1(stack: List[Scope]): Either[List[String], DeclarationType] = stack match {
      case Scope(vars, _, _) :: scopes => {
        vars.get(name).map(Right(_))
          .getOrElse(findVar1(scopes))
      }
      case Nil => Left(List("Variable not found: %s".format(name)))
    }

    findVar1(this.scopeStack)
  }


  def findFunc(name: String): Either[List[String], Expectation] = {
    def findFunc1(stack: List[Scope]): Either[List[String], Expectation] = stack match {
      case Scope(_, funcs, _) :: scopes => {
        funcs.get(name).map(Right(_))
          .getOrElse(findFunc1(scopes))
      }
      case Nil => Left(List("Function %s not found."))
    }

    findFunc1(this.scopeStack)
  }

  def addVar(name: String,
             decType: DeclarationType,
            ): Either[List[String], ScopeContext] = {
    scopeStack match {
      case Scope(vars, funcs, returnType) :: scopes => {
        if (vars.contains(name)) Left(List("Variable %s has already been defined in this scope".format(name)))
        else Right(new ScopeContext(Scope(vars.updated(name, decType), funcs, returnType) :: scopes))
      }
      case _ => throw new InvalidAttributeValueException("Empty context: this should never happen.")
    }
  }

  def addFunc(name: String, expects: Expectation): Either[List[String], ScopeContext] = scopeStack match {
    case Scope(vars, funcs, returnType) :: scopes => {

    }
      if (findFunc(name).isRight) Left(List("Function re-definition: %s\b"))
      else {
        val currentScope = scopeStack.head
        Right(new ScopeContext(List(Scope(currentScope.vars, currentScope.funcs.updated(name, expects), returnType))))
      }
  }
  def expectedReturn(): Expectation = TypeMatcher.oneOf(List(nextReturn()))

  def nextReturn(): DeclarationType = scopeStack.head.nextReturn

  def newScope(returnType: DeclarationType): ScopeContext =
    new ScopeContext(new Scope(Map(), Map(), returnType) :: scopeStack)

  def newScope(): ScopeContext = newScope(nextReturn())

  def scopeVarSize(): Int = {
    return scopeStack.last.vars.size // returns the size of the variables in this scope
  }

  def scopeLevel(): Int = {
    return scopeStack.length - 1;
  }
}


object TypeValidator {

  import scala.language.implicitConversions

  implicit def declarationTypeToEither[List[String], DeclarationType](t: DeclarationType): Either[List[String], DeclarationType] = Right(t)

  implicit def makeBaseType(t: BaseTypeType): BaseType = BaseType(t)

  implicit def decTypeToList(x: DeclarationType): List[DeclarationType] = List(x)

  implicit def retTypeToList(x: Either[List[String], DeclarationType]): List[Either[List[String], DeclarationType]] = List(x)

  def returnType(expr: Expr)(implicit context: ScopeContext): Either[List[String], DeclarationType] = expr match {
    case IntLiteral(_) => BaseType(Int_T)
    case BoolLiteral(_) => BaseType(Bool_T)
    case CharLiteral(_) => BaseType(Char_T)
    case StringLiteral(_) => BaseType(String_T)
    case PairLiteral() => PairType(Any_T, Any_T)
    case expr: ArrayElem => lValType(expr)
    case IdentLiteral(name) => context.findVar(name)
    case UnaryOp(op, x) => UnaryOpExpectations(op) matchedWith returnType(x)
    case BinaryOp(op, x1, x2) => BinaryOpExpectations(op) matchedWith List(returnType(x1), returnType(x2))
  }

  def isHomogenousList(list: List[Expr])(implicit scopeContext: ScopeContext): Either[List[String], DeclarationType] = {
    list match {
      case expr :: exprs if exprs.isEmpty => returnType(expr)
      case expr :: exprs if !exprs.isEmpty => returnType(expr) match {
        case Right(retType) => TypeProcessor.simple(List(retType) -> retType) matchedWith (isHomogenousList(exprs))
        case Left(errors) => Left(errors)
      }
      case Nil => Right(BaseType(Any_T))
    }
  }


  private val UnaryOpExpectations = Map[UnOp, Expectation](
    Not -> TypeProcessor.simple(List(Bool_T) -> Bool_T),
    Neg -> TypeProcessor.simple(List(Int_T) -> Int_T),
    Len -> TypeProcessor.simple(List(ArrayType(BaseType(Any_T))) -> Int_T),
    Ord -> TypeProcessor.simple(List(Char_T) -> Int_T),
    Chr -> TypeProcessor.simple(List(Int_T) -> Char_T)
  )

  private val boolComparisonTypes =
    TypeProcessor.conditional(List(List(Int_T, Int_T) -> Bool_T, List(Char_T, Char_T) -> Bool_T))

  private val BinaryOpExpectations = Map[BinOp, Expectation](
    BinaryOpType.Mul -> TypeProcessor.simple(List(Int_T, Int_T) -> Int_T),
    BinaryOpType.Div -> TypeProcessor.simple(List(Int_T, Int_T) -> Int_T),
    BinaryOpType.Mod -> TypeProcessor.simple(List(Int_T, Int_T) -> Int_T),
    BinaryOpType.Add -> TypeProcessor.simple(List(Int_T, Int_T) -> Int_T),
    BinaryOpType.Sub -> TypeProcessor.simple(List(Int_T, Int_T) -> Int_T),
    BinaryOpType.Gt -> boolComparisonTypes,
    BinaryOpType.Gte -> boolComparisonTypes,
    BinaryOpType.Lt -> boolComparisonTypes,
    BinaryOpType.Lte -> boolComparisonTypes,
    BinaryOpType.Eq -> TypeMatcher.identicalTypes(Bool_T),
    BinaryOpType.Neq -> TypeMatcher.identicalTypes(Bool_T),
    BinaryOpType.And -> TypeProcessor.simple(List(Bool_T, Bool_T) -> Bool_T),
    BinaryOpType.Or -> TypeProcessor.simple(List(Bool_T, Bool_T) -> Bool_T)
  )
}
