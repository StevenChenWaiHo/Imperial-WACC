package wacc

import wacc.AbstractSyntaxTree.BaseT._
import wacc.AbstractSyntaxTree.BinaryOpType.BinOp
import wacc.AbstractSyntaxTree.UnaryOpType._
import wacc.AbstractSyntaxTree._

import javax.management.InvalidAttributeValueException


case class Scope(val vars: Map[String, DeclarationType],
                 val funcs: Map[String, Expectation],
                 val nextReturn: Expectation)

class ScopeContext(scopeStack: List[Scope]) {
  if (scopeStack.isEmpty) throw new InvalidAttributeValueException("Context cannot be empty")

  def this() = this(List(new Scope(Map(), Map(), null)))

  def findVar(name: String): Option[DeclarationType] = {

    def findVar1(stack: List[Scope]): Option[DeclarationType] = stack match {
      case Scope(vars, _, _) :: scopes => {
        val result = vars.get(name)
        if (result.isEmpty) findVar1(scopes) else result
      }
      case Nil => Option.empty
    }

    findVar1(this.scopeStack)
  }

  def findFunc(name: String): Option[Expectation] = {

    def findFunc1(stack: List[Scope]): Option[Expectation] = stack match {
      case Scope(_, funcs, _) :: scopes => {
        val result = funcs.get(name)
        if (result.isEmpty) findFunc1(scopes) else result
      }
      case Nil => Option.empty
    }

    findFunc1(this.scopeStack)
  }

  def addVar(name: String,
             decType: DeclarationType,
            ): Either[List[String], ScopeContext] = scopeStack match {
    case Scope(vars, funcs, returnType) :: scopes => {
      if (vars.contains(name) && vars.get(name).equals(decType)) Left(List("Variable %s has already been defined in this scope\n".format(name)))
      else Right(new ScopeContext(Scope(vars.updated(name, decType), funcs, returnType) :: scopes))
    }
    case _ => throw new InvalidAttributeValueException("Empty context: this should never happen.")
  }

  def addFunc(name: String, expects: Expectation): Either[List[String], ScopeContext] = scopeStack match {
    case Scope(vars, funcs, returnType) :: scopes => {

    }
      if (findFunc(name).isDefined) {
        Left(List("Function re-definition: %s\b"))
      } else {
        val currentScope = scopeStack.head
        Right(new ScopeContext(List(Scope(currentScope.vars, currentScope.funcs.updated(name, expects), returnType))))
      }
  }
}

object ScopeContext {
  def fromFunctionList(funcs: List[Func]): Either[List[String], ScopeContext] =
    funcs.foldLeft(Right(new ScopeContext()): Either[List[String], ScopeContext]) {
      (context, func) =>
        context match {
          case Left(error) => Left(error)
          case Right(newContext) => newContext.addFunc(func.ident.name, TypeProcessor.fromFunction(func))
        }
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
    case IdentLiteral(name) => context.findVar(name) match {
      case None => Left(List("Variable %s is not defined\n".format(name)))
      case Some(declarationType) => declarationType
    }
    case UnaryOp(op, x) => UnaryOpExpectations(op) matchedWith returnType(x)
    case BinaryOp(op, x1, x2) => BinaryOpExpectations(op) matchedWith List(returnType(x1), returnType(x2))
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

  private val identicalTypes = simpleExpectation((inputs: List[DeclarationType]) => {
    if (inputs(0) == inputs(1)) Right(BaseType(Bool_T))
    else Left(List("Only matching types may be compared using == and !=\n"))
  })

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
    BinaryOpType.Eq -> identicalTypes,
    BinaryOpType.Neq -> identicalTypes,
    BinaryOpType.And -> TypeProcessor.simple(List(Bool_T, Bool_T) -> Bool_T),
    BinaryOpType.Or -> TypeProcessor.simple(List(Bool_T, Bool_T) -> Bool_T)
  )
}
