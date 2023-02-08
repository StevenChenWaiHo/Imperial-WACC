package wacc

import wacc.AbstractSyntaxTree.BaseT.BaseTypeType
import wacc.AbstractSyntaxTree.{BaseType, DeclarationType, Func, NestedPair}

class Expectation(val expecting: TypeMatcher, var contextMessage: String) {
  def matchedWith(inputs: List[Either[List[String], DeclarationType]]): Either[List[String], DeclarationType] = {
    expecting(inputs).left.map(contextMessage :: _)
  }

  /* Union of two expectations. If both produce errors, return the error from the rightmost expectation. */
//  def ~>(other: Expectation): Expectation = new Expectation(
//    (inputs: List[Either[List[String], DeclarationType]]) => {
//      (this matchedWith inputs) orElse (other matchedWith inputs)
//    })

  /* Opposite of ~>. Not strictly necessary, but it could make the syntax a bit nicer. */
//  def <~(other: Expectation): Expectation = other ~> this
}

object simpleExpectation {
  /* Simpler constructor. Handles Left Either[List[String], DeclarationType]s automatically. */
  def apply(decTypeMatcher: List[DeclarationType] => Either[List[String], DeclarationType], contextMessage: String = "")
  = new Expectation(
    (inputs: List[Either[List[String], DeclarationType]]) => {
      val maybeError = inputs.find(_.isLeft)
      if (maybeError.isDefined) maybeError.get.left.map(contextMessage :: _)
      else decTypeMatcher(inputs.map(_.toOption.get))
    }, contextMessage)
}

object TypeProcessor {
  private def countMatches(expected: List[DeclarationType], inputs: List[DeclarationType]): Int = {
    var count = 0
    for ((expectedInput, input) <- expected zip inputs)
      if (expectedInput.equals(input)) count += 1
    count
  }

  private def firstMismatch(expected: List[DeclarationType], inputs: List[DeclarationType]): Int = {
    var count = 1
    for ((expectedInput, input) <- expected zip inputs) {
      expectedInput match {
        /* TODO: change this implementation (only some values are checked) */
        case BaseType(baseType) => input match {
          case BaseType(baseType) => 
          case _ => return count
        }
        case _ => return count
      }
      count += 1
    }
    count
  }

  /* Returns a function that matches the input types ('inputs') to the first in a list of valid input types ('valids'),
  *   returning the corresponding output type. */
  private def conditionalExpectation(valids: List[(List[DeclarationType], DeclarationType)])
                                    (inputs: List[Either[List[String], DeclarationType]]): Either[List[String], DeclarationType] = {
    val maybeError = inputs.find(_.isLeft)
    if (maybeError.isDefined) return maybeError.get

    val definitelyInputs = inputs.map(_.toOption.get)
    val orderedMatches = valids.sortBy(x => countMatches(x._1, definitelyInputs))
    val bestMatch = orderedMatches.head
    val mismatch = firstMismatch(bestMatch._1, definitelyInputs)
    if (mismatch > bestMatch._1.length) return Right(bestMatch._2)

    // val errorMessage = "Mismatched argument. Best guess: argument %i should be of type: \n%s\nbut it was of type: \n%s"
    //   .format(mismatch, bestMatch._1(mismatch - 1), definitelyInputs(mismatch - 1))
    val errorMessage = "Mismatched arguments"
    Left(List(errorMessage))
  }

  private def simpleExpectation(valids: (List[DeclarationType], DeclarationType))
                               (inputs: List[Either[List[String], DeclarationType]]): Either[List[String], DeclarationType] =
    conditionalExpectation(List(valids))(inputs)

  def conditional[A, B](ts: List[(List[A], B)])
                 (implicit fromA: A => DeclarationType, fromB: B => DeclarationType) : Expectation =
    new Expectation(conditionalExpectation(ts.map(x => (x._1.map(fromA), fromB(x._2)))): TypeMatcher, "")

  def simple[A, B](ts: (List[A], B), contextMessage: String = "")
                 (implicit fromA: A => DeclarationType, fromB: B => DeclarationType) : Expectation =
    new Expectation(simpleExpectation(ts._1.map(fromA), fromB(ts._2)): TypeMatcher, contextMessage)

  def fromFunction(func: Func): Expectation = simple(func.types.map(_._1) -> func.returnType)
}