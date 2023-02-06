package wacc

import wacc.AbstractSyntaxTree.BaseT.BaseTypeType
import wacc.AbstractSyntaxTree.{BaseType, DeclarationType, NestedPair}

import scala.util.control.Breaks.break
class Expectation(val expecting: ReturnTypeMatcher, var contextMessage: String = "") {

  /* Simpler constructor. Handles Left ReturnTypes automatically. */
  def this(decTypeMatcher: List[DeclarationType] => ReturnType, contextMessage: String = "") = this(
    (inputs: List[ReturnType]) => {
      val maybeError = inputs.find(_.isLeft)
      if (maybeError.isDefined) maybeError.get.left.map(contextMessage :: _)
      else decTypeMatcher(inputs.map(_.toOption.get))
    }, contextMessage)

  def matchedWith(inputs: List[ReturnType]): ReturnType = {
    expecting(inputs).left.map(contextMessage :: _)
  }

  /* Union of two expectations. If both produce errors, return the error from the rightmost expectation. */
  def ~>(other: Expectation): Expectation = new Expectation(
    (inputs: List[ReturnType]) => {
      val result1 = this matchedWith inputs
      val result2 = other matchedWith inputs
      if (result1.isRight && result2.isRight) throw new RuntimeException("Ambiguous expectation: This should never happen.")
      result2 orElse result1
    })

  /* Opposite of ~>. Not strictly necessary, but it could make the syntax a bit nicer. */
  def <~(other: Expectation): Expectation = other ~> this
}

object TypeProcessor {
  private def countMatches(expected: List[DeclarationType], inputs: List[DeclarationType]): Int = {
    var count = 0
    for ((expectedInput, input) <- expected zip inputs)
      if (expectedInput == input) count += 1
    count
  }

  private def firstMismatch(expected: List[DeclarationType], inputs: List[DeclarationType]): Int = {
    var count = 1
    for ((expectedInput, input) <- expected zip inputs) {
      if (expectedInput != input) break()
      count += 1
    }
    count
  }

  /* Returns a function that matches the input types ('inputs') to the first in a list of valid input types ('valids'),
  *   returning the corresponding output type. */
  private def conditionalExpectation(valids: List[(List[DeclarationType], DeclarationType)])
                                    (inputs: List[ReturnType]): ReturnType = {
    val maybeError = inputs.find(_.isLeft)
    if (maybeError.isDefined) return maybeError.get

    val definitelyInputs = inputs.map(_.toOption.get)
    val orderedMatches = valids.sortBy(x => countMatches(x._1, definitelyInputs)).reverse
    val bestMatch = orderedMatches.head
    val mismatch = firstMismatch(bestMatch._1, definitelyInputs)
    if (mismatch > bestMatch._1.length) return Right(bestMatch._2)

    val errorMessage = "Mismatched argument. Best guess: argument %i should be of type: \n%s\nbut it was of type: \n%s"
      .formatted(mismatch, bestMatch._1(mismatch - 1), definitelyInputs(mismatch - 1))
    Left(List(errorMessage))
  }

  private def simpleExpectation(valids: (List[DeclarationType], DeclarationType))
                               (inputs: List[ReturnType]): ReturnType =
    conditionalExpectation(List(valids))(inputs)

//  def apply(ts: (BaseTypeType, BaseTypeType), contextMessage: String = ""): Expectation =
//    new Expectation(simpleExpectation(List(BaseType(ts._1)), BaseType(ts._2)): ReturnTypeMatcher, contextMessage)

//  def apply(in: DeclarationType, out: DeclarationType, contextMessage: String = ""): Expectation =
//    new Expectation(simpleExpectation(List(in), out): ReturnTypeMatcher, contextMessage)


  def conditional[A, B](ts: List[(List[A], B)])
                 (implicit fromA: A => DeclarationType, fromB: B => DeclarationType) : Expectation =
    new Expectation(conditionalExpectation(ts.map(x => (x._1.map(fromA), fromB(x._2)))): ReturnTypeMatcher)

  def simple[A, B](ts: (List[A], B), contextMessage: String = "")
                 (implicit fromA: A => DeclarationType, fromB: B => DeclarationType) : Expectation =
    new Expectation(simpleExpectation(ts._1.map(fromA), fromB(ts._2)): ReturnTypeMatcher, contextMessage)

}