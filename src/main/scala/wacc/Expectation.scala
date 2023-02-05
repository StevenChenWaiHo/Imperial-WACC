package wacc

import wacc.AbstractSyntaxTree.DeclarationType

import scala.util.control.Breaks.break

type ReturnType = Either[List[String], DeclarationType]

class Expectation(expectation: List[ReturnType] => ReturnType) {
  var context = ""

  def matchedWith(inputs: List[ReturnType]): ReturnType = {
    var result = expectation(inputs)
    if(context != "") result.left.map[List[String]](x => context :: x)
    result
  }
}

case class Type(declarationType: DeclarationType)
case class Mismatch(messages: List[String])


object Expectation {

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
    if(maybeError.isDefined) return maybeError.get

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


  def apply(valids: List[(List[DeclarationType], DeclarationType)]): Expectation =
    new Expectation(conditionalExpectation(valids))

  def apply(valid: (List[DeclarationType], DeclarationType)): Expectation =
    new Expectation(simpleExpectation(valid))
}