package wacc

import wacc.AbstractSyntaxTree.BaseT.Int_T
import wacc.AbstractSyntaxTree.{BaseType, DeclarationType, Func}

class Expectation(val expecting: TypeMatcher, var contextMessage: String) {
  def matchedWith(inputs: List[Either[List[String], DeclarationType]]): Either[List[String], DeclarationType] = {
    expecting(inputs).left.map(contextMessage :: _)
  }

  def withContext(context: String): Expectation = {
    this.contextMessage = context
    this
  }
}

object simpleExpectation {
  /** Takes a function of type List[DeclarationType] => Either[List[String], DeclarationType] and returns an expectation
   * created with it. Handles error-message inputs automatically. */
  def apply(decTypeMatcher: List[DeclarationType] => Either[List[String], DeclarationType], contextMessage: String = "")
  = new Expectation(
    (inputs: List[Either[List[String], DeclarationType]]) => {
      val maybeError = inputs.find(_.isLeft)
      if (maybeError.isDefined) maybeError.get.left.map(contextMessage :: _)
      else decTypeMatcher(inputs.map(_.toOption.get))
    }, contextMessage)
}

object TypeMatcher {
  /** Matches if both types are identical, returning returnType. Returns an error otherwise. */
  def identicalTypes(returnType: DeclarationType): Expectation = simpleExpectation((inputs: List[DeclarationType]) => {
    if (inputs(0) is inputs(1)) Right(returnType)
    else Left(List("Type mismatch: %s does not match %s\n".format(inputs(0), inputs(1))))
  })

  /** Matches if both types are identical, returning the first one. Returns an error otherwise. */
  def identicalTypes: Expectation = simpleExpectation((inputs: List[DeclarationType]) => {
    if (inputs(0) is inputs(1)) Right(inputs(0))
    else Left(List("Type mismatch: %s does not match %s\n".format(inputs(0), inputs(1))))
  })

  /** Matches if 'valids' contains the type it is matched with. Returns an error otherwise. */
  def oneOf(valids: List[DeclarationType]): Expectation = simpleExpectation((input: List[DeclarationType]) => {
    if (valids.find(_ is input(0)).isDefined) Right(input(0))
    else Left(List(s"Type mismatch - Expected one of:  ${if(valids.length == 1) valids(0) else valids}  " +
      s"but received ${input(0)}"))
  })
}

object TypeProcessor {
  private def countMatches(expected: List[DeclarationType], inputs: List[DeclarationType]): Int = {
    var count = 0
    for ((expectedInput, input) <- expected zip inputs)
      if (expectedInput is input) count += 1
    count
  }

  private def firstMismatch(expected: List[DeclarationType], inputs: List[DeclarationType]): Int = {
    var count = 1
    for ((expectedInput, input) <- expected zip inputs) {
      if (!expectedInput.is(input)) return count
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
    val orderedMatches = valids.sortBy(x => countMatches(x._1, definitelyInputs)).reverse
    val bestMatch = orderedMatches.head
    val mismatch = firstMismatch(bestMatch._1, definitelyInputs)
    if (mismatch > bestMatch._1.length) return Right(bestMatch._2)

    val errorMessage = "Mismatched argument. Best guess: argument %d should be of type: %s but it was of type: %s"
      .format(mismatch, bestMatch._1(mismatch - 1), definitelyInputs(mismatch - 1))
    Left(List(errorMessage))
  }

  private def matchingExpectation(valids: (List[DeclarationType], DeclarationType))
                                 (inputs: List[Either[List[String], DeclarationType]]): Either[List[String], DeclarationType] =
    conditionalExpectation(List(valids))(inputs)

  /** Takes a list of possible input/output pairs that this function can process.
   * For example, TypeProcessor.conditional(List(List(Int_T, Int_T) -> Int_T, List(Char_T, Char_T) -> Bool_T)) will
   * match two inputs of types Int_T, Int_T or two inputs of type Char_T, Char_T and return an Int_T or Bool_T
   * respectively. If the inputs don't match any of these patterns, returns an error message.
   */
  def conditional[A, B](ts: List[(List[A], B)])
                       (implicit fromA: A => DeclarationType, fromB: B => DeclarationType): Expectation =
    new Expectation(conditionalExpectation(ts.map(x => (x._1.map(fromA), fromB(x._2)))): TypeMatcher, "")

  /** Like a matchingExpectation, only takes one input/output pair. */
  def simple[A, B](ts: (List[A], B), contextMessage: String = "")
                  (implicit fromA: A => DeclarationType, fromB: B => DeclarationType): Expectation =
    new Expectation(matchingExpectation(ts._1.map(fromA), fromB(ts._2)): TypeMatcher, contextMessage)

  def fromFunction(func: Func): Expectation = simple(func.types.map(_._1) -> func.returnType)
}