package wacc

import parsley.{Failure, Parsley, Success}
import parsley.character.digit
import parsley.expr.chain
import parsley.implicits.character.charLift
import wacc.AbstractSyntaxTree.ArrayElem
import wacc.Parser.StatementParser.statement

import scala.io.Source


object Main {
    def main(args: Array[String]): Unit = {
        // TODO: Handle error when file does not exist?
        val file = Source.fromFile(args.head)
        val inputProgram = file.getLines.mkString
        // Close the file
        file.close
        println("read file " + args.head)

        println(statement.parse(inputProgram))
    }
}

