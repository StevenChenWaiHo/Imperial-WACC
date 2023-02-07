package wacc

import wacc.Parser.ProgramParser.program
import wacc.SemanticAnalyser.verifyProgram

import java.io.FileNotFoundException
import scala.io.Source


object Main {
    def main(args: Array[String]): Unit = {
        if(args.length != 1) throw new IllegalArgumentException(
            "Incorrect number of arguments provided. Received: " + args.length + ", Expected 1."
        )
        val file = Option(Source.fromFile(args.head))
          .getOrElse(throw new FileNotFoundException("File: " + args.head + " does not exist."))
        val inputProgram = file.mkString
        // Close the file
        file.close
        println("read file " + args.head)

        val ast = program.parse(inputProgram)
        println(ast)
        if (ast.isFailure) {
            sys.exit(100)
        }
        val errOrVerified = verifyProgram(ast.get)
        println(errOrVerified)
        if (errOrVerified.isLeft) {
            sys.exit(200)
        }
        sys.exit(0)
    }
}



