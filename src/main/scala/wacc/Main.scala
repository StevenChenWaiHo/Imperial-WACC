package wacc

import wacc.Parser.ProgramParser.program
import wacc.SemanticAnalyser.verifyProgram

import java.io.FileNotFoundException
import scala.io.Source
import parsley.Failure
import parsley.Success


object Main {
    def main(args: Array[String]): Unit = {
        if(args.length != 1) throw new IllegalArgumentException(
            "Incorrect number of arguments provided. Received: " + args.length + ", Expected 1."
        )
        val file = Option(Source.fromFile(args.head))
          .getOrElse(throw new FileNotFoundException("File: " + args.head + " does not exist."))
        val inputProgram = file.mkString
        file.close
        println(args.head)

        /* Compile */
        val ast = program.parse(inputProgram)
        ast match { 
            case Failure(err) => {
                //println("Syntax Error: %s".format(err))
                println("syntax")
                sys.exit(100)
            }
            case Success(x) =>
        }

        val verified = verifyProgram(ast.get)
//////////////////////////////////////////////////////////
      if (verified.isLeft) {
        print("SEMANTIC")
        sys.exit()
      }
      else sys.exit(0)

        if (verified.isLeft) {
            verified.left.foreach(errList => {
                errList.reverse.foreach(err => {
                  if(err != Nil) println("Semantic Error: %s".format(err))
                })
            })
            sys.exit(200)
        }
        println("Compilation Successful!")
        sys.exit(0)
    }
}



