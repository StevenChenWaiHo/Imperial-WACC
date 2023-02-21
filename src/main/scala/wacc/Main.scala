package wacc

import wacc.Parser.ProgramParser.program.parse
import wacc.SemanticAnalyser.verifyProgram
import wacc.Translator.delegateASTNode

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

        /* Compile */
        // Lexing and Parsing
        val ast = parse(inputProgram)
        ast match { 
            case Failure(err) => {
                println("Syntax Error: %s".format(err))
                sys.exit(100)
            }
            case Success(x) =>
        }
        // Semantic Analysis
        val verified = verifyProgram(ast.get)
        if (verified.isLeft) {
          print("Semantic Error: ")
            verified.left.foreach(errList => {
                errList.reverse.foreach(err => {
                  if(err != Nil && err.nonEmpty) println(err)
                })
            })
            sys.exit(200)
        }
        // Intermediate Code Gen
        delegateASTNode(ast.get) match {
            case (tacList, _) => {
                tacList.foreach(s => println(s))
            }
        }
        // Output Code Gen
        //assemble()
        println("Compilation Successful!")
        sys.exit(0)
    }
}



