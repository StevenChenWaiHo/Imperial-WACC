package wacc

import parsley.{Failure, Success}
import wacc.Parser.ProgramParser.program
import wacc.SemanticAnalyser.verifyProgram
import wacc.Translator.delegateASTNode

import java.io.{BufferedWriter, File, FileNotFoundException, FileWriter}
import scala.io.Source


object Main {
  val OutputAssemblyFile = false

  val SyntaxErrorCode = 100
  val SemanticErrorCode = 200
  val SuccessCode = 0

  def main(args: Array[String]): Unit = {
    if (args.length != 1) throw new IllegalArgumentException(
      "Incorrect number of arguments provided. Received: " + args.length + ", Expected 1."
    )
    val file = Option(Source.fromFile(args.head))
      .getOrElse(throw new FileNotFoundException("File: " + args.head + " does not exist."))
    val inputProgram = file.mkString
    file.close

    /* Compile */
    val ast = program.parse(inputProgram)
    ast match {
      case Failure(err) => {
        println("Syntax Error: %s".format(err))
        sys.exit(SyntaxErrorCode)
      }
      case Success(x) =>
    }

    val verified = verifyProgram(ast.get)
    if (verified.isLeft) {
      print("Semantic Error: ")
      verified.left.foreach(errList => {
        errList.reverse.foreach(err => {
          if (err != Nil && err.nonEmpty) println(err)
        })
      })
      sys.exit(SemanticErrorCode)
    }
    println("Compilation Successful!")

    //val result = Assembler.translateProgram(...)
    val result = List("Temporary placeholder!")

    /* Output the assembly file */
    if(OutputAssemblyFile) {
      val fileName = args.head.split(".").head + ".s"
      val outputFile = new File(fileName)
      val fileWriter = new BufferedWriter(new FileWriter(outputFile))
      for(line <- result.reverse) fileWriter.write(line)
      fileWriter.close()
    }
    sys.exit(SuccessCode)
  }
}



