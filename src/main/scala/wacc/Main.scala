package wacc

import parsley.{Failure, Success}
import wacc.Parser.ProgramParser.program
import wacc.SemanticAnalyser.verifyProgram
import wacc.Translator.delegateASTNode
import wacc.Assembler
import wacc.Parser.ProgramParser

import java.io.{BufferedWriter, File, FileNotFoundException, FileWriter}
import scala.io.Source


object Main {
  val OutputAssemblyFile = true

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
    
    // Translate the ast in to IR
    val tac = delegateASTNode(ast.get)._1
    println("--- TAC ---")
    tac.foreach(l => println(l))

    // Convert the IR to ARM
    val assembler = new Assembler()
    val result = assembler.assembleProgram(tac)
    println("--- ARM ---")
    print(result)

    /* Output the assembly file */
    if(OutputAssemblyFile) {
      val inputFilename = args.last.split("/").last
      val outputFilename = inputFilename.replace(".wacc", ".s")
      val outputFile = new File(outputFilename)
      val fileWriter = new BufferedWriter(new FileWriter(outputFile))
      for(line <- result) fileWriter.write(line)
      fileWriter.close()
    }
    println("\n\nCompilation Successful!")
    sys.exit(SuccessCode)
  }
}



