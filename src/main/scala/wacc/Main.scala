package wacc

import parsley.{Failure, Success}
import wacc.Parser.ProgramParser.program
import wacc.SemanticAnalyser.verifyProgram
import wacc.Translator.delegateASTNode
import wacc.Inlining.inline_delegateASTNode
import wacc.ARM11Assembler
import wacc.TAC._

import java.io.{BufferedWriter, File, FileNotFoundException, FileWriter}
import scala.io.Source


object Main {
  val OutputAssemblyFile = true

  val SyntaxErrorCode = 100
  val SemanticErrorCode = 200
  val SuccessCode = 0

  def main(args: Array[String]): Unit = {
    if (args.length != 2) throw new IllegalArgumentException(
      "Incorrect number of arguments provided. Received: " + args.length + ", Expected 2."
    )
    val filename = args.head
    val file = Option(Source.fromFile(filename))
      .getOrElse(throw new FileNotFoundException("File: " + filename + " does not exist."))
    val inputProgram = file.mkString
    file.close

    println(inputProgram + "\n\n")
    val optionalFlagString = args(1)
    val inlineFlag = optionalFlagString.contains("i")

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
          if (err != null && err.nonEmpty) println(err)
        })
      })
      sys.exit(SemanticErrorCode)
    }
    
    // Translate the ast to TAC
    var tac = List[TAC]()
    if (inlineFlag){
      println("--- INLINED TAC ---")
      tac = inline_delegateASTNode(ast.get)._1
    }
    else {
      println("--- TAC ---")
      tac = delegateASTNode(ast.get)._1
    }
    
  
    tac.foreach(l => println(l))

    // Convert the TAC to IR
    val assembler = new Assembler()
    val (result, helperFuncs) = assembler.assembleProgram(tac)

    println("--- FinalIR ---")
    result.foreach{x => println(x)}

    // Convert the IR to ARM
    val arm = ARM11Assembler.assemble(result, helperFuncs)
    println("--- ARM ---")
    print(arm)

    /* Output the assembly file */
    if(OutputAssemblyFile) {
      val inputFilename = filename.split("/").last
      val outputFilename = inputFilename.replace(".wacc", ".s")
      val outputFile = new File(outputFilename)
      val fileWriter = new BufferedWriter(new FileWriter(outputFile))
      fileWriter.write(arm + "\n")
      fileWriter.close()
    }
    println("\n\nCompilation Successful!")
    sys.exit(SuccessCode)
  }
}



