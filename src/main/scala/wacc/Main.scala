package wacc

import parsley.{Failure, Success}
import wacc.Parser.ProgramParser.program
import wacc.SemanticAnalyser.verifyProgram
import wacc.Translator.delegateASTNode
import wacc.PeepholeOptimisation.PeepholeOptimise
import wacc.ArchitectureType.getArchitecture
import wacc.ARM11HighLevelAssembler
import wacc.ARM11LowLevelAssembler
import wacc.X86HighLevelAssembler
import wacc.X86LowLevelAssembler

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
    val file = Option(Source.fromFile(args.head))
      .getOrElse(throw new FileNotFoundException("File: " + args.head + " does not exist."))
    val inputProgram = file.mkString
    file.close

    println(inputProgram)

    val target = getArchitecture(args(1))
      .getOrElse(throw new FileNotFoundException("Architecture: " + args(1) + " does not exist."))

    /* Compile */
    // Parse input file
    val ast = program.parse(inputProgram)
    ast match {
      case Failure(err) => {
        println("Syntax Error: %s".format(err))
        sys.exit(SyntaxErrorCode)
      }
      case Success(x) =>
    }

    // Apply semantic analysis
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
    val tac = delegateASTNode(ast.get)._1
    println("--- TAC ---")
    tac.foreach(l => println(l))


    var asm = new String()
    target match {
      case ArchitectureType.ARM11 => {
        // Convert the TAC to IR
        val assembler = new ARM11HighLevelAssembler()
        val (result, funcs) = assembler.assembleProgram(tac)
        // Apply optimisations here
        // TODO: only optimise based on cmdline flags
        // val result = PeepholeOptimise(ir)
        // Convert the IR to ARM11
        asm = ARM11LowLevelAssembler.assemble(result, funcs)
        println("--- ARM ---")
      }
      case ArchitectureType.X86 => {
        // Convert the TAC to IR
        val assembler = new X86HighLevelAssembler()
        val (result, funcs) = assembler.assembleProgram(tac)
        // Apply optimisations here
        // TODO: only optimise based on cmdline flags
        // val result = PeepholeOptimise(ir)
        // Convert the IR to X86_64
        val x86 = X86LowLevelAssembler.assemble(result, funcs)
        println("--- X86_64 ---")
      }
    }
    print(asm)

    /* Output the assembly file */
    if(OutputAssemblyFile) {
      val inputFilename = args.head.split("/").last
      val outputFilename = inputFilename.replace(".wacc", ".s")
      val outputFile = new File(outputFilename)
      val fileWriter = new BufferedWriter(new FileWriter(outputFile))
      fileWriter.write(asm + "\n")
      fileWriter.close()
    }
    println("\n\nCompilation Successful!")
    sys.exit(SuccessCode)
  }
}



