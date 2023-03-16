package wacc

import parsley.{Failure, Success}
import wacc.Parser.ProgramParser.program
import wacc.SemanticAnalyser.verifyProgram
import wacc.Translator.delegateASTNode
import wacc.PeepholeOptimisation.PeepholeOptimise
import wacc.ARM11Assembler
import wacc.ArchitectureType.getArchitecture

import java.io.{BufferedWriter, File, FileNotFoundException, FileWriter}
import scala.io.Source


object Main {
  val OutputAssemblyFile = true

  val SyntaxErrorCode = 100
  val SemanticErrorCode = 200
  val SuccessCode = 0

  var target = ArchitectureType.ARM11

  def main(args: Array[String]): Unit = {
    val file = Option(Source.fromFile(args.head))
      .getOrElse(throw new FileNotFoundException("File: " + args.head + " does not exist."))
    val inputProgram = file.mkString
    file.close

    println(inputProgram)

    if (args.length == 2) {
    target = getArchitecture(args(1))
      .getOrElse(throw new FileNotFoundException("Architecture: " + args(1) + " does not exist."))
    }

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

    // Convert the TAC to IR
    val assembler = new Assembler()
    val (ir, funcs) = assembler.assembleProgram(tac)

    // Apply optimisations here
    // TODO: only optimise based on cmdline flags
    val result = PeepholeOptimise(ir)

    var asm = new String()
    target match {
      case ArchitectureType.ARM11 => {
        // Convert the IR to ARM11
        asm = ARM11Assembler.assemble(result, funcs)
        println("--- ARM ---")
      }
      case ArchitectureType.X86 => {
        // Convert the IR to X86_64
        //val x86 = X86Assembler.assemble(result, funcs)
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


