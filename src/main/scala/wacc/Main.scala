package wacc

import parsley.{Failure, Success}
import wacc.AssemblerTypes._
import wacc.Parser.ProgramParser.program
import wacc.SemanticAnalyser.verifyProgram
import wacc.Translator.delegateASTNode
import wacc.cfgutils.CFG.CFGBuilder
import wacc.cfgutils.{GraphColouringAllocator, TACLiveRange}
import wacc.Optimisations.Inlining.inline_delegateASTNode
import wacc.ARM11Assembler
import wacc.TAC._
import wacc.Optimisations.PeepholeOptimisation.PeepholeOptimise
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
    var argsNum = 2
    // Optional Flags
    val optionalFlagString = args(1)
    val inlineFlag = optionalFlagString.contains("i")
    val peepholeFlag = optionalFlagString.contains("p")
    val crossCompilerFlag = optionalFlagString.contains("c")
    if (crossCompilerFlag){
      argsNum=3
      val archName = args(2)
      target = archName match{
        case "x86" => ArchitectureType.X86
        case _: String => throw new IllegalArgumentException("Cannot find Architecture")
      }
    }

    if (args.length != argsNum) throw new IllegalArgumentException(
      "Incorrect number of arguments provided. Received: " + args.length + ", Expected " + argsNum
    )
    val filename = args.head
    val file = Option(Source.fromFile(filename))
      .getOrElse(throw new FileNotFoundException("File: " + filename + " does not exist."))
    val inputProgram = file.mkString
    file.close

    println(inputProgram + "\n\n")

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

    val regsList = target match {
      case ArchitectureType.ARM11 => List(r4, r5, r6, r7, AssemblerTypes.r8, AssemblerTypes.r10)
      case ArchitectureType.X86 => List(X86AssemblerTypes.rcx, X86AssemblerTypes.r8, X86AssemblerTypes.r9, X86AssemblerTypes.r10, X86AssemblerTypes.r11, X86AssemblerTypes.r12, X86AssemblerTypes.r13, X86AssemblerTypes.r14, X86AssemblerTypes.r15)
    }

    // Convert the TAC to IR
    val assembler = new Assembler(target, 
      new GraphColouringAllocator[AssemblerTypes.Register](
        regsList, tac.toVector, new CFGBuilder(TACLiveRange)))


    val (ir, funcs) = assembler.assembleProgram(tac)

    println("--- FinalIR ---")
    ir.foreach{x => println(x)}

    // Apply optimisations here
    var result = ir 
    if (peepholeFlag){
      result = PeepholeOptimise(ir)
    }

    var asm = new String()
    target match {
      case ArchitectureType.ARM11 => {
        // Convert the IR to ARM11
        asm = ARM11Assembler.assemble(result, funcs)
        println("--- ARM ---")
      }
      case ArchitectureType.X86 => {
        // Convert the IR to X86_64
        val asm = X86LowLevelAssembler.assemble(result, funcs)
        println("--- X86_64 ---")
      }
    }
    print(asm)

    /* Output the assembly file */
    if (OutputAssemblyFile) {
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


