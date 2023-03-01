package wacc

import wacc.Assembler._
import wacc.RegisterAllocator._
import wacc.TAC._

object HardcodeFunctions {

  def translate_errDivZero(): List[String] = {
    translateTAC(Label("_errDivZero"))
    // TODO: implement hardcode function
  }

  def translate_errOverflow(): List[String] = {
    translateLdr("", r0, r0, new LabelString(".L._errOverflow_str0")) :: 
    translateBranchLink("", new BranchString("_prints")) ::
    translateMove("", r0, new ImmediateInt(255)) ::
    translateBranchLink("", new BranchString("exit")) :: List()
  }

  def translate_arrStoreB(): List[String] = {
    translatePush("", List(lr)) :: 
    translateCompare("", r10, new ImmediateInt(0)) ::
    translateMove("", r1 , r10) ::
    translateBranchLink("lt", new BranchString("_boundsCheck")) :: 
    translateLdr("", lr, r3, new ImmediateInt(-4)) :: 
    translateCompare("eq", r10, lr) ::
    translateMove("ge", r1, r10) :: 
    translateBranchLink("ge", new BranchString("_boundsCheck")) :: 
    //translateStrb() :: 
    translatePop("", List(pc)) :: List()
  }

  def translate_boundsCheck(): List[String] = {
    translateLdr("", r0, r0, new LabelString(".L._boundsCheck_str_0")) :: 
    translateBranchLink("", new BranchString("printf")) :: 
    translateMove("", r0, new ImmediateInt(255)) :: 
    translateBranchLink("", new BranchString("fflush")) :: 
    translateMove("", r0, new ImmediateInt(255)) :: 
    translateBranchLink("", new BranchString("exit")) :: List()
  }

  def translate_print(pType: String): List[String] = {
    pType match {
      case "_prints" => translate_prints()
      case "_printi" => translate_printi()
      case "_printc" => translate_printc()
      case "_printb" => translate_printb()
      case "_println" => translate_println()
      case _ => translate_prints()
    }
  }

  def translate_prints(): List[String] = {
    val sLbl = new Label(".L._prints_str0")
    translateTAC(DataSegmentTAC()) ++ 
    translateTAC(Comments("length of " + sLbl.name)) ++
    translateTAC(StringLengthDefinitionTAC(4, sLbl)) ++
    translateTAC(StringDefinitionTAC("%.*s", sLbl)) ++
    translateTAC(TextSegmentTAC()) ++ 
    translateTAC(Label("_prints")) ++
    (translatePush("", List(lr)) ::
    translateMove("", r2, r0) ::
    translateLdr("", r0, r0, new LabelString(sLbl.name)) ::
    translateBranchLink("", new BranchString("printf")) ::
    translateMove("", r0, new ImmediateInt(0)) ::
    translateBranchLink("", new BranchString("fflush")) ::
    translatePop("", List(pc)) :: List())
  }

  def translate_printc(): List[String] = {
    val sLbl = new Label(".L._printc_str0")
    translateTAC(DataSegmentTAC()) ++ 
    translateTAC(Comments("length of " + sLbl.name)) ++
    translateTAC(StringLengthDefinitionTAC(2, sLbl)) ++
    translateTAC(StringDefinitionTAC("%c", sLbl)) ++
    translateTAC(TextSegmentTAC()) ++ 
    translateTAC(Label("_printc")) ++
    (translatePush("", List(lr)) ::
    translateMove("", r1, r0) ::
    translateLdr("", r0, r0, new LabelString(sLbl.name)) ::
    translateBranchLink("", new BranchString("printf")) ::
    translateMove("", r0, new ImmediateInt(0)) ::
    translateBranchLink("", new BranchString("fflush")) ::
    translatePop("", List(pc)) :: List())
  }

  def translate_printi(): List[String] = {
    val sLbl = new Label(".L._printi_str0")
    translateTAC(DataSegmentTAC()) ++ 
    translateTAC(Comments("length of " + sLbl.name)) ++
    translateTAC(StringLengthDefinitionTAC(2, sLbl)) ++
    translateTAC(StringDefinitionTAC("%d", sLbl)) ++
    translateTAC(TextSegmentTAC()) ++ 
    translateTAC(Label("_printi")) ++
    (translatePush("", List(lr)) ::
    translateMove("", r1, r0) ::
    translateLdr("", r0, r0, new LabelString(sLbl.name)) ::
    translateBranchLink("", new BranchString("printf")) ::
    translateMove("", r0, new ImmediateInt(0)) ::
    translateBranchLink("", new BranchString("fflush")) ::
    translatePop("", List(pc)) :: List())
  }

  def translate_println(): List[String] = {
    val sLbl = new Label(".L._println_str0")
    translateTAC(DataSegmentTAC()) ++ 
    translateTAC(Comments("length of " + sLbl.name)) ++
    translateTAC(StringLengthDefinitionTAC(0, sLbl)) ++
    translateTAC(StringDefinitionTAC("", sLbl)) ++
    translateTAC(TextSegmentTAC()) ++ 
    translateTAC(Label("_println")) ++
    (translatePush("", List(lr)) ::
    translateLdr("", r0, r0, new LabelString(sLbl.name)) ::
    translateBranchLink("", new BranchString("puts")) ::
    translateMove("", r0, new ImmediateInt(0)) ::
    translateBranchLink("", new BranchString("fflush")) ::
    translatePop("", List(pc)) :: List())
  }

  def translate_printb(): List[String] = {
    val fLbl = new Label(".L._printb_str0")
    val tLbl = new Label(".L._printb_str1")
    val sLbl = new Label(".L._printb_str2")
    
    translateTAC(DataSegmentTAC()) ++
    translateTAC(Comments("length of " + fLbl.name)) ++
    translateTAC(StringLengthDefinitionTAC(5, fLbl)) ++
    translateTAC(StringDefinitionTAC("false", fLbl)) ++
    translateTAC(Comments("length of " + tLbl.name)) ++
    translateTAC(StringLengthDefinitionTAC(4, tLbl)) ++
    translateTAC(StringDefinitionTAC("true", tLbl)) ++ 
    translateTAC(Comments("length of " + sLbl.name)) ++
    translateTAC(StringLengthDefinitionTAC(4, sLbl)) ++
    translateTAC(StringDefinitionTAC("%.*s", sLbl)) ++
    translateTAC(TextSegmentTAC()) ++ 
    translateTAC(Label("_printb")) ++
    (translatePush("", List(lr)) ::
    translateCompare("", r0, new ImmediateInt(0)) ::
    translateBranch("ne", ".L_printb0") ::
    translateLdr("", r2, r0, new LabelString(fLbl.name)) ::
    translateBranch("", ".L_printb1") ::
    translateTAC(Label(".L_printb0"))) ++
    (translateLdr("", r2, r0, new LabelString(tLbl.name)) ::
    translateTAC(Label(".L_printb1"))) ++ 
    ("ldr r1, [r2, #-4]" ::
    translateLdr("", r0, r0, new LabelString(sLbl.name)) ::
    translateBranchLink("", new BranchString("printf")) ::
    translateMove("", r0, new ImmediateInt(0)) ::
    translateBranchLink("", new BranchString("fflush")) ::
    translatePop("", List(pc)) :: List())
  }

  def translate_read(rType: String): List[String] = {
    rType match {
      case "_readi" => translate_readi()
      case _ => translate_readi()
    }
  }

  def translate_readi(): List[String] = {
    List(
    ".data",
	  "@ length of .L._readi_str0",
		".word 2",
	  ".L._readi_str0:",
		".asciz \"%d\"",
	  ".text",
	  "_readi:",
		"@ R0 contains the \"original\" value of the destination of the read",
		"push {lr}",
		"@ allocate space on the stack to store the read and place the original value there",
		"@ if scanf cannot read because of EOF, the read will appear to do nothing",
		"str r0, [sp, #-4]! @ push {r0}",
		"mov r1, sp",
		"ldr r0, =.L._readi_str0",
		"bl scanf",
		"ldr r0, [sp, #0]",
		"add sp, sp, #4",
		"pop {pc}")
  }
}
