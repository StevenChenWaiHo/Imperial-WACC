package wacc

import wacc.ARM11HighLevelAssembler._
import wacc.ARM11AssemblerTypes._
import wacc.AssemblerTypes._
import wacc.RegisterAllocator._
import wacc.TAC._
import wacc.FinalIR.FinalIR

import scala.collection.mutable.ListBuffer

class ARM11HelperFunctions {
  private[this] val state = new AssemblerState(ListBuffer(r4, r5, r6, r7, r8, r9, r10, r11))

  implicit private[this] def toStrings(state: AssemblerState) = state.code.toList

  implicit private[this] def updateState(instr: FinalIR): AssemblerState = state.addInstruction(instr)

  def assemble_errNull(): List[FinalIR] = {
    val sLbl = new Label(".L._errNull_str0")
    assembleTAC(DataSegmentTAC()) ++
    assembleTAC(Comments("length of " + sLbl.name)) ++
    assembleTAC(StringLengthDefinitionTAC(45, sLbl)) ++
    assembleTAC(StringDefinitionTAC("fatal error: null pair dereferenced or freed\n", sLbl)) ++
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_errNull")) ++
    (FinalIR.Ldr("", r0, new LabelString(".L._errNull_str0"), r0) ::
    FinalIR.BranchLink("", new BranchString("_prints")) ::
    FinalIR.Mov("", new ImmediateInt(255), r0) ::
    FinalIR.BranchLink("", new BranchString("exit")) :: List())
  }

  def assemble_freepair(): List[FinalIR] = {
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_freepair")) ++
    (FinalIR.Push("", List(lr)) ::
    FinalIR.Mov("", r0, r1) ::
    FinalIR.Cmp("", r1, ImmediateInt(0)) ::
    FinalIR.BranchLink("eq", new BranchString("_errNull")) ::
    FinalIR.Ldr("", r1, ImmediateInt(0), r0) ::
    FinalIR.Push("", List(r1)) ::
    FinalIR.BranchLink("", new BranchString("free")) ::
    FinalIR.Pop("", List(r1)) ::
    FinalIR.Ldr("", r1, ImmediateInt(POINTER_BYTE_SIZE), r0) ::
    FinalIR.Push("", List(r1)) ::
    FinalIR.BranchLink("", new BranchString("free")) ::
    FinalIR.Pop("", List(r1)) ::
    FinalIR.Mov("", r1, r0) ::
    FinalIR.Push("", List(r1)) ::
    FinalIR.BranchLink("", new BranchString("free")) ::
    FinalIR.Pop("", List(r1)) ::
    FinalIR.Pop("", List(pc)) :: List())
  }

  
  def assemble_errDivZero(): List[FinalIR] = {
    val sLbl = Label(".L._errDivZero_str0")
    assembleTAC(DataSegmentTAC()) ++
    assembleTAC(Comments("length of " + sLbl.name)) ++
    assembleTAC(StringLengthDefinitionTAC(40, sLbl)) ++
    assembleTAC(StringDefinitionTAC("fatal error: division or modulo by zero\n", sLbl)) ++
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_errDivZero")) ++
    (FinalIR.Ldr("", null, LabelString(sLbl.name), r0) ::
    FinalIR.BranchLink("", BranchString("_prints")) ::
    FinalIR.Mov("", ImmediateInt(255), r0) ::
    FinalIR.BranchLink("", BranchString("exit")) :: List())
  }

  def assemble_errOverflow(): List[FinalIR] = {
    val sLbl = Label(".L._errOverflow_str0")
    assembleTAC(DataSegmentTAC()) ++
    assembleTAC(Comments("length of " + sLbl.name)) ++
    assembleTAC(StringLengthDefinitionTAC(52, sLbl)) ++
    assembleTAC(StringDefinitionTAC("fatal error: integer overflow or underflow\n", sLbl)) ++
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_errOverflow")) ++
    (FinalIR.Ldr("", null, LabelString(sLbl.name), r0) ::
    FinalIR.BranchLink("", new BranchString("_prints")) ::
    FinalIR.Mov("", new ImmediateInt(255), r0) ::
    FinalIR.BranchLink("", new BranchString("exit")) :: List())
  }

  // Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
  // ie r3 = r3[r10] (from reference compiler)
  // we instead use r0 = r3[r2]
  def assemble_arrLoad(): List[FinalIR] = {
    assembleTAC(Label("_arrLoad")) ++
    (FinalIR.Push("", List(lr)) ::
      FinalIR.Cmp("", r2, new ImmediateInt(0)) ::
      FinalIR.Mov("lt", r2, r1) ::
      FinalIR.BranchLink("lt", new BranchString("_boundsCheck")) ::
      FinalIR.Ldr("", r3, new ImmediateInt(-POINTER_BYTE_SIZE), lr) ::
      FinalIR.Cmp("", r2, lr) ::
      FinalIR.Mov("ge", r2, r1) ::
      FinalIR.BranchLink("ge", new BranchString("_boundsCheck")) ::
      FinalIR.Ldr("", r3, LogicalShiftLeft(r2, Right(2)), r0) ::
      FinalIR.Pop("", List(pc)) :: List())
  }

  // Special calling convention: array ptr passed in R3, index in R10, value to store in R8, LR (R14) is used as general register
  // ie r3[r10] = r8 (from reference compiler)
  // we instead use r3[r0] = r2
  def assemble_arrStore(): List[FinalIR] = {
    assembleTAC(Label("_arrStore")) ++
    (FinalIR.Push("", List(lr)) ::
      FinalIR.Cmp("", r0, new ImmediateInt(0)) ::
      FinalIR.Mov("lt", r0, r1) :: // r0 < 0
      FinalIR.BranchLink("lt", new BranchString("_boundsCheck")) ::
      FinalIR.Ldr("", r3, new ImmediateInt(-POINTER_BYTE_SIZE), lr) ::
      FinalIR.Cmp("", r0, lr) ::
      FinalIR.Mov("ge", r0, r1) :: // r0 >= lr
      FinalIR.BranchLink("ge", new BranchString("_boundsCheck")) ::
      FinalIR.Str("", LogicalShiftLeft(r0, Right(2)), r2, r3) :: 
      FinalIR.Pop("", List(pc)) :: List())
  }

  def assemble_boundsCheck(): List[FinalIR] = {
    val sLbl = new Label(".L._boundsCheck_str_0")
    assembleTAC(DataSegmentTAC()) ++
    assembleTAC(Comments("length of " + sLbl.name)) ++
    assembleTAC(StringLengthDefinitionTAC(42, sLbl)) ++
    assembleTAC(StringDefinitionTAC("fatal error: array index %d out of bounds\n", sLbl)) ++
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_boundsCheck")) ++
    (FinalIR.Ldr("", r0, new LabelString(sLbl.name), r0) ::
      FinalIR.BranchLink("", new BranchString("printf")) ::
      FinalIR.Mov("", new ImmediateInt(0), r0) ::
      FinalIR.BranchLink("", new BranchString("fflush")) ::
      FinalIR.Mov("", new ImmediateInt(255), r0) ::
      FinalIR.BranchLink("", new BranchString("exit")) :: List())
  }

  def assemble_print(pType: String): List[FinalIR] = {
    pType match {
      case "_prints" => assemble_prints()
      case "_printi" => assemble_printi()
      case "_printc" => assemble_printc()
      case "_printb" => assemble_printb()
      case "_printp" => assemble_printp()
      case "_println" => assemble_println()
      case _ => assemble_prints()
    }
  }

   def assemble_printp(): List[FinalIR] = {
    val sLbl = new Label(".L._printp_str0")
    assembleTAC(DataSegmentTAC()) ++
    assembleTAC(Comments("length of " + sLbl.name)) ++
    assembleTAC(StringLengthDefinitionTAC(2, sLbl)) ++
    assembleTAC(StringDefinitionTAC("%p", sLbl)) ++
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_printp")) ++
    (FinalIR.Push("", List(lr)) ::
        FinalIR.Mov("", r0, r1) ::
        FinalIR.Ldr("", r0, new LabelString(sLbl.name), r0) ::
        FinalIR.BranchLink("", new BranchString("printf")) ::
        FinalIR.Mov("", ImmediateInt(0), r0) ::
        FinalIR.BranchLink("", new BranchString("fflush")) ::
        FinalIR.Pop("", List(pc)) :: List())
   }

  def assemble_prints(): List[FinalIR] = {
    val sLbl = new Label(".L._prints_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(4, sLbl)) ++
      assembleTAC(StringDefinitionTAC("%.*s", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_prints")) ++
      (FinalIR.Push("", List(lr)) ::
        FinalIR.Mov("", r0, r2) ::
        FinalIR.Ldr("", r0, ImmediateInt(-POINTER_BYTE_SIZE), r1) ::
        FinalIR.Ldr("", r0, new LabelString(sLbl.name), r0) ::
        FinalIR.BranchLink("", new BranchString("printf")) ::
        FinalIR.Mov("", new ImmediateInt(0), r0) ::
        FinalIR.BranchLink("", new BranchString("fflush")) ::
        FinalIR.Pop("", List(pc)) :: List())
  }

  def assemble_printc(): List[FinalIR] = {
    val sLbl = new Label(".L._printc_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(2, sLbl)) ++
      assembleTAC(StringDefinitionTAC("%c", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_printc")) ++
      (FinalIR.Push("", List(lr)) ::
        FinalIR.Mov("", r0, r1) ::
        FinalIR.Ldr("", r0, new LabelString(sLbl.name), r0) ::
        FinalIR.BranchLink("", new BranchString("printf")) ::
        FinalIR.Mov("", new ImmediateInt(0), r0) ::
        FinalIR.BranchLink("", new BranchString("fflush")) ::
        FinalIR.Pop("", List(pc)) :: List())
  }

  def assemble_printi(): List[FinalIR] = {
    val sLbl = new Label(".L._printi_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(2, sLbl)) ++
      assembleTAC(StringDefinitionTAC("%d", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_printi")) ++
      (FinalIR.Push("", List(lr)) ::
        FinalIR.Mov("", r0, r1) ::
        FinalIR.Ldr("", r0, new LabelString(sLbl.name), r0) ::
        FinalIR.BranchLink("", new BranchString("printf")) ::
        FinalIR.Mov("", new ImmediateInt(0), r0) ::
        FinalIR.BranchLink("", new BranchString("fflush")) ::
        FinalIR.Pop("", List(pc)) :: List())
  }

  def assemble_println(): List[FinalIR] = {
    val sLbl = new Label(".L._println_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(0, sLbl)) ++
      assembleTAC(StringDefinitionTAC("", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_println")) ++
      (FinalIR.Push("", List(lr)) ::
        FinalIR.Ldr("", r0, new LabelString(sLbl.name), r0) ::
        FinalIR.BranchLink("", new BranchString("puts")) ::
        FinalIR.Mov("", new ImmediateInt(0), r0) ::
        FinalIR.BranchLink("", new BranchString("fflush")) ::
        FinalIR.Pop("", List(pc)) :: List())
  }

  def assemble_printb(): List[FinalIR] = {
    val fLbl = new Label(".L._printb_str0")
    val tLbl = new Label(".L._printb_str1")
    val sLbl = new Label(".L._printb_str2")

    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + fLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(5, fLbl)) ++
      assembleTAC(StringDefinitionTAC("false", fLbl)) ++
      assembleTAC(Comments("length of " + tLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(4, tLbl)) ++
      assembleTAC(StringDefinitionTAC("true", tLbl)) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(4, sLbl)) ++
      assembleTAC(StringDefinitionTAC("%.*s", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_printb")) ++
      (FinalIR.Push("", List(lr)) ::
        FinalIR.Cmp("", r0, new ImmediateInt(0)) ::
        FinalIR.Branch("ne", ".L_printb0") ::
        FinalIR.Ldr("", r0, new LabelString(fLbl.name), r2) ::
        FinalIR.Branch("", ".L_printb1") ::
        assembleTAC(Label(".L_printb0"))) ++
      (FinalIR.Ldr("", r0, new LabelString(tLbl.name), r2) ::
        assembleTAC(Label(".L_printb1"))) ++
      (FinalIR.Ldr("", r2, ImmediateInt(-POINTER_BYTE_SIZE), r1) ::
        FinalIR.Ldr("", r0, new LabelString(sLbl.name), r0) ::
        FinalIR.BranchLink("", new BranchString("printf")) ::
        FinalIR.Mov("", new ImmediateInt(0), r0) ::
        FinalIR.BranchLink("", new BranchString("fflush")) ::
        FinalIR.Pop("", List(pc)) :: List())
  }

  def assemble_read(rType: String): List[FinalIR] = {
    rType match {
      case "_readi" => assemble_readi()
      case "_readc" => assemble_readc()
      case _ => assemble_readi()
    }
  }

  def assemble_readi(): List[FinalIR] = {
    val lbl = new Label(".L._readi_str0")
    List(
      DataSegmentTAC(),
      Comments("length of " + lbl.name),
      StringLengthDefinitionTAC(2, lbl),
      StringDefinitionTAC("%d", lbl),
      TextSegmentTAC(),
      Label("_readi")).map(tac => assembleTAC(tac)).flatten ++
      (FinalIR.Push("", List(lr)) ::
      FinalIR.StrPre("", sp, new ImmediateInt(-POINTER_BYTE_SIZE), r0) ::
      FinalIR.Mov("", sp, r1) ::
      FinalIR.Ldr("", null, new LabelString(lbl.name), r0) ::
      FinalIR.BranchLink("", new BranchString("scanf")) ::
      FinalIR.Ldr("", sp, new ImmediateInt(0), r0) ::
      FinalIR.Add("", None(), sp, new ImmediateInt(4), sp) ::
      FinalIR.Pop("", List(pc)) :: List())
  }

  def assemble_readc(): List[FinalIR] = {
    val lbl = new Label(".L._readc_str0")
    List(
      DataSegmentTAC(),
      Comments("length of " + lbl.name),
      StringLengthDefinitionTAC(3, lbl),
      StringDefinitionTAC(" %c", lbl),
      TextSegmentTAC(),
      Label("_readc")).map(tac => assembleTAC(tac)).flatten ++
      (FinalIR.Push("", List(lr)) :: 
      FinalIR.StrPre("b", sp, new ImmediateInt(-1), r0) ::
      FinalIR.Mov("", sp, r1) :: 
      FinalIR.Ldr("", null, new LabelString(lbl.name), r0) ::
      FinalIR.BranchLink("", new BranchString("scanf")) ::
      FinalIR.Ldr("sb", sp, new ImmediateInt(0), r0) ::
      FinalIR.Add("", None(), sp, new ImmediateInt(1), sp) ::
      FinalIR.Pop("", List(pc)) :: List())
  }

}
