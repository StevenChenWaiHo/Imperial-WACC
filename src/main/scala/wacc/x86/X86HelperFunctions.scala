package wacc

import wacc.X86HighLevelAssembler._
import wacc.X86AssemblerTypes._
import wacc.ArchitectureType._
import wacc.AssemblerTypes._
import wacc.RegisterAllocator._
import wacc.TAC._
import wacc.FinalIR.FinalIR

import scala.collection.mutable.ListBuffer

//TODO Change all to x86_64 Architecture

class X86HelperFunctions {
  private[this] val state = new AssemblerState(ListBuffer(rcx, r8, r9, r10, r11, r12, r13, r14, r15), ArchitectureType.X86)

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
    (FinalIR.Ldr("", rax, X86LabelString(".L._errNull_str0"), rax) ::
    FinalIR.BranchLink("", X86BranchString("_prints")) ::
    FinalIR.Mov("", X86ImmediateInt(255), rax) ::
    FinalIR.BranchLink("", X86BranchString("exit")))
  }

  def assemble_freepair(): List[FinalIR] = {
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_freepair")) ++
    (FinalIR.Push("", List(rbx)) ::
    FinalIR.Mov("", rax, rdi) ::
    FinalIR.Cmp("", rdi, X86ImmediateInt(0)) ::
    FinalIR.BranchLink("eq", X86BranchString("_errNull")) ::
    FinalIR.Ldr("", rdi, X86ImmediateInt(0), rax) ::
    FinalIR.Push("", List(rdi)) ::
    FinalIR.BranchLink("", X86BranchString("free")) ::
    FinalIR.Pop("", List(rdi)) ::
    FinalIR.Ldr("", rdi, X86ImmediateInt(POINTER_BYTE_SIZE), rax) ::
    FinalIR.Push("", List(rdi)) ::
    FinalIR.BranchLink("", X86BranchString("free")) ::
    FinalIR.Pop("", List(rdi)) ::
    FinalIR.Mov("", rdi, rax) ::
    FinalIR.Push("", List(rdi)) ::
    FinalIR.BranchLink("", X86BranchString("free")) ::
    FinalIR.Pop("", List(rdi)) ::
    FinalIR.Pop("", List(rbx)))
  }

  
  def assemble_errDivZero(): List[FinalIR] = {
    val sLbl = Label(".L._errDivZero_str0")
    assembleTAC(DataSegmentTAC()) ++
    assembleTAC(Comments("length of " + sLbl.name)) ++
    assembleTAC(StringLengthDefinitionTAC(40, sLbl)) ++
    assembleTAC(StringDefinitionTAC("fatal error: division or modulo by zero\n", sLbl)) ++
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_errDivZero")) ++
    (FinalIR.Ldr("", null, X86LabelString(sLbl.name), rax) ::
    FinalIR.BranchLink("", X86BranchString("_prints")) ::
    FinalIR.Mov("", X86ImmediateInt(255), rax) ::
    FinalIR.BranchLink("", X86BranchString("exit")))
  }

  def assemble_errOverflow(): List[FinalIR] = {
    val sLbl = Label(".L._errOverflow_str0")
    assembleTAC(DataSegmentTAC()) ++
    assembleTAC(Comments("length of " + sLbl.name)) ++
    assembleTAC(StringLengthDefinitionTAC(52, sLbl)) ++
    assembleTAC(StringDefinitionTAC("fatal error: integer overflow or underflow\n", sLbl)) ++
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_errOverflow")) ++
    (FinalIR.Ldr("", null, X86LabelString(sLbl.name), rax) ::
    FinalIR.BranchLink("", X86BranchString("_prints")) ::
    FinalIR.Mov("", X86ImmediateInt(255), rax) ::
    FinalIR.BranchLink("", X86BranchString("exit")))
  }

  // Special calling convention: array ptr passed in rdx, index in R10, rbx (R14) is used as general register, and return into rdx
  // ie rdx = rdx[r10] (from reference compiler)
  // we instead use rax = rdx[rsi]
  def assemble_arrLoad(): List[FinalIR] = {
    assembleTAC(Label("_arrLoad")) ++
    (FinalIR.Push("", List(rbx)) ::
      FinalIR.Cmp("", rsi, X86ImmediateInt(0)) ::
      FinalIR.Mov("", rsi, rdi) ::
      FinalIR.BranchLink("lt", X86BranchString("_boundsCheck")) ::
      FinalIR.Ldr("", rdx, X86ImmediateInt(-POINTER_BYTE_SIZE), rbx) ::
      FinalIR.Cmp("", rsi, rbx) ::
      FinalIR.Mov("ge", rsi, rdi) ::
      FinalIR.BranchLink("ge", X86BranchString("_boundsCheck")) ::
      FinalIR.Ldr("", rdx, LogicalShiftLeft(rsi, Right(2)), rax) ::
      FinalIR.Pop("", List(rbx)))
  }

  // Special calling convention: array ptr passed in rdx, index in R10, value to store in R8, rbx (R14) is used as general register
  // ie rdx[r10] = r8 (from reference compiler)
  // we instead use rdx[rax] = rsi
  def assemble_arrStore(): List[FinalIR] = {
    assembleTAC(Label("_arrStore")) ++
    (FinalIR.Push("", List(rbx)) ::
      FinalIR.Cmp("", rax, X86ImmediateInt(0)) ::
      FinalIR.Mov("lt", rax, rdi) :: // rax < 0
      FinalIR.BranchLink("lt", X86BranchString("_boundsCheck")) ::
      FinalIR.Ldr("", rdx, X86ImmediateInt(-POINTER_BYTE_SIZE), rbx) ::
      FinalIR.Cmp("", rax, rbx) ::
      FinalIR.Mov("ge", rax, rdi) :: // rax >= rbx
      FinalIR.BranchLink("ge", X86BranchString("_boundsCheck")) ::
      FinalIR.Str("", LogicalShiftLeft(rax, Right(2)), rsi, rdx) :: // TODO: Logical shift does not work
      FinalIR.Pop("", List(rbx)))
  }

  def assemble_boundsCheck(): List[FinalIR] = {
    val sLbl = new Label(".L._boundsCheck_str_0")
    assembleTAC(DataSegmentTAC()) ++
    assembleTAC(Comments("length of " + sLbl.name)) ++
    assembleTAC(StringLengthDefinitionTAC(42, sLbl)) ++
    assembleTAC(StringDefinitionTAC("fatal error: array index %d out of bounds\n", sLbl)) ++
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_boundsCheck")) ++
    (FinalIR.Ldr("", rax, X86LabelString(sLbl.name), rax) ::
      FinalIR.BranchLink("", X86BranchString("printf")) ::
      FinalIR.Mov("", X86ImmediateInt(0), rax) ::
      FinalIR.BranchLink("", X86BranchString("fflush")) ::
      FinalIR.Mov("", X86ImmediateInt(255), rax) ::
      FinalIR.BranchLink("", X86BranchString("exit")))
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
    (FinalIR.Push("", List(rbx)) ::
        FinalIR.Mov("", rax, rdi) ::
        FinalIR.Ldr("", rax, X86LabelString(sLbl.name), rax) ::
        FinalIR.BranchLink("", X86BranchString("printf")) ::
        FinalIR.Mov("", X86ImmediateInt(0), rax) ::
        FinalIR.BranchLink("", X86BranchString("fflush")) ::
        FinalIR.Pop("", List(rbx)))
   }

  def assemble_prints(): List[FinalIR] = {
    val sLbl = new Label(".L._prints_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(4, sLbl)) ++
      assembleTAC(StringDefinitionTAC("%.*s", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_prints")) ++
      (FinalIR.Push("", List(rbx)) ::
        FinalIR.Mov("", rax, rsi) ::
        FinalIR.Ldr("", rax, X86ImmediateInt(-POINTER_BYTE_SIZE), rdi) ::
        FinalIR.Ldr("", rax, X86LabelString(sLbl.name), rax) ::
        FinalIR.BranchLink("", X86BranchString("printf")) ::
        FinalIR.Mov("", X86ImmediateInt(0), rax) ::
        FinalIR.BranchLink("", X86BranchString("fflush")) ::
        FinalIR.Pop("", List(rbx)))
  }

  def assemble_printc(): List[FinalIR] = {
    val sLbl = new Label(".L._printc_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(2, sLbl)) ++
      assembleTAC(StringDefinitionTAC("%c", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_printc")) ++
      (FinalIR.Push("", List(rbx)) ::
        FinalIR.Mov("", rax, rdi) ::
        FinalIR.Ldr("", rax, X86LabelString(sLbl.name), rax) ::
        FinalIR.BranchLink("", X86BranchString("printf")) ::
        FinalIR.Mov("", X86ImmediateInt(0), rax) ::
        FinalIR.BranchLink("", X86BranchString("fflush")) ::
        FinalIR.Pop("", List(rbx)))
  }

  def assemble_printi(): List[FinalIR] = {
    val sLbl = new Label(".L._printi_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(2, sLbl)) ++
      assembleTAC(StringDefinitionTAC("%d", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_printi")) ++
      (FinalIR.Push("", List(rbx)) ::
        FinalIR.Mov("", rax, rdi) ::
        FinalIR.Ldr("", rax, X86LabelString(sLbl.name), rax) ::
        FinalIR.BranchLink("", X86BranchString("printf")) ::
        FinalIR.Mov("", X86ImmediateInt(0), rax) ::
        FinalIR.BranchLink("", X86BranchString("fflush")) ::
        FinalIR.Pop("", List(rbx)))
  }

  def assemble_println(): List[FinalIR] = {
    val sLbl = new Label(".L._println_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(0, sLbl)) ++
      assembleTAC(StringDefinitionTAC("", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_println")) ++
      (FinalIR.Push("", List(rbx)) ::
        FinalIR.Ldr("", rax, X86LabelString(sLbl.name), rax) ::
        FinalIR.BranchLink("", X86BranchString("puts")) ::
        FinalIR.Mov("", X86ImmediateInt(0), rax) ::
        FinalIR.BranchLink("", X86BranchString("fflush")) ::
        FinalIR.Pop("", List(rbx)))
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
      (FinalIR.Push("", List(rbx)) ::
        FinalIR.Cmp("", rax, X86ImmediateInt(0)) ::
        FinalIR.Branch("ne", ".L_printb0") ::
        FinalIR.Ldr("", rax, X86LabelString(fLbl.name), rsi) ::
        FinalIR.Branch("", ".L_printb1") ::
        assembleTAC(Label(".L_printb0"))) ++
      (FinalIR.Ldr("", rax, X86LabelString(tLbl.name), rsi) ::
        assembleTAC(Label(".L_printb1"))) ++
      (FinalIR.Ldr("", rsi, X86ImmediateInt(-POINTER_BYTE_SIZE), rdi) ::
        FinalIR.Ldr("", rax, X86LabelString(sLbl.name), rax) ::
        FinalIR.BranchLink("", X86BranchString("printf")) ::
        FinalIR.Mov("", X86ImmediateInt(0), rax) ::
        FinalIR.BranchLink("", X86BranchString("fflush")) ::
        FinalIR.Pop("", List(rbx)))
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
      assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + lbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(2, lbl)) ++
      assembleTAC(StringDefinitionTAC("%d", lbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_readi")) ++
      (FinalIR.Push("", List(rbx)) ::
      FinalIR.StrPre("", rsp, X86ImmediateInt(-POINTER_BYTE_SIZE), rax) ::
      FinalIR.Mov("", rsp, rdi) ::
      FinalIR.Ldr("", null, X86LabelString(lbl.name), rax) ::
      FinalIR.BranchLink("", X86BranchString("scanf")) ::
      FinalIR.Ldr("", rsp, X86ImmediateInt(0), rax) ::
      FinalIR.Add("", X86None(), rsp, X86ImmediateInt(POINTER_BYTE_SIZE), rsp) ::
      FinalIR.Pop("", List(rbx)))
  }

  def assemble_readc(): List[FinalIR] = {
    val lbl = new Label(".L._readc_str0")
      assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + lbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(3, lbl)) ++ 
      assembleTAC(StringDefinitionTAC(" %c", lbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_readc")) ++
      (FinalIR.Push("", List(rbx)) :: 
      FinalIR.StrPre("b", rsp, X86ImmediateInt(-1), rax) ::
      FinalIR.Mov("", rsp, rdi) :: 
      FinalIR.Ldr("", null, X86LabelString(lbl.name), rax) ::
      FinalIR.BranchLink("", X86BranchString("scanf")) ::
      FinalIR.Ldr("sb", rsp, X86ImmediateInt(0), rax) ::
      FinalIR.Add("", X86None(), rsp, X86ImmediateInt(1), rsp) ::
      FinalIR.Pop("", List(rbx)))
  }

}
