package wacc

import wacc.X86AssemblerTypes._
import wacc.RegisterAllocator._
import wacc.TAC._
import wacc.FinalIR.FinalIR

import scala.collection.mutable.ListBuffer

//TODO Change all to x86_64 Architecture, most comments are currently outdated

object X86HelperFunctions {
  private[this] val state = new AssemblerState(ListBuffer(rcx, r8, r9, r10, r11, r12, r13, r14, r15))

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
    (FinalIR.Ldr("", rax, new X86LabelString(".L._errNull_str0"), rax) ::
    FinalIR.BranchLink("", new X86BranchString("_prints")) ::
    FinalIR.Mov("", new X86ImmediateInt(255), rax) ::
    FinalIR.BranchLink("", new X86BranchString("exit")))
  }

  def assemble_freepair(): List[FinalIR] = {
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_freepair")) ++
    (FinalIR.Push("", List(rbx)) ::
    FinalIR.Mov("", rax, rdi) ::
    FinalIR.Cmp("", rdi, new X86ImmediateInt(0)) ::
    FinalIR.BranchLink("e", new X86BranchString("_errNull")) ::
    FinalIR.Ldr("", rdi, new X86ImmediateInt(0), rax) ::
    FinalIR.Push("", List(rdi)) ::
    FinalIR.BranchLink("", new X86BranchString("free")) ::
    FinalIR.Pop("", List(rdi)) ::
    FinalIR.Ldr("", rdi, new X86ImmediateInt(POINTER_BYTE_SIZE), rax) ::
    FinalIR.Push("", List(rdi)) ::
    FinalIR.BranchLink("", new X86BranchString("free")) ::
    FinalIR.Pop("", List(rdi)) ::
    FinalIR.Mov("", rdi, rax) ::
    FinalIR.Push("", List(rdi)) ::
    FinalIR.BranchLink("", new X86BranchString("free")) ::
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
    (FinalIR.Ldr("", null, new X86LabelString(sLbl.name), rax) ::
    FinalIR.BranchLink("", new X86BranchString("_prints")) ::
    FinalIR.Mov("", new X86ImmediateInt(255), rax) ::
    FinalIR.BranchLink("", new X86BranchString("exit")))
  }

  def assemble_errOverflow(): List[FinalIR] = {
    val sLbl = Label(".L._errOverflow_str0")
    assembleTAC(DataSegmentTAC()) ++
    assembleTAC(Comments("length of " + sLbl.name)) ++
    assembleTAC(StringLengthDefinitionTAC(52, sLbl)) ++
    assembleTAC(StringDefinitionTAC("fatal error: integer overflow or underflow\n", sLbl)) ++
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_errOverflow")) ++
    (FinalIR.Ldr("", null, new X86LabelString(sLbl.name), rax) ::
    FinalIR.BranchLink("", new X86BranchString("_prints")) ::
    FinalIR.Mov("", new X86ImmediateInt(255), rax) ::
    FinalIR.BranchLink("", new X86BranchString("exit")))
  }

  // Special calling convention: array ptr passed in R9, index in R10, and return into R9
  // ie r9 = r9[r10] (from reference compiler)
  // we instead use rax = rdx[rsi]
  def assemble_arrLoad(): List[FinalIR] = {
    assembleTAC(Label("_arrLoad")) ++
    (FinalIR.Push("", List(rbx)) ::
      FinalIR.Cmp("", rsi, new X86ImmediateInt(0)) ::
      FinalIR.Mov("l", rsi, rdi) ::
      FinalIR.BranchLink("l", new X86BranchString("_boundsCheck")) ::
      FinalIR.Ldr("", rdx, new X86ImmediateInt(-POINTER_BYTE_SIZE), rbx) ::
      FinalIR.Cmp("", rsi, rbx) ::
      FinalIR.Mov("ge", rsi, rdi) ::
      FinalIR.BranchLink("ge", new X86BranchString("_boundsCheck")) ::
      FinalIR.Ldr("sx", rdx, AssemblerTypes.LogicalShiftLeft(rsi, Right(2)), rax) ::
      FinalIR.Pop("", List(rbx)) ::
      FinalIR.Ret())
  }

  // Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
  // ie r9[r10] = rax (from reference compiler)
  // we instead use rdx[rax] = rsi
  def assemble_arrStore(): List[FinalIR] = {
    assembleTAC(Label("_arrStore")) ++
    (FinalIR.Push("", List(rbx)) ::
      FinalIR.Cmp("", rax, new X86ImmediateInt(0)) ::
      FinalIR.Mov("l", rax, rdi) :: // rax < 0
      FinalIR.BranchLink("l", new X86BranchString("_boundsCheck")) ::
      FinalIR.Ldr("", rdx, new X86ImmediateInt(-POINTER_BYTE_SIZE), rbx) ::
      FinalIR.Cmp("", rax, rbx) ::
      FinalIR.Mov("ge", rax, rdi) :: // rax >= rbx
      FinalIR.BranchLink("ge", new X86BranchString("_boundsCheck")) ::
      FinalIR.Str("", AssemblerTypes.LogicalShiftLeft(rax, Right(2)), rsi, rdx) :: // TODO: Logical shift does not work
      FinalIR.Pop("", List(rbx)) ::
      FinalIR.Ret())
  }

  def assemble_boundsCheck(): List[FinalIR] = {
    val sLbl = new Label(".L._boundsCheck_str_0")
    assembleTAC(DataSegmentTAC()) ++
    assembleTAC(Comments("length of " + sLbl.name)) ++
    assembleTAC(StringLengthDefinitionTAC(42, sLbl)) ++
    assembleTAC(StringDefinitionTAC("fatal error: array index %d out of bounds\n", sLbl)) ++
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_boundsCheck")) ++
    (FinalIR.And("", rsp, new X86ImmediateInt(-16)) ::
      FinalIR.Ldr("", null, new X86LabelString(sLbl.name), rax) ::
      FinalIR.BranchLink("", new X86BranchString("printf@plt")) ::
      FinalIR.Mov("", new X86ImmediateInt(0), rax) ::
      FinalIR.BranchLink("", new X86BranchString("fflush@plt")) ::
      FinalIR.Mov("", new X86ImmediateInt(-1), rdi) ::
      FinalIR.BranchLink("", new X86BranchString("exit@plt")))
  }

  def assemble_malloc(): List[FinalIR] = {
    FinalIR.Push("", List(rbp)) ::
      FinalIR.Mov("", rsp, rbp) ::
      FinalIR.And("", rsp, new X86ImmediateInt(-16)) ::
      FinalIR.BranchLink("", new X86BranchString("malloc@plt")) ::
      FinalIR.Mov("", rbp, rsp) ::
      FinalIR.Pop("", List(rbp)) ::
      FinalIR.Ret()
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
      (FinalIR.Push("", List(rbp)) ::
        FinalIR.Mov("", rsp, rbp) ::
        FinalIR.And("", rsp, new X86ImmediateInt(-16)) ::
        FinalIR.Mov("", rdi, rsi) ::
        FinalIR.Ldr("", null, new X86LabelString(sLbl.name), rdx) ::
        FinalIR.Mov("", new X86ImmediateInt(0), rax) ::
        FinalIR.BranchLink("", new X86BranchString("printf@plt")) ::
        FinalIR.Mov("", new X86ImmediateInt(0), rdi) ::
        FinalIR.BranchLink("", new X86BranchString("fflush@plt")) ::
        FinalIR.Mov("", rbp, rsp) ::
        FinalIR.Pop("", List(rbp)) ::
        FinalIR.Ret())
   }

  def assemble_prints(): List[FinalIR] = {
    val sLbl = new Label(".L._prints_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(4, sLbl)) ++
      assembleTAC(StringDefinitionTAC("%.*s", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_prints")) ++
      (FinalIR.Push("", List(rbp)) ::
        FinalIR.Mov("", rsp, rbp) ::
        FinalIR.And("", rsp, new X86ImmediateInt(-16)) ::
        FinalIR.Mov("", rdi, rdx) ::
        FinalIR.Str("", rdi, new X86ImmediateInt(-4), rsi) ::
        FinalIR.Ldr("", null, new X86LabelString(sLbl.name), rdi) ::
        FinalIR.Mov("", new X86ImmediateInt(0), rax) ::
        FinalIR.BranchLink("", new X86BranchString("printf@plt")) ::
        FinalIR.Mov("", new X86ImmediateInt(0), rdi) ::
        FinalIR.BranchLink("", new X86BranchString("fflush@plt")) ::
        FinalIR.Mov("", rbp, rsp) ::
        FinalIR.Pop("", List(rbp)) ::
        FinalIR.Ret())
  }

  def assemble_printc(): List[FinalIR] = {
    val sLbl = new Label(".L._printc_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(2, sLbl)) ++
      assembleTAC(StringDefinitionTAC("%c", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_printc")) ++
      (FinalIR.Push("", List(rbp)) ::
        FinalIR.Mov("", rsp, rbp) ::
        FinalIR.And("", rsp, new X86ImmediateInt(-16)) ::
        FinalIR.Mov("", rdi, rsi) ::
        FinalIR.Ldr("", null, new X86LabelString(sLbl.name), rdi) ::
        FinalIR.Mov("", new X86ImmediateInt(0), rax) ::
        FinalIR.BranchLink("", new X86BranchString("printf@plt")) ::
        FinalIR.Mov("", new X86ImmediateInt(0), rdi) ::
        FinalIR.BranchLink("", new X86BranchString("fflush@plt")) ::
        FinalIR.Mov("", rbp, rsp) ::
        FinalIR.Pop("", List(rbp)) ::
        FinalIR.Ret())
  }

  def assemble_printi(): List[FinalIR] = {
    val sLbl = new Label(".L._printi_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(2, sLbl)) ++
      assembleTAC(StringDefinitionTAC("%d", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_printi")) ++
      (FinalIR.Push("", List(rbp)) ::
        FinalIR.Mov("", rsp, rbp) ::
        FinalIR.And("", rsp, new X86ImmediateInt(-16)) ::
        FinalIR.Mov("", rdi, rsi) ::
        FinalIR.Ldr("", null, new X86LabelString(sLbl.name), rdi) ::
        FinalIR.Mov("", new X86ImmediateInt(0), rax) ::
        FinalIR.BranchLink("", new X86BranchString("printf@plt")) ::
        FinalIR.Mov("", new X86ImmediateInt(0), rdi) ::
        FinalIR.BranchLink("", new X86BranchString("fflush@plt")) ::
        FinalIR.Mov("", rbp, rsp) ::
        FinalIR.Pop("", List(rbp)) ::
        FinalIR.Ret())
  }

  def assemble_println(): List[FinalIR] = {
    val sLbl = new Label(".L._println_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(0, sLbl)) ++
      assembleTAC(StringDefinitionTAC("", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_println")) ++
      (FinalIR.Push("", List(rbp)) ::
        FinalIR.Mov("", rsp, rbp) ::
        FinalIR.And("", rsp, new X86ImmediateInt(-16)) ::
        FinalIR.Ldr("", null, new X86LabelString(sLbl.name), rdi) ::
        FinalIR.BranchLink("", new X86BranchString("printf@plt")) ::
        FinalIR.Mov("", new X86ImmediateInt(0), rdi) ::
        FinalIR.BranchLink("", new X86BranchString("fflush@plt")) ::
        FinalIR.Mov("", rbp, rsp) ::
        FinalIR.Pop("", List(rbp)) ::
        FinalIR.Ret())
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
      (FinalIR.Push("", List(rbp)) ::
        FinalIR.Mov("", rsp, rbp) ::
        FinalIR.And("", rsp, new X86ImmediateInt(-16)) ::
        FinalIR.Cmp("", rdi, new X86ImmediateInt(0)) ::
        FinalIR.Branch("ne", ".L_printb0") ::
        FinalIR.Ldr("", null, new X86LabelString(fLbl.name), rdx) ::
        FinalIR.Branch("mp", ".L_printb1") ::
        assembleTAC(Label(".L_printb0"))) ++
      (FinalIR.Ldr("", null, new X86LabelString(tLbl.name), rdx) ::
        assembleTAC(Label(".L_printb1"))) ++
      (FinalIR.Ldr("", rsi, new X86ImmediateInt(-POINTER_BYTE_SIZE), rdx) ::
        FinalIR.Ldr("", null, new X86LabelString(sLbl.name), rax) ::
        FinalIR.Mov("", new X86ImmediateInt(0), rax) ::
        FinalIR.BranchLink("", new X86BranchString("printf@plt")) ::
        FinalIR.Mov("", new X86ImmediateInt(0), rdi) ::
        FinalIR.BranchLink("", new X86BranchString("fflush@plt")) ::
        FinalIR.Mov("", rbp, rsp) ::
        FinalIR.Pop("", List(rbp)) ::
        FinalIR.Ret())
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
      (FinalIR.Push("", List(rbp)) :: 
        FinalIR.Mov("", rsp, rbp) ::
        FinalIR.And("", rsp, new X86ImmediateInt(-16)) ::
        FinalIR.Sub("", new X86None(), rsp, new X86ImmediateInt(16), rsp) ::
        FinalIR.StrPre("", rsp, null, rax) ::
        FinalIR.Lea("", rsp, rsi) :: 
        FinalIR.Ldr("", null, new X86LabelString(lbl.name), rdi) ::
        FinalIR.Mov("", new X86ImmediateInt(0), rax) ::
        FinalIR.BranchLink("", new X86BranchString("scanf@plt")) ::
        FinalIR.Ldr("sx", rsp, null, rax) ::
        FinalIR.Add("", new X86None(), rsp, new X86ImmediateInt(16), rsp) ::
        FinalIR.Mov("", rbp, rsp) ::
        FinalIR.Pop("", List(rbp)) ::
        FinalIR.Ret())
  }

  def assemble_readc(): List[FinalIR] = {
    val lbl = new Label(".L._readc_str0")
      assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + lbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(3, lbl)) ++ 
      assembleTAC(StringDefinitionTAC(" %c", lbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_readc")) ++
      (FinalIR.Push("", List(rbp)) :: 
        FinalIR.Mov("", rsp, rbp) ::
        FinalIR.And("", rsp, new X86ImmediateInt(-16)) ::
        FinalIR.Sub("", new X86None(), rsp, new X86ImmediateInt(16), rsp) ::
        FinalIR.StrPre("", rsp, null, rax) ::
        FinalIR.Lea("", rsp, rsi) :: 
        FinalIR.Ldr("", null, new X86LabelString(lbl.name), rdi) ::
        FinalIR.Mov("", new X86ImmediateInt(0), rax) ::
        FinalIR.BranchLink("", new X86BranchString("scanf@plt")) ::
        FinalIR.Ldr("sx", rsp, null, rax) ::
        FinalIR.Add("", new X86None(), rsp, new X86ImmediateInt(16), rsp) ::
        FinalIR.Mov("", rbp, rsp) ::
        FinalIR.Pop("", List(rbp)) ::
        FinalIR.Ret())
  }

}
