package wacc

import wacc.AssemblerTypes._
import wacc.RegisterAllocator._
import wacc.TAC._

import scala.collection.mutable.ListBuffer

class HelperFunctions extends Assembler {
  private[this] val state = new AssemblerState(ListBuffer(r4, r5, r6, r7, r8, r9, r10, r11))

  implicit private[this] def toStrings(state: AssemblerState) = state.code.toList

  implicit private[this] def updateState(str: String): AssemblerState = state.addInstruction(str)

  def assemble_errNull(): List[String] = {
    val sLbl = new Label(".L._errNull_str0")
    assembleTAC(DataSegmentTAC()) ++
    assembleTAC(Comments("length of " + sLbl.name)) ++
    assembleTAC(StringLengthDefinitionTAC(45, sLbl)) ++
    assembleTAC(StringDefinitionTAC("fatal error: null pair dereferenced or freed\n", sLbl)) ++
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_errNull")) ::
    assembleLdr("", r0, r0, new LabelString(".L._errNull_str0")) ::
    assembleBranchLink("", new BranchString("_prints")) ::
    assembleMove("", r0, new ImmediateInt(255)) ::
    assembleBranchLink("", new BranchString("exit"))
  }

  def assemble_freepair(): List[String] = {
    assembleTAC(TextSegmentTAC()) ::
    assembleTAC(Label("_freepair")) ::
      assemblePush("", List(lr)) ::
      assembleMove("", r1, r0) ::
      assembleCompare("", r1, ImmediateInt(0)) ::
      assembleBranchLink("eq", new BranchString("_errNull")) ::
      assembleLdr("", r0, r1, ImmediateInt(0)) ::
      assemblePush("", List(r1)) ::
      assembleBranchLink("", new BranchString("free")) ::
      assemblePop("", List(r1)) ::
      assembleLdr("", r0, r1, ImmediateInt(4)) ::
      assemblePush("", List(r1)) ::
      assembleBranchLink("", new BranchString("free")) ::
      assemblePop("", List(r1)) ::
      assembleMove("", r0, r1) ::
      assemblePush("", List(r1)) ::
      assembleBranchLink("", new BranchString("free")) ::
      assemblePop("", List(r1)) ::
      assemblePop("", List(pc))
  }

  
  def assemble_errDivZero(): List[String] = {
    val sLbl = Label(".L._errDivZero_str0")
    assembleTAC(DataSegmentTAC()) ++
    assembleTAC(Comments("length of " + sLbl.name)) ++
    assembleTAC(StringLengthDefinitionTAC(40, sLbl)) ++
    assembleTAC(StringDefinitionTAC("fatal error: division or modulo by zero\n", sLbl)) ++
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_errDivZero")) ::
      assembleLdr("", r0, null, LabelString(sLbl.name)) ::
        assembleBranchLink("", BranchString("_prints")) ::
        assembleMove("", r0, ImmediateInt(255))
        assembleBranchLink("", BranchString("exit"))
  }

  def assemble_errOverflow(): List[String] = {
    val sLbl = Label(".L._errOverflow_str0")
    assembleTAC(DataSegmentTAC()) ++
    assembleTAC(Comments("length of " + sLbl.name)) ++
    assembleTAC(StringLengthDefinitionTAC(52, sLbl)) ++
    assembleTAC(StringDefinitionTAC("fatal error: integer overflow or underflow\n", sLbl)) ++
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_errOverflow")) ::
    assembleLdr("", r0, null, LabelString(sLbl.name)) ::
    assembleBranchLink("", new BranchString("_prints")) ::
    assembleMove("", r0 , new ImmediateInt(255)) ::
    assembleBranchLink("", new BranchString("exit"))
  }

  // r2 = r3[r0]
  def assemble_arrLoad(): List[String] = {
    assembleTAC(Label("_arrLoad")) ++
    (assemblePush("", List(lr)) ::
      assembleCompare("", r0, new ImmediateInt(0)) ::
      assembleMove("", r1, r0) ::
      assembleBranchLink("lt", new BranchString("_boundsCheck")) ::
      assembleLdr("", lr, r3, new ImmediateInt(-4)) ::
      assembleCompare("eq", r0, lr) ::
      assembleMove("ge", r1, r0) ::
      assembleBranchLink("ge", new BranchString("_boundsCheck")) ::
      assembleLdr("", r2, r3, LogicalShiftLeft(r0, Right(2))) ::
      assemblePop("", List(pc)))
  }

  // r3[r0] = r2
  def assemble_arrStore(): List[String] = {
    assembleTAC(Label("_arrStore")) ++
    (assemblePush("", List(lr)) ::
      assembleCompare("", r0, new ImmediateInt(0)) ::
      assembleMove("", r1, r0) ::
      assembleBranchLink("lt", new BranchString("_boundsCheck")) ::
      assembleLdr("", lr, r3, new ImmediateInt(-4)) ::
      assembleCompare("eq", r0, lr) ::
      assembleMove("ge", r1, r0) ::
      assembleBranchLink("ge", new BranchString("_boundsCheck")) ::
      assembleStr("", r2, r3, LogicalShiftLeft(r0, Right(2))) ::
      assemblePop("", List(pc)))
  }

  def assemble_boundsCheck(): List[String] = {
    val sLbl = new Label(".L._boundsCheck_str_0")
    assembleTAC(DataSegmentTAC()) ++
    assembleTAC(Comments("length of " + sLbl.name)) ++
    assembleTAC(StringLengthDefinitionTAC(42, sLbl)) ++
    assembleTAC(StringDefinitionTAC("fatal error: array index %d out of bounds\n", sLbl)) ++
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_boundsCheck")) ++
    (assembleLdr("", r0, r0, new LabelString(sLbl.name)) ::
      assembleBranchLink("", new BranchString("printf")) ::
      assembleMove("", r0, new ImmediateInt(0)) ::
      assembleBranchLink("", new BranchString("fflush")) ::
      assembleMove("", r0, new ImmediateInt(255)) ::
      assembleBranchLink("", new BranchString("exit")))
  }

  def assemble_print(pType: String): List[String] = {
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

   def assemble_printp(): List[String] = {
    val sLbl = new Label(".L._printp_str0")
    assembleTAC(DataSegmentTAC()) ++
    assembleTAC(Comments("length of " + sLbl.name)) ++
    assembleTAC(StringLengthDefinitionTAC(2, sLbl)) ++
    assembleTAC(StringDefinitionTAC("%p", sLbl)) ++
    assembleTAC(TextSegmentTAC()) ++
    assembleTAC(Label("_printp")) ++
    (assemblePush("", List(lr)) ::
        assembleMove("", r1, r0) ::
        assembleLdr("", r0, r0, new LabelString(sLbl.name)) ::
        assembleBranchLink("", new BranchString("printf")) ::
        assembleMove("", r0, new ImmediateInt(0)) ::
        assembleBranchLink("", new BranchString("fflush")) ::
        assemblePop("", List(pc)))
   }

  def assemble_prints(): List[String] = {
    val sLbl = new Label(".L._prints_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(4, sLbl)) ++
      assembleTAC(StringDefinitionTAC("%.*s", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_prints")) ++
      (assemblePush("", List(lr)) ::
        assembleMove("", r2, r0) ::
        assembleLdr("", r1, r0, ImmediateInt(-4)) ::
        assembleLdr("", r0, r0, new LabelString(sLbl.name)) ::
        assembleBranchLink("", new BranchString("printf")) ::
        assembleMove("", r0, new ImmediateInt(0)) ::
        assembleBranchLink("", new BranchString("fflush")) ::
        assemblePop("", List(pc)))
  }

  def assemble_printc(): List[String] = {
    val sLbl = new Label(".L._printc_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(2, sLbl)) ++
      assembleTAC(StringDefinitionTAC("%c", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_printc")) ++
      (assemblePush("", List(lr)) ::
        assembleMove("", r1, r0) ::
        assembleLdr("", r0, r0, new LabelString(sLbl.name)) ::
        assembleBranchLink("", new BranchString("printf")) ::
        assembleMove("", r0, new ImmediateInt(0)) ::
        assembleBranchLink("", new BranchString("fflush")) ::
        assemblePop("", List(pc)))
  }

  def assemble_printi(): List[String] = {
    val sLbl = new Label(".L._printi_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(2, sLbl)) ++
      assembleTAC(StringDefinitionTAC("%d", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_printi")) ++
      (assemblePush("", List(lr)) ::
        assembleMove("", r1, r0) ::
        assembleLdr("", r0, r0, new LabelString(sLbl.name)) ::
        assembleBranchLink("", new BranchString("printf")) ::
        assembleMove("", r0, new ImmediateInt(0)) ::
        assembleBranchLink("", new BranchString("fflush")) ::
        assemblePop("", List(pc)))
  }

  def assemble_println(): List[String] = {
    val sLbl = new Label(".L._println_str0")
    assembleTAC(DataSegmentTAC()) ++
      assembleTAC(Comments("length of " + sLbl.name)) ++
      assembleTAC(StringLengthDefinitionTAC(0, sLbl)) ++
      assembleTAC(StringDefinitionTAC("", sLbl)) ++
      assembleTAC(TextSegmentTAC()) ++
      assembleTAC(Label("_println")) ++
      (assemblePush("", List(lr)) ::
        assembleLdr("", r0, r0, new LabelString(sLbl.name)) ::
        assembleBranchLink("", new BranchString("puts")) ::
        assembleMove("", r0, new ImmediateInt(0)) ::
        assembleBranchLink("", new BranchString("fflush")) ::
        assemblePop("", List(pc)))
  }

  def assemble_printb(): List[String] = {
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
      (assemblePush("", List(lr)) ::
        assembleCompare("", r0, new ImmediateInt(0)) ::
        assembleBranch("ne", ".L_printb0") ::
        assembleLdr("", r2, r0, new LabelString(fLbl.name)) ::
        assembleBranch("", ".L_printb1") ::
        assembleTAC(Label(".L_printb0"))) ++
      (assembleLdr("", r2, r0, new LabelString(tLbl.name)) ::
        assembleTAC(Label(".L_printb1"))) ++
      (assembleLdr("", r1, r2, ImmediateInt(-4)) ::
        assembleLdr("", r0, r0, new LabelString(sLbl.name)) ::
        assembleBranchLink("", new BranchString("printf")) ::
        assembleMove("", r0, new ImmediateInt(0)) ::
        assembleBranchLink("", new BranchString("fflush")) ::
        assemblePop("", List(pc)))
  }

  def assemble_read(rType: String): List[String] = {
    rType match {
      case "_readi" => assemble_readi()
      case "_readc" => assemble_readc()
      case _ => assemble_readi()
    }
  }

  def assemble_readi(): List[String] = {
    val lbl = new Label(".L._readi_str0")
    List(
      DataSegmentTAC(),
      Comments("length of " + lbl.name),
      StringLengthDefinitionTAC(2, lbl),
      StringDefinitionTAC("%d", lbl),
      TextSegmentTAC(),
      Label("_readi")).map(tac => assembleTAC(tac))

      assemblePush("", List(lr))
      assembleStrPre("", r0, sp, new ImmediateInt(-4))
      assembleMove("", r1, sp)
      assembleLdr("", r0, null, new LabelString(lbl.name))
      assembleBranchLink("", new BranchString("scanf"))
      assembleLdr("", r0, sp, new ImmediateInt(0))
      assembleAdd("", None(), sp, sp, new ImmediateInt(4))
      assemblePop("", List(pc))
  }

  def assemble_readc(): List[String] = {
    val lbl = new Label(".L._readc_str0")
    List(
      DataSegmentTAC(),
      Comments("length of " + lbl.name),
      StringLengthDefinitionTAC(3, lbl),
      StringDefinitionTAC(" %c", lbl),
      TextSegmentTAC(),
      Label("_readc")).map(tac => assembleTAC(tac))

      assemblePush("", List(lr))
      assembleStrPre("b", r0, sp, new ImmediateInt(-1))
      assembleMove("", r1, sp)
      assembleLdr("", r0, null, new LabelString(lbl.name))
      assembleBranchLink("", new BranchString("scanf"))
      assembleLdr("sb", r0, sp, new ImmediateInt(0))
      assembleAdd("", None(), sp, sp, new ImmediateInt(1))
      assemblePop("", List(pc))
  }

}
