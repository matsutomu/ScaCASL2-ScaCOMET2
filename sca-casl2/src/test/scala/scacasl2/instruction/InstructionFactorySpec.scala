package scacasl2.instruction

import scacasl2.operand._

import org.scalatest._


class InstructionFactorySpec extends FlatSpec with DiagrammedAssertions {

  "InstructionFactory" can " parse Machine Code and operands " in {
    val nop = InstructionFactory.parseOperand("NOP",List.empty,"")
    assert(nop.right.get === 
      MachineInstruction("NOP", OperandNoArg(), InstructionInfo(0x00, 1), ""))


  }

  "InstructionFactory" can " parse Machine Code and operands (error)" in {
    assertNoGoodOperand(InstructionFactory.parseOperand("NOP",List("GR0", "#1"),""),
      "No Good Operands (NOP: GR0,#1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("LD",List("#1"),""),
      "No Good Operands (LD: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("LD",List.empty,""),
      "No Good Operands (LD: )")

    assertNoGoodOperand(InstructionFactory.parseOperand("ST",List("#1"),""),
      "No Good Operands (ST: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("LAD",List("#1"),""),
      "No Good Operands (LAD: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("ADDA",List("#1"),""),
      "No Good Operands (ADDA: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("ADDL",List("#1"),""),
      "No Good Operands (ADDL: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("SUBA",List("#1"),""),
      "No Good Operands (SUBA: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("SUBL",List("#1"),""),
      "No Good Operands (SUBL: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("AND",List("#1"),""),
      "No Good Operands (AND: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("OR",List("#1"),""),
      "No Good Operands (OR: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("XOR",List("#1"),""),
      "No Good Operands (XOR: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("CPA",List("#1"),""),
      "No Good Operands (CPA: #1)")
    assertNoGoodOperand(InstructionFactory.parseOperand("CPL",List("#1"),""),
      "No Good Operands (CPL: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("SLA",List("#1"),""),
      "No Good Operands (SLA: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("SRA",List("#1"),""),
      "No Good Operands (SRA: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("SLL",List("#1"),""),
      "No Good Operands (SLL: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("SRL",List("#1"),""),
      "No Good Operands (SRL: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("JPL",List("GR0"),""),
      "No Good Operands (JPL: GR0)")

    assertNoGoodOperand(InstructionFactory.parseOperand("JMI",List("GR0"),""),
      "No Good Operands (JMI: GR0)")

    assertNoGoodOperand(InstructionFactory.parseOperand("JNZ",List("GR0"),""),
      "No Good Operands (JNZ: GR0)")

    assertNoGoodOperand(InstructionFactory.parseOperand("JZE",List("GR0"),""),
      "No Good Operands (JZE: GR0)")

    assertNoGoodOperand(InstructionFactory.parseOperand("JOV",List("GR0"),""),
      "No Good Operands (JOV: GR0)")

    assertNoGoodOperand(InstructionFactory.parseOperand("JUMP",List("GR0"),""),
      "No Good Operands (JUMP: GR0)")

    assertNoGoodOperand(InstructionFactory.parseOperand("PUSH",List("GR0"),""),
      "No Good Operands (PUSH: GR0)")

    assertNoGoodOperand(InstructionFactory.parseOperand("POP",List("#1"),""),
      "No Good Operands (POP: #1)")

    assertNoGoodOperand(InstructionFactory.parseOperand("CALL",List("GR0"),""),
      "No Good Operands (CALL: GR0)")

    assertNoGoodOperand(InstructionFactory.parseOperand("RET",List("#1"),""),
      "No Good Operands (RET: #1)")
  }

  
  def assertNoGoodOperand(either: Either[String, Instruction], errMsg: String): Unit = {
    assert(either.left.get === errMsg)

  }
}
