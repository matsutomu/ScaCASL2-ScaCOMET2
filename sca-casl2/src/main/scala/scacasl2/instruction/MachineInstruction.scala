package scacasl2.instruction

import scacasl2.operand._

/** Machine Instruction
  * NOP, LD, ST etc.
  *
  * @param code
  * @param ope
  * @param info
  */
case class MachineInstruction(code: String,
                              ope: Operand,
                              info: InstructionInfo,
                              scope: String)
    extends Instruction

/** For valid Machine Instructions code search
  *
  */
object MachineInstruction {

  val NOP = "NOP"

  val LD = "LD"
  val LD2 = "LD2"
  val LD1 = "LD1"
  val ST = "ST"
  val LAD = "LAD"
  val ADDA = "ADDA"
  val ADDA2 = "ADDA2"
  val ADDA1 = "ADDA1"
  val SUBA = "SUBA"
  val SUBA2 = "SUBA2"
  val SUBA1 = "SUBA1"
  val ADDL = "ADDL"
  val ADDL2 = "ADDL2"
  val ADDL1 = "ADDL1"
  val SUBL = "SUBL"
  val SUBL2 = "SUBL2"
  val SUBL1 = "SUBL1"

  val AND = "AND"
  val AND2 = "AND2"
  val AND1 = "AND1"
  val OR = "OR"
  val OR2 = "OR2"
  val OR1 = "OR1"
  val XOR = "XOR"
  val XOR2 = "XOR2"
  val XOR1 = "XOR1"

  val CPA = "CPA"
  val CPA2 = "CPA2"
  val CPA1 = "CPA1"
  val CPL = "CPL"
  val CPL2 = "CPL2"
  val CPL1 = "CPL1"

  val SLA = "SLA"
  val SRA = "SRA"
  val SLL = "SLL"
  val SRL = "SRL"

  val JMI = "JMI"
  val JNZ = "JNZ"
  val JZE = "JZE"
  val JUMP = "JUMP"
  val JPL = "JPL"
  val JOV = "JOV"

  val PUSH = "PUSH"
  val POP = "POP"

  val CALL = "CALL"
  val RET = "RET"

  val SVC = "SVC"

  val instructions = Set(NOP,
                         LD,
                         ST,
                         LAD,
                         LD,
                         ADDA,
                         SUBA,
                         ADDL,
                         SUBL,
                         AND,
                         OR,
                         XOR,
                         CPA,
                         CPL,
                         SLA,
                         SRA,
                         SLL,
                         JMI,
                         JNZ,
                         JZE,
                         JUMP,
                         JPL,
                         JOV,
                         PUSH,
                         POP,
                         CALL,
                         RET,
                         SVC)


}
