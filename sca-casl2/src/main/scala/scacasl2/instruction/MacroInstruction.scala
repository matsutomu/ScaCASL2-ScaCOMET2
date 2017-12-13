package scacasl2.instruction

import scacasl2.operand._

/** Macro Instruction
  * IN, OUT, RPUSH, RPOP
  *
  * @param code
  * @param ope
  * @param info
  */
case class MacroInstruction(code: String,
                            ope: Operand,
                            info: InstructionInfo,
                            scope: String)
    extends Instruction

/** For valid Macro Instructions code search
  *
  */
object MacroInstruction {

  val IN = "IN"
  val OUT = "OUT"
  val RPUSH = "RPUSH"
  val RPOP = "RPOP"

  val instructions = Set(IN, OUT, RPUSH, RPOP)

  def contain(code: String) = instructions.contains(code)

}
