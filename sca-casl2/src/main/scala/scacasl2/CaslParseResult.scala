package scacasl2

import scacasl2.instruction.Instruction

/**
 * Public parse result
 *
 * @param instructions
 * @param symbolTable
 * @param errors
 */
case class CaslParseResult(instructions: List[InstructionRichInfo],
                           symbolTable: Map[String, Int],
                           errors: List[ParseError]) {
  def isValid = this.errors.isEmpty

  def instructionModels = instructions.map { _.model }

}

case class InstructionRichInfo(line: Option[InstructionLine], model: Instruction)

case class ParseError(lineNumber: Int,
                      msg: String,
                      detailMsg: String,
                      line: ProgramLine)