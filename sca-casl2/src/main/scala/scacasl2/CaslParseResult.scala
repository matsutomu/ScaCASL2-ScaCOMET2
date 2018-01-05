package scacasl2

import scacasl2.instruction.Instruction

/**
  * Public parse result
  *
  * @param instructions Instruction Lines And More Information
  * @param symbolTable Label & Address
  * @param errors Error Messages 
  *               
  */
case class CaslParseResult(instructions: List[InstructionRichInfo],
                           symbolTable: Map[String, Int],
                           errors: List[ParseError]) {
  def isValid: Boolean = this.errors.isEmpty

  def instructionModels: List[Instruction] = instructions.map { _.model }

}

case class InstructionRichInfo(line: InstructionLine, model: Instruction)

case class ParseError(lineNumber: Int,
                      msg: String,
                      detailMsg: String,
                      rawLine: String)
