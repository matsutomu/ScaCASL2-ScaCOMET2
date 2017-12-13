package scacasl2.instruction

/** Instruction Information
  *
  * @param byteCode START, END etc. Instruction Literal.
  * @param wordSize Instruction Word Size. Ordinal 1 or 2. But DS or DC is more size.
  */
case class InstructionInfo(byteCode: Int, wordSize: Int)
