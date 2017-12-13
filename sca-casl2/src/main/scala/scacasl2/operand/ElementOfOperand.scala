package scacasl2.operand

/**
  * Element Of Operand
  *  Label, Address, Const(Decimal, Hex, String etc), Register
  *
  */
trait ElementOfOperand
trait ElementOfAddressOperand { def adrValue: Int }
trait ElementOfDcInstruction

/**
  * AddressOfOperand
  *  Machine Instruction use the operand.
  * @param adr decimal,Hex,Label,Literal(=XXXX etc)
  * @param value only address(0 - 65535)
  */
case class AddressOfOperand(adr: String, value: Int)
    extends ElementOfOperand
    with ElementOfAddressOperand {
  def adrValue = { this.value }
}

/**
  * Label
  *  Assembly Instruction, Macro Instruction and Machine Instruction use the operand.
  * @param name Label literal
  * @param value only address(0 - 65535)
  */
case class LabelOfOperand(name: String, value: Option[Int])
    extends ElementOfOperand
    with ElementOfAddressOperand
    with ElementOfDcInstruction {
  def adrValue = value.getOrElse(0)
}

/**
  * Constants Number
  * Assembly Instruction use the operand. Only DC Instruction.
  * @param literal decimal, hex
  * @param value
  */
case class ConstsNumOfOperand(literal: String, value: Int)
    extends ElementOfOperand
    with ElementOfDcInstruction

/**
  * Constants String
  * Assembly Instruction use the operand. Only DC Instruction.
  * @param literal String
  * @param array_char
  */
case class ConstsStringOfOperand(literal: String, array_char: List[Int])
    extends ElementOfOperand
    with ElementOfDcInstruction

/**
  * Incorrect Description Operands
  *
  * @param literal String
  */
case class IncorrectDescription(literal: String)
    extends ElementOfOperand
    with ElementOfAddressOperand
    with ElementOfDcInstruction {
  def adrValue = 0
}
