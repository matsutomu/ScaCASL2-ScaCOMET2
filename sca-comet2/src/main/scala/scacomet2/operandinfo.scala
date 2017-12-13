package scacomet2

sealed abstract class InstructionsOfAorL
object InstructionsOfAorL {
  case object ArithmeticInstruct extends InstructionsOfAorL
  case object LogicalInstruct extends InstructionsOfAorL
  case object OtherInstruct extends InstructionsOfAorL
}

sealed abstract class BinaryNumber
object BinaryNumber {
  case object Zero extends BinaryNumber {
    override def toString: String = "0"
  }
  case object One extends BinaryNumber{
    override def toString: String = "1"
  }
}

sealed abstract class OperandType
object OperandType {
  case object ArgNo extends OperandType
  case object ArgR1R2 extends OperandType
  case object ArgRAdrX extends OperandType
  case object ArgAdrX extends OperandType
  case object ArgR extends OperandType
}
