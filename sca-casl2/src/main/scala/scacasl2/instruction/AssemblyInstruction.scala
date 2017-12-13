package scacasl2.instruction

import scacasl2.operand._

/** Assembly Instruction
  *  START, END, DS, DC
  * @param code
  * @param ope
  * @param info
  */
case class AssemblyInstruction(code: String,
                               ope: Operand,
                               info: InstructionInfo,
                               scope: String)
    extends Instruction {

  override def wordSize: Int = {
    val addWord: Int = ope match {
      case o: OperandDs => o.decimal
      case o: OperandDc => {
        o.consts.foldLeft(0)((acc, e) => {
          acc + (e match {
            case eo: ConstsNumOfOperand => 1
            case es: ConstsStringOfOperand => es.array_char.size.toInt
            case el: LabelOfOperand => 1
            case _ => 0
          })
        })
      }
      case _ => 0
    }
    info.wordSize + addWord
  }

}

/** For valid Assembly Instructions code search
  *
  */
object AssemblyInstruction {

  val START = "START"
  val END = "END"
  val DS = "DS"
  val DC = "DC"

  val instructions = Set(START, END, DS, DS)

  def contain(code: String) = instructions.contains(code)

}
