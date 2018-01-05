package scacasl2.instruction

import scacasl2.operand._

/**
  * Assembly Instruction
  * START, END, DS, DC
  * @param code Assembly Instruction Code 
  * @param ope  NoArg, Address, Word Count, Constant
  * @param info ByteCode & Word Size
  * @param scope Label Scope
  */
case class AssemblyInstruction(code: String,
                               ope: Operand,
                               info: InstructionInfo,
                               scope: String)
    extends Instruction {

  override def wordSize: Int =
    info.wordSize +
      (ope match {
        case o: OperandDs => o.decimal
        case o: OperandDc =>
          o.consts.foldLeft(0)((acc, e) => {
            acc + (e match {
              case es: ConstsStringOfOperand => es.array_char.size
              case _: ConstsNumOfOperand     => 1
              case _: LabelOfOperand         => 1
              case _                         => 0
            })
          })
        case _ => 0
      })
}

/**
  * For valid Assembly Instructions code search
  *
  */
object AssemblyInstruction {

  val START = "START"
  val END = "END"
  val DS = "DS"
  val DC = "DC"

  val instructions = Set(START, END, DS, DS)

}
