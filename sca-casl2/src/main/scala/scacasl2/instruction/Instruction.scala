package scacasl2.instruction

import scacasl2.operand._

/**
  *
  * Instruction Operand
  *   - Assembla
  *      START Label
  *      END
  *      DS    decimal_const
  *      DC    const[,const]*
  *
  *   - Macro
  *      IN    Label, Label
  *      OUT   Label, Label
  *      RPUSH
  *      RPOP
  *
  *   - Machine
  *      XXX   GRX,GRX
  *      XXX   GRX,=const,IGR
  *      XXX   =const,IGR
  *      XXX   GRX
  *
  */
trait Instruction {

  val code: String
  val ope: Operand
  val info: InstructionInfo
  val scope: String
  val ERR_NOT_REPLACE_LABEL_ADDRESS = "LABEL not replace address"
  val EXECUTABLE_FILE_START = Array(('C' << 8) | 'A', ('S' << 8) | 'L')

  /**
    * For DS and DC. The Word Size not fix size.
    *
    * @return
    */
  def wordSize: Int = info.wordSize

  /**
    * get word array[Int]. 32bit length.
    *
    * ex.
    *   MAIN START BEGIN
    *   ↓ Int(= 32bit)
    *   '0000 0000 0000 0000 ' '0100 0011' (= 'C') + '0100 0001' (= 'A')
    *   '0000 0000 0000 0000 ' '0101 0011' (= 'S') + '0100 1100' (= 'L')
    *   '0000 0000 0000 0000 ' '0123 4567 89AB CDEF' (= address value)
    *   '0000 0000 0000 0000 ' '0000 0000 0000 0000' (= Filler1)
    *   '0000 0000 0000 0000 ' '0000 0000 0000 0000' (= Filler2)
    *   '0000 0000 0000 0000 ' '0000 0000 0000 0000' (= Filler3)
    *   '0000 0000 0000 0000 ' '0000 0000 0000 0000' (= Filler4)
    *   '0000 0000 0000 0000 ' '0000 0000 0000 0000' (= Filler5)
    *
    *   RET
    *   '0000 0000 0000 0000 ' '1000 0001' (= 0x81) + '0000 0000' (= all 0)
    *
    *   ADDA GR1, GR2
    *   '0000 0000 0000 0000 ' '0010 0100' (= 0x24) + '0001 0002' (= GR1 , GR2)
    *
    *
    *
    * @param symbolTbl for LABEL to Address
    * @return
    */
  def convertToWords(symbolTbl: Map[String, Int]): Array[Int] =
    // #todo refactor
    this.ope match {
      case o: OperandStart => if (o.includeLabel) {
          symbolTbl
            .get(scope + "." + o.l.get.name)
            .map { adr_value =>
              this.EXECUTABLE_FILE_START ++ Array(adr_value) ++ Array.fill(5)(0)
            }
            .getOrElse(throw new IllegalArgumentException(
              ERR_NOT_REPLACE_LABEL_ADDRESS + s"($code: ${o.l.get.name})"))
        } else {
          this.EXECUTABLE_FILE_START ++ Array.fill(6)(0)
        }
      case _: OperandNoArg => Array(info.byteCode << 8)
      case o: OperandR1R2  => Array(info.byteCode << 8 | (o.r1 << 4 | o.r2))
      case o: OperandR_ADR_X => if (o.includeLabel) {
          val lblName = o.address.asInstanceOf[LabelOfOperand].name
          symbolTbl
            .get(scope + "." + lblName)
            .map { adr_value =>
              Array(info.byteCode << 8 | (o.r << 4 | o.x), adr_value)
            }
            .getOrElse(throw new IllegalArgumentException(
              ERR_NOT_REPLACE_LABEL_ADDRESS + s"($code: $lblName)"))
        } else {
          Array(info.byteCode << 8 | (o.r << 4 | o.x),
                o.address.asInstanceOf[AddressOfOperand].value)
        }
      case o: OperandR => Array(info.byteCode << 8 | o.r << 4)
      case o: OperandADR_X => if (o.includeLabel) {
          val lblName = o.address.asInstanceOf[LabelOfOperand].name
          symbolTbl
            .get(scope + "." + lblName)
            .map { adr_value =>
              Array(info.byteCode << 8 | o.x, adr_value)
            }
            .getOrElse(throw new IllegalArgumentException(
              ERR_NOT_REPLACE_LABEL_ADDRESS + s"($code: $lblName)"))
        } else {
          Array(info.byteCode << 8 | o.x,
                o.address.asInstanceOf[AddressOfOperand].value)
        }
      case o: OperandInOrOut =>
        Array.concat(
          Array(info.byteCode << 8),
          o.ml.map { l =>
            symbolTbl
              .get(scope + "." + l.name)
              .map { num =>
                num
              }
              .getOrElse(throw new IllegalArgumentException(
                ERR_NOT_REPLACE_LABEL_ADDRESS + s"($code: ${l.name})"))
          }.toArray
        )
      case o: OperandDs => Array.fill(o.decimal)(0)
      case o: OperandDc =>
        o.consts
          .flatMap(c =>
            c match {
              case e: LabelOfOperand =>
                List(
                  symbolTbl.getOrElse(
                    scope + "." + e.name,
                    throw new IllegalArgumentException(
                      ERR_NOT_REPLACE_LABEL_ADDRESS + s"($code: ${e.name})")))
              case e: ConstsNumOfOperand    => List(e.value)
              case e: ConstsStringOfOperand => e.array_char
          })
          .toArray

      case o: OperandADR => if (o.includeLabel) {
          val lblName = o.address.asInstanceOf[LabelOfOperand].name
        
          symbolTbl
            .get(scope + "." + lblName)
            .map { adr_value =>
              Array(info.byteCode << 8 | 0, adr_value)
            }
            .getOrElse(throw new IllegalArgumentException(
              ERR_NOT_REPLACE_LABEL_ADDRESS + s"($code: $lblName)"))
        } else {
          Array(info.byteCode << 8 | 0,
                o.address.asInstanceOf[AddressOfOperand].value)
        }
    }
  
}
