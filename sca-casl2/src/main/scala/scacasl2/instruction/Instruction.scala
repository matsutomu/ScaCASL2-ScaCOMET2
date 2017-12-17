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

  /** For DS and DC. The Word Size not fix size.
    *
    * @return
    */
  def wordSize: Int = info.wordSize

  /** get word array[Int]. 32bit length.
    *
    * @param symbolTbl for LABEL to Address
    * @return
    */
  def convertToWords(symbolTbl: Map[String, Int]): Array[Int] =
    this.ope match {
      case o: OperandStart => {
        //val code: Array[Int] = ("CASL".splitAt(2) match { case (ca, sl) =>
        //                        List(ca, sl)}).map( s => s.charAt(0).toInt << 8 + s.charAt(1)).toArray
        val code: Array[Int] =
          Array((('C' << 8) | 'A').toInt, (('S' << 8) | 'L').toInt)
        if (o.l.isDefined) {
          symbolTbl
            .get(scope + "." + o.l.get.name)
            .map { adr_value =>
              code ++ Array(adr_value) ++ Array.fill(5)(0)
            }
            .getOrElse(throw new IllegalArgumentException(
              ERR_NOT_REPLACE_LABEL_ADDRESS + s"($code, $ope)"))
        } else {
          code ++ Array.fill(6)(0)
        }
      }
      case o: OperandNoArg => Array(info.byteCode << 8)
      case o: OperandR1R2 => Array(info.byteCode << 8 | (o.r1 << 4 | o.r2))
      case o: OperandR_ADR_X if (o.address.isInstanceOf[LabelOfOperand]) => {
        symbolTbl
          .get(scope + "." + o.address.asInstanceOf[LabelOfOperand].name)
          .map { adr_value =>
            Array(info.byteCode << 8 | (o.r << 4 | o.x), adr_value)
          }
          .getOrElse(throw new IllegalArgumentException(
            ERR_NOT_REPLACE_LABEL_ADDRESS + s"($code, $ope)"))
      }
      case o: OperandR_ADR_X if (o.address.isInstanceOf[AddressOfOperand]) => {
        Array(info.byteCode << 8 | (o.r << 4 | o.x),
              o.address.asInstanceOf[AddressOfOperand].value)
      }
      case o: OperandR => Array(info.byteCode << 8 | o.r << 4)
      case o: OperandADR_X if (o.address.isInstanceOf[LabelOfOperand]) => {
        symbolTbl
          .get(scope + "." + o.address.asInstanceOf[LabelOfOperand].name)
          .map { adr_value =>
            Array(info.byteCode << 8 | o.x, adr_value)
          }
          .getOrElse(throw new IllegalArgumentException(
            ERR_NOT_REPLACE_LABEL_ADDRESS + s"($code, $ope)"))
      }
      case o: OperandADR_X if (o.address.isInstanceOf[AddressOfOperand]) => {
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
                ERR_NOT_REPLACE_LABEL_ADDRESS + s"($code, $ope)"))
          }.toArray
        )
      case o: OperandDs => Array.fill(o.decimal)(0)
      case o: OperandDc => {
        val ope = o.consts
          .map(c =>
            c match {
              case e: LabelOfOperand =>
                List(
                  symbolTbl.getOrElse(
                    scope + "." + e.name,
                    throw new IllegalArgumentException(
                      ERR_NOT_REPLACE_LABEL_ADDRESS + s"($code, $o)")))
              case e: ConstsNumOfOperand => List(e.value)
              case e: ConstsStringOfOperand => e.array_char
          })
          .flatten
        ope.toArray
      }

      case o: OperandADR if (o.address.isInstanceOf[LabelOfOperand]) => {
        symbolTbl
          .get(scope + "." + o.address.asInstanceOf[LabelOfOperand].name)
          .map { adr_value =>
            Array(info.byteCode << 8 | 0, adr_value)
          }
          .getOrElse(throw new IllegalArgumentException(
            ERR_NOT_REPLACE_LABEL_ADDRESS + s"($code, $ope)"))
      }

      case o: OperandADR if (o.address.isInstanceOf[AddressOfOperand]) => {
        Array(info.byteCode << 8 | 0,
              o.address.asInstanceOf[AddressOfOperand].value)
      }

    }

}
