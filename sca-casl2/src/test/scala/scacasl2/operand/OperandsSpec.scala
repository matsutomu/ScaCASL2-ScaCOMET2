package scacasl2.operand

import org.scalatest._

class OperandsSpec extends FlatSpec with DiagrammedAssertions {

  "Operand" should " dissassemble and check existence (OperandNoArg) " in {
    val ope = OperandNoArg()
    assert(ope.disassemble  === "")
    assert(ope.includeLabel === false)

  }

  it should " dissassemble and check existence (Operand R1 R2) " in {
    val ope = OperandR1R2(1, 1)
    assert(ope.disassemble  === "GR1, GR1")
    assert(ope.includeLabel === false)
  }

  it should " dissassemble and check existence (Operand R Address X) " in {
    val ope = OperandR_ADR_X(1, scacasl2.operand.LabelOfOperand("LBL1", Some(1)), 2)
    assert(ope.disassemble  === "GR1, #0001, GR2")
    assert(ope.includeLabel === true)

    val ope2 = OperandR_ADR_X(1, scacasl2.operand.AddressOfOperand("1", 1), 2)
    assert(ope2.disassemble  === "GR1, #0001, GR2")
    assert(ope2.includeLabel === false)

    val ope3 = OperandR_ADR_X(1, scacasl2.operand.AddressOfOperand("1", 1), 0)
    assert(ope3.disassemble  === "GR1, #0001")
    assert(ope3.includeLabel === false)

  }

  it should " dissassemble and check existence (Operand Address X) " in {
    val ope = OperandADR_X(scacasl2.operand.LabelOfOperand("LBL1", Some(1)), 2)
    assert(ope.disassemble  === "#0001, GR2")
    assert(ope.includeLabel === true)

    val ope2 = OperandADR_X(scacasl2.operand.AddressOfOperand("1", 1), 2)
    assert(ope2.disassemble  === "#0001, GR2")
    assert(ope2.includeLabel === false)
  }

  it should " dissassemble and check existence (Operand Address) " in {
    val ope = OperandADR(scacasl2.operand.LabelOfOperand("LBL1", Some(1)))
    assert(ope.disassemble  === "#0001")
    assert(ope.includeLabel === true)

    val ope2 = OperandADR(scacasl2.operand.AddressOfOperand("1", 1))
    assert(ope2.disassemble  === "#0001")
    assert(ope2.includeLabel === false)
  }

  it should " dissassemble and check existence (Operand R) " in {
    val ope = OperandR(1)
    assert(ope.disassemble  === "GR1")
    assert(ope.includeLabel === false)
  }

  it should " dissassemble and check existence (Operand START) " in {
    val ope = OperandStart(None)
    assertThrows[UnsupportedOperationException] { //
      ope.disassemble
    }
    assert(ope.includeLabel === false)

    val ope2 = OperandStart(Some(LabelOfOperand("LBL1", Some(1))))
    assertThrows[UnsupportedOperationException] { //
      ope2.disassemble
    }
    assert(ope2.includeLabel === true)

  }

  it should " dissassemble and check existence (Operand In Or Out) " in {
    val ope = OperandInOrOut(List(LabelOfOperand("LBL1", Some(1)),LabelOfOperand("LBL2", Some(2))))
    assertThrows[UnsupportedOperationException] { //
      ope.disassemble
    }
    assert(ope.includeLabel === true)

  }

  it should " dissassemble and check existence (Operand DS) " in {
    val ope = OperandDs(1)
    assertThrows[UnsupportedOperationException] { //
      ope.disassemble
    }
    assert(ope.includeLabel === false)

  }

  it should " dissassemble and check existence (Operand DC) " in {
    val ope = OperandDc(List(LabelOfOperand("LBL1", Some(1)),LabelOfOperand("LBL2", Some(2))))
    assertThrows[UnsupportedOperationException] { //
      ope.disassemble
    }
    assert(ope.includeLabel === true)

  }


}
