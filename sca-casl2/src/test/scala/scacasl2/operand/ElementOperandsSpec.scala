package scacasl2.operand

import org.scalatest._

class ElementOperandsSpec extends FlatSpec with DiagrammedAssertions {

  "ElementOperands" should " IncorrectDescription " in {
    val ope = IncorrectDescription("error")
    assert(ope.literal  === "error")
    assert(ope.adrValue === 0)

  }


}
