package scacasl2.operand

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ElementOperandsSpec extends AnyFlatSpec with Matchers {

  "ElementOperands" should " IncorrectDescription " in {
    val ope = IncorrectDescription("error")
    assert(ope.literal  === "error")
    assert(ope.adrValue === 0)

  }


}
