package scacasl2

import org.scalatest._

class HelperSpec extends FlatSpec with DiagrammedAssertions {

  " ParseInt Hex or Decimal" should " convert Int " in {
    assert(Helper.parseInt("#0000") === 0)
    assert(Helper.parseInt("#ffff") === 65535)
    assert(Helper.parseInt("#FFFF") === 65535)
    assert(Helper.parseInt("#fffff") === 1048575)

  }

  " CASLII Char " should " Instruction Line START" in {

    assert(Helper.intToCharForCaslII(0x001F) === '.')
    assert(Helper.intToCharForCaslII(0x0020) === ' ')
    assert(Helper.intToCharForCaslII(0x0041) === 'A')
    assert(Helper.intToCharForCaslII(0x007E) === '~')
    assert(Helper.intToCharForCaslII(0x007F) === '.')
  }

  " Address Checker " should " check string integer " in {
    assert(Helper.includeAddress("-1")      === false)
    assert(Helper.includeAddress("0")       === true)
    assert(Helper.includeAddress("000000")  === true)
    assert(Helper.includeAddress("#FFFF")    === true)
    assert(Helper.includeAddress("#1FFFF")   === false)
    assert(Helper.includeAddress("#")   === false)
  }


}
