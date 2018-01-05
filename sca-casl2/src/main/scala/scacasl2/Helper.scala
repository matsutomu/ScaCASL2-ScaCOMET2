package scacasl2

object Helper {

  /**
    * '#000f' -> 16
    * '16'    -> 16
    *
    * @param address Hex or Dec
    * @return
    */
  def parseInt(address: String): Int = {
    if (address.head == '#') Integer.parseInt(address.tail, 16)
    else Integer.parseInt(address)
  }

  /**
    * 0x41 => 'A'
    *
    *
    * @param i Ascii Integer
    * @return
    */
  def intToCharForCaslII(i: Int): Char = {
    val c = i & 0x00ff
    if (0x20 <= c && c <= 0x7e) {
      c.toChar
    } else {
      '.'
    }
  }

  /**
    * Parmeter Address between 0 and MAX Memory Address  
    *
    * @param address Check Address
    * @return
    */
  def includeAddress(address: String): Boolean = {

    val convertAddr =
      if (address.head == '#' && address.tail.matches("[0-9a-fA-F]+")) {
        Integer.parseInt(address.tail, 16)
      } else if (address.matches("[0-9]+")) {
        Integer.parseInt(address)
      } else {
        -1
      }

    0 <= convertAddr && convertAddr <= 0xFFFF
  }

  /**
    * Bit(32bit) to Word Value(Short). Internal Type is Int, cause consider overflow
    *
    * (32bit is Int)
    * 0000 0000 0000 0000 0000 0000 0000 0000 is 0
    * 1000 0000 0000 0000 0000 0000 0000 0000 is -2,147,483,648 (0x8000 0000)
    * 0111 1111 1111 1111 1111 1111 1111 1111 is  2,147,483,647 (0x7FFF FFFF)
    *
    * CASL2 & COMET2 's word is 16 bit
    * Short(16bit)
    * ---- ---- ---- ---- 0000 0000 0000 0000 is 0
    * ---- ---- ---- ---- 1000 0000 0000 0000 is -32,768 (0x8000)
    * ---- ---- ---- ---- 0000 0000 0000 0000 is  32,767 (0x7FFF)
    *
    * first  step : Int to Short
    * second step : unsigned to Signed Short
    *
    *             0 1000 0000 0000 0000  (0x8000)
    *           & 0 1111 1111 1111 1111  (0xFFFF)
    *           -----------------------
    *             0 1000 0000 0000 0000  (0x08000)
    *           - 1 0000 0000 0000 0000  (0x10000)
    *           -----------------------
    *               1000 0000 0000 0000  (0x08000)
    *
    * @param x target binary data
    * @return Signed Short
    */
  def bitToSignedShort(x: Int): Int = x & 0xffff match {
    case y: Int if 0x0000 <= y && y <= 0x7fff => y
    case y: Int                               => y - 0x10000
  }

  /**
    * Bit(32bit) to Word Value(Short). Internal Type is Int, cause consider overflow
    *
    * @param x target binary data
    * @return Unsigned Short
    */
  def bitToUnsignedShort(x: Int): Int = x & 0xffff

}
