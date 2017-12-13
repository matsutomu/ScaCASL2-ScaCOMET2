package scacasl2

object Helper {


  /**
   * '#000f' -> 16
   * '16'    -> 16
   *
   * @param address
   * @return
   */
  def parseInt(address: String): Int = {
    if(address.head == '#') Integer.parseInt(address.tail, 16)
    else  Integer.parseInt(address)
  }

  /**
   * 0x41 => 'A'
   *
   *
   * @param i
   * @return
   */
  def intToCharForCaslII(i: Int) = {
    val c = i & 0x00ff
    if(0x20 <= c && c <= 0x7e){
      c.toChar
    } else {
      '.'
    }
  }

  /**
   *
   * @param address
   * @return
   */
  def includeAddress(address: String): Boolean = {

    val convertAddr = if(address.head == '#' && address.tail.matches("[0-9a-fA-F]+")){
      Integer.parseInt(address.tail, 16)
    } else if (address.matches("[0-9]+")){
      Integer.parseInt(address)
    } else {
      -1
    }

    (0 <= convertAddr && convertAddr <= 0xFFFF)
  }


}
