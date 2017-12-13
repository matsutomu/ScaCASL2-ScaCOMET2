package scacasl2.operand

/**
  * Operand
  *  Like parameter.
  */
trait Operand {
  def disassemble: String
}

/**
  * No Args
  *
  */
case class OperandNoArg() extends Operand {
  override def disassemble: String = ""
}

/**
  *  Only Register Parameter
  *
  * @param r1 GR0 - GR8
  * @param r2 GR0 - GR8
  */
case class OperandR1R2(r1: Int, r2: Int) extends Operand {
  override def disassemble: String = f"GR$r1%1d, GR$r2%1d"
}

/**
  *  Register, Address, Index Register .
  *
  * @param r Register(GR0 - GR8)
  * @param address Address
  * @param x Index Register
  */
case class OperandR_ADR_X(r: Int, address: ElementOfAddressOperand, x: Int)
    extends Operand {
  override def disassemble: String = {
    if(x == 0) f"GR$r%1d, #${address.adrValue}%04X"
    else  f"GR$r%1d, #${address.adrValue}%04X, GR$x%1d"
  }
}

/**
  *  Address, Index Register
  *
  * @param address Address
  * @param x Index Register
  */
case class OperandADR_X(address: ElementOfAddressOperand, x: Int)
    extends Operand {
  override def disassemble: String = {
    if (x == 0) f"#${address.adrValue}%04X"
    else f"#${address.adrValue}%04X, GR$x%1d"
  }
}
/**
  *  Only Address
  *
  * @param address Address
  */
case class OperandADR(address: ElementOfAddressOperand) extends Operand {
  override def disassemble: String = {
    f"#${address.adrValue}%04X"
  }
}

/** Register
  *
  * @param r Register(GR0 - GR8)
  */
case class OperandR(r: Int) extends Operand {
  override def disassemble: String = f"GR$r%1d"
}

/** Single Label
  * START Instruction use.
  * @param l Label Of Operand.
  */
case class OperandStart(l: Option[LabelOfOperand]) extends Operand {
  override def disassemble: String = ???
}

/** Multi Label
  * IN/OUT Instruction use.
  * @param ml Multi Label Of Operand.
  */
case class OperandInOrOut(ml: List[LabelOfOperand]) extends Operand {
  override def disassemble: String = ???
}

  /** DS Instruction use.
  *
  * @param decimal Number of words to ensure.
  */
case class OperandDs(decimal: Int) extends Operand {
    override def disassemble: String = ???
  }

/** DC Instruction use.
  *
  * @param consts decimal, hex, string, Label
  */
case class OperandDc(consts: List[ElementOfDcInstruction]) extends Operand {
  override def disassemble: String = ???
}
