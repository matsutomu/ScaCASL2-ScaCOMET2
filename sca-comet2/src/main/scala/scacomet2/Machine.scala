package scacomet2

import scacasl2.Helper
import scacasl2.operand._
import scacomet2.Register._

import scala.collection.mutable.ListBuffer

class Machine {

  //
  // memory
  //
  private[scacomet2] var memory = new Array[Int](65536)

  def storeToMemory(binaries: Array[Int]): Unit = {
    this.programRunning = true
    binaries.copyToArray(this.memory, 0, binaries.length)
  }



  //
  // Registers
  //
  val gr0 = GeneralRegister(0)
  val gr1 = IndexRegister(0)
  val gr2 = IndexRegister(0)
  val gr3 = IndexRegister(0)
  val gr4 = IndexRegister(0)
  val gr5 = IndexRegister(0)
  val gr6 = IndexRegister(0)
  val gr7 = IndexRegister(0)

  private val INITIAL_STACK_POINTER_ADR = 0xFF00
  val gr8 = StackPointer(INITIAL_STACK_POINTER_ADR)
  val SP  = gr8

  private def stackPointer = this.gr8.word
  private def stackPointer(v: Int): Unit = this.gr8.word = v

  val generalRegisters = Array(gr0, gr1, gr2, gr3, gr4, gr5, gr6, gr7, gr8)

  val PR = ProgramRegister(0)
  private val fr = FlagRegister(0)


  //
  // Flag Registers
  //

  private val POSITION_OVER_FLOW_FLAG: Byte = 0
  private val POSITION_SIGN_FLAG: Byte = 1
  private val POSITION_ZERO_FLAG: Byte = 2

  def OF: BinaryNumber = this.getFlagInfo(POSITION_OVER_FLOW_FLAG)
  def SF: BinaryNumber = this.getFlagInfo(POSITION_SIGN_FLAG)
  def ZF: BinaryNumber = this.getFlagInfo(POSITION_ZERO_FLAG)

  private def getFlagInfo(position: Byte): BinaryNumber = {
    this.fr.word & 1 << position match {
      case 0 => BinaryNumber.Zero
      case _ => BinaryNumber.One
    }
  }

  // 0110 & 0***
  private def overFlowFlgOff(): Unit = this.fr.word = 0x0006 & this.fr.word

  // 0001 | 0***
  private def overFlowFlgOn(): Unit  = this.fr.word = 0x0001 | this.fr.word

  // 0101 & 0***
  private def signFlgOff(): Unit     = this.fr.word = 0x0005 & this.fr.word

  // 0010 | 0***
  private def signFlgOn(): Unit      = this.fr.word = 0x0002 | this.fr.word

  // 0011 & 0***
  private def zeroFlgOff(): Unit     = this.fr.word = 0x0003 & this.fr.word

  // 0100 | 0***
  private def zeroFlgOn(): Unit      = this.fr.word = 0x0004 | this.fr.word

  private def flagLiteral(): String  = this.OF.toString + this.SF.toString + this.ZF.toString

  private def getBit(x: Int, position: Byte): Int = if ((x & (1 << position)) == 0) 0 else 1

  private def changeFlags(result: Int,
                          typeofinstruct: InstructionsOfAorL): Unit = {
    // for Overflow flag
    val tmpOF = {
      if (typeofinstruct == InstructionsOfAorL.ArithmeticInstruct) {
        if (result < -32768 || 0x7fff < result) 0x0001
        else 0x0000
      } else if (typeofinstruct == InstructionsOfAorL.LogicalInstruct) {
        if (result < 0 || 0xffff < result) 0x0001
        else 0x0000
      } else { // not change
        if (this.getFlagInfo(this.POSITION_OVER_FLOW_FLAG) == BinaryNumber.Zero) 0x0000
        else 0x0001
      }
    }

    // for Sign Flag (Arithmetic & Logical)
    val tmpSF = getBit(result, 15) << this.POSITION_SIGN_FLAG

    // for Zero Flag
    val tmpZF = (if ((result & 0xffff) == 0) 1 else 0) << this.POSITION_ZERO_FLAG

    // flags set
    this.fr.word = tmpOF | tmpSF | tmpZF
  }

  private def changeFlagByCompare(result: Int): Unit = {
    // Sign Flag
    if (result >= 0) this.signFlgOff()
    else this.signFlgOn()

    // Zero Flag
    if (result == 0) this.zeroFlgOn()
    else this.zeroFlgOff()

    // Over Flag is Zero
    this.overFlowFlgOff()

  }

  //
  // Program Call Control
  //

  private var callLevelCounter = 0
  private var stepCounter = 0
  def stepCount = stepCounter

  private var programRunning = false

  private var breakPoints: Seq[Int] = Seq.empty[Int]

  //#todo case class Decode[+A <: Operand](argType: OperandType, execute: A => Unit )
  private case class Decode(argType: OperandType, execute: Operand => Unit)

  private val INSTRUCTION_MAP: Map[Int, Decode] = Map(
    0x00 -> Decode(OperandType.ArgNo, this.Nop),
    0x10 -> Decode(OperandType.ArgRAdrX, this.load2),
    0x11 -> Decode(OperandType.ArgRAdrX, this.store),
    0x12 -> Decode(OperandType.ArgRAdrX, this.loadAddress),
    0x14 -> Decode(OperandType.ArgR1R2, this.load1),
    0x20 -> Decode(OperandType.ArgRAdrX, this.addArithmetic2),
    0x21 -> Decode(OperandType.ArgRAdrX, this.subArithmetic2),
    0x22 -> Decode(OperandType.ArgRAdrX, this.addLogical2),
    0x23 -> Decode(OperandType.ArgRAdrX, this.subLogical2),
    0x24 -> Decode(OperandType.ArgR1R2, this.addArithmetic1),
    0x25 -> Decode(OperandType.ArgR1R2, this.subArithmetic1),
    0x26 -> Decode(OperandType.ArgR1R2, this.addLogical1),
    0x27 -> Decode(OperandType.ArgR1R2, this.subLogical1),
    0x30 -> Decode(OperandType.ArgRAdrX, this.and2),
    0x31 -> Decode(OperandType.ArgRAdrX, this.or2),
    0x32 -> Decode(OperandType.ArgRAdrX, this.exclusiveOr2),
    0x34 -> Decode(OperandType.ArgR1R2, this.and1),
    0x35 -> Decode(OperandType.ArgR1R2, this.or1),
    0x36 -> Decode(OperandType.ArgR1R2, this.exclusiveOr1),
    0x40 -> Decode(OperandType.ArgRAdrX, this.compareArithmetic2),
    0x41 -> Decode(OperandType.ArgRAdrX, this.compareLogical2),
    0x44 -> Decode(OperandType.ArgR1R2, this.compareArithmetic1),
    0x45 -> Decode(OperandType.ArgR1R2, this.compareLogical1),
    0x50 -> Decode(OperandType.ArgRAdrX, this.shiftLeftArithmetic2),
    0x51 -> Decode(OperandType.ArgRAdrX, this.shiftRightArithmetic2),
    0x52 -> Decode(OperandType.ArgRAdrX, this.shiftLeftLogical2),
    0x53 -> Decode(OperandType.ArgRAdrX, this.shiftRightLogical2),
    0x61 -> Decode(OperandType.ArgAdrX, this.jumpOnMinus),
    0x62 -> Decode(OperandType.ArgAdrX, this.jumpOnNonZero),
    0x63 -> Decode(OperandType.ArgAdrX, this.jumpOnZero),
    0x64 -> Decode(OperandType.ArgAdrX, this.unconditionalJump),
    0x65 -> Decode(OperandType.ArgAdrX, this.jumpOnPlus),
    0x66 -> Decode(OperandType.ArgAdrX, this.jumpOnOverflow),
    0x70 -> Decode(OperandType.ArgAdrX, this.push),
    0x71 -> Decode(OperandType.ArgR, this.pop),
    0x80 -> Decode(OperandType.ArgAdrX, this.call),
    0x81 -> Decode(OperandType.ArgNo, this.ret),
    0x90 -> Decode(OperandType.ArgNo, this.in),
    0x91 -> Decode(OperandType.ArgNo, this.out),
    0xa0 -> Decode(OperandType.ArgNo, this.rpush),
    0xa1 -> Decode(OperandType.ArgNo, this.rpop),
    0xF0 -> Decode(OperandType.ArgAdrX, this.supervisorCall)
  )


  /**
    * Binary Code run 1 step
    *
    */
  def step(): Boolean = {
    //#todo if finished ?
    this.execute(this.PR.word)
    this.stepCounter = this.stepCounter + 1

    this.programRunning
  }

  /**
    * Binary Code run 1 step
    *
    * @param address
    */
  private def execute(address: Int): Unit = {
    val word1 = this.memory(address)
    val word2 = this.memory(address + 1)
    val opeCode = ((word1 & 0xff00) >> 8).toShort // **** **** 0000 0000

    val ope_values: Operand = decodeOperand(word1, word2, opeCode)
    INSTRUCTION_MAP(opeCode).execute(ope_values)
  }

  /**
    * Parse BinaryCode
    *
    * @param word1
    * @param word2
    * @param opeCode
    * @return
    */
  private def decodeOperand(word1: Int, word2: Int, opeCode: Short): Operand = {

    val decode = INSTRUCTION_MAP(opeCode)
    decode.argType match {
      case OperandType.ArgNo => OperandNoArg()
      case OperandType.ArgR1R2 =>
        OperandR1R2(
          ((word1 & 0x00f0) >> 4), // 0000 0000 **** 0000
          ((word1 & 0x000f)) // 0000 0000 0000 ****
        )

      case OperandType.ArgRAdrX => {
        OperandR_ADR_X(
          ((word1 & 0x00f0) >> 4), // adr  : 0000 0000 **** 0000
          AddressOfOperand(
            ((word2 & 0xffff)).toString,
            ((word2 & 0xffff)).toShort), // adr+1: **** **** **** ****
          ((word1 & 0x000f)) // adr  : 0000 0000 0000 ****
        )
      }

      case OperandType.ArgAdrX => {
        OperandADR_X(
          AddressOfOperand(
            ((word2 & 0xffff)).toString,
            ((word2 & 0xffff)).toShort), // adr+1: **** **** **** ****
          ((word1 & 0x000f)) // 0000 0000 0000 ****
        )
      }

      case OperandType.ArgR =>
        OperandR(
          ((word1 & 0x00f0) >> 4) // 0000 0000 **** 0000
        )

    }
  }

  private def getEffectiveAddress(address: Int, x: Int): Int = {
    if (x == 0) Helper.bitToUnsignedShort(address)
    else Helper.bitToUnsignedShort(address + this.generalRegisters(x).word)
  }

  private def getValueEffectiveAddress(address: Int, x: Int): Int = {
    if (x == 0) this.memory(address)
    else this.memory(Helper.bitToUnsignedShort(address + this.generalRegisters(x).word))
  }




  ///
  /// BreakPoints
  ///

  def addBreakPoint(address: Int): Unit = {
    this.breakPoints = (this.breakPoints :+ address).sorted
  }

  def containsBreakPoint(address: Int): Boolean = {
    this.breakPoints.contains(address)
  }

  def breakPointInfo(): List[String] = {
    this.breakPoints.sorted.zipWithIndex.map { case (address, i) =>
      val index = i + 1
        f"$index%4d : #$address%04x"
    }.toList
  }

  def deleteBreakPoint(index: Int): Unit = {
    val i = index - 1
    if(this.breakPoints.indices.contains(i)){
      this.breakPoints = this.breakPoints.filterNot(_ == this.breakPoints(i)).sorted
    } else {
      throw new ArrayIndexOutOfBoundsException
    }
  }

  /**  Watch Register, Memory **/
  private var watchTargets: Set[String] = Set.empty[String]
  def addWatch(target: String): Unit = {
    if(target.matches(Machine.WATCH_VARIABLE_REGS)){
      this.watchTargets += target
    } else {
      throw new IllegalArgumentException(target)
    }
  }

  def delWatch(target: String): Unit = this.watchTargets -= target

  def watchInfo(decimal: Boolean = false): List[String] = watchTargets.map(this.watchInfoFlag(_, decimal)).toList

  private def watchInfoFlag(target: String, decimal: Boolean): String = {
    val grs = "GR[0-8]".r
    val (k, v) = target match {
      case "OF"  => (target, this.OF.toString.toInt)
      case "SF"  => (target, this.SF.toString.toInt)
      case "ZF"  => (target, this.ZF.toString.toInt)
      case "PR"  => (target, this.PR.word)
      case grs() => (target, generalRegisters(target.takeRight(1).toInt).word)
      case _  => {
        val address = Helper.parseInt(target)
        if(address < 0 || 0xffff < address){
          throw new RuntimeException
        } else {
          (f"#$address%04X", this.memory(address))
        }
      }
    }

    if(target.matches("OF|SF|ZF") || decimal){
      f"$k=$v%d"
    } else {
      f"$k=#$v%04X"
    }
  }


  /**
   *  Binary Data TO CASLII Code
   *
   * @param startAddress
   * @param stepCount
   * @return
   */
  def disassemble(startAddress: Int, stepCount: Int): List[String] = {
    var address = startAddress
    var tempRet = new ListBuffer[String]

    for(i <- 0 until stepCount){
      val word1 = this.memory(address)
      val word2 = this.memory(address + 1)
      val word3 = this.memory(address + 2)
      val opeCode = ((word1 & 0xff00) >> 8).toShort // **** **** 0000 0000

      val opeValues: Operand = decodeOperand(word1, word2, opeCode)

      import scacasl2.instruction.InstructionFactory
      val (wordSize: Int, literal: String) = InstructionFactory.INSTRUCTION_INF_MAP.find( p => p._2.byteCode == opeCode).map {
        case (ope, info) => {

          val disassembleCode = ope match {
            case "IN" | "OUT" => f"${ope.replaceAll("[12]","")}%-8s #$word2%04X, #$word3%04X"
            case _            => f"${ope.replaceAll("[12]","")}%-8s ${opeValues.disassemble}"
          }

          (info.wordSize,
            info.wordSize match {
              case 1 => f"#$address%04X: #$word1%04X               $disassembleCode"
              case 2 => f"#$address%04X: #$word1%04X #$word2%04X         $disassembleCode"
              case 3 => f"#$address%04X: #$word1%04X #$word2%04X #$word3%04X   $disassembleCode"
              case _ => throw new IllegalArgumentException(s"not supported word size${info.wordSize}")
            }
          )
        }
      }.getOrElse{
        val c = Helper.intToCharForCaslII(word1)
        (1,
          f"#$address%04X: #$word1%04X                         $c")
      }

      tempRet += literal.trim
      address = address + wordSize
    }

    tempRet.toList
  }


  /**
   * For Print Registers
   *
   */
  def statusInfo(): List[String] = {

    val disassemble = this.disassemble(this.PR.word, 1).mkString
    List(f"PR #${this.PR.word}%04X [ $disassemble%-30s ]  STEP ${this.stepCount}%d",
         f"SP #${this.SP.word}%04X(${this.SP.word}%7d) FR(OF, SF, ZF) ${this.flagLiteral}%3s  (${this.fr.word}%7d)") :::
    (this.generalRegisters.filter(p => !p.isInstanceOf[StackPointer]).zipWithIndex.map { case (e, i) =>
      val signed = Helper.bitToSignedShort(e.word)
      f"GR$i #${e.word}%04X($signed%7d)"
    }.splitAt(4) match {
      case (a, b) => List(a.mkString(" "), b.mkString(" "))
      case _      => Nil
    })
  }


  /*****************************************************************************
    *
    * Instruction Code
    *
   *****************************************************************************/
  
  def Nop = (arg: Operand) => {
    this.PR ++ 1
  }

  def load2 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]

    this.generalRegisters(arg.r).word =
      this.getValueEffectiveAddress(arg.address.adrValue, arg.x)
    this.changeFlags(this.generalRegisters(arg.r).word, InstructionsOfAorL.OtherInstruct)
    this.overFlowFlgOff() // *1
    this.PR ++ 2
    ()
  }

  def store = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]
    val eAdr = this.getEffectiveAddress(arg.address.adrValue, arg.x)
    this.memory(eAdr) = this.generalRegisters(arg.r).word.toShort
    this.PR ++ 2
  }

  def loadAddress = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]
    this.generalRegisters(arg.r).word =
      this.getEffectiveAddress(arg.address.adrValue, arg.x)
    this.PR ++ 2
  }

  def load1 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR1R2]
    this.generalRegisters(arg.r1).word = this.generalRegisters(arg.r2).word
    this.changeFlags(this.generalRegisters(arg.r1).word, InstructionsOfAorL.OtherInstruct)
    this.overFlowFlgOff() // *1
    this.PR ++ 1
  }

  def addArithmetic2 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]

    val eAdr = this.getValueEffectiveAddress(arg.address.adrValue, arg.x)
    val result = Helper.bitToSignedShort(this.generalRegisters(arg.r).word) + Helper.bitToSignedShort(eAdr)
    this.generalRegisters(arg.r).word = Helper.bitToUnsignedShort(result)
    this.changeFlags(result, InstructionsOfAorL.ArithmeticInstruct)
    this.PR ++ 2

  }

  def addLogical2 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]

    val eAdr = this.getValueEffectiveAddress(arg.address.adrValue, arg.x)
    val result = this.generalRegisters(arg.r).word + eAdr
    this.generalRegisters(arg.r).word = (result) & 0xffff
    this.changeFlags(result, InstructionsOfAorL.LogicalInstruct)
    this.PR ++ 2

  }

  def subArithmetic2 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]

    val eAdr = this.getValueEffectiveAddress(arg.address.adrValue, arg.x)
    val result = Helper.bitToSignedShort(this.generalRegisters(arg.r).word) - Helper.bitToSignedShort(eAdr)
    this.generalRegisters(arg.r).word = Helper.bitToUnsignedShort(result)
    this.changeFlags(result, InstructionsOfAorL.ArithmeticInstruct)
    this.PR ++ 2

  }

  def subLogical2 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]

    val eAdr = this.getValueEffectiveAddress(arg.address.adrValue, arg.x)
    val result = this.generalRegisters(arg.r).word - eAdr
    this.generalRegisters(arg.r).word = (result) & 0xffff
    this.changeFlags(result, InstructionsOfAorL.LogicalInstruct)
    this.PR ++ 2
  }

  def and2 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]

    val eAdr = this.getValueEffectiveAddress(arg.address.adrValue, arg.x)
    this
      .generalRegisters(arg.r)
      .word = this.generalRegisters(arg.r).word & eAdr
    this.changeFlags(this.generalRegisters(arg.r).word, InstructionsOfAorL.LogicalInstruct)
    this.overFlowFlgOff()
    this.PR ++ 2

  }

  def or2 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]

    val eAdr = this.getValueEffectiveAddress(arg.address.adrValue, arg.x)
    this
      .generalRegisters(arg.r)
      .word = this.generalRegisters(arg.r).word | eAdr
    this.changeFlags(this.generalRegisters(arg.r).word, InstructionsOfAorL.LogicalInstruct)
    this.overFlowFlgOff()
    this.PR ++ 2

  }

  def exclusiveOr2 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]

    val eAdr = this.getValueEffectiveAddress(arg.address.adrValue, arg.x)
    this
      .generalRegisters(arg.r)
      .word = this.generalRegisters(arg.r).word ^ eAdr
    this.changeFlags(this.generalRegisters(arg.r).word, InstructionsOfAorL.LogicalInstruct)
    this.overFlowFlgOff()
    this.PR ++ 2

  }

  private def compareArithmetic2 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]
    val eAdr = this.getValueEffectiveAddress(arg.address.adrValue, arg.x)
    val diff = Helper.bitToSignedShort(this.generalRegisters(arg.r).word) - Helper.bitToSignedShort(eAdr)
    this.changeFlagByCompare(diff)
    this.PR ++ 2
  }

  private def compareLogical2 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]
    val eAdr = this.getValueEffectiveAddress(arg.address.adrValue, arg.x)
    val diff = this.generalRegisters(arg.r).word - eAdr
    this.changeFlagByCompare(diff)
    this.PR ++ 2

  }

  def addArithmetic1 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR1R2]
    val result = Helper.bitToSignedShort(this.generalRegisters(arg.r1).word) + Helper.bitToSignedShort(
      this.generalRegisters(arg.r2).word)
    this.generalRegisters(arg.r1).word = Helper.bitToUnsignedShort(result)
    this.changeFlags(result, InstructionsOfAorL.ArithmeticInstruct)
    this.PR ++ 1

  }

  def addLogical1 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR1R2]
    val result = this.generalRegisters(arg.r1).word + this
      .generalRegisters(arg.r2)
      .word
    this.generalRegisters(arg.r1).word = (result) & 0xffff
    this.changeFlags(result, InstructionsOfAorL.LogicalInstruct)
    this.PR ++ 1

  }

  def subArithmetic1 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR1R2]
    val result = Helper.bitToSignedShort(this.generalRegisters(arg.r1).word) - Helper.bitToSignedShort(
      this.generalRegisters(arg.r2).word)
    this.generalRegisters(arg.r1).word = Helper.bitToUnsignedShort(result)
    this.changeFlags(result, InstructionsOfAorL.ArithmeticInstruct)
    this.PR ++ 1

  }

  def subLogical1 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR1R2]
    val result = this.generalRegisters(arg.r1).word - this
      .generalRegisters(arg.r2)
      .word
    this.generalRegisters(arg.r1).word = (result) & 0xffff
    this.changeFlags(result, InstructionsOfAorL.LogicalInstruct)
    this.PR ++ 1

  }

  def and1 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR1R2]

    this.generalRegisters(arg.r1).word = this
      .generalRegisters(arg.r1)
      .word & this.generalRegisters(arg.r2).word
    this.changeFlags(this.generalRegisters(arg.r1).word, InstructionsOfAorL.LogicalInstruct)
    this.overFlowFlgOff()
    this.PR ++ 1

  }

  def or1 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR1R2]

    this.generalRegisters(arg.r1).word = this
      .generalRegisters(arg.r1)
      .word | this.generalRegisters(arg.r2).word
    this.changeFlags(this.generalRegisters(arg.r1).word, InstructionsOfAorL.LogicalInstruct)
    this.overFlowFlgOff()
    this.PR ++ 1

  }

  def exclusiveOr1 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR1R2]

    this.generalRegisters(arg.r1).word = this
      .generalRegisters(arg.r1)
      .word ^ this.generalRegisters(arg.r2).word
    this.changeFlags(this.generalRegisters(arg.r1).word, InstructionsOfAorL.LogicalInstruct)
    this.overFlowFlgOff()
    this.PR ++ 1

  }

  private def compareArithmetic1 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR1R2]
    val diff = Helper.bitToSignedShort(this.generalRegisters(arg.r1).word) - Helper.bitToSignedShort(
      this.generalRegisters(arg.r2).word)
    this.changeFlagByCompare(diff)
    this.PR ++ 1
  }

  private def compareLogical1 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR1R2]
    val diff = this.generalRegisters(arg.r1).word - this
      .generalRegisters(arg.r2)
      .word
    this.changeFlagByCompare(diff)
    this.PR ++ 1
  }

  def shiftLeftArithmetic2 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]

    val v = this.getEffectiveAddress(arg.address.adrValue, arg.x)

    val p = Helper.bitToSignedShort(this.generalRegisters(arg.r).word)
    val sign = this.getBit(this.generalRegisters(arg.r).word, 15)
    this.generalRegisters(arg.r).word = if (sign == 0) {
      (p << v) & 0x7FFF
    } else {
      ((p << v) & 0x7FFF) | 0x8000
    }
    this.changeFlags(this.generalRegisters(arg.r).word, InstructionsOfAorL.ArithmeticInstruct)

    if (0 < v) {
      if (this.getBit(p, (15 - v).toByte) == 0) {
        this.overFlowFlgOff()
      } else {
        this.overFlowFlgOn()
      }
    }

    this.PR ++ 2

  }

  def shiftRightArithmetic2 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]

    val v = this.getEffectiveAddress(arg.address.adrValue, arg.x)

    val p = Helper.bitToSignedShort(this.generalRegisters(arg.r).word)
    val sign = this.getBit(this.generalRegisters(arg.r).word, 15)
    this.generalRegisters(arg.r).word = if (sign == 0) {
      (p >> v) & 0x7FFF
    } else {
      ((p >> v) & 0x7FFF) | 0x8000
    }
    this.changeFlags(this.generalRegisters(arg.r).word, InstructionsOfAorL.ArithmeticInstruct)

    if (0 < v) {
      if (this.getBit(p, (v - 1).toByte) == 0) {
        this.overFlowFlgOff()
      } else {
        this.overFlowFlgOn()
      }
    }

    this.PR ++ 2

  }

  def shiftLeftLogical2 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]

    val v = this.getEffectiveAddress(arg.address.adrValue, arg.x)

    val p = this.generalRegisters(arg.r).word
    this.generalRegisters(arg.r).word = (p << v) & 0xFFFF
    this.changeFlags(this.generalRegisters(arg.r).word, InstructionsOfAorL.LogicalInstruct)

    if (0 < v) {
      if (this.getBit(p, (15 - (v - 1)).toByte) == 0) {
        this.overFlowFlgOff()
      } else {
        this.overFlowFlgOn()
      }
    }

    this.PR ++ 2
  }

  def shiftRightLogical2 = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR_ADR_X]

    val v = this.getEffectiveAddress(arg.address.adrValue, arg.x)

    val p = this.generalRegisters(arg.r).word
    this.generalRegisters(arg.r).word = (p >> v) & 0xFFFF
    this.changeFlags(this.generalRegisters(arg.r).word, InstructionsOfAorL.LogicalInstruct)

    if (0 < v) {
      if (this.getBit(p, (v - 1).toByte) == 0) {
        this.overFlowFlgOff()
      } else {
        this.overFlowFlgOn()
      }
    }

    this.PR ++ 2
  }

  private def jumpByCondition(arg: OperandADR_X, condition: Boolean) = {
    if (condition) {
      this.PR.word = this.getEffectiveAddress(arg.address.adrValue, arg.x)
    } else {
      this.PR ++ 2
    }
  }

  private def jumpOnPlus = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandADR_X]
    jumpByCondition(arg, this.SF == BinaryNumber.Zero && this.ZF == BinaryNumber.Zero)
  }

  private def jumpOnMinus = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandADR_X]
    jumpByCondition(arg, this.SF == BinaryNumber.One)
  }

  private def jumpOnNonZero = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandADR_X]
    jumpByCondition(arg, this.ZF == BinaryNumber.Zero)
  }

  private def jumpOnZero = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandADR_X]
    jumpByCondition(arg, this.ZF == BinaryNumber.One)
  }

  def jumpOnOverflow = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandADR_X]
    jumpByCondition(arg, this.OF == BinaryNumber.One)
  }

  def unconditionalJump = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandADR_X]
    jumpByCondition(arg, true)
  }

  def push = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandADR_X]

    this.stackPointer(this.stackPointer - 1)
    val v = this.getEffectiveAddress(arg.address.adrValue, arg.x)
    this.memory(this.stackPointer) = v.toShort
    this.PR ++ 2
  }

  def pop = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandR]
    this.generalRegisters(arg.r).word = this.memory(this.stackPointer)
    this.stackPointer(this.stackPointer + 1)
    this.PR ++ 1
  }

  def call = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandADR_X]

    this.stackPointer(this.stackPointer - 1)

    this.memory(this.stackPointer) = this.PR.word.toShort
    this.PR.word = this.getEffectiveAddress(arg.address.adrValue, arg.x)

    this.callLevelCounter = this.callLevelCounter + 1

  }

  def ret = (ope: Operand) => {
    if (this.callLevelCounter == 0) {
      this.stepCounter = this.stepCounter + 1
      this.programRunning = false

    } else {
      this.PR.word = this.memory(this.stackPointer) + 2
      this.stackPointer(this.stackPointer + 1)
      this.callLevelCounter = this.callLevelCounter - 1
    }
  }


  def in = (ope: Operand) => {

    var inputString = scala.io.StdIn.readLine()
    val startAdr = this.memory(this.PR.word + 1)
    val lengthAdr = this.memory(this.PR.word + 2)

    if (256 < inputString.size) {
      inputString = inputString.substring(0, 256)
    }

    this.memory(lengthAdr) = inputString.size.toShort
    inputString.zipWithIndex.foreach {
      case (c: Char, i: Int) =>
        this.memory(startAdr + i) = c.toShort
    }

    this.PR ++ 3

  }

  def out = (ope: Operand) => {
    var outputString = ""
    val startAdr = this.memory(this.PR.word + 1)
    val lengthAdr = this.memory(this.PR.word + 2)

    val length = this.memory(lengthAdr)

    for (i <- 0 until length) {
      if (Character.isValidCodePoint(this.memory(startAdr + i))) {
        outputString += this.memory(startAdr + i).toChar.toString
      } else {
        throw new IllegalArgumentException(s"no good 'out' parameter(position:$i)")
      }
    }
    println(outputString)
    this.PR ++ 3
  }

  def rpush = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandNoArg]
    for (r <- this.generalRegisters if (r != gr0 && r != gr8)) {
      this.stackPointer(this.stackPointer - 1)
      this.memory(this.stackPointer) = r.word.toShort
    }
    this.PR ++ 1
  }

  def rpop = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandNoArg]
    for (r <- this.generalRegisters.reverse if (r != gr0 && r != gr8)) {
      r.word = this.memory(this.stackPointer)
      this.stackPointer(this.stackPointer + 1)
    }
    this.PR ++ 1
  }

  def supervisorCall = (ope: Operand) => {
    val arg = ope.asInstanceOf[OperandADR_X]

    // NOP
  }

}

object Machine {

  val WATCH_VARIABLE_REGS = "PR|OF|SF|ZF|GR[0-8]|SP|\\d+|#[\\a-fA-F]+"

}