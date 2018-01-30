package scacasl2.instruction

import scacasl2.operand._
import scacasl2.Helper

/**
  * Create Instruction from Instruction Literal.
  *  Like : LD GR0, 1 # This is Load to GR0.
  *
  */
object InstructionFactory {

  val REGEX_LABEL = "[A-Z][A-Za-z0-9]{1,6}"
  val REGEX_DS_NUM = "[0-9]{1,4}"
  val REGEX_NUM = "-?[0-9]+"
  val REGEX_HEX = "#[A-Fa-f0-9]+"
  val DS_MIN_NUM = 0
  val DS_MAX_NUM = 2000
  val DC_STRING_MAX_LENGTH = 2000

  val NOT_ALLOW_LABEL: Set[String] = Range(0, 8).map(x => s"GR$x").toSet

  val GENERAL_REGISTERS_MAP: Map[String, Int] =
    Range(0, 9).map(x => s"GR$x" -> x).toMap
  val INDEX_REGISTERS_MAP: Map[String, Int] =
    Range(1, 8).map(x => s"GR$x" -> x).toMap

  val ERR_UNSUPPORTED_OPERATION_CODE = "Unsupported Instruction"
  private val ERR_NO_GOOD_OPERAND = "No Good Operands"

  val INSTRUCTION_ANALYZE_MAP = Map(
    /* Assembly Instructions */
    AssemblyInstruction.START -> analyzeStart,
    AssemblyInstruction.END -> analyzeEnd,
    AssemblyInstruction.DS -> analyzeDs,
    AssemblyInstruction.DC -> analyzeDc,
    /* Macro Instructions */
    MacroInstruction.IN -> analyzeInOrOut,
    MacroInstruction.OUT -> analyzeInOrOut,
    MacroInstruction.RPUSH -> analyzeRpushOrRpop,
    MacroInstruction.RPOP -> analyzeRpushOrRpop,
    /* Only Nop */
    MachineInstruction.NOP -> analyzeMachineInstruction,
    /* */
    MachineInstruction.LD -> analyzeMachineInstruction,
    MachineInstruction.ST -> analyzeMachineInstruction,
    MachineInstruction.LAD -> analyzeMachineInstruction,
    /* */
    MachineInstruction.ADDA -> analyzeMachineInstruction,
    MachineInstruction.SUBA -> analyzeMachineInstruction,
    MachineInstruction.ADDL -> analyzeMachineInstruction,
    MachineInstruction.SUBL -> analyzeMachineInstruction,
    /* */
    MachineInstruction.AND -> analyzeMachineInstruction,
    MachineInstruction.OR -> analyzeMachineInstruction,
    MachineInstruction.XOR -> analyzeMachineInstruction,
    /* */
    MachineInstruction.CPA -> analyzeMachineInstruction,
    MachineInstruction.CPL -> analyzeMachineInstruction,
    /* */
    MachineInstruction.SLA -> analyzeMachineInstruction,
    MachineInstruction.SRA -> analyzeMachineInstruction,
    MachineInstruction.SLL -> analyzeMachineInstruction,
    MachineInstruction.SRL -> analyzeMachineInstruction,
    /* */
    MachineInstruction.JMI -> analyzeMachineInstruction,
    MachineInstruction.JNZ -> analyzeMachineInstruction,
    MachineInstruction.JZE -> analyzeMachineInstruction,
    MachineInstruction.JUMP -> analyzeMachineInstruction,
    MachineInstruction.JPL -> analyzeMachineInstruction,
    MachineInstruction.JOV -> analyzeMachineInstruction,
    /* */
    MachineInstruction.PUSH -> analyzeMachineInstruction,
    MachineInstruction.POP -> analyzeMachineInstruction,
    MachineInstruction.CALL -> analyzeMachineInstruction,
    MachineInstruction.RET -> analyzeMachineInstruction,
    MachineInstruction.SVC -> analyzeMachineInstruction
  )

  def existsInstruction(code: String): Boolean =
    this.INSTRUCTION_ANALYZE_MAP.contains(code)

  val INSTRUCTION_INF_MAP = Map(
    /* Assembly Instructions */
    AssemblyInstruction.START -> InstructionInfo(-100, 0),
    AssemblyInstruction.END -> InstructionInfo(-101, 0),
    AssemblyInstruction.DS -> InstructionInfo(0, 0),
    AssemblyInstruction.DC -> InstructionInfo(0, 0),
    /* Macro Instructions */
    MacroInstruction.IN -> InstructionInfo(0x90, 3),
    MacroInstruction.OUT -> InstructionInfo(0x91, 3),
    MacroInstruction.RPUSH -> InstructionInfo(0xa0, 1),
    MacroInstruction.RPOP -> InstructionInfo(0xa1, 1),
    /* Only Nop */
    MachineInstruction.NOP -> InstructionInfo(0x00, 1),
    /* */
    MachineInstruction.LD2 -> InstructionInfo(0x10, 2),
    MachineInstruction.ST -> InstructionInfo(0x11, 2),
    MachineInstruction.LAD -> InstructionInfo(0x12, 2),
    MachineInstruction.LD1 -> InstructionInfo(0x14, 1),
    /* */
    MachineInstruction.ADDA2 -> InstructionInfo(0x20, 2),
    MachineInstruction.SUBA2 -> InstructionInfo(0x21, 2),
    MachineInstruction.ADDL2 -> InstructionInfo(0x22, 2),
    MachineInstruction.SUBL2 -> InstructionInfo(0x23, 2),
    MachineInstruction.ADDA1 -> InstructionInfo(0x24, 1),
    MachineInstruction.SUBA1 -> InstructionInfo(0x25, 1),
    MachineInstruction.ADDL1 -> InstructionInfo(0x26, 1),
    MachineInstruction.SUBL1 -> InstructionInfo(0x27, 1),
    /* */
    MachineInstruction.AND2 -> InstructionInfo(0x30, 2),
    MachineInstruction.OR2 -> InstructionInfo(0x31, 2),
    MachineInstruction.XOR2 -> InstructionInfo(0x32, 2),
    MachineInstruction.AND1 -> InstructionInfo(0x34, 1),
    MachineInstruction.OR1 -> InstructionInfo(0x35, 1),
    MachineInstruction.XOR1 -> InstructionInfo(0x36, 1),
    /* */
    MachineInstruction.CPA2 -> InstructionInfo(0x40, 2),
    MachineInstruction.CPL2 -> InstructionInfo(0x41, 2),
    MachineInstruction.CPA1 -> InstructionInfo(0x44, 1),
    MachineInstruction.CPL1 -> InstructionInfo(0x45, 1),
    /* */
    MachineInstruction.SLA -> InstructionInfo(0x50, 2),
    MachineInstruction.SRA -> InstructionInfo(0x51, 2),
    MachineInstruction.SLL -> InstructionInfo(0x52, 2),
    MachineInstruction.SRL -> InstructionInfo(0x53, 2),
    /* */
    MachineInstruction.JMI -> InstructionInfo(0x61, 2),
    MachineInstruction.JNZ -> InstructionInfo(0x62, 2),
    MachineInstruction.JZE -> InstructionInfo(0x63, 2),
    MachineInstruction.JUMP -> InstructionInfo(0x64, 2),
    MachineInstruction.JPL -> InstructionInfo(0x65, 2),
    MachineInstruction.JOV -> InstructionInfo(0x66, 2),
    /* */
    MachineInstruction.PUSH -> InstructionInfo(0x70, 2),
    MachineInstruction.POP -> InstructionInfo(0x71, 1),
    MachineInstruction.CALL -> InstructionInfo(0x80, 2),
    MachineInstruction.RET -> InstructionInfo(0x81, 1),
    MachineInstruction.SVC -> InstructionInfo(0xf0, 2)
  )

  /**
    * parse instruction Code & operands List
    *
    * @param code Like 'START', 'END', 'DS' etc.
    * @param operands GR0 - GR8, Address, Constants etc.
    * @param scope Instruction Scope
    * @return
    */
  def parseOperand(code: String,
                   operands: List[String],
                   scope: String): Either[String, Instruction] = {
    if (INSTRUCTION_ANALYZE_MAP.contains(code)) {
      INSTRUCTION_ANALYZE_MAP(code)(code, operands, scope)
    } else
      Left(
        ERR_UNSUPPORTED_OPERATION_CODE + s" ($code: ${operands.mkString(",")})")
  }

  /*--------------------------------------------
   * Below Function For Analyze Each Operands
   *
   *--------------------------------------------
   */

  private def analyzeStart =
    (code: String, operands: List[String], scope: String) => {

      val info = INSTRUCTION_INF_MAP(code)
      if (operands == List.empty)
        Right(AssemblyInstruction(code, OperandStart(None), info, scope))
      else if (operands.size == 1
               && operands.head.matches(REGEX_LABEL)
               && !NOT_ALLOW_LABEL.contains(operands.head)) {
        val ope = OperandStart(Some(LabelOfOperand(operands.head, None)))
        Right(AssemblyInstruction(code, ope, info, scope))
      } else Left(ERR_NO_GOOD_OPERAND + s" ($code: ${operands.mkString(",")})")
    }

  private def analyzeEnd =
    (code: String, operands: List[String], scope: String) => {
      val info = INSTRUCTION_INF_MAP(code)
      if (operands == List.empty)
        Right(new AssemblyInstruction(code, new OperandNoArg, info, scope))
      else Left(ERR_NO_GOOD_OPERAND + s"($code, ${operands.mkString(",")})")
    }

  private def analyzeDs =
    (code: String, operands: List[String], scope: String) => {
      val info = INSTRUCTION_INF_MAP(code)
      if (operands.size == 1
          && operands.head.matches(REGEX_DS_NUM)
          && operands.head.toInt >= DS_MIN_NUM
          && operands.head.toInt <= DS_MAX_NUM) {
        val ope = OperandDs(operands.head.toInt)
        Right(AssemblyInstruction(code, ope, info, scope))
      } else Left(ERR_NO_GOOD_OPERAND + s"($code, ${operands.mkString(",")})")
    }

  private def analyzeDc =
    (code: String, operands: List[String], scope: String) => {
      val info = INSTRUCTION_INF_MAP(code)
      if (operands.isEmpty) Left(ERR_NO_GOOD_OPERAND + s"($code )")
      else {
        val elements = operands.map(element => analyzeConstants(element))
        if (elements.exists(e => e.isInstanceOf[IncorrectDescription])) {
          Left(ERR_NO_GOOD_OPERAND + s" ($code: ${operands.mkString(",")})")
        } else
          Right(AssemblyInstruction(code, OperandDc(elements), info, scope))
      }
    }

  private def analyzeInOrOut =
    (code: String, operands: List[String], scope: String) => {
      val info = INSTRUCTION_INF_MAP(code)
      if (operands.size == 2
          && operands.count(_.matches(REGEX_LABEL)) == 2
          && operands.count(!NOT_ALLOW_LABEL.contains(_)) == 2) {
        val ope = OperandInOrOut(operands.map(LabelOfOperand(_, None)))
        Right(MacroInstruction(code, ope, info, scope))
      } else {
        Left(ERR_NO_GOOD_OPERAND + s"($code, ${operands.mkString(",")})")
      }
    }

  private def analyzeRpushOrRpop =
    (code: String, operands: List[String], scope: String) => {
      val info = INSTRUCTION_INF_MAP(code)
      if (operands == List.empty) {
        Right(new MacroInstruction(code, new OperandNoArg, info, scope))
      } else {
        Left(ERR_NO_GOOD_OPERAND + s"($code, ${operands.mkString(",")})")
      }
    }

  private def analyzeMachineInstruction =
    (code: String, operands: List[String], scope: String) => {
      analyzeMachineOperands(operands)
        .map { ope =>
          this
            .createMachineInstruction(code, ope, scope)
            .map { mi =>
              this
                .validCodeAndOperand(mi)
                .map { m =>
                  Right(m)
                }
                .getOrElse(Left(
                  ERR_NO_GOOD_OPERAND + s" ($code: ${operands.mkString(",")})"))
            }
            .getOrElse(Left(
              ERR_NO_GOOD_OPERAND + s" ($code: ${operands.mkString(",")})"))
        }
        .getOrElse(Left(
          ERR_NO_GOOD_OPERAND + s" ($code: ${operands.mkString(",")})"))
    }

  /**
    *  few instruction code is ***.
    *  LD GR0, 1, 1 -> binary  0x10 0, 1, 1
    *  LD GR0, GR1  -> binary  0x14 1, 1
    *
    * @param code Instruction Code (without Number)
    * @param ope Operand
    * @param scope Instruction Scope
    * @return
    */
  private def createMachineInstruction(
      code: String,
      ope: Operand,
      scope: String): Option[MachineInstruction] = {
    
    val internalCode: String = (code, ope) match {
      case (MachineInstruction.LD, _ : OperandR_ADR_X) =>
        MachineInstruction.LD2
      case (MachineInstruction.LD, _ : OperandR1R2) =>
        MachineInstruction.LD1
      case (MachineInstruction.ADDA, _ : OperandR_ADR_X) =>
        MachineInstruction.ADDA2
      case (MachineInstruction.SUBA, _ : OperandR_ADR_X) =>
        MachineInstruction.SUBA2
      case (MachineInstruction.ADDL, _ : OperandR_ADR_X) =>
        MachineInstruction.ADDL2
      case (MachineInstruction.SUBL, _ : OperandR_ADR_X) =>
        MachineInstruction.SUBL2

      case (MachineInstruction.ADDA, _ : OperandR1R2) =>
        MachineInstruction.ADDA1
      case (MachineInstruction.SUBA, _ : OperandR1R2) =>
        MachineInstruction.SUBA1
      case (MachineInstruction.ADDL, _ : OperandR1R2) =>
        MachineInstruction.ADDL1
      case (MachineInstruction.SUBL, _ : OperandR1R2) =>
        MachineInstruction.SUBL1

      case (MachineInstruction.AND, _ : OperandR_ADR_X) =>
        MachineInstruction.AND2
      case (MachineInstruction.OR, _ : OperandR_ADR_X) =>
        MachineInstruction.OR2
      case (MachineInstruction.XOR, _ : OperandR_ADR_X) =>
        MachineInstruction.XOR2

      case (MachineInstruction.AND, _ : OperandR1R2) =>
        MachineInstruction.AND1
      case (MachineInstruction.OR, _ : OperandR1R2) => MachineInstruction.OR1
      case (MachineInstruction.XOR, _ : OperandR1R2) =>
        MachineInstruction.XOR1

      case (MachineInstruction.CPA, _ : OperandR_ADR_X) =>
        MachineInstruction.CPA2
      case (MachineInstruction.CPL, _ : OperandR_ADR_X) =>
        MachineInstruction.CPL2

      case (MachineInstruction.CPA, _ : OperandR1R2) =>
        MachineInstruction.CPA1
      case (MachineInstruction.CPL, _ : OperandR1R2) =>
        MachineInstruction.CPL1

      case _ => code
    }

    if (INSTRUCTION_INF_MAP.exists(_._1 == internalCode)) {
      val info = INSTRUCTION_INF_MAP(internalCode)
      Some(MachineInstruction(internalCode, ope, info, scope))
    } else {
      None
    }
  }

  private def analyzeMachineOperands(operands: List[String]): Option[Operand] =
    operands match {
      case Nil => Some(new OperandNoArg)
      case List(r, adr, x)
          if this.GENERAL_REGISTERS_MAP.contains(r) && 
            this.INDEX_REGISTERS_MAP.contains(x) => 
        val adr_ope = analyzeAddressOperand(adr)
        Some(
          OperandR_ADR_X(this.GENERAL_REGISTERS_MAP(r),
                         adr_ope,
                         this.INDEX_REGISTERS_MAP(x)))
      case List(r1, r2)
          if this.GENERAL_REGISTERS_MAP.contains(r1) &&
            this.GENERAL_REGISTERS_MAP.contains(r2) => 
        Some(
          OperandR1R2(this.GENERAL_REGISTERS_MAP(r1),
                      this.GENERAL_REGISTERS_MAP(r2)))
      case List(r, adr) if this.GENERAL_REGISTERS_MAP.contains(r) => 
        val adr_ope = analyzeAddressOperand(adr)
        Some(OperandR_ADR_X(this.GENERAL_REGISTERS_MAP(r), adr_ope, 0))
      case List(r) if this.GENERAL_REGISTERS_MAP.contains(r) => 
        Some(OperandR(this.GENERAL_REGISTERS_MAP(r)))
      case List(adr, x) =>
        val adr_ope = analyzeAddressOperand(adr)
        Some(OperandADR_X(adr_ope, this.INDEX_REGISTERS_MAP(x)))
      case List(adr) =>
        val adr_ope = analyzeAddressOperand(adr)
        Some(OperandADR(adr_ope))
      case _ => None
    }

  private def analyzeAddressOperand(adr: String): ElementOfAddressOperand = {
    try {
      if (adr.startsWith("=")) { // this is bug
        throw new IllegalArgumentException(s" please $adr replace to LABEL ")
      } else if (adr.matches(REGEX_NUM)) {
        AddressOfOperand(
          adr,
          Helper.bitToUnsignedShort(java.lang.Integer.parseInt(adr)))
      } else if (adr.matches(REGEX_HEX)) {
        AddressOfOperand(adr, java.lang.Integer.parseInt(adr.drop(1), 16))
      } else if (adr.matches(REGEX_LABEL)
                 && !NOT_ALLOW_LABEL.contains(adr)) {
        LabelOfOperand(adr, None)
      } else {
        IncorrectDescription(adr)
      }
    } catch {
      case _: NumberFormatException =>
        IncorrectDescription(adr)
    }

  }

  private def analyzeConstants(const: String): ElementOfDcInstruction = {
    try {
      if (const.matches(REGEX_NUM)) {
        ConstsNumOfOperand(
          const,
          Helper.bitToUnsignedShort(java.lang.Integer.parseInt(const)))
      } else if (const.matches(REGEX_HEX)) {
        ConstsNumOfOperand(const, java.lang.Integer.parseInt(const.drop(1), 16))
      } else if (const.startsWith("'") && const.endsWith("'")) {
        if (const.length > DC_STRING_MAX_LENGTH)
          IncorrectDescription(const)
        else
          ConstsStringOfOperand(const,
                                const
                                  .drop(1)
                                  .dropRight(1)
                                  .replace("''", "'")
                                  .toCharArray
                                  .map(c => c.toInt)
                                  .toList)
      } else if (const.matches(REGEX_LABEL)
                 && !NOT_ALLOW_LABEL.contains(const)) {
        LabelOfOperand(const, None)
      } else {
        IncorrectDescription(const)
      }
    } catch {
      case _: NumberFormatException =>
        IncorrectDescription(const)
    }
  }

  private def validCodeAndOperand(
      mi: MachineInstruction): Option[Instruction] = {
    (mi.code, mi.ope) match {
      case (MachineInstruction.NOP, _: OperandNoArg) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.LD2, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.ST, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.LAD, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.LD1, _: OperandR1R2) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.ADDA2, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.SUBA2, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.ADDL2, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.SUBL2, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.ADDA1, _: OperandR1R2) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.SUBA1, _: OperandR1R2) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.ADDL1, _: OperandR1R2) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.SUBL1, _: OperandR1R2) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.AND2, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.OR2, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.XOR2, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.AND1, _: OperandR1R2) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.OR1, _: OperandR1R2) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.XOR1, _: OperandR1R2) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.CPA2, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.CPL2, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.CPA1, _: OperandR1R2) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.CPL1, _: OperandR1R2) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.SLA, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.SRA, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.SLL, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.SRL, _: OperandR_ADR_X) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.JMI | MachineInstruction.JNZ |
            MachineInstruction.JZE | MachineInstruction.JUMP |
            MachineInstruction.JPL | MachineInstruction.JOV,
            (_: OperandADR_X | _: OperandADR)) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.PUSH, (_: OperandADR_X | _: OperandADR)) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.POP, _: OperandR) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.CALL, (_: OperandADR_X | _: OperandADR)) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.RET, _: OperandNoArg) =>
        validIncorrectDescription(mi)
      case (MachineInstruction.SVC, (_: OperandADR_X | _: OperandADR)) =>
        validIncorrectDescription(mi)
      case _ => None
    }
  }

  private def validIncorrectDescription(
      mi: MachineInstruction): Option[Instruction] = {
    val add = mi.ope match {
      case ope: OperandADR     => List(ope.address)
      case ope: OperandR_ADR_X => List(ope.address)
      // case ope: OperandDc      => ope.consts // Not MachineInstruction
      case ope: OperandADR_X => List(ope.address)
      case _                 => List.empty
    }

    if (add.exists(e => e.isInstanceOf[IncorrectDescription])) {
      None
    } else Some(mi)
  }

}
