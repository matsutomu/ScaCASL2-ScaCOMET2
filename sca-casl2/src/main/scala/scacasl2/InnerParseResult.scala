package scacasl2

import scacasl2.instruction._
import scacasl2.operand.OperandDc

private[scacasl2] case class InnerParseResult(
    lineNumber: Int,
    instructions: List[InstructionRichInfo],
    symbolTable: Map[String, Int],
    errors: List[ParseError],
    additionalDc: List[AdditionalDc],
    errAdditionalDc: List[String],
    startFound: Boolean = false,
    isDataExists: Boolean = false,
    currentScope: String,
    instStepCounter: Int,
    currentOperands: List[String]) {

  /**
    * Good Parse Result
    *
    * @return
    */
  def isValid: Boolean = this.errors.isEmpty && this.errAdditionalDc.isEmpty

  /**
    * CASL2 check Start for current scope
    * @return
    */
  private def checkFirst(line: InstructionLine): InnerParseResult = {
    if (line.code == AssemblyInstruction.START && this.currentScope.isEmpty)
      this.copy(currentScope = line.lbl.getOrElse(""))
    else
      this
  }

  /**
    * Parse Line and Results convert InnerParseResult(this)
    *
    * @param line is ProgramLine
    * @return
    */
  def parseEachLine(line: ProgramLine): InnerParseResult = line match {
    case _: CommentLine => this.copy(lineNumber = this.lineNumber + 1)
    case r: InstructionLine =>
      this
        .checkFirst(r)
        .convertForEqualConstants(r.operands, this.currentScope)
        .parseInstructionLine(r)
  }

  /**
    * Equal Constants replace
    *
    */
  private def convertForEqualConstants(
      operands: List[String],
      currentScope: String): InnerParseResult = 
    operands match {
      case Nil => this.copy(currentOperands = List.empty[String])
      case _ =>
        val replaced = this.procIncludeEqualOperands(operands)
        this.copy(currentOperands = replaced.replacedOperand,
          additionalDc = replaced.dc ::: this.additionalDc,
          errAdditionalDc = replaced.errdc ::: this.errAdditionalDc)
        
    }

  case class EqualConstantsResult(replacedOperand: List[String],
                                  dc: List[AdditionalDc],
                                  errdc: List[String])

  private def procIncludeEqualOperands(
      operands: List[String]): EqualConstantsResult = {
    val reg_equal = "=-?[0-9]+|=#[0-9A-Fa-f]{4}|='[^¥s]+'"

    def innerIncludeEqualOperands(
        operands: List[String],
        currentIndex: Int,
        result: EqualConstantsResult): EqualConstantsResult = {
      operands match {
        case Nil => result
        case x :: y if x.matches(reg_equal) => 
          val newLabel = s"EL${this.lineNumber}C$currentIndex"
          val res = InstructionFactory.parseOperand(AssemblyInstruction.DC,
                                                    List(x.drop(1)),
                                                    currentScope) match {
            case Right(c) =>
              result.copy(
                replacedOperand = newLabel :: result.replacedOperand,
                dc = AdditionalDc(currentScope + "." + newLabel, c) :: result.dc)
            case Left(msg) =>
              result.copy(replacedOperand = newLabel :: result.replacedOperand,
                          errdc = msg :: result.errdc)
          }
          innerIncludeEqualOperands(y, currentIndex + 1, res)
        case x :: y => 
          val res = result.copy(replacedOperand = x :: result.replacedOperand)
          innerIncludeEqualOperands(y, currentIndex + 1, res)
      }
    }

    val res = innerIncludeEqualOperands(
      operands,
      1,
      EqualConstantsResult(List.empty[String],
                           List.empty[AdditionalDc],
                           List.empty[String]))

    res.copy(replacedOperand = res.replacedOperand.reverse,
             dc = res.dc.reverse,
             errdc = res.errdc.reverse)
  }

  /**
    * LABEL & Address to Map(Label, Address)
    * 
    */
  private def createNewSymbol(
      instructionLine: InstructionLine): Option[Map[String, Int]] = {
    if (instructionLine.lbl.isDefined && this.isValid) {
      if (this.currentScope == instructionLine.lbl.get) // global start
        Some(Map("." + instructionLine.lbl.get -> this.instStepCounter))
      else
        Some(Map(
          this.currentScope + "." + instructionLine.lbl.get -> this.instStepCounter))
    } else None
  }

  /**
    * InstructionLine convert to InnerParseResult
    * 
    */
  private def parseInstructionLine(
      instruction: InstructionLine): InnerParseResult = {

    InstructionFactory.parseOperand(instruction.code,
                                    this.currentOperands,
                                    this.currentScope) match {
      case Left(msg) => this.appendError(msg, "", instruction.raw_string)
      case Right(c) => 
        val newSymbol = this.createNewSymbol(instruction)

        instruction.code match {
          case AssemblyInstruction.START => 
            // No Label Or exists Start
            if (instruction.lbl.isEmpty) {
              this.createInnerResult(
                instruction,
                Some(c),
                newSymbol,
                None,
                Some(
                  ParseError(instruction.line_number,
                             "START need Label",
                             "",
                             instruction.raw_string)),
                startFound = true,
                this.isDataExists
              )
            } else if (this.startFound) {
              this.createInnerResult(
                instruction,
                Some(c),
                newSymbol,
                None,
                Some(
                  ParseError(instruction.line_number,
                             "START is found before END",
                             "",
                             instruction.raw_string)),
                startFound = true,
                this.isDataExists
              )
            } else {
              this.createInnerResult(instruction,
                                     Some(c),
                                     newSymbol,
                                     instruction.lbl,
                                     None,
                                     startFound = true,
                                     this.isDataExists)
            }
          
          case AssemblyInstruction.END => 
            if (!this.startFound) {
              this.createInnerResult(
                instruction,
                None,
                newSymbol,
                None,
                Some(
                  ParseError(instruction.line_number,
                             "START is not found.",
                             "",
                             instruction.raw_string)),
                startFound = false,
                this.isDataExists
              )
            } else {
              this.createInnerResult(instruction,
                                     None,
                                     newSymbol,
                                     None,
                                     None,
                                     startFound = false,
                                     this.isDataExists)
            }

          case MachineInstruction.RET => 
            if (this.isDataExists) {
              this.createInnerResult(
                instruction,
                Some(c),
                newSymbol,
                Some(this.currentScope),
                Some(
                  ParseError(instruction.line_number,
                             "Data definition in program.",
                             "",
                             instruction.raw_string)),
                this.startFound,
                isDataExists = false
              )
            } else {
              this.createInnerResult(instruction,
                                     Some(c),
                                     newSymbol,
                                     Some(this.currentScope),
                                     None,
                                     this.startFound,
                                     isDataExists = false)
            }

          case AssemblyInstruction.DS => 
            this.createInnerResult(instruction,
                                   Some(c),
                                   newSymbol,
                                   Some(this.currentScope),
                                   None,
                                   this.startFound,
                                   isDataExists = true)

          case AssemblyInstruction.DC =>
            this.createInnerResult(instruction,
                                   Some(c),
                                   newSymbol,
                                   Some(this.currentScope),
                                   None,
                                   this.startFound,
                                   isDataExists = true)

          case _ =>
            this.createInnerResult(instruction,
                                   Some(c),
                                   newSymbol,
                                   Some(this.currentScope),
                                   None,
                                   this.startFound,
                                   this.isDataExists)
        }
    }
  }

  /**
    * Create InnerResult from current status and Option Parameter
    *
    */
  private def createInnerResult(instructionLine: InstructionLine,
                                instruction: Option[Instruction],
                                newSymbol: Option[Map[String, Int]],
                                scope: Option[String],
                                parseError: Option[ParseError],
                                startFound: Boolean,
                                isDataExists: Boolean): InnerParseResult = {
    this.copy(
      lineNumber = this.lineNumber + 1,
      instructions =
        if (instruction.isDefined)
          InstructionRichInfo(instructionLine, instruction.get) :: this.instructions
        else this.instructions,
      symbolTable =
        if (newSymbol.isDefined) this.symbolTable ++ newSymbol.get
        else this.symbolTable,
      currentScope = if (scope.isDefined) scope.get else "",
      instStepCounter =
        if (instruction.isDefined)
          this.instStepCounter + instruction.get.wordSize
        else this.instStepCounter,
      errors =
        if (parseError.isDefined) parseError.get :: this.errors
        else this.errors,
      startFound = startFound,
      isDataExists = isDataExists
    )

  }

  /**
    * ParseError append to Current ErrorList
    * @param title error summary
    * @param message error detail
    * @return
    */
  def appendError(title: String,
                  message: String,
                  line: String): InnerParseResult = {
    this.copy(
      lineNumber = this.lineNumber + 1,
      errors = ParseError(this.lineNumber, title, message, line) :: this.errors)
  }

  /**
    * CASL2 need END LINE
    * @return
    */
  def checkLast(): InnerParseResult = {
    if (this.startFound)
      this.copy(errors = ParseError(this.lineNumber,
                                    "END is not found.",
                                    "",
                                    "") :: this.errors)
    else this.copy()
  }

  /**
    * Addtional DC append & Convert To ParseResult
    * @return
    */
  def convertCaslParseResult(): CaslParseResult = {
    if (this.additionalDc.isEmpty) this.parseResult
    else {

      val (tmpSymbol, tmpCounter, tmpInst) =
        this.additionalDc.foldLeft(
          (Map.empty[String, Int],
           this.instStepCounter,
           List.empty[InstructionRichInfo])) { (e, a) =>
          (e._1 ++ Map(a.label -> e._2),
           e._2 + a.instruction.wordSize,
           InstructionRichInfo(
             InstructionLine(Some(a.label),
                             AssemblyInstruction.DC,
                             a.instruction.ope
                               .asInstanceOf[OperandDc]
                               .consts
                               .map(y => y.toString),
                             None,
                             this.lineNumber,
                             ""),
             a.instruction
           ) :: e._3)
        }

      this
        .copy(
          symbolTable = this.symbolTable ++ tmpSymbol,
          instStepCounter = tmpCounter,
          instructions = tmpInst ::: this.instructions,
        )
        .parseResult
    }
  }

  /**
    * Output Parse Result
    * @return
    */
  def parseResult: CaslParseResult =
    if (this.isValid)
      CaslParseResult(this.instructions.reverse,
                      this.symbolTable,
                      this.errors.reverse)
    else
      CaslParseResult(this.instructions.reverse, Map.empty, this.errors.reverse)

}

object InnerParseResult {

  def default(): InnerParseResult = apply()

  def apply(): InnerParseResult = {
    InnerParseResult(
      1,
      List.empty[InstructionRichInfo],
      Map.empty[String, Int],
      List.empty[ParseError],
      additionalDc = List.empty[AdditionalDc],
      errAdditionalDc = List.empty[String],
      currentScope = "",
      instStepCounter = 0,
      currentOperands = List.empty[String]
    )
  }
}

private[scacasl2] case class AdditionalDc(label: String,
                                          instruction: Instruction)
