package scacasl2

import scacasl2.instruction._

import scala.collection.mutable.ListBuffer

private[scacasl2] case class InnerParseResult(lineNumber: Int,
                                              instructions:  List[InstructionRichInfo],
                                              symbolTable:   Map[String, Int],
                                              errors:        List[ParseError],
                                              additionalDc:  List[AdditionalDc],
                                              errAdditionalDc: List[String],
                                              startFound:   Boolean = false,
                                              isDataExists: Boolean = false,
                                              currentScope: String,
                                              instStepCounter: Int) {


  /**
   * Good Parse Result
   *
   * @return
   */
  def isValid: Boolean = this.errors.isEmpty && this.errAdditionalDc.isEmpty


  /**
   * Parse Line and Results convert InnerParseResult(this)
   *
   * @param line is ProgramLine
   * @return
   */
  def parseEachLine(line: ProgramLine): InnerParseResult = line match {
    case _: CommentLine     => this.copy(lineNumber = this.lineNumber + 1)
    case r: InstructionLine => {

      val (tmpResult, tmpOperands) = r.operands.map {
        this.convertForEqualConstants(_, this.currentScope)
      } getOrElse(this.copy(), None)

      InstructionFactory.parseOperand(r.code, tmpOperands.getOrElse(List.empty), tmpResult.currentScope) match {
        case Right(c) => {
          val newSymbol = this.createNewSymbol(r)

          r.code match {
            case AssemblyInstruction.START => {
              // No Label Or exists Start
              if (r.lbl.isEmpty) {
                this.createInnerResult(r, Some(c), newSymbol, None,
                  Some(ParseError(r.line_number, "START need Label", "", r)), startFound = true, tmpResult.isDataExists)

              } else if (tmpResult.startFound) {
                this.createInnerResult(r, Some(c), newSymbol, None,
                  Some(ParseError(r.line_number, "START is found before END", "", r)), startFound = true, tmpResult.isDataExists)
              } else {
                this.createInnerResult(r, Some(c), newSymbol, r.lbl,
                  None, startFound = true, tmpResult.isDataExists)
              }
            }

            case AssemblyInstruction.END if !tmpResult.startFound =>
              this.createInnerResult(r, None, newSymbol, None,
                Some(ParseError(r.line_number, "START is not found.", "", r)), startFound = false, tmpResult.isDataExists)

            case AssemblyInstruction.END =>
              this.createInnerResult(r, None, newSymbol, None, None, startFound = false, this.isDataExists)

            case MachineInstruction.RET  if tmpResult.isDataExists =>
              this.createInnerResult(r, Some(c), newSymbol, Some(tmpResult.currentScope),
                Some(ParseError(r.line_number, "Data definition in program.", "", r)), tmpResult.startFound, isDataExists = false)

            case MachineInstruction.RET =>
              this.createInnerResult(r, Some(c), newSymbol, Some(tmpResult.currentScope), None, tmpResult.startFound, isDataExists = false)

            case AssemblyInstruction.DS =>
              this.createInnerResult(r, Some(c), newSymbol, Some(tmpResult.currentScope), None,
                tmpResult.startFound, isDataExists = true)

            case AssemblyInstruction.DC=>
              this.createInnerResult(r, Some(c), newSymbol, Some(tmpResult.currentScope), None,
                tmpResult.startFound, isDataExists = true)

            case _ =>
              this.createInnerResult(r, Some(c), newSymbol, Some(tmpResult.currentScope), None,
                tmpResult.startFound, tmpResult.isDataExists)

          }
        }
        case Left(msg) => this.appendError(msg, "")
      }
    }
  }

  /**
   * Equal Constants replace
   *
   * @param operands
   * @param currentScope
   * @return
   */
  private def convertForEqualConstants(operands: List[String],
                                       currentScope: String): (InnerParseResult, Some[List[String]]) = {
    val reg_equal = "=-?[0-9]+|=#[0-9A-Fa-f]{4}|='[^¥s]+'"

    val newOperands: ListBuffer[String] = new ListBuffer
    val tempDc: ListBuffer[AdditionalDc] = new ListBuffer
    val errMsg: ListBuffer[String] = new ListBuffer

    // what return ?
    operands.zipWithIndex.map {
      case (m, i) =>
        if (m.matches(reg_equal)) {
          val newLabel = s"%EQLABEL$i"

          newOperands += newLabel
          InstructionFactory.parseOperand(AssemblyInstruction.DC,
            List(m.drop(1)),
            currentScope) match {
            case Right(c) => tempDc += AdditionalDc(currentScope + "." + newLabel, c)
            case Left(msg) => errMsg += msg
          }
        } else {
          newOperands += m
        }
    }

    (this.copy(additionalDc = tempDc.toList ::: this.additionalDc,
      errAdditionalDc = errMsg.toList ::: this.errAdditionalDc),
      Some(newOperands.toList))
  }

  private def createNewSymbol(instructionLine: InstructionLine): Option[Map[String, Int]] = {
    if (instructionLine.lbl.isDefined && this.isValid)
      Some(Map(this.currentScope + "." + instructionLine.lbl.get -> this.instStepCounter))
    else None
  }

  private def createInnerResult(instructionLine: InstructionLine,
                                instruction: Option[Instruction],
                                newSymbol:   Option[Map[String, Int]],
                                scope: Option[String],
                                parseError:  Option[ParseError],
                                startFound: Boolean,
                                isDataExists: Boolean): InnerParseResult = {
    this.copy(lineNumber = this.lineNumber + 1,
      instructions  = if(instruction.isDefined) InstructionRichInfo(Some(instructionLine), instruction.get) :: this.instructions else this.instructions,
      symbolTable   = if(newSymbol.isDefined) newSymbol.get ++ this.symbolTable else this.symbolTable,
      currentScope  = if(scope.isDefined) scope.get else "",
      instStepCounter = if(instruction.isDefined) this.instStepCounter + instruction.get.wordSize else this.instStepCounter,
      errors = if (parseError.isDefined) parseError.get :: this.errors else this.errors,
      startFound = startFound,
      isDataExists = isDataExists
    )

  }


  def appendError(title: String, message: String): InnerParseResult =
    this.copy(lineNumber = this.lineNumber + 1,
              errors     = ParseError(this.lineNumber, title, message, null) :: this.errors)


  def checkLast(): InnerParseResult = if (this.startFound)
    this.copy(errors = ParseError(this.lineNumber, "END is not found.", "", null) :: this.errors)
    else this.copy()

  def convertCaslParseResult(): CaslParseResult = {
    if(this.additionalDc.isEmpty) this.parseResult
    else {
      val dcMap = this.additionalDc.map(e => e.label -> e.instruction.wordSize).toMap
      this.copy(
        symbolTable = dcMap ++ this.symbolTable,
        instStepCounter = this.instStepCounter + this.additionalDc.foldLeft(0)((i, add) => i + add.instruction.wordSize),
        instructions    = this.additionalDc.map(e => InstructionRichInfo(None, e.instruction)) ::: this.instructions,
      ).parseResult
    }
  }

  def parseResult: CaslParseResult = if(this.isValid) CaslParseResult(this.instructions.reverse, this.symbolTable, this.errors.reverse)
  else CaslParseResult(this.instructions.reverse, Map.empty, this.errors.reverse)

}

object InnerParseResult {

  def default(): InnerParseResult = apply()

  def apply(): InnerParseResult = {
    InnerParseResult(1,
      List.empty[InstructionRichInfo],
      Map.empty[String, Int],
      List.empty[ParseError],
      additionalDc = List.empty[AdditionalDc],
      errAdditionalDc = List.empty[String],
      startFound = false,
      isDataExists = false,
      currentScope = "",
      instStepCounter = 0)
  }
}


private[scacasl2] case class AdditionalDc(label: String, instruction: Instruction)
