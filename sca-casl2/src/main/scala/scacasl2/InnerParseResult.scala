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


  def isValid: Boolean = this.errAdditionalDc.isEmpty & this.errors.isEmpty


  def parseEachLine(line: ProgramLine): InnerParseResult = line match {
    case _: CommentLine     => this.copy(lineNumber = this.lineNumber + 1)
    case r: InstructionLine => {
      // 'Equal Const' convert to 'LABEL'
      // get
      //    - InnerResult (Addition DC Changed)
      //    - ConvertInstructionLine (Const to Label)
      val (tmpResult, tmpOperands) = r.operands.map {
        this.convertForEqualConstants(_, this.currentScope)
      } getOrElse (this.copy(), None)

      InstructionFactory.parseOperand(r.code, tmpOperands.getOrElse(List.empty), tmpResult.currentScope) match {
        case Right(c) => {
          // Add Symbol Table
          val newSymbol = if (r.lbl.isDefined && tmpResult.isValid)
            Some(Map(tmpResult.currentScope + "." + r.lbl.get -> tmpResult.instStepCounter))
          else None

          // START Opecode
          if (r.code == AssemblyInstruction.START) {
            // No Label Or exists Start
            val parseError = if (r.lbl.isEmpty) {
              Some(ParseError(r.line_number,"START need Label", "", r))
            } else if (tmpResult.startFound) {
              Some(ParseError(r.line_number, "START is found before END", "", r))
            } else {
              None
            }

            tmpResult.copy(lineNumber = tmpResult.lineNumber + 1,
              instructions = InstructionRichInfo(Some(r), c) :: tmpResult.instructions,
              symbolTable =  if(newSymbol.isDefined) newSymbol.get ++ tmpResult.symbolTable else tmpResult.symbolTable,
              currentScope = if(parseError.isDefined) "" else r.lbl.get ,
              instStepCounter = tmpResult.instStepCounter + c.wordSize,
              errors = if(parseError.isDefined) parseError.get :: tmpResult.errors else tmpResult.errors,
              startFound = true
            )

          } else if (r.code == AssemblyInstruction.END) {
            // END Opecode
            val parseError = if (!tmpResult.startFound) {
              Some(ParseError(r.line_number, "START is not found.", "", r))
            } else {
              None
            }

            // instructions = InstructionRichInfo(Some(r), c) :: tmpResult.instructions,
            tmpResult.copy(lineNumber = tmpResult.lineNumber + 1,
              symbolTable =  if(newSymbol.isDefined) newSymbol.get ++ tmpResult.symbolTable else tmpResult.symbolTable,
              currentScope = "",
              instStepCounter = tmpResult.instStepCounter + c.wordSize,
              errors = if(parseError.isDefined) parseError.get :: tmpResult.errors else tmpResult.errors,
              startFound = false
            )


          } else if (r.code == MachineInstruction.RET) {
            // RET
            val parseError = if (tmpResult.isDataExists)
              Some(ParseError(r.line_number, "Data definition in program.", "", r))
            else None

            tmpResult.copy(lineNumber = tmpResult.lineNumber + 1,
              instructions = InstructionRichInfo(Some(r), c) :: tmpResult.instructions,
              symbolTable =  if(newSymbol.isDefined) newSymbol.get ++ tmpResult.symbolTable else tmpResult.symbolTable,
              instStepCounter = tmpResult.instStepCounter + c.wordSize,
              errors = if(parseError.isDefined) parseError.get :: tmpResult.errors else tmpResult.errors,
              isDataExists = false
            )

          } else if (r.code == AssemblyInstruction.DS || r.code == AssemblyInstruction.DC) {
            tmpResult.copy(lineNumber = tmpResult.lineNumber + 1,
              instructions = InstructionRichInfo(Some(r), c) :: tmpResult.instructions,
              symbolTable =  if(newSymbol.isDefined) newSymbol.get ++ tmpResult.symbolTable else tmpResult.symbolTable,
              instStepCounter = tmpResult.instStepCounter + c.wordSize,
              isDataExists = true
            )

          } else {
            tmpResult.copy(lineNumber = tmpResult.lineNumber + 1,
              instructions = InstructionRichInfo(Some(r), c) :: tmpResult.instructions,
              symbolTable =  if(newSymbol.isDefined) newSymbol.get ++ tmpResult.symbolTable else tmpResult.symbolTable,
              instStepCounter = tmpResult.instStepCounter + c.wordSize,
            )
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
            case Right(c)  => tempDc += AdditionalDc(currentScope + "." + newLabel, c)
            case Left(msg) => errMsg += msg
          }
        } else {
          newOperands += m
        }
    }

    (this.copy(additionalDc    = tempDc.toList ::: this.additionalDc,
               errAdditionalDc = errMsg.toList ::: this.errAdditionalDc),
      Some(newOperands.toList))
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
