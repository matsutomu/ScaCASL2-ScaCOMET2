package scacasl2

import instruction._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => MutableMap}
import scala.util.parsing.combinator._

/**
  * base of program line CASL2
  *
  */
trait ProgramLine

/**
  *
  * Instruction Line
  * All the attributes are character string.
  *
  * @param lbl Label
  * @param code instruction code (Assembly, Macro, Machine)
  * @param operands operands (R1, R2, ADR, x Constants)
  * @param comment annotation, note
  * @param line_number file line number
  * @param raw_string input line not edited
  */
case class InstructionLine(lbl: Option[String],
                           code: String,
                           operands: Option[List[String]],
                           comment: Option[String],
                           line_number: Int,
                           raw_string: String)
    extends ProgramLine

/**
  * Comment Line
  * only annotation line
  *
  * @param comment annotaition, note
  * @param line_number file line number
  * @param raw_string input line not edited
  */
case class CommentLine(comment: String, line_number: Int, raw_string: String)
    extends ProgramLine

/**
  * Each Line Parser
  * I assemble none ofs the files
  *
  *  1. [Label] {white_space} {instruction_code} {operands} [";" annotation]
  *  2. [Label] {white_space} {instruction_code} [";" annotation]
  *  3. [white_space] {instruction_code} [";" annotation]
  *
  *  these info to "String"
  *
  */
object ProgramLineParser extends RegexParsers {

  /***********************************
    * Parser Combinator
   ***********************************/
  private def instructions = inst_line_with_ope | inst_line_no_ope | comment_line

  private def inst_line_with_ope =
    opt(label) ~ casl_white_space ~ code ~ operands ~ opt(annotation) ^^ {
      case lbl ~ sp1 ~ inst_code ~ opes ~ cmt =>
        InstructionLine(lbl, inst_code.trim, Some(opes.map(e => e.trim)), cmt, 0, "")
    }

  private def inst_line_no_ope =
    opt(label) ~ casl_white_space ~ code ~ opt(annotation) ^^ {
      case lbl ~ sp ~ inst_code ~ cmt =>
        InstructionLine(lbl, inst_code, None, cmt, 0, "")
    }

  private def comment_line = opt(casl_white_space) ~> annotation ^^ {
    case cmt => CommentLine(cmt, 0, "")
  }

  private def label = InstructionFactory.REGEX_LABEL.r

  private def code = """[A-Z]+""".r

  private def operands = casl_white_space ~> rep(arg_pat1 <~ opt(comma))
  private def comma = ","
  private def arg_pat1 = """('.*(?<!')'.*')|([^,;]+)""".r

  protected override val whiteSpace = """""".r

  private def casl_white_space = """\s+""".r

  private def annotation = """;.*$""".r

  /*******************************************************************/
  /**  Parser CASL2                                                  */
  /**                                                                */
  /**                                                                */
  /*******************************************************************/
  /**
    *  Parse Result State for PyCasl2
    *
    * @param instructions
    * @param symbolTable
    * @param errors
    * @param additionalDc
    * @param errAdditionalDc
    */
  private case class InnerParseResult(lineNumber: Int,
                                      instructions:  List[InstructionRichInfo],
                                      symbolTable:   Map[String, Int],
                                      errors:        List[ParseError],
                                      additionalDc:  List[AdditionalDc],
                                      errAdditionalDc: List[String],
                                      startFound:   Boolean = false,
                                      isDataExists: Boolean = false,
                                      currentScope: String,
                                      instStepCounter: Int) {

    def isValid = this.errAdditionalDc.isEmpty & this.errors.isEmpty

    def parseResult = if(this.isValid) ParseResult(this.instructions.reverse, this.symbolTable, this.errors.reverse)
    else ParseResult(this.instructions.reverse, Map.empty, this.errors.reverse)

  }

  /**
    * Public parse result
    *
    * @param instructions
    * @param symbolTable
    * @param errors
    */
  case class ParseResult(instructions: List[InstructionRichInfo],
                         symbolTable: Map[String, Int],
                         errors: List[ParseError]) {
    def isValid = this.errors.isEmpty

    def instructionModels = instructions.map { _.model }

  }

  case class InstructionRichInfo(line: Option[InstructionLine],
                                 model: Instruction)

  case class ParseError(lineNumber: Int,
                        msg: String,
                        detailMsg: String,
                        line: ProgramLine)

  case class AdditionalDc(label: String, instruction: Instruction)

  /**
    * Parse Lines
    *
    * @param codeLines
    * @return
    */
  def parseFirst(codeLines: List[String]): ParseResult = {

    val innerResult: InnerParseResult =
      InnerParseResult(1, List.empty[InstructionRichInfo], Map.empty[String, Int], List.empty[ParseError],
        List.empty[AdditionalDc], List.empty[String], startFound = false, isDataExists = false, "", 0)

    def parseEachLine(innerResult: InnerParseResult, line: ProgramLine): InnerParseResult = {
      line match {
        case _: CommentLine => innerResult.copy(lineNumber = innerResult.lineNumber + 1)
        case r: InstructionLine => {
          // 'Equal Const' convert to 'LABEL'
          // get
          //    - InnerResult (Addition DC Changed)
          //    - ConvertInstructionLine (Const to Label)
          val (tmpResult, tmpOperands) = r.operands.map {
            case e => convertForEqualConstants(innerResult, e, innerResult.currentScope)
          } getOrElse (innerResult, None)

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
            case Left(msg) => innerResult.copy(lineNumber = innerResult.lineNumber + 1,
                errors = ParseError(r.line_number, msg, "", r) :: tmpResult.errors
              )
          }
        }
      }
    }

    val finalInnerResult = codeLines.foldLeft(innerResult) { (result, line) =>
      this.parseLine(line, result.lineNumber) match {
        case Right(r) =>  parseEachLine(result, r)
        case Left(msg) =>
          result.copy(lineNumber = result.lineNumber + 1,
            errors = ParseError(result.lineNumber, "parse error", msg, null) :: result.errors)
      }
    }

    val finalInnerResult2 = if (finalInnerResult.startFound)
      finalInnerResult.copy(errors = ParseError(finalInnerResult.lineNumber, "END is not found.", "", null) :: finalInnerResult.errors)
    else finalInnerResult

    // append addtional DC
    if(finalInnerResult2.additionalDc.size == 0) finalInnerResult2.parseResult
    else {
      val a = finalInnerResult2.additionalDc.map(add => add.label -> add.instruction.wordSize).toMap
      finalInnerResult2.copy(
        symbolTable = a ++ finalInnerResult2.symbolTable,
        instStepCounter = finalInnerResult2.instStepCounter + finalInnerResult2.additionalDc.foldLeft(0)((i, add) => i + add.instruction.wordSize),
        instructions = finalInnerResult2.additionalDc.map(e => InstructionRichInfo(None, e.instruction)) ::: finalInnerResult2.instructions,
      ).parseResult
    }
  }

  /**
    * Parse Line
    *
    * @param input   each line
    * @param lineNo When I encode it and made a mistake, to display it
    * @return
    */
  def parseLine(input: String, lineNo: Int): Either[String, ProgramLine] =
    this.parseAll(this.instructions, input) match {
      case Success(result, _) => result match {
        case r: CommentLine     => Right(CommentLine(r.comment, lineNo, input))
        case r: InstructionLine =>
          if (InstructionFactory.existsInstruction(r.code))
            Right(InstructionLine(r.lbl, r.code, r.operands, r.comment, lineNo, input))
          else
            Left(InstructionFactory.ERR_UNSUPPORTED_OPERATION_CODE +
              s"(${r.code}, ${r.operands.getOrElse(List.empty).mkString(",")})")
      }
      case Failure(msg, _) => Left(msg)
      case Error(msg, _)   => Left(msg)
    }

  /**
    * Equal Constants replace
    *
    * @param innerResult
    * @param operands
    * @param currentScope
    * @return
    */
  private def convertForEqualConstants(innerResult: InnerParseResult,
                                       operands: List[String],
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

    (innerResult.copy(additionalDc = tempDc.toList ::: innerResult.additionalDc,
      errAdditionalDc = errMsg.toList ::: innerResult.errAdditionalDc),
      Some(newOperands.toList))
  }

  /**
    *
    *
    * @param instructs
    * @param symbol_tables
    * @return
    */
  def convertBinaryCode(instructs: List[Instruction],
                        symbol_tables: Map[String, Int]): List[Byte] = {

    val bits: ListBuffer[Byte] = new ListBuffer
    for (e <- instructs;
         w <- e.convertToWords(symbol_tables)) {
      bits += ((w.toShort >> 8) & 0xff).toByte
      bits += (w.toShort & 0xff).toByte
    }
    bits.toList
  }

}
