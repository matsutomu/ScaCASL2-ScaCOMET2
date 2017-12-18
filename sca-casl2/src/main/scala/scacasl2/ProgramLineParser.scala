package scacasl2

import instruction._

import scala.collection.mutable.ListBuffer
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
    *  Parse Result for PyCasl2
    *
    * @param instructions
    * @param symbolTable
    * @param errors
    * @param additionalDc
    * @param equalReplaced
    * @param errAdditionalDc
    */
  private case class InnerParseResult(
      instructions: scala.collection.mutable.ListBuffer[InstructionRichInfo],
      symbolTable: scala.collection.mutable.Map[String, Int],
      errors: scala.collection.mutable.ListBuffer[ParseError],
      additionalDc: ListBuffer[Instruction],
      equalReplaced: ListBuffer[String],
      errAdditionalDc: ListBuffer[String]) {
    def isValid = {
      this.errAdditionalDc.isEmpty & this.errors.isEmpty
    }

    def parseResult = {
      ParseResult(this.instructions.toList,
                  this.symbolTable.toMap,
                  this.errors.toList)
    }

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
    def isValid = {
      this.errors.isEmpty
    }

    def instructionModels = {
      instructions.map { e =>
        e.model
      }
    }
  }

  case class InstructionRichInfo(line: Option[InstructionLine],
                                 model: Instruction)

  case class ParseError(lineNumber: Int,
                        msg: String,
                        detailMsg: String,
                        line: ProgramLine)

  /**
    * Parse Lines
    *
    * @param codeLines
    * @return
    */
  def parseFirst(codeLines: List[String]): ParseResult = {

    // 状態保持すべき情報が多すぎる

    var innerResult: InnerParseResult = InnerParseResult(
      new ListBuffer,
      scala.collection.mutable.Map.empty[String, Int],
      new ListBuffer,
      new ListBuffer,
      new ListBuffer,
      new ListBuffer
    )

    var startFound = false
    var isDataExists = false
    var currentScope = ""
    var instStepCounter = 0

    // parse 1
    for ((line, n) <- codeLines.zipWithIndex) {
      // cause n start 0
      parseLine(line, n + 1) match {
        case Right(r) =>
          r match { // Instruction or Comment
            case r: InstructionLine => {


              // 'Equal Const' convert to 'LABEL'
              val (tmpResult, tmpOperands) = r.operands.map {
                case e =>
                  convertForEqualConstants(innerResult, e, currentScope)
              } getOrElse (innerResult, None)
              innerResult = tmpResult


              val replacedInst = new InstructionLine(r.lbl,
                                                     r.code,
                                                     tmpOperands,
                                                     r.comment,
                                                     r.line_number,
                                                     r.raw_string)


              InstructionFactory.parseOperand(
                replacedInst.code,
                replacedInst.operands.getOrElse(List.empty),
                currentScope) match {
                case Right(c) => {
                  // Add Symbol Table
                  if (r.lbl.isDefined && innerResult.isValid) {
                    val newKey = currentScope + "." + r.lbl.get
                    innerResult.symbolTable += (newKey -> instStepCounter)
                  }

                  // START Opecode
                  if (replacedInst.code == AssemblyInstruction.START) {
                    // No Label Or exists Start
                    if (r.lbl.isEmpty) {
                      innerResult.errors += ParseError(r.line_number,"START need Label", "", r)
                      currentScope = ""
                    } else {
                      currentScope = r.lbl.get
                    }

                    if (startFound) {
                      innerResult.errors += ParseError(r.line_number, "START is found before END", "", r)
                      currentScope = ""
                    } else {
                      // Success
                      instStepCounter += c.wordSize
                      innerResult.instructions += InstructionRichInfo(Some(r), c)
                    }
                    // flag
                    startFound = true

                  } else if (replacedInst.code == AssemblyInstruction.END) {
                    // END Opecode
                    if (startFound) {
                      currentScope = ""
                      startFound = false
                    } else {
                      innerResult.errors += ParseError(r.line_number, "START is not found.", "", r)

                      currentScope = ""
                    }
                  } else if (replacedInst.code == MachineInstruction.RET) {
                    // RET
                    if (isDataExists) {
                      innerResult.errors += ParseError(r.line_number, "Data definition in program.", "", r)
                    }
                    isDataExists = false
                    instStepCounter += c.wordSize
                    innerResult.instructions += InstructionRichInfo(Some(r), c)

                  } else if (replacedInst.code == AssemblyInstruction.DS ||
                             replacedInst.code == AssemblyInstruction.DC) {
                    isDataExists = true
                    instStepCounter += c.wordSize
                    innerResult.instructions += InstructionRichInfo(Some(r), c)
                  } else {
                    instStepCounter += c.wordSize
                    innerResult.instructions += InstructionRichInfo(Some(r), c)
                  }
                }
                case Left(msg) => innerResult.errors += ParseError(r.line_number, msg, "", r)
              }
            }
            case _ : CommentLine => // nop
          }
        case Left(msg) => innerResult.errors += ParseError(n, "parse error", msg, null)
      }
    }

    if (startFound) innerResult.errors += ParseError(codeLines.size, "END is not found.", "", null)

    // append addtional DC
    (innerResult.equalReplaced, innerResult.additionalDc).zipped.map {
      (dcLabel, e) =>
        if (innerResult.isValid) {
          innerResult.symbolTable += (dcLabel -> instStepCounter)
        }
        instStepCounter += e.wordSize
        innerResult.instructions += InstructionRichInfo(None, e)
    }

    // return
    innerResult.parseResult

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
              s"(${r.code}, ${r.operands.mkString(",")})")
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
                                       currentScope: String) = {
    val reg_equal = "=-?[0-9]+|=#[0-9A-Fa-f]{4}|='[^¥s]+'"

    val newOperands: ListBuffer[String] = new ListBuffer

    // what return ?
    operands.zipWithIndex.map {
      case (m, i) =>
        if (m.matches(reg_equal)) {
          val newLabel = s"%EQLABEL$i"

          // (1) new label with scope
          innerResult.equalReplaced += currentScope + "." + newLabel
          // (2) new label
          newOperands += newLabel

          InstructionFactory.parseOperand(AssemblyInstruction.DC,
                                          List(m.drop(1)),
                                          currentScope) match {
            case Right(c)  => innerResult.additionalDc    += c    // (3 success) additional DC
            case Left(msg) => innerResult.errAdditionalDc += msg  // (3 fail   ) error msg
          }
        } else {
          // (1) ordinal operand
          newOperands += m
        }
    }

    (innerResult, Some(newOperands.toList))
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
