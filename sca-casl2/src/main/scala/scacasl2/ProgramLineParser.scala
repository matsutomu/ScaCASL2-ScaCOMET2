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

  private def comment_line = opt(casl_white_space) ~> annotation ^^ { CommentLine( _ , 0, "") }

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
    * Parse Lines
    *
    * @param codeLines CASL2 program lines
    * @return
    */
  def parseFirst(codeLines: List[String]): CaslParseResult = {

    codeLines.foldLeft(InnerParseResult.default()) { (result, line) =>
      this.parseLine(line, result.lineNumber) match {
        case Right(r)  => result.parseEachLine(r)
        case Left(msg) => result.appendError("parse error", msg, line)
      }
    }.checkLast()
      .convertCaslParseResult()

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
