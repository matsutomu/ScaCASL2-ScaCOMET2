package scacasl2

import java.security.InvalidParameterException
import better.files.File
import scala.collection.mutable.ListBuffer

/**
  * CALSII Assembler main object class
  *
  */
object ScaCasl2 {

  /**
    * command line
    */
  private val USAGE = "usage:" + ScaCasl2.getClass.getSimpleName + " [options] input.cas [output.com]"

  /**
    * entry point
    *
    * @param args
    */
  def main(args: Array[String]): Unit = {
    try {
      val options = parseArgs(args)
      val message = parseFile(options)
      message.foreach(println)

    } catch {
      case e: InvalidParameterException => {
        println(e.getMessage)
        println(USAGE)
      }
    }
  }

  /**
    * args convert to Options
    *
    * @param dump
    * @param version
    * @param help
    * @param argList
    */
  case class Options(dump: Boolean,
                     version: Boolean,
                     help: Boolean,
                     argList: List[String]) {

    def invalidOption = {
      if (help || argList.isEmpty || argList.size >= 3) true
      else false
    }

    def casFileName = argList.head

    def comFileName = {
      if (argList.size == 1 && argList.head.contains(".")) {
        argList.head.split('.')(0) + ".com"
      } else if (argList.size == 2) {
        argList(1)
      } else {
        throw new InvalidParameterException
      }
    }

  }

  /**
    * parse args
    *
    * @param args
    * @return
    */
  def parseArgs(args: Array[String]) = {

    var dump = false
    var version = false
    var help = false
    var tmpArgList = scala.collection.mutable.ListBuffer.empty[String]

    for (e <- args) {
      e match {
        case "-a" => dump = true
        case "-v" | "--version" => version = true
        case "-h" | "--help" => help = true
        case option if (option.startsWith("-")) =>
          throw new InvalidParameterException(option)
        case other => tmpArgList += other
      }
    }

    Options(dump, version, help, tmpArgList.toList)
  }

  /**
    * cals file convert to assembly file.
    *
    * #todo no good : return message & write result to file
    *
    * @param options
    * @return
    */
  def parseFile(options: Options): List[String] = {
    var tmpList: ListBuffer[String] = new ListBuffer[String]
    if (options.version) {
      tmpList += "CASLII Assembler version 0.1 (Scala) "
    } else if (options.invalidOption) {
      tmpList += USAGE
      tmpList += "  -a           turn on verbose listings"
      tmpList += "  -v --version display version and exit"
    } else {
      val comName = options.comFileName

      try {
        val f1 = File(options.casFileName)
        if (f1.exists) {
          val lines = f1.lines.toList
          val result = ProgramLineParser.parseFirst(lines)
          if (result.errors.size == 0) {
            try {
              val binaryData = ProgramLineParser.convertBinaryCode(
                result.instructionModels,
                result.symbolTable)
              val fw = File(comName)
              fw.writeByteArray(binaryData.toArray)

              tmpList += "[success]outout to " + fw.path.getFileName.toString
              if (options.dump) {
                tmpList = tmpList ++ dump(result.instructions,
                                          result.symbolTable)
              }

            } catch {
              case e: java.io.IOException =>
                tmpList += e.getMessage
                e.getStackTrace.foreach(e => tmpList += e.toString)
            }
          } else {
            tmpList += s"[error] It failed to assemble the ${f1.path.getFileName.toString}."
            result.errors.foreach { x =>
              tmpList += s"line: ${x.lineNumber}, message:  ${x.msg}"
              tmpList += s"¥t¥t  ${x.detailMsg}"
            }

          }

        } else {
          tmpList += s"[error] no input file path " + f1.path.getFileName.toString

        }

      } catch {
        case e: Exception =>
          tmpList += e.getMessage

          e.printStackTrace

          tmpList += USAGE
          tmpList += "  -a           turn on verbose listings"
          tmpList += "  -v --version display version and exit"
      }
    }

    tmpList.toList
  }

  /**
    * dump for console
    *
    * @param instructions
    * @param symbolTbl
    * @return
    */
  def dump(instructions: List[ProgramLineParser.InstructionRichInfo],
           symbolTbl: Map[String, Int]) = {
    var tmpList: ListBuffer[String] = new ListBuffer[String]
    var addr = 0
    tmpList += "Addr\tOp\t\tLine\tSource code"
    for (e <- instructions) {

      e.line
        .map { line =>
          if (line.code != "START") {
            val wordList =
              convertWordLiteral(e.model.convertToWords(symbolTbl))
            for (w <- wordList) {
              if (w == wordList.head) {
                val output = e.line
                  .map(l => l.line_number + "\t" + l.raw_string)
                  .getOrElse("")
                tmpList += "#" + "%04X".format(addr) + "\t" + s"#$w" + "\t\t" + output
              } else {
                tmpList += "#" + "%04X".format(addr) + "\t" + s"#$w" + "\t\t"
              }
              addr = addr + 1
            }
          }
        }
        .getOrElse {
          val additionalWordList =
            convertWordLiteral(e.model.convertToWords(symbolTbl))
          for (w <- additionalWordList) {
            if (w == additionalWordList.head) {
              tmpList += "#" + "%04X".format(addr) + "\t" + s"#$w" + "\t\t" + e.model.code + " " + e.model.ope
            } else {
              tmpList += "#" + "%04X".format(addr) + "\t" + s"#$w" + "\t\t"
            }
            addr = addr + 1
          }
        }
    }

    tmpList += ""
    tmpList += "Defined labels"
    for (lbl <- symbolTbl.toSeq.sortBy(_._2)) {
      tmpList += lbl._1 + " " + "#" + "%04X".format(lbl._2)
    }

    tmpList.toList
  }

  private def convertWordLiteral(binary: Array[Int]) = {
    val bits: ListBuffer[String] = new ListBuffer

    for (w <- binary) {
      bits += "%02x".format((w.toShort >> 8) & 0xff) +
        "%02x".format(w.toShort & 0xff)
    }
    bits.toList
  }

}
