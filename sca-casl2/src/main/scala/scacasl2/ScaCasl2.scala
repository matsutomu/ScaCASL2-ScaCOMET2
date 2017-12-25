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
      val options = this.parseArgs(args)
      this.parseFile(options)

    } catch {
      case e: Exception => {
        println(e.getMessage)
        println(USAGE)
      }
    }
  }

  /**
    * args convert to Options
    *
    * @param argList
    */
  case class Options(command: CaslCliCommand,
                     argList: List[String]) {

    def casFileName: String = argList.head

    def comFileName: String = {
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
  def parseArgs(args: Array[String]): Options = {

    val cliCommand = args.head match {
      case "-a" => CaslCliCommand.Dump
      case "-v" | "--version" => CaslCliCommand.Version
      case "-h" | "--help"    => CaslCliCommand.Help
      case option if option.startsWith("-") =>
        CaslCliCommand.InputError
      case _ => CaslCliCommand.Run
    }

    cliCommand match {
      case CaslCliCommand.Run =>
        Options(CaslCliCommand.Run, args.toList)
      case CaslCliCommand.InputError =>
        Options(CaslCliCommand.InputError, args.toList)
      case _ =>
        Options(cliCommand, args.tail.toList)

    }
  }

  /**
    * cals file convert to assembly file.
    *
    * #todo no good : return message & write result to file
    *
    * @param options
    * @return
    */
  def parseFile(options: Options): Unit = options.command match {
    case CaslCliCommand.Version =>
      println("CASLII Assembler version 0.1 (Scala) ")

    case CaslCliCommand.Help | CaslCliCommand.InputError => {
      println(USAGE)
      println("  -a           turn on verbose listings")
      println("  -v --version display version and exit")
    }

    case CaslCliCommand.Run | CaslCliCommand.Dump => {
      val f1 = File(options.casFileName)
      if (f1.exists) {
        val result = ProgramLineParser.parseFirst(f1.lines.toList)
        if (result.errors.isEmpty) {
          try {
            val binaryData = ProgramLineParser.convertBinaryCode(
              result.instructionModels,
              result.symbolTable)

            val fw = File(options.comFileName)
            fw.writeByteArray(binaryData.toArray)

            println(s"[success]output to ${fw.pathAsString}")
            if (options.command == CaslCliCommand.Dump) {
              println(dump(result.instructions, result.symbolTable))
            }
          } catch {
            case e: java.io.IOException => e.printStackTrace()
          }
        } else {
          println(s"[error] It failed to assemble. path:${f1.pathAsString}")
          result.errors.foreach { x =>
            println(s"line: ${x.lineNumber}, message:  ${x.msg}")
            println(s"\t\t  ${x.detailMsg}")
          }
        }
      } else {
        println(s"[error] no input file. path:${f1.pathAsString}")
      }
    }
  }

  /**
    * dump for console
    *
    * @param instructions
    * @param symbolTbl
    * @return
    */
  def dump(instructions: List[InstructionRichInfo],
           symbolTbl: Map[String, Int]): Unit = {
    var addr = 0
    println("Addr\tOp\t\tLine\tSource code")

    for(l      <- instructions.filter(p => p.line.code != "START");
        (w, i) <- l.model.convertToWords(symbolTbl).zipWithIndex.toList){



      println("#" + "%04X".format(addr) + "\t" + s"#$w" + "\t\t" +
        (if(i == 0) l.line.line_number + "\t" + l.line.raw_string else ""))

      addr = addr + 1

    }

    println("")
    println("Defined labels")
    for (lbl <- symbolTbl.toSeq.sortBy(_._2)) {
      println(lbl._1 + " " + "#" + "%04X".format(lbl._2))
    }
  }

}
