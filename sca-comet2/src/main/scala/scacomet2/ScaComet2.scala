package scacomet2

import better.files.File

import scala.collection.mutable.ListBuffer
import scacasl2.Helper

object ScaComet2 {

  /*** messages ***/
  private val MSG_VERSION = "COMETII version 0.1 (Scala)"

  private val MSG_USAGE = "usage:" + ScaComet2.getClass.getSimpleName + " [options] input.com"

  private val MSG_HELP = MSG_USAGE + f"%n" +
    f"Options:%n" +
    f"  -h,  --help         show this help message and exit.%n" +
    f"  -c,  --count-step   count step.%n" +
    f"  -d,  --debug        debug execute.%n" +
    f"  -du, --dump         dump last status to last_state.txt.%n" +
    f"  -w,  --watch        watch registers.%n" +
    f"  -v,  --version display       version and  exit."

  private val MSG_HELP_FOR_WAIT_LOOP =
    """b ADDR        Set a breakpoint at specified address.
      |d NUM         Delete breakpoints.
      |di ADDR       Disassemble 32 words from specified address.
      |du ADDR       Dump 128 words of memory.
      |h             Print help.
      |i             Print breakpoints.
      |j ADDR        Set PR to ADDR.
      |m ADDR VAL    Change the memory at ADDR to VAL.
      |p             Print register status.
      |q             Quit.
      |r             Start execution of program.
      |s             Step execution.
      |st            Dump 128 words of stack image.""".stripMargin

  /*** constants ***/
  val CASL_FILE_PREFIX = 8
  val CASL_LITERAL = 2
  val CASL_START_PR_INDEX = 2 // start 0
  val OCTET = 8
  val DUMP_LINE_COUNT = 16

  /**
    * COMETII running control
    */
  private[scacomet2] var running = false

  /**
    * COMETII entry point
    *
    * @param args
    */
  def main(args: Array[String]): Unit = {
    try {
      val options = parseArgs(args)
      options.cliCommand match {
        case CliCommand.Version    => println(this.MSG_VERSION)
        case CliCommand.Help       => println(this.MSG_HELP)
        case CliCommand.InputError => println(this.MSG_HELP)
        case CliCommand.Run | CliCommand.Debug => {
          // #todo file close ?
          val f1 = File(options.comFileName)
          if (f1.exists) {
            val machine = new Machine()
            println(s"load ${f1.pathAsString} ...")
            load(f1) match {
              case Right(l) => {
                println(s"done.")
                //l.copyToArray(objM.memory, 0, l.length)
                machine.storeToMemory(l.startPr, l.binaryData.toArray)

                this.running = true
                options.watchVariables.foreach(machine.addWatch)
                if (options.cliCommand == CliCommand.Run) {
                  this.run(machine, options.watch, options.decimal)
                } else {
                  this.waitForCommand(machine)
                }

                if (options.countStep)
                  System.out.println("Step count: " + machine.stepCount)

                if (options.dump)
                  this.dumpToFile(machine)

              }
              case Left(message) => println(message)
            }
          } else {
            println(s"[error] no input file path " + f1.pathAsString)
          }
        }
      }
    } catch {
      case e: Exception => {
        println(e.getMessage)

        e.printStackTrace

        println(MSG_HELP)
      }
    }

  }

  /**
    * parse args
    *
    * @param args
    * @return
    */
  def parseArgs(args: Array[String]): CLIOptions = {

    var cliCommand: CliCommand =
      if (args.isEmpty) CliCommand.InputError else CliCommand.Run
    var countStep = false
    var dump = false
    var watch = false
    var decimal = false
    var tmpArgList = scala.collection.mutable.ListBuffer.empty[String]

    var before = ""
    val WATCH_COMMAND = "--watch="
    var tempWatchVariable = ""
    for (e <- args) {
      e match {
        case "-d" | "--debug" | "-v" | "--version" | "-h" | "--help"
            if cliCommand != CliCommand.Run =>
          cliCommand = CliCommand.Help
        case "-d" | "--debug"   => cliCommand = CliCommand.Debug
        case "-v" | "--version" => cliCommand = CliCommand.Version
        case "-h" | "--help"    => cliCommand = CliCommand.Help

        case "-c" | "--count-step" => countStep = true
        case "-du" | "--dump"      => dump = true
        case "-D" | "--decimal"    => decimal = true
        case "-w" => {
          watch = true
          before = "-w"
        }
        case option if before == "-w" => {
          tempWatchVariable = option
          before = ""
        }
        case option if option.startsWith(WATCH_COMMAND) => {
          watch = true
          tempWatchVariable = option.substring(WATCH_COMMAND.length)
        }
        case option if option.startsWith("-") =>
          cliCommand = CliCommand.InputError
        case other => tmpArgList += other
      }
    }

    this.parseWatchVariable(tempWatchVariable) match {
      case Right(v) =>
        CLIOptions(cliCommand,
                   countStep,
                   dump,
                   watch,
                   v,
                   decimal,
                   tmpArgList.toList)
      case Left(e) =>
        CLIOptions(CliCommand.InputError,
                   countStep,
                   dump,
                   watch,
                   Nil,
                   decimal,
                   tmpArgList.toList)
    }
  }

  def parseWatchVariable(targets: String): Either[String, List[String]] = {
    val targetList = targets.split(",").map(_.trim.toUpperCase).toList

    if (targets.trim.length == 0) Right(Nil)
    else if (targetList
               .filterNot(s => s.matches(Machine.WATCH_VARIABLE_REGS))
               .size == 0) {
      Right(targetList)
    } else {
      Left(s"invalid watch variable: $targets")
    }
  }

  /**
    * CLI args convert to Options
    *
    * @param cliCommand
    * @param countStep
    * @param dump
    * @param watch
    * @param argList
    */
  case class CLIOptions(cliCommand: CliCommand,
                        countStep: Boolean,
                        dump: Boolean,
                        watch: Boolean,
                        watchVariables: List[String],
                        decimal: Boolean,
                        argList: List[String]) {

    def comFileName = argList.head

  }

  /**
    * Loaded program run
    *
    * @param machine
    * @param watch
    * @param watchDecimal
    */
  def run(machine: Machine,
          watch: Boolean = false,
          watchDecimal: Boolean = false): Unit = {

    while (this.running && !machine.containsBreakPoint(machine.PR.word)) {
      if (watch) {
        machine.watchInfo(watchDecimal).foreach(println)
      }
      this.running = machine.step()
    }

    if (machine.containsBreakPoint(machine.PR.word)) {
      machine.deleteBreakPoint(1) // for run
    }
  }

  case class BinaryData(startPr: Int, binaryData: List[Int])

  /**
    * Assembly File Convert To Binary Data
    *
    * @param file
    * @return
    */
  def load(file: File): Either[String, BinaryData] = {

    if (file.size != 0) {
      val buffByte = file.byteArray
      if (buffByte.size % 2 == 0) { // 1 word = 16 bit = 2 * 1 Byte
        val buffInt = new ListBuffer[Int]
        val it = buffByte.grouped(2)
        while (it.hasNext) {
          val e = it.next()
          buffInt.append(((e(0) & 0x000000ff) << 8) | (e(1) & 0x000000ff))
        }
        if (buffInt.take(CASL_LITERAL).toList == List(0x4341, 0x534C)) // CASL
          Right(
            BinaryData(buffInt(CASL_START_PR_INDEX),
                       buffInt.drop(CASL_FILE_PREFIX).toList))
        else
          Left(s"no CASLII file: ${file.path}")
      } else {
        Left(s"no good file size: ${buffByte.size}")
      }
    } else {
      Left(s"input file no data: ${file.path}")
    }

  }

  /**
    *
    * @param machine
    */
  def waitForCommand(machine: Machine): Unit = {

    while (this.running) {
      print("ScaComet2>")
      val input = scala.io.StdIn.readLine().split("""\s""")
      val optForWait = parseArgsWaitCommand(input)
      optForWait.command match {
        case WaitForCommand.Quit => this.running = false
        case WaitForCommand.AddBreakPoints =>
          optForWait.breakPoints.foreach(machine.addBreakPoint)
        case WaitForCommand.DeleteBreakPoints =>
          optForWait.breakPointIndexes.foreach(machine.deleteBreakPoint)
        case WaitForCommand.PrintBreakPoints =>
          machine.breakPointInfo.foreach(println)
        case WaitForCommand.JumpToAddress =>
          optForWait.targetAddress1.map { address =>
            machine.PR.word = address
          }
        case WaitForCommand.WriteMemory =>
          optForWait.targetAddress1.map { address =>
            optForWait.targetAddress2.map { value =>
              machine.memory(address) = value
            }
          }
        case WaitForCommand.Run => {
          this.run(machine)
        }
        case WaitForCommand.Step => this.running = machine.step()
        case WaitForCommand.PrintStatus =>
          machine.statusInfo.foreach(println(_))
        case WaitForCommand.Disassemble =>
          machine
            .disassemble(optForWait.targetAddress1.getOrElse(0x0000), 16)
            .foreach(println)
        case WaitForCommand.DumpToConsole =>
          this.dump(machine, optForWait.targetAddress1.get).foreach(println(_))
        case WaitForCommand.DumpToFile => {
          this.dumpToFile(machine)
        }
        case WaitForCommand.DumpStack =>
          this.dump(machine, machine.SP.word).foreach(println(_))
        case WaitForCommand.PrintHelp => println(this.MSG_HELP_FOR_WAIT_LOOP)
        case WaitForCommand.Retry     => println(this.MSG_HELP_FOR_WAIT_LOOP)
      }
    }
  }

  /**
    * Wait Loop  Parse Input Command
    *
    * @param args
    * @return
    */
  def parseArgsWaitCommand(args: Array[String]) = {

    val (cmd: WaitForCommand,
         bp: List[Int],
         bpi: List[Int],
         add1: Option[Int],
         add2: Option[Int]) =
      //#todo no good - Nil, Array.empty, null . but "Array()" is Ok.
      (args.head, args.tail) match {
        case ("s", Array()) => (WaitForCommand.Step, Nil, Nil, None, None)
        case ("q", Array()) => (WaitForCommand.Quit, Nil, Nil, None, None)

        case ("b", param)
            if param.nonEmpty && args.tail.forall(e =>
              Helper.includeAddress(e)) =>
          (WaitForCommand.AddBreakPoints,
           args.tail.map(e => Helper.parseInt(e)).toList,
           Nil,
           None,
           None)
        case ("b", Array()) => (WaitForCommand.Retry, Nil, Nil, None, None)

        case ("df", Array()) =>
          (WaitForCommand.DumpToFile, Nil, Nil, None, None)

        case ("di", param)
            if param.nonEmpty && param.length == 1 && Helper.includeAddress(
              param.head) =>
          (WaitForCommand.Disassemble,
           Nil,
           Nil,
           Option(Helper.parseInt(args.tail.head)),
           None)
        case ("di", Array()) => (WaitForCommand.Retry, Nil, Nil, None, None)

        case ("du", param)
            if param.nonEmpty && param.length == 1 && Helper.includeAddress(
              param.head) =>
          (WaitForCommand.DumpToConsole,
           Nil,
           Nil,
           Option(Helper.parseInt(args.tail.head)),
           None)
        case ("du", Array()) =>
          (WaitForCommand.DumpToConsole, Nil, Nil, None, None)

        case ("d", param)
            if param.nonEmpty && param.forall(e => Helper.includeAddress(e)) =>
          (WaitForCommand.DeleteBreakPoints,
           Nil,
           param.map(e => Helper.parseInt(e)).toList,
           None,
           None)
        case ("d", Array()) => (WaitForCommand.Retry, Nil, Nil, None, None)

        case ("i", Array()) =>
          (WaitForCommand.PrintBreakPoints, Nil, Nil, None, None)

        case ("j", param)
            if param.nonEmpty && param.length == 1 && Helper.includeAddress(
              param.head) =>
          (WaitForCommand.JumpToAddress,
           Nil,
           Nil,
           Option(Helper.parseInt(args.tail.head)),
           None)
        case ("j", Array()) => (WaitForCommand.Retry, Nil, Nil, None, None)

        case ("m", param)
            if param.nonEmpty && param.length == 2 && param.forall(
              Helper.includeAddress(_)) =>
          (WaitForCommand.WriteMemory,
           Nil,
           Nil,
           Option(Helper.parseInt(param.head)),
           Option(Helper.parseInt(param.tail.head)))

        case ("m", Array()) => (WaitForCommand.Retry, Nil, Nil, None, None)
        case ("p", Array()) =>
          (WaitForCommand.PrintStatus, Nil, Nil, None, None)
        case ("r", Array())  => (WaitForCommand.Run, Nil, Nil, None, None)
        case ("st", Array()) => (WaitForCommand.DumpStack, Nil, Nil, None, None)
        case ("h", Array())  => (WaitForCommand.PrintHelp, Nil, Nil, None, None)
        case _               => (WaitForCommand.Retry, Nil, Nil, None, None)
      }

    WatchOptions(cmd, bp, bpi, add1, add2)

  }

  /**
    *
    * Watch args convert to Options For Watch Mode
    *
    * @param command
    * @param breakPoints
    * @param breakPointIndexes
    * @param targetAddress1
    * @param targetAddress2
    */
  case class WatchOptions(command: WaitForCommand,
                          breakPoints: List[Int],
                          breakPointIndexes: List[Int],
                          targetAddress1: Option[Int],
                          targetAddress2: Option[Int]) {}

  /***********************************************************************
    *
    * Dump
    *
   ***********************************************************************/
  def dump(machine: Machine): List[String] = {
    this.dump(machine, 0x0000)
  }

  def dump(machine: Machine, startAddress: Int): List[String] = {
    this.dumpMemory(machine, startAddress, DUMP_LINE_COUNT)
  }

  def dumpAll(machine: Machine): List[String] = {
    val tmpList: ListBuffer[String] = new ListBuffer[String]

    val t = machine.stepCount
    tmpList += f"Step Count: $t"
    tmpList += f"PR:  #${machine.PR.word}%04X"
    tmpList += f"SP:  #${machine.SP.word}%04X"
    tmpList += f"OF:  ${machine.OF}"
    tmpList += f"SF:  ${machine.SF}"
    tmpList += f"ZF:  ${machine.ZF}"

    machine.generalRegisters.zipWithIndex.foreach {
      case (r: Register, i: Int) =>
        tmpList += f"GR$i%d: #${r.word}%04X"
    }

    tmpList ++= this.dumpMemory(machine, 0, 0xFFFF / OCTET)

    tmpList.toList
  }

  def dumpMemory(machine: Machine,
                 startAddress: Int,
                 lines: Int): List[String] = {
    val tmpList: ListBuffer[String] = new ListBuffer[String]
    for (i <- 0 to lines) {
      val pos = startAddress + i * OCTET
      val target = machine.memory.slice(pos, pos + OCTET)

      val address = f"#$pos%04X"
      val hexValues = target.map(x => f"#$x%04X").mkString(" ")
      val charValues = target.map(Helper.intToCharForCaslII).mkString

      tmpList.append(f"$address : $hexValues%-39s $charValues%-8s".trim)

    }
    tmpList.toList
  }

  def dumpToFile(machine: Machine): Unit = {
    val f = File("last_state.txt") // todo constant
    f.write(dumpAll(machine).mkString(f"%n"))
  }

}
