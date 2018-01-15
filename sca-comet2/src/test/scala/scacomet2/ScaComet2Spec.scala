package scacomet2

import java.io.{ByteArrayOutputStream, PrintStream, StringReader}

import org.scalatest._
import scacomet2.ScaComet2.CLIOptions
import scacomet2.ScaComet2.BinaryData


class ScaComet2Spec extends FlatSpec with DiagrammedAssertions {

  "ScaComet2 main " should " various parameters processing " in {
    assert(this.runCometOut(Array("-h")) ===
      "usage:ScaComet2$ [options] input.com" + f"%n" +
        f"Options:%n" +
        f"  -h,  --help         show this help message and exit.%n" +
        f"  -c,  --count-step   count step.%n" +
        f"  -d,  --debug        debug execute.%n" +
        f"  -du, --dump         dump last status to last_state.txt.%n" +
        f"  -w,  --watch        watch registers.%n" +
        f"  -v,  --version display       version and  exit.%n"
    )

    assert(this.runCometOut(Array("-v")) ===
      "COMETII version 0.1 (Scala)" + f"%n"
    )

    val currentDirectory = new java.io.File(".").getCanonicalPath
    assert(this.runCometOut(Array("./sca-comet2/src/test/resources/count1.com")) ===
      f"load $currentDirectory/sca-comet2/src/test/resources/count1.com ...%n" +
      f"done.%n"
    )

    assert(this.runCometOut(Array("./sca-comet2/src/test/resources/count1_aaa.com")) ===
      f"[error] no input file path $currentDirectory/sca-comet2/src/test/resources/count1_aaa.com%n"
    )

    assert(this.runCometOut(Array("-a")) ===
      "usage:ScaComet2$ [options] input.com" + f"%n" +
        f"Options:%n" +
        f"  -h,  --help         show this help message and exit.%n" +
        f"  -c,  --count-step   count step.%n" +
        f"  -d,  --debug        debug execute.%n" +
        f"  -du, --dump         dump last status to last_state.txt.%n" +
        f"  -w,  --watch        watch registers.%n" +
        f"  -v,  --version display       version and  exit.%n"
    )

  }


  def runCometOut(args: Array[String]): String = {
    val outStream = new ByteArrayOutputStream
    val out = new PrintStream(new java.io.BufferedOutputStream(outStream), true, "utf-8")
    Console.withOut(out) {
      ScaComet2.main(args)
      out.flush()
      outStream.toString("utf-8")
    }
  }

  def runCometWithInAndOut(args: Array[String], watchIn: String): String = {
    val input     = new StringReader(s"$watchIn")
    val outStream = new ByteArrayOutputStream
    val out = new PrintStream(new java.io.BufferedOutputStream(outStream), true, "utf-8")
    Console.withIn(input){
      Console.withOut(out) {
        ScaComet2.main(args)
        out.flush()
        outStream.toString("utf-8")
      }
    }
  }


  "ScaComet2 main parameter parser " should " parse argument for CLI Commands " in {
    assert(ScaComet2.parseArgs(Array.empty[String]) ===
      CLIOptions(CliCommand.InputError,false,false,false,List(),false,List()))

    assert(ScaComet2.parseArgs(Array("-h")) ===
      CLIOptions(CliCommand.Help,false,false,false,List(),false,List()))

    assert(ScaComet2.parseArgs(Array("--help")) ===
      CLIOptions(CliCommand.Help,false,false,false,List(),false,List()))

    assert(ScaComet2.parseArgs(Array("-c")) ===
      CLIOptions(CliCommand.Run, true, false, false, List(), false, List()))

    assert(ScaComet2.parseArgs(Array("--count-step")) ===
      CLIOptions(CliCommand.Run, true, false, false, List(), false, List()))

    assert(ScaComet2.parseArgs(Array("-d")) ===
      CLIOptions(CliCommand.Debug,false,false,false,List(),false,List()))

    assert(ScaComet2.parseArgs(Array("--debug")) ===
      CLIOptions(CliCommand.Debug,false,false,false,List(),false,List()))

    assert(ScaComet2.parseArgs(Array("-du")) ===
      CLIOptions(CliCommand.Run, false, true, false, List(), false, List()))

    assert(ScaComet2.parseArgs(Array("--dump")) ===
      CLIOptions(CliCommand.Run, false, true, false, List(), false, List()))

    assert(ScaComet2.parseArgs(Array("-w","PR,OF,SF")) ===
      CLIOptions(CliCommand.Run, false, false, true, List("PR","OF","SF"), false, Nil))

    assert(ScaComet2.parseArgs(Array("--watch=PR,OF,SF")) ===
      CLIOptions(CliCommand.Run, false, false, true, List("PR","OF","SF"), false, Nil))

    assert(ScaComet2.parseArgs(Array("-v")) ===
      CLIOptions(CliCommand.Version, false, false, false, List(), false, List()))

    assert(ScaComet2.parseArgs(Array("--version")) ===
      CLIOptions(CliCommand.Version, false, false, false, List(), false, List()))

    assert(ScaComet2.parseArgs(Array("-h", "-c", "-d", "-du", "-w", "PR,OF,SF", "-v","other-optisons")) ===
      CLIOptions(CliCommand.Help, true, true, true, List("PR", "OF", "SF"), false, List("other-optisons")))

    // Not Supported Decimal
  }

  it should " parse argument invalid param Or no good watch variables " in {
    assert(ScaComet2.parseArgs(Array("-e","AR,OF,SF")) ===
      CLIOptions(CliCommand.InputError, false, false, false, Nil, false, List("AR,OF,SF")))


    assert(ScaComet2.parseArgs(Array("-w","AR,OF,SF")) ===
      CLIOptions(CliCommand.InputError, false, false, true, Nil, false, Nil))
  }

  it should " parse watch parameter  " in {
    assert(ScaComet2.parseWatchVariable("PR, OF , SF ,ZF, GR0,GR1,GR2,GR3,GR4,GR5,GR6,GR7,GR8,SP,1234,#ffff")
      === Right(List("PR", "OF", "SF", "ZF", "GR0", "GR1", "GR2", "GR3", "GR4", "GR5", "GR6", "GR7", "GR8", "SP","1234","#FFFF")))
  }

  it should " parse watch parameter(invalid watch variables)" in {
    assert(ScaComet2.parseWatchVariable("PR, GR9,#Gfff")
      === Left("invalid watch variable: PR, GR9,#Gfff"))
  }

  "ScaComet2 file loader " should " loading binary data " in {
    val path = getClass.getClassLoader.getResource("count1.com").getPath
    val f1 = better.files.File(path)
    // Skip(16 Byte): 0x43, 0x41, 0x53, 0x4C, 0 (8bit) * 12
    assert(ScaComet2.load(f1)
      === Right(BinaryData(0, List(0x7001,    0,   0x7002,   0,0x2522, 0x3411,
        0x6300,    0x000F, 0x1222, 0x0001, 0x1201,0xFFFF, 0x3410,
        0x6300, 0x0008, 0x1402, 0x7120, 0x7110,0x8100))))
  }

  it should " loading error binary data " in {
    val path = getClass.getClassLoader.getResource("count1_err.com").getPath
    val f1 = better.files.File(path)
    assert(ScaComet2.load(f1) === Left(s"no good file size: ${f1.size}"))

    val path2 = getClass.getClassLoader.getResource("count1_err_zero.com").getPath
    val f2 = better.files.File(path2)
    assert(ScaComet2.load(f2) === Left(s"input file no data: $path2"))

    val path3 = getClass.getClassLoader.getResource("count1_no_casl.com").getPath
    val f3 = better.files.File(path3)
    assert(ScaComet2.load(f3) === Left(s"no CASLII file: $path3"))


  }

  "ScaComet2 watch mode parameter parser " should " parse argument for watch mode " in {
    assert(ScaComet2.parseArgsWaitCommand(List("s")) ===
      ScaComet2.WatchOptions(WaitForCommand.Step, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("q")) ===
      ScaComet2.WatchOptions(WaitForCommand.Quit, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("b", "0", "#1", "#ffff", "#FFFF")) ===
      ScaComet2.WatchOptions(WaitForCommand.AddBreakPoints, List(0, 1, 0xffff, 0xffff), Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("df")) ===
      ScaComet2.WatchOptions(WaitForCommand.DumpToFile, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("di", "0")) ===
      ScaComet2.WatchOptions(WaitForCommand.Disassemble, Nil, Nil, Some(0), None))

    assert(ScaComet2.parseArgsWaitCommand(List("di", "#ffff")) ===
      ScaComet2.WatchOptions(WaitForCommand.Disassemble, Nil, Nil, Some(0xffff), None))

    assert(ScaComet2.parseArgsWaitCommand(List("du")) ===
      ScaComet2.WatchOptions(WaitForCommand.DumpToConsole, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("d", "0", "#1", "#ffff", "#FFFF")) ===
      ScaComet2.WatchOptions(WaitForCommand.DeleteBreakPoints, Nil, List(0, 1, 0xffff, 0xffff), None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("i")) ===
      ScaComet2.WatchOptions(WaitForCommand.PrintBreakPoints, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("j", "0")) ===
      ScaComet2.WatchOptions(WaitForCommand.JumpToAddress, Nil, Nil, Some(0), None))

    assert(ScaComet2.parseArgsWaitCommand(List("j", "#ffff")) ===
      ScaComet2.WatchOptions(WaitForCommand.JumpToAddress, Nil, Nil, Some(0xffff), None))

    assert(ScaComet2.parseArgsWaitCommand(List("m", "0", "0")) ===
      ScaComet2.WatchOptions(WaitForCommand.WriteMemory, Nil, Nil, Some(0), Some(0)))

    assert(ScaComet2.parseArgsWaitCommand(List("m", "#ffff", "#ffff")) ===
      ScaComet2.WatchOptions(WaitForCommand.WriteMemory, Nil, Nil, Some(0xffff), Some(0xffff)))

    assert(ScaComet2.parseArgsWaitCommand(List("p")) ===
      ScaComet2.WatchOptions(WaitForCommand.PrintStatus, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("r")) ===
      ScaComet2.WatchOptions(WaitForCommand.Run, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("st")) ===
      ScaComet2.WatchOptions(WaitForCommand.DumpStack, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("h")) ===
      ScaComet2.WatchOptions(WaitForCommand.PrintHelp, Nil, Nil, None, None))

  }

  it should " parse argument for watch mode (invalid parameter -> 'Retry') " in {
    assert(ScaComet2.parseArgsWaitCommand(List("")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("A")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("s", "p1")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("q", "p1")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("b")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("df", "p1")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("di")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("du", "p1")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("d")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("i", "p1")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("j", "ffff")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("m", "#ffff")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("m")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("p", "p1")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("r", "p1")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("st", "p1")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List("h", "p1")) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))

    assert(ScaComet2.parseArgsWaitCommand(List.empty) ===
      ScaComet2.WatchOptions(WaitForCommand.Retry, Nil, Nil, None, None))
  }

  "ScaComet2 " should " load start address " in {

    // load from file
    val path = getClass.getClassLoader.getResource("start_address.com").getPath
    val f1 = better.files.File(path)
    // Skip(16 Byte): 0x43, 0x41, 0x53, 0x4C, 0 (8bit) * 12
    val result = ScaComet2.load(f1)

    val machine = new Machine
    //result.map( l => l.copyToArray(machine.memory, 0, l.length))
    result.map(e => machine.storeToMemory(e.startPr, e.binaryData.toArray))


  }

  "ScaComet2 memory " should " dump to any type " in {

    // load from file
    val path = getClass.getClassLoader.getResource("count1.com").getPath
    val f1 = better.files.File(path)
    // Skip(16 Byte): 0x43, 0x41, 0x53, 0x4C, 0 (8bit) * 12
    val result = ScaComet2.load(f1)
    assert(result === Right(BinaryData(0,
      List(
      0x7001, 0x0000, 0x7002, 0x0000,
      0x2522, 0x3411, 0x6300, 0x000F,
      0x1222, 0x0001, 0x1201, 0xFFFF,
      0x3410, 0x6300, 0x0008, 0x1402,
      0x7120, 0x7110, 0x8100))))

    val machine = new Machine
    //result.map( l => l.copyToArray(machine.memory, 0, l.length))
    result.map(e => machine.storeToMemory(e.startPr, e.binaryData.toArray))

    assert(ScaComet2.dumpMemory(machine, 0, 4) ===
          List("""#0000 : #7001 #0000 #7002 #0000 #2522 #3411 #6300 #000F ...."...""",
               """#0008 : #1222 #0001 #1201 #FFFF #3410 #6300 #0008 #1402 ".......""",
               """#0010 : #7120 #7110 #8100 #0000 #0000 #0000 #0000 #0000  .......""",
               """#0018 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........""",
               """#0020 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........"""))

    // fill
    val fillList = (5 to 16).toList.map {case i =>
      val pos = i * 8
      f"#$pos%04X : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........"
    }
    assert(ScaComet2.dump(machine) ===
      List("""#0000 : #7001 #0000 #7002 #0000 #2522 #3411 #6300 #000F ...."...""",
        """#0008 : #1222 #0001 #1201 #FFFF #3410 #6300 #0008 #1402 ".......""",
        """#0010 : #7120 #7110 #8100 #0000 #0000 #0000 #0000 #0000  .......""",
        """#0018 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........""",
        """#0020 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........""")
      ::: fillList)

    // fill
    val fillList2 = (5 to 17).toList.map {case i =>
      val pos = i * 8
      f"#$pos%04X : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........"
    }
    assert(ScaComet2.dump(machine, 8) ===
      List(
        """#0008 : #1222 #0001 #1201 #FFFF #3410 #6300 #0008 #1402 ".......""",
        """#0010 : #7120 #7110 #8100 #0000 #0000 #0000 #0000 #0000  .......""",
        """#0018 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........""",
        """#0020 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........""")
        ::: fillList2)
  }

  "ScaComet2 Registers " should " print status " in {

    // load from file
    val path = getClass.getClassLoader.getResource("count1.com").getPath
    val f1 = better.files.File(path)
    // Skip(16 Byte): 0x43, 0x41, 0x53, 0x4C, 0 (8bit) * 12
    val result = ScaComet2.load(f1)
    assert(result === Right(BinaryData(0,
      List(
      0x7001, 0x0000, 0x7002, 0x0000,
      0x2522, 0x3411, 0x6300, 0x000F,
      0x1222, 0x0001, 0x1201, 0xFFFF,
      0x3410, 0x6300, 0x0008, 0x1402,
      0x7120, 0x7110, 0x8100))))

    val machine = new Machine
    //result.map( l => l.copyToArray(machine.memory, 0, l.length))
    result.map(e => machine.storeToMemory(e.startPr, e.binaryData.toArray))

    assert(machine.statusInfo() ===
      List("PR #0000 [ #0000: #7001 #0000         PUSH     #0000, GR1 ]  STEP 0",
           "SP #FF00(  65280) FR(OF, SF, ZF) 000  (      0)",
           "GR0 #0000(      0) GR1 #0000(      0) GR2 #0000(      0) GR3 #0000(      0)",
           "GR4 #0000(      0) GR5 #0000(      0) GR6 #0000(      0) GR7 #0000(      0)"))

  }

  it should " dump to any type (all memory) " in {

    // load from file
    val path = getClass.getClassLoader.getResource("count1.com").getPath
    val f1 = better.files.File(path)
    // Skip(16 Byte): 0x43, 0x41, 0x53, 0x4C, 0 (8bit) * 12
    val result = ScaComet2.load(f1)
    assert(result === Right(BinaryData(0,
      List(
      0x7001, 0x0000, 0x7002, 0x0000,
      0x2522, 0x3411, 0x6300, 0x000F,
      0x1222, 0x0001, 0x1201, 0xFFFF,
      0x3410, 0x6300, 0x0008, 0x1402,
      0x7120, 0x7110, 0x8100))))

    val machine = new Machine
    //result.map( l => l.copyToArray(machine.memory, 0, l.length))
    result.map(e => machine.storeToMemory(e.startPr, e.binaryData.toArray))

    // fill
    val fillList = (5 to (0xffff / 8)).toList.map { case i =>
      val pos = i * 8
      f"#$pos%04X : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........"
    }
    assert(ScaComet2.dumpAll(machine) ===
      List(
        f"Step Count: 0",
        """PR:  #0000""",
        """SP:  #FF00""",
        """OF:  0""",
        """SF:  0""",
        """ZF:  0""",
        """GR0: #0000""",
        """GR1: #0000""",
        """GR2: #0000""",
        """GR3: #0000""",
        """GR4: #0000""",
        """GR5: #0000""",
        """GR6: #0000""",
        """GR7: #0000""",
        """GR8: #FF00""",
        """#0000 : #7001 #0000 #7002 #0000 #2522 #3411 #6300 #000F ...."...""",
        """#0008 : #1222 #0001 #1201 #FFFF #3410 #6300 #0008 #1402 ".......""",
        """#0010 : #7120 #7110 #8100 #0000 #0000 #0000 #0000 #0000  .......""",
        """#0018 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........""",
        """#0020 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........""")
        ::: fillList)
  }

  "ScaComet2 Machine " can  " run (breakpoints & watch ) " in {

    val currentDirectory = new java.io.File(".").getCanonicalPath
    val consoleOut = this.runCometWithInAndOut(Array("-d","./sca-comet2/src/test/resources/count1.com"),
                """b #2
                  |d 1
                  |b 2
                  |b 6
                  |di 1
                  |du 1
                  |h
                  |i
                  |j 0
                  |m #13 #0041
                  |p
                  |s
                  |st
                  |du 0
                  |r
                  |q""".stripMargin)

    assert(consoleOut ===
      f"load $currentDirectory/sca-comet2/src/test/resources/count1.com ...%n" +
        """|done.
           |ScaComet2>ScaComet2>ScaComet2>ScaComet2>ScaComet2>#0001: #0000               NOP
           |#0002: #7002 #0000         PUSH     #0000, GR2
           |#0004: #2522               SUBA     GR2, GR2
           |#0005: #3411               AND      GR1, GR1
           |#0006: #6300 #000F         JZE      #000F
           |#0008: #1222 #0001         LAD      GR2, #0001, GR2
           |#000A: #1201 #FFFF         LAD      GR0, #FFFFFFFF, GR1
           |#000C: #3410               AND      GR1, GR0
           |#000D: #6300 #0008         JZE      #0008
           |#000F: #1402               LD       GR0, GR2
           |#0010: #7120               POP      GR2
           |#0011: #7110               POP      GR1
           |#0012: #8100               RET
           |#0013: #0000               NOP
           |#0014: #0000               NOP
           |#0015: #0000               NOP
           |ScaComet2>#0001 : #0000 #7002 #0000 #2522 #3411 #6300 #000F #1222 ..."..."
           |#0009 : #0001 #1201 #FFFF #3410 #6300 #0008 #1402 #7120 .......
           |#0011 : #7110 #8100 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0019 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0021 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0029 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0031 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0039 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0041 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0049 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0051 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0059 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0061 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0069 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0071 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0079 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0081 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |ScaComet2>b ADDR        Set a breakpoint at specified address.
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
           |st            Dump 128 words of stack image.
           |ScaComet2>   1 : #0002
           |   2 : #0006
           |ScaComet2>ScaComet2>ScaComet2>PR #0000 [ #0000: #7001 #0000         PUSH     #0000, GR1 ]  STEP 0
           |SP #FF00(  65280) FR(OF, SF, ZF) 000  (      0)
           |GR0 #0000(      0) GR1 #0000(      0) GR2 #0000(      0) GR3 #0000(      0)
           |GR4 #0000(      0) GR5 #0000(      0) GR6 #0000(      0) GR7 #0000(      0)
           |ScaComet2>ScaComet2>#FEFF : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF07 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF0F : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF17 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF1F : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF27 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF2F : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF37 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF3F : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF47 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF4F : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF57 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF5F : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF67 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF6F : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF77 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#FF7F : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |ScaComet2>#0000 : #7001 #0000 #7002 #0000 #2522 #3411 #6300 #000F ...."...
           |#0008 : #1222 #0001 #1201 #FFFF #3410 #6300 #0008 #1402 ".......
           |#0010 : #7120 #7110 #8100 #0041 #0000 #0000 #0000 #0000  ..A....
           |#0018 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0020 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0028 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0030 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0038 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0040 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0048 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0050 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0058 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0060 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0068 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0070 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0078 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |#0080 : #0000 #0000 #0000 #0000 #0000 #0000 #0000 #0000 ........
           |ScaComet2>ScaComet2>""".stripMargin)

  }

  it can  " run (input command error ) " in {

    val currentDirectory = new java.io.File(".").getCanonicalPath
    val consoleOut = this.runCometWithInAndOut(Array("-d", "./sca-comet2/src/test/resources/count1.com"),
      """b #9999999999999
        |
        |q
      """.stripMargin)

    //println(consoleOut)
    // d 1 ArrayIndexOutOfBoundsException
    //#todo error input data
  }

 it can  " run (debug execute & no input (= null) ) " in {

    val currentDirectory = new java.io.File(".").getCanonicalPath
    val consoleOut = this.runCometWithInAndOut(Array("-d","./sca-comet2/src/test/resources/count1.com"),
      """
        |
        |q""".stripMargin)

    assert(consoleOut ===
      f"load $currentDirectory/sca-comet2/src/test/resources/count1.com ...%n" +
        """|done.
           |ScaComet2>b ADDR        Set a breakpoint at specified address.
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
           |st            Dump 128 words of stack image.
           |ScaComet2>b ADDR        Set a breakpoint at specified address.
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
           |st            Dump 128 words of stack image.
           |ScaComet2>""".stripMargin)

  }

  it can  " run (step count & dump file)" in {

    val currentDirectory = new java.io.File(".").getCanonicalPath
    val consoleOut = this.runCometOut(Array("-c","-du","./sca-comet2/src/test/resources/count1.com"))

    // todo compare dumptext
    assert(consoleOut === f"load $currentDirectory/sca-comet2/src/test/resources/count1.com ...%n" +
        """|done.
          |Step count: 9
          |""".stripMargin)

  }

  it can  " run (wait loop & dump file)" in {

    val currentDirectory = new java.io.File(".").getCanonicalPath
    val consoleOut = this.runCometWithInAndOut(Array("-d","./sca-comet2/src/test/resources/count1.com"),
      """df
        |q""".stripMargin)

    println(consoleOut)
    // todo compare dumptext
    assert(consoleOut === f"load $currentDirectory/sca-comet2/src/test/resources/count1.com ...%n" +
      """done.
        |ScaComet2>ScaComet2>""".stripMargin)

  }
  it can  " run (watch variables)" in {

    val currentDirectory = new java.io.File(".").getCanonicalPath
    val consoleOut = this.runCometOut(Array("-w","PR,GR0,GR1,ZF,","./sca-comet2/src/test/resources/count1.com"))

    assert(consoleOut === f"load $currentDirectory/sca-comet2/src/test/resources/count1.com ...%n" +
      """|done.
         |0000: PR=#0000,GR0=#0000,GR1=#0000,ZF=0
         |0001: PR=#0002,GR0=#0000,GR1=#0000,ZF=0
         |0002: PR=#0004,GR0=#0000,GR1=#0000,ZF=0
         |0003: PR=#0005,GR0=#0000,GR1=#0000,ZF=1
         |0004: PR=#0006,GR0=#0000,GR1=#0000,ZF=1
         |0005: PR=#000F,GR0=#0000,GR1=#0000,ZF=1
         |0006: PR=#0010,GR0=#0000,GR1=#0000,ZF=1
         |0007: PR=#0011,GR0=#0000,GR1=#0000,ZF=1
         |0008: PR=#0012,GR0=#0000,GR1=#0000,ZF=1
         |""".stripMargin)

  }

  it should " display error input file " in {

    val currentDirectory = new java.io.File(".").getCanonicalPath
    val consoleOut = this.runCometOut(Array("./sca-comet2/src/test/resources/count1_err.com"))

    assert(consoleOut === f"load $currentDirectory/sca-comet2/src/test/resources/count1_err.com ...%n" +
      """|no good file size: 53
         |""".stripMargin)

  }

  it should " display error null param " in {

    val consoleOut = this.runCometOut(Array(null))

    assert(consoleOut ===  """java.lang.NullPointerException
        |usage:ScaComet2$ [options] input.com
        |Options:
        |  -h,  --help         show this help message and exit.
        |  -c,  --count-step   count step.
        |  -d,  --debug        debug execute.
        |  -du, --dump         dump last status to last_state.txt.
        |  -w,  --watch        watch registers.
        |  -v,  --version display       version and  exit.
        |""".stripMargin)

  }

}
