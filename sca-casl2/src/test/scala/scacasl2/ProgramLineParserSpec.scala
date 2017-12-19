package scacasl2

import org.scalatest._
import scacasl2.instruction._
import scacasl2.operand._
import scacasl2.ProgramLineParser._


class ProgramLineParserSpec extends FlatSpec with DiagrammedAssertions {

  "Program Line Parser" should " parse  START" in {
    assert(ProgramLineParser.parseLine("CASL START MAIN", 1) ===
      Right(InstructionLine(Option("CASL"), "START", Option(List("MAIN")), None, 1, "CASL START MAIN")))

    assert(ProgramLineParser.parseLine("CASL START main", 1) ===
      Right(InstructionLine(Option("CASL"), "START", Option(List("main")), None, 1, "CASL START main")))

    assert(ProgramLineParser.parseLine("CASL START  GR0, GR1", 1) ===
      Right(InstructionLine(Option("CASL"), "START", Option(List("GR0", "GR1")), None, 1, "CASL START  GR0, GR1")))

    assert(ProgramLineParser.parseLine(" START GR0, GR1", 2) ===
      Right(InstructionLine(None, "START", Option(List("GR0", "GR1")), None, 2, " START GR0, GR1")))

  }

  it should "parse LAD (free operands)" in {
    assert(ProgramLineParser.parseLine(" LAD GR0, GR1 ,  GR2", 3) ===
      Right(InstructionLine(None, "LAD", Option(List("GR0", "GR1", "GR2")), None, 3, " LAD GR0, GR1 ,  GR2")))

  }

  it should "parse comment " in {
    assert(ProgramLineParser.parseLine("; LAD GR0, GR1 ,  GR2", 4) ===
      Right(CommentLine("; LAD GR0, GR1 ,  GR2", 4, "; LAD GR0, GR1 ,  GR2")))

    assert(ProgramLineParser.parseLine("; LAD GR0, GR1 ,  ; GR2", 4) ===
      Right(CommentLine("; LAD GR0, GR1 ,  ; GR2", 4, "; LAD GR0, GR1 ,  ; GR2")))

  }

  it should " parse error operation code " in {
    ProgramLineParser.parseLine("CASL PRINT MAIN", 1) match {
      case Right(result) => throw new RuntimeException
      case Left(msg) => {
        assert(msg === "Unsupported Instruction(PRINT, List(MAIN))")
      }
    }

  }

  /**
   *  Common Assert
   *
   * @param line
   * @param opecode
   * @param byteCode
   * @param ope
   * @param wSize
   * @return
   */
  private def checkInstructionConvert(line: String,
                                      opecode: String,
                                      byteCode: Int,
                                      ope: Operand,
                                      wSize: Int) = {
    ProgramLineParser.parseLine(line, 1) match {
      case Right(result) => {
        val pl = result.asInstanceOf[InstructionLine]
        InstructionFactory.parseOperand(pl.code, pl.operands.getOrElse(List.empty), "") match {
          case Right(result) => {
            val instruct = result.asInstanceOf[Instruction]

            assert(instruct.code === opecode)
            assert(instruct.info.byteCode === byteCode)
            assert(instruct.ope === ope)
            assert(instruct.wordSize === wSize)
          }
          case Left(msg) => throw new RuntimeException(msg)
        }
      }
      case Left(msg) => throw new RuntimeException(msg)
    }
  }


  "InstructionFactory " should " parse operand (Assembly Instruction) " in {

    val stringTest = "'''abcde,'',fghijklmnopqrstuxyz'''"


    val stringTest2 =
      """' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUXYZ""" +
        """[¥]^_`abcdefghijklmnopqrstuxyz{|}~""" +
        """'"""

    val args: List[(String, String, Int, Operand, Int)] = List(
      ("CASL START MAIN",
        "START", -100, OperandStart(Option(LabelOfOperand("MAIN", None))), 0),
      ("CASL START",
        "START", -100, OperandStart(None), 0),
      (" END",
        "END", -101, OperandNoArg(), 1),
      (" DS 0",
        "DS", 0x00, OperandDs(0), 0),
      ("TEST DS 3",
        "DS", 0x00, OperandDs(3), 3),
      ("TEST DS 2000",
        "DS", 0x00, OperandDs(2000), 2000),
      (s" DC 32767,#ffff,$stringTest",
        "DC", 0x00,
        OperandDc(
          List(ConstsNumOfOperand("32767", 32767),
            ConstsNumOfOperand("#ffff", 65535),
            ConstsStringOfOperand(stringTest,
              stringTest.drop(1).dropRight(1).replace("''", "'").toCharArray.map(e => e.toInt).toList),
          )), 31),
      ("TEST DC -32769, -32768, -1,0,1,32767,32768,#0000,#0001, #fffe, #ffff  ",
        "DC", 0x00,
        OperandDc(
          List(
            ConstsNumOfOperand("-32769", Helper.bitToUnsignedShort(32767)),
            ConstsNumOfOperand("-32768", Helper.bitToUnsignedShort(-32768)),
            ConstsNumOfOperand("-1", Helper.bitToUnsignedShort(-1)),
            ConstsNumOfOperand("0", Helper.bitToUnsignedShort(0)),
            ConstsNumOfOperand("1", Helper.bitToUnsignedShort(1)),
            ConstsNumOfOperand("32767", Helper.bitToUnsignedShort(32767)),
            ConstsNumOfOperand("32768", Helper.bitToUnsignedShort(-32768)),
            ConstsNumOfOperand("#0000", 0),
            ConstsNumOfOperand("#0001", 1),
            ConstsNumOfOperand("#fffe", 65534),
            ConstsNumOfOperand("#ffff", 65535),
          )), 11),
      ("TEST DC TESTLBL, TESTLB2  ,TestLb3 ",
        "DC", 0x00,
        OperandDc(
          List(
            LabelOfOperand("TESTLBL", None),
            LabelOfOperand("TESTLB2", None),
            LabelOfOperand("TestLb3", None),
          )), 3),
      (s"TEST DC $stringTest2",
        "DC", 0x00,
        OperandDc(
          List(
            ConstsStringOfOperand(stringTest2,
              stringTest2.drop(1).dropRight(1).replace("''", "'").toCharArray.map(e => e.toInt).toList),
          )), 91),
    )

    args.map { case (line, opecode, bytecode, ope, wsize) =>
      checkInstructionConvert(line, opecode, bytecode, ope, wsize)
    }
  }


  it should " parse macro instruction " in {
    val args: List[(String, String, Int, Operand, Int)] = List(
      (" IN INPUT , INLEN",
        "IN", 0x90, OperandInOrOut(List(LabelOfOperand("INPUT", None), LabelOfOperand("INLEN", None))), 3),
      (" OUT OUTPUT , OUTLEN",
        "OUT", 0x91, OperandInOrOut(List(LabelOfOperand("OUTPUT", None), LabelOfOperand("OUTLEN", None))), 3),
      (" RPUSH",
        "RPUSH", 0xa0, OperandNoArg(), 1),
      (" RPOP",
        "RPOP", 0xa1, OperandNoArg(), 1),
    )

    args.map { case (line, opecode, bytecode, ope, wsize) =>
      checkInstructionConvert(line, opecode, bytecode, ope, wsize)
    }

  }


  it should " parse machine instruction " in {
    val args: List[(String, String, Int, Operand, Int)] = List(
      (" NOP",
        "NOP", 0x00, OperandNoArg(), 1),

      (" LD GR0, ADRTEST , GR1",
        "LD2", 0x10, OperandR_ADR_X(0, LabelOfOperand("ADRTEST", None), 1), 2),
      (" ST GR1, ADRTEST , GR2",
        "ST", 0x11, OperandR_ADR_X(1, LabelOfOperand("ADRTEST", None), 2), 2),
      (" LAD GR2, ADRTEST , GR3",
        "LAD", 0x12, OperandR_ADR_X(2, LabelOfOperand("ADRTEST", None), 3), 2),
      (" LD GR3, GR0",
        "LD1", 0x14, OperandR1R2(3, 0), 1),

      (" ADDA GR4, ADRTEST , GR4",
        "ADDA2", 0x20, OperandR_ADR_X(4, LabelOfOperand("ADRTEST", None), 4), 2),
      (" SUBA GR5, ADRTEST , GR5",
        "SUBA2", 0x21, OperandR_ADR_X(5, LabelOfOperand("ADRTEST", None), 5), 2),
      (" ADDL GR6, ADRTEST , GR6",
        "ADDL2", 0x22, OperandR_ADR_X(6, LabelOfOperand("ADRTEST", None), 6), 2),
      (" SUBL GR7, ADRTEST , GR7",
        "SUBL2", 0x23, OperandR_ADR_X(7, LabelOfOperand("ADRTEST", None), 7), 2),
      (" ADDA GR8, GR8",
        "ADDA1", 0x24, OperandR1R2(8, 8), 1),
      (" SUBA GR0, GR1",
        "SUBA1", 0x25, OperandR1R2(0, 1), 1),
      (" ADDL GR1, GR2",
        "ADDL1", 0x26, OperandR1R2(1, 2), 1),
      (" SUBL GR2, GR3",
        "SUBL1", 0x27, OperandR1R2(2, 3), 1),

      (" AND GR3, ADRTEST , GR4",
        "AND2", 0x30, OperandR_ADR_X(3, LabelOfOperand("ADRTEST", None), 4), 2),
      (" OR  GR4, ADRTEST , GR5",
        "OR2", 0x31, OperandR_ADR_X(4, LabelOfOperand("ADRTEST", None), 5), 2),
      (" XOR GR5, ADRTEST , GR6",
        "XOR2", 0x32, OperandR_ADR_X(5, LabelOfOperand("ADRTEST", None), 6), 2),
      (" AND GR8, GR8",
        "AND1", 0x34, OperandR1R2(8, 8), 1),
      (" OR  GR0, GR1",
        "OR1", 0x35, OperandR1R2(0, 1), 1),
      (" XOR GR1, GR2",
        "XOR1", 0x36, OperandR1R2(1, 2), 1),

      (" CPA  GR4, ADRTEST , GR5",
        "CPA2", 0x40, OperandR_ADR_X(4, LabelOfOperand("ADRTEST", None), 5), 2),
      (" CPL GR5, ADRTEST , GR6",
        "CPL2", 0x41, OperandR_ADR_X(5, LabelOfOperand("ADRTEST", None), 6), 2),
      (" CPA GR8, GR8",
        "CPA1", 0x44, OperandR1R2(8, 8), 1),
      (" CPL  GR0, GR1",
        "CPL1", 0x45, OperandR1R2(0, 1), 1),
      (" SLA  GR4, ADRTEST , GR5",
        "SLA", 0x50, OperandR_ADR_X(4, LabelOfOperand("ADRTEST", None), 5), 2),
      (" SRA GR5, ADRTEST , GR6",
        "SRA", 0x51, OperandR_ADR_X(5, LabelOfOperand("ADRTEST", None), 6), 2),
      (" SLL  GR4, ADRTEST , GR5",
        "SLL", 0x52, OperandR_ADR_X(4, LabelOfOperand("ADRTEST", None), 5), 2),
      (" SRL GR5, ADRTEST , GR6",
        "SRL", 0x53, OperandR_ADR_X(5, LabelOfOperand("ADRTEST", None), 6), 2),

      (" JMI  ADRTEST , GR1",
        "JMI", 0x61, OperandADR_X(LabelOfOperand("ADRTEST", None), 1), 2),
      (" JNZ  ADRTEST , GR2",
        "JNZ", 0x62, OperandADR_X(LabelOfOperand("ADRTEST", None), 2), 2),
      (" JZE  ADRTEST , GR3",
        "JZE", 0x63, OperandADR_X(LabelOfOperand("ADRTEST", None), 3), 2),
      (" JZE  ADRTEST",
        "JZE", 0x63, OperandADR(LabelOfOperand("ADRTEST", None)), 2),
      (" JUMP  ADRTEST, GR4",
        "JUMP", 0x64, OperandADR_X(LabelOfOperand("ADRTEST", None), 4), 2),
      (" JPL  ADRTEST , GR5",
        "JPL", 0x65, OperandADR_X(LabelOfOperand("ADRTEST", None), 5), 2),
      (" JOV  ADRTEST , GR6",
        "JOV", 0x66, OperandADR_X(LabelOfOperand("ADRTEST", None), 6), 2),

      (" PUSH  ADRTEST , GR7",
        "PUSH", 0x70, OperandADR_X(LabelOfOperand("ADRTEST", None), 7), 2),
      (" POP  GR8",
        "POP", 0x71, OperandR(8), 1),

      (" CALL  ADRTEST , GR1",
        "CALL", 0x80, OperandADR_X(LabelOfOperand("ADRTEST", None), 1), 2),
      (" RET",
        "RET", 0x81, OperandNoArg(), 1),

      (" SVC  ADRTEST , GR1",
        "SVC", 0xF0, OperandADR_X(LabelOfOperand("ADRTEST", None), 1), 2),
    )

    args.map { case (line, opecode, bytecode, ope, wsize) =>
      checkInstructionConvert(line, opecode, bytecode, ope, wsize)
    }
  }


  it should " parse NOP instruction (No Good) " in {
    // no operands
    ProgramLineParser.parseLine(" NOP  START  ", 2) match {
      case Right(result) => {
        val pl = result.asInstanceOf[InstructionLine]
        InstructionFactory.parseOperand(pl.code, pl.operands.getOrElse(List.empty), "") match {
          case Right(result) => // nop
          case Left(msg) => assert(msg === "No Good Operands (NOP: START)")
        }
      }
      case Left(msg) => throw new RuntimeException(msg)
    }
  }


  it should " START Instruction (no good operands) " in {

    ProgramLineParser.parseLine("CASL START GR0", 1) match {
      case Right(result) => {
        val pl = result.asInstanceOf[InstructionLine]
        InstructionFactory.parseOperand(pl.code, pl.operands.getOrElse(List.empty), "") match {
          case Right(result) => throw new RuntimeException
          case Left(msg) => assert(msg === "No Good Operands (START: GR0)")
        }
      }
      case Left(msg) => throw new RuntimeException(msg)
    }

    ProgramLineParser.parseLine("CASL START GR1", 1) match {
      case Right(result) => {
        val pl = result.asInstanceOf[InstructionLine]
        InstructionFactory.parseOperand(pl.code, pl.operands.getOrElse(List.empty), "") match {
          case Right(result) => throw new RuntimeException
          case Left(msg) => assert(msg === "No Good Operands (START: GR1)")
        }
      }
      case Left(msg) => throw new RuntimeException(msg)
    }

    ProgramLineParser.parseLine("CASL START GR2", 1) match {
      case Right(result) => {
        val pl = result.asInstanceOf[InstructionLine]
        InstructionFactory.parseOperand(pl.code, pl.operands.getOrElse(List.empty), "") match {
          case Right(result) => throw new RuntimeException
          case Left(msg) => assert(msg === "No Good Operands (START: GR2)")
        }
      }
      case Left(msg) => throw new RuntimeException(msg)
    }

    ProgramLineParser.parseLine("CASL START GR3", 1) match {
      case Right(result) => {
        val pl = result.asInstanceOf[InstructionLine]
        InstructionFactory.parseOperand(pl.code, pl.operands.getOrElse(List.empty), "") match {
          case Right(result) => throw new RuntimeException
          case Left(msg) => assert(msg === "No Good Operands (START: GR3)")
        }
      }
      case Left(msg) => throw new RuntimeException(msg)
    }

    ProgramLineParser.parseLine("CASL START GR4", 1) match {
      case Right(result) => {
        val pl = result.asInstanceOf[InstructionLine]
        InstructionFactory.parseOperand(pl.code, pl.operands.getOrElse(List.empty), "") match {
          case Right(result) => throw new RuntimeException
          case Left(msg) => assert(msg === "No Good Operands (START: GR4)")
        }
      }
      case Left(msg) => throw new RuntimeException(msg)
    }

    ProgramLineParser.parseLine("CASL START GR5", 1) match {
      case Right(result) => {
        val pl = result.asInstanceOf[InstructionLine]
        InstructionFactory.parseOperand(pl.code, pl.operands.getOrElse(List.empty), "") match {
          case Right(result) => throw new RuntimeException
          case Left(msg) => assert(msg === "No Good Operands (START: GR5)")
        }
      }
      case Left(msg) => throw new RuntimeException(msg)
    }

    ProgramLineParser.parseLine("CASL START GR6", 1) match {
      case Right(result) => {
        val pl = result.asInstanceOf[InstructionLine]
        InstructionFactory.parseOperand(pl.code, pl.operands.getOrElse(List.empty), "") match {
          case Right(result) => throw new RuntimeException
          case Left(msg) => assert(msg === "No Good Operands (START: GR6)")
        }
      }
      case Left(msg) => throw new RuntimeException(msg)
    }

    ProgramLineParser.parseLine("CASL START GR7", 1) match {
      case Right(result) => {
        val pl = result.asInstanceOf[InstructionLine]
        InstructionFactory.parseOperand(pl.code, pl.operands.getOrElse(List.empty), "") match {
          case Right(result) => throw new RuntimeException
          case Left(msg) => assert(msg === "No Good Operands (START: GR7)")
        }
      }
      case Left(msg) => throw new RuntimeException(msg)
    }

    ProgramLineParser.parseLine("CASL START main", 1) match {
      case Right(result) => {
        val pl = result.asInstanceOf[InstructionLine]
        InstructionFactory.parseOperand(pl.code, pl.operands.getOrElse(List.empty), "") match {
          case Right(result) => throw new RuntimeException
          case Left(msg) => assert(msg === "No Good Operands (START: main)")
        }
      }
      case Left(msg) => throw new RuntimeException(msg)
    }

  }


  "ProgramLineParser" should " parse sample instructions " in {

    val programLines = List(
      "COUNT1 START          ;",
      ";      入力    GR1: 検索する語",
      ";      処理    GR1 中の'1'のビットの個数を求める",
      ";      出力    GR0:GR1中の'1'のビットの個数",
      "       PUSH    0, GR1        ;",
      "       PUSH    0, GR2        ;",
      "       SUBA    GR2, GR2      ;  Count = 0",
      "       AND     GR1, GR1      ;  全部のビットが'0'? ",
      "       JZE     RETURN        ;  全部のビットが'0'なら終了 ",
      "MORE   LAD     GR2, 1, GR2   ;  Count = Count + 1 ",
      "       LAD     GR0,-1, GR1   ;  最下位の'1'のビット1個を ",
      "       AND     GR1,GR0       ;    '0'に変える ",
      "       JZE     MORE          ;  '1'のビットが残っていれば繰り返し ",
      "RETURN LD      GR0,GR2       ;  GR0 = Count ",
      "       POP     GR2           ;  ",
      "       POP     GR1           ;  ",
      "       RET                   ;  呼び出しプログラムへ戻る ",
      "       END                   ; ",
    )

    val result = ProgramLineParser.parseFirst(programLines)

    val answer = List(
      AssemblyInstruction("START", OperandStart(None), InstructionFactory.INSTRUCTION_INF_MAP("START"), ""),  // 0 word
      MachineInstruction("PUSH", OperandADR_X(AddressOfOperand("0", 0), 1), InstructionFactory.INSTRUCTION_INF_MAP("PUSH"), "COUNT1"), // 2 word
      MachineInstruction("PUSH", OperandADR_X(AddressOfOperand("0", 0), 2), InstructionFactory.INSTRUCTION_INF_MAP("PUSH"), "COUNT1"), // 2 word
      MachineInstruction("SUBA1", OperandR1R2(2, 2), InstructionFactory.INSTRUCTION_INF_MAP("SUBA1"), "COUNT1"), // 1 word
      MachineInstruction("AND1" , OperandR1R2(1, 1), InstructionFactory.INSTRUCTION_INF_MAP("AND1"), "COUNT1"),  // 1 word
      MachineInstruction("JZE"  , OperandADR(LabelOfOperand("RETURN", None)), InstructionFactory.INSTRUCTION_INF_MAP("JZE"), "COUNT1"), // 2 word
      MachineInstruction("LAD"  , OperandR_ADR_X(2, AddressOfOperand("1" , Helper.bitToUnsignedShort(1 )), 2), InstructionFactory.INSTRUCTION_INF_MAP("LAD"), "COUNT1"), // 2 word LABEL MORE: 10
      MachineInstruction("LAD"  , OperandR_ADR_X(0, AddressOfOperand("-1", Helper.bitToUnsignedShort(-1)), 1), InstructionFactory.INSTRUCTION_INF_MAP("LAD"), "COUNT1"), // 2 word
      MachineInstruction("AND1" , OperandR1R2(1, 0), InstructionFactory.INSTRUCTION_INF_MAP("AND1"), "COUNT1"), // 1 word
      MachineInstruction("JZE"  , OperandADR(LabelOfOperand("MORE", None)), InstructionFactory.INSTRUCTION_INF_MAP("JZE"), "COUNT1"), // 2 word
      MachineInstruction("LD1"  , OperandR1R2(0, 2), InstructionFactory.INSTRUCTION_INF_MAP("LD1"), "COUNT1"),  // 1 word LABEL RETURN: 17
      MachineInstruction("POP"  , OperandR(2), InstructionFactory.INSTRUCTION_INF_MAP("POP"), "COUNT1"),  // 1 word
      MachineInstruction("POP"  , OperandR(1), InstructionFactory.INSTRUCTION_INF_MAP("POP"), "COUNT1"),  // 1 word
      MachineInstruction("RET"  , OperandNoArg(), InstructionFactory.INSTRUCTION_INF_MAP("RET"), "COUNT1"), // 1 word
      //AssemblyInstruction("END"  , OperandNoArg(), InstructionFactory.INSTRUCTION_INF_MAP("END"), "COUNT1"), // 1 word
    )

    val answerSymbols = Map(".COUNT1" -> 0, "COUNT1.MORE" -> 8, "COUNT1.RETURN" -> 15)


    assert(result.errors.isEmpty)
    assert(result.instructions.map(e => e.model) === answer)
    assert(result.symbolTable  === answerSymbols)


    val byteCode = ProgramLineParser.convertBinaryCode(result.instructions.map(e => e.model), result.symbolTable)
    val rightByte = List('C', 'A',  'S', 'L',    0,    0,    0,
         0,    0,    0,   0,    0,    0,    0,
         0,    0, 0x70,0x01,    0,    0, 0x70,
      0x02,    0,    0,0x25, 0x22, 0x34, 0x11,
      0x63,    0,    0,0x0F, 0x12, 0x22, 0x00,
      0x01, 0x12, 0x01,0xFF, 0xFF, 0x34, 0x10,
      0x63, 0x00, 0x00,0x08, 0x14, 0x02, 0x71,
      0x20, 0x71, 0x10,0x81, 0x00
    ).map(e => e.toByte)

    assert(byteCode === rightByte)


  }

  it should " parse sample instructions (Simple Out) " in {

    val programLines = List(
      "SOUT1    START",
      "         OUT      BUFF1, LEN",
      "         RET",
      "LEN      DC      5",
      "BUFF1    DC      'CASL2'",
      "BUFF2    DC      'COMET2'",
      "         END"
    )

    val result = ProgramLineParser.parseFirst(programLines)

    val answer = List(
      AssemblyInstruction("START", OperandStart(None), InstructionFactory.INSTRUCTION_INF_MAP("START"), ""),  // 0 word
      MacroInstruction("OUT",  OperandInOrOut(List(LabelOfOperand("BUFF1", None), LabelOfOperand("LEN", None))), InstructionFactory.INSTRUCTION_INF_MAP("OUT"),"SOUT1"), // 3 word
      MachineInstruction("RET"  , OperandNoArg(), InstructionFactory.INSTRUCTION_INF_MAP("RET"), "SOUT1"), // 1 word
      AssemblyInstruction("DC",  OperandDc(List(ConstsNumOfOperand("5",5))) , InstructionFactory.INSTRUCTION_INF_MAP("DC"),"SOUT1"), // 1 word
      AssemblyInstruction("DC",  OperandDc(List(ConstsStringOfOperand("'CASL2'",  List('C','A','S','L','2')))) , InstructionFactory.INSTRUCTION_INF_MAP("DC"),"SOUT1"), // 5 word
      AssemblyInstruction("DC",  OperandDc(List(ConstsStringOfOperand("'COMET2'", List('C','O','M','E','T','2')))) , InstructionFactory.INSTRUCTION_INF_MAP("DC"),"SOUT1"), // 6 word
      //AssemblyInstruction("END"  , OperandNoArg(), InstructionFactory.INSTRUCTION_INF_MAP("END"), "COUNT1"), // 1 word
    )

    val answerSymbols = Map(".SOUT1" -> 0, "SOUT1.LEN" -> 4, "SOUT1.BUFF1" -> 5, "SOUT1.BUFF2" -> 10)

    assert(result.errors.isEmpty)
    assert(result.instructions.map(e => e.model) === answer)
    assert(result.symbolTable  === answerSymbols)


    val byteCode = ProgramLineParser.convertBinaryCode(result.instructions.map(e => e.model), result.symbolTable)
    val rightByte = List('C', 'A',  'S', 'L',    0,    0,    0,
         0,    0,    0,   0,    0,    0,    0,
         0,    0, 0x91,0x00,    0, 0x05,    0,0x04,
      0x81,    0,    0,0x05,    0,  'C',    0, 'A',
         0,  'S',    0, 'L',    0,  '2',    0, 'C',
         0,  'O',    0, 'M',    0,  'E',    0, 'T',  0, '2'
    ).map(e => e.toByte)

    assert(byteCode === rightByte)
  }

  it should " parse sample instructions  ([ERROR] No Start Label) " in {

    val programLines = List(
      "         START",
      "         OUT      BUFF1, LEN",
      "         RET",
      "LEN      DC      5",
      "BUFF1    DC      'CASL2'",
      "BUFF2    DC      'COMET2'",
      "         END"
    )

    // #todo error is ALL ParseError
    val result = ProgramLineParser.parseFirst(programLines)

    val answer = List(
      AssemblyInstruction("START", OperandStart(None), InstructionFactory.INSTRUCTION_INF_MAP("START"), ""),  // 0 word
      MacroInstruction("OUT",  OperandInOrOut(List(LabelOfOperand("BUFF1", None), LabelOfOperand("LEN", None))), InstructionFactory.INSTRUCTION_INF_MAP("OUT"),""), // 3 word
      MachineInstruction("RET", OperandNoArg(), InstructionFactory.INSTRUCTION_INF_MAP("RET"), ""), // 1 word
      AssemblyInstruction("DC",  OperandDc(List(ConstsNumOfOperand("5",5))) , InstructionFactory.INSTRUCTION_INF_MAP("DC"),""), // 1 word
      AssemblyInstruction("DC",  OperandDc(List(ConstsStringOfOperand("'CASL2'",  List('C','A','S','L','2')))) , InstructionFactory.INSTRUCTION_INF_MAP("DC"),""), // 5 word
      AssemblyInstruction("DC",  OperandDc(List(ConstsStringOfOperand("'COMET2'", List('C','O','M','E','T','2')))) , InstructionFactory.INSTRUCTION_INF_MAP("DC"),""), // 6 word
      //AssemblyInstruction("END"  , OperandNoArg(), InstructionFactory.INSTRUCTION_INF_MAP("END"), "COUNT1"), // 1 word
    )

    val answerSymbols = Map()

    assert(result.errors === List(ParseError(1,"START need Label","",InstructionLine(None,"START",None,None,1,"         START"))))
    assert(result.instructions.map(e => e.model) === answer)
    assert(result.symbolTable  === answerSymbols)

  }

  it should " parse sample instructions ([ERROR] START again,  before END) " in {

    val programLines = List(
      "TEST1   START",
      "         OUT      BUFF1, LEN",
      "TEST2   START",
      "         RET",
      "LEN      DC      5",
      "BUFF1    DC      'CASL2'",
      "BUFF2    DC      'COMET2'",
      "         END"
    )

    // #todo error is ALL ParseError
    val result = ProgramLineParser.parseFirst(programLines)

    val answer = List(
      AssemblyInstruction("START", OperandStart(None), InstructionFactory.INSTRUCTION_INF_MAP("START"), ""),  // 0 word
      MacroInstruction("OUT",  OperandInOrOut(List(LabelOfOperand("BUFF1", None), LabelOfOperand("LEN", None))), InstructionFactory.INSTRUCTION_INF_MAP("OUT"),"TEST1"), // 3 word
      MachineInstruction("RET"  , OperandNoArg(), InstructionFactory.INSTRUCTION_INF_MAP("RET"), ""), // 1 word
      AssemblyInstruction("DC",  OperandDc(List(ConstsNumOfOperand("5",5))) , InstructionFactory.INSTRUCTION_INF_MAP("DC"),""), // 1 word
      AssemblyInstruction("DC",  OperandDc(List(ConstsStringOfOperand("'CASL2'",  List('C','A','S','L','2')))) , InstructionFactory.INSTRUCTION_INF_MAP("DC"),""), // 5 word
      AssemblyInstruction("DC",  OperandDc(List(ConstsStringOfOperand("'COMET2'", List('C','O','M','E','T','2')))) , InstructionFactory.INSTRUCTION_INF_MAP("DC"),""), // 6 word
      //AssemblyInstruction("END"  , OperandNoArg(), InstructionFactory.INSTRUCTION_INF_MAP("END"), "COUNT1"), // 1 word
    )

    val answerSymbols = Map()

    assert(result.errors === List(ParseError(3,"START is found before END","",InstructionLine(Some("TEST2"),"START",None,None,3,"TEST2   START"))))
    assert(result.instructions.map(e => e.model) === answer)
    assert(result.symbolTable  === answerSymbols)
  }


  it should " parse sample instructions ([ERROR] START is not there,  before END) " in {

    val programLines = List(
      "         OUT      BUFF1, LEN",
      "         END"
    )

    // #todo error is ALL ParseError
    val result = ProgramLineParser.parseFirst(programLines)

    val answer = List(
      MacroInstruction("OUT",  OperandInOrOut(List(LabelOfOperand("BUFF1", None), LabelOfOperand("LEN", None))), InstructionFactory.INSTRUCTION_INF_MAP("OUT"),""), // 3 word
      //AssemblyInstruction("END"  , OperandNoArg(), InstructionFactory.INSTRUCTION_INF_MAP("END"), "COUNT1"), // 1 word
    )

    val answerSymbols = Map()

    assert(result.errors === List(ParseError(2,"START is not found.","",InstructionLine(None,"END",None,None,2,"         END"))))
    assert(result.instructions.map(e => e.model) === answer)
    assert(result.symbolTable  === answerSymbols)
  }

  it should " parse sample instructions ([ERROR] data definition in program) " in {

    val programLines = List(
      "TEST1   START",
      "         OUT      BUFF1, LEN",
      "LEN      DC      5",
      "         RET",
      "BUFF1    DC      'CASL2'",
      "BUFF2    DC      'COMET2'",
      "         END"
    )

    // #todo error is ALL ParseError
    val result = ProgramLineParser.parseFirst(programLines)

    val answer = List(
      AssemblyInstruction("START", OperandStart(None), InstructionFactory.INSTRUCTION_INF_MAP("START"), ""),  // 0 word
      MacroInstruction("OUT",  OperandInOrOut(List(LabelOfOperand("BUFF1", None), LabelOfOperand("LEN", None))), InstructionFactory.INSTRUCTION_INF_MAP("OUT"),"TEST1"), // 3 word
      AssemblyInstruction("DC",  OperandDc(List(ConstsNumOfOperand("5",5))) , InstructionFactory.INSTRUCTION_INF_MAP("DC"),"TEST1"), // 1 word
      MachineInstruction("RET", OperandNoArg(), InstructionFactory.INSTRUCTION_INF_MAP("RET"), "TEST1"), // 1 word
      AssemblyInstruction("DC",  OperandDc(List(ConstsStringOfOperand("'CASL2'",  List('C','A','S','L','2')))) , InstructionFactory.INSTRUCTION_INF_MAP("DC"),"TEST1"), // 5 word
      AssemblyInstruction("DC",  OperandDc(List(ConstsStringOfOperand("'COMET2'", List('C','O','M','E','T','2')))) , InstructionFactory.INSTRUCTION_INF_MAP("DC"),"TEST1"), // 6 word
      //AssemblyInstruction("END"  , OperandNoArg(), InstructionFactory.INSTRUCTION_INF_MAP("END"), "COUNT1"), // 1 word
    )

    val answerSymbols = Map()

    assert(result.errors === List(ParseError(4,"Data definition in program.","",InstructionLine(None,"RET",None,None,4,"         RET"))))
    assert(result.instructions.map(e => e.model) === answer)
    assert(result.symbolTable  === answerSymbols)
  }


  it should " parse sample instructions ([ERROR] error grammer) " in {

    val programLines = List(
      " SOUT    START",
      "OUT      BUFF1, LEN",
      "RET",
      " LEN      DC      5",
      " BUFF1    DC      'CASL2'",
      " BUFF2    DC      'COMET2'",
      "         END"
    )

    val result = ProgramLineParser.parseFirst(programLines)

    val answer = List()

    val answerSymbols = Map()

    assert(result.errors === List(
      ParseError(1,"parse error","Unsupported Instruction(SOUT, List(START))",null),
      ParseError(2,"parse error","string matching regex `;.*$' expected but `1' found",null),
      ParseError(3,"parse error","""string matching regex `\s+' expected but end of source found""",null),
      ParseError(4,"parse error","Unsupported Instruction(LEN, List(DC      5))",null),
      ParseError(5,"parse error","string matching regex `;.*$' expected but `1' found",null),
      ParseError(6,"parse error","string matching regex `;.*$' expected but `2' found",null),
      ParseError(7,"START is not found.","",InstructionLine(None,"END",None,None,7,"         END"))))
    assert(result.instructions.map(e => e.model) === answer)
    assert(result.symbolTable  === answerSymbols)

  }


}