package scacasl2

import java.io.{ByteArrayOutputStream, PrintStream}

import org.scalatest._


class ScaCasl2Spec extends FlatSpec with DiagrammedAssertions {

  "ScaCasl2 main " should " execute " in {
    assert(this.runCasl2Out(List("-v","count1.cas").toArray) ===
      f"CASLII Assembler version 0.1 (Scala) %n")

    assert(this.runCasl2Out(List("-h","count1.cas").toArray) ===
      """usage:ScaCasl2$ [options] input.cas [output.com]
        |  -a           turn on verbose listings
        |  -v --version display version and exit
        |""".stripMargin)

    val currentDirectory = new java.io.File(".").getCanonicalPath

    // #todo read & bynary compare
    assert(this.runCasl2Out(List(s"${currentDirectory}/sca-casl2/src/test/resources/count1.cas").toArray) ===
      s"""[success]output to ${currentDirectory}/sca-casl2/src/test/resources/count1.com
        |""".stripMargin)

    // #todo read & bynary compare
    assert(this.runCasl2Out(List(s"${currentDirectory}/sca-casl2/src/test/resources/count1.cas",
      s"${currentDirectory}/sca-casl2/src/test/resources/test.com").toArray) ===
      s"""[success]output to ${currentDirectory}/sca-casl2/src/test/resources/test.com
         |""".stripMargin)

    // #todo read & bynary compare
    assert(this.runCasl2Out(List("-a", s"${currentDirectory}/sca-casl2/src/test/resources/count1.cas",
      s"${currentDirectory}/sca-casl2/src/test/resources/test.com").toArray) ===
      s"""[success]output to ${currentDirectory}/sca-casl2/src/test/resources/test.com
         |Addr	Op		Line	Source code
         |#0000	#7001		5	       PUSH    0, GR1        ;
         |#0001	#0000
         |#0002	#7002		6	       PUSH    0, GR2        ;
         |#0003	#0000
         |#0004	#2522		7	       SUBA    GR2, GR2      ;  Count = 0
         |#0005	#3411		8	       AND     GR1, GR1      ;  全部のビットが'0'?
         |#0006	#6300		9	       JZE     RETURN        ;  全部のビットが'0'なら終了
         |#0007	#000F
         |#0008	#1222		10	MORE   LAD     GR2, 1, GR2   ;  Count = Count + 1
         |#0009	#0001
         |#000A	#1201		11	       LAD     GR0,-1, GR1   ;  最下位の'1'のビット1個を
         |#000B	#FFFF
         |#000C	#3410		12	       AND     GR1,GR0       ;    '0'に変える
         |#000D	#6300		13	       JZE     MORE          ;  '1'のビットが残っていれば繰り返し
         |#000E	#0008
         |#000F	#1402		14	RETURN LD      GR0,GR2       ;  GR0 = Count
         |#0010	#7120		15	       POP     GR2           ;
         |#0011	#7110		16	       POP     GR1           ;
         |#0012	#8100		17	       RET                   ;  呼び出しプログラムへ戻る
         |
         |Defined labels
         |.COUNT1 #0000
         |COUNT1.MORE #0008
         |COUNT1.RETURN #000F
         |""".stripMargin)

  }

  it can " not execute " in {
    val currentDirectory = new java.io.File(".").getCanonicalPath

    assert(this.runCasl2Out(List(s"${currentDirectory}/sca-casl2/src/test/resources/count1_err.cas").toArray) ===
      s"""[error] It failed to assemble. path:${currentDirectory}/sca-casl2/src/test/resources/count1_err.cas
          |line: 10, message:  No Good Operands (LAD: GR2,1,GR0)
          |""".stripMargin + "\t\t  " + f"%n")

    assert(this.runCasl2Out(List("count1.cas").toArray) ===
    s"""[error] no input file. path:${currentDirectory}/count1.cas
       |""".stripMargin)

    assert(this.runCasl2Out(List("-D","count1.cas").toArray) ===
      """usage:ScaCasl2$ [options] input.cas [output.com]
        |  -a           turn on verbose listings
        |  -v --version display version and exit
        |""".stripMargin)

    assert(this.runCasl2Out(List("-d","count1.cas","b","count1.com").toArray) ===
      """usage:ScaCasl2$ [options] input.cas [output.com]
        |  -a           turn on verbose listings
        |  -v --version display version and exit
        |""".stripMargin)

  }

  it should " execute & dump" in {

    val currentDirectory = new java.io.File(".").getCanonicalPath

    val test = this.runCasl2Out(List("-a",
      s"${currentDirectory}/sca-casl2/src/test/resources/count1.cas").toArray)

    // #todo stripMargin last space!
    assert(test ===
      s"""[success]output to ${currentDirectory}/sca-casl2/src/test/resources/count1.com
         |Addr	Op		Line	Source code
         |#0000	#7001		5	       PUSH    0, GR1        ;
         |#0001	#0000
         |#0002	#7002		6	       PUSH    0, GR2        ;
         |#0003	#0000
         |#0004	#2522		7	       SUBA    GR2, GR2      ;  Count = 0
         |#0005	#3411		8	       AND     GR1, GR1      ;  全部のビットが'0'?
         |#0006	#6300		9	       JZE     RETURN        ;  全部のビットが'0'なら終了
         |#0007	#000F
         |#0008	#1222		10	MORE   LAD     GR2, 1, GR2   ;  Count = Count + 1
         |#0009	#0001
         |#000A	#1201		11	       LAD     GR0,-1, GR1   ;  最下位の'1'のビット1個を
         |#000B	#FFFF
         |#000C	#3410		12	       AND     GR1,GR0       ;    '0'に変える
         |#000D	#6300		13	       JZE     MORE          ;  '1'のビットが残っていれば繰り返し
         |#000E	#0008
         |#000F	#1402		14	RETURN LD      GR0,GR2       ;  GR0 = Count
         |#0010	#7120		15	       POP     GR2           ;
         |#0011	#7110		16	       POP     GR1           ;
         |#0012	#8100		17	       RET                   ;  呼び出しプログラムへ戻る
         |
         |Defined labels
         |.COUNT1 #0000
         |COUNT1.MORE #0008
         |COUNT1.RETURN #000F
         |""".stripMargin)

  }


  def runCasl2Out(args: Array[String]): String = {
    val outStream = new ByteArrayOutputStream
    val out = new PrintStream(new java.io.BufferedOutputStream(outStream), true, "utf-8")
    Console.withOut(out) {
      ScaCasl2.main(args)
      out.flush()
      outStream.toString("utf-8")
    }
  }

}
