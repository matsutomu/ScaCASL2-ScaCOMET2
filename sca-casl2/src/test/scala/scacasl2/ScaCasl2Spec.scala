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

  }

  it should " execute & dump" in {

    /*
    val currentDirectory = new java.io.File(".").getCanonicalPath

    val test = this.runCasl2Out(List("-a",
      s"${currentDirectory}/sca-casl2/src/test/resources/count1.cas").toArray)

    print(test)

    assert(test ===
      s"""[success]output to ${currentDirectory}/sca-casl2/src/test/resources/count1.com
         |""".stripMargin)

    */
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
