package scacasl2

import org.scalatest._


class ScaCasl2Spec extends FlatSpec with DiagrammedAssertions {

  "ScaCasl2 main " should " execute " in {
    ScaCasl2.main(List("count1.cas").toArray)
    ScaCasl2.main(List("-v","count1.cas").toArray)

  }

}
