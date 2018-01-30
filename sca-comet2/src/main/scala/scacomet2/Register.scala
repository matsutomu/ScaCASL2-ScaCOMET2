package scacomet2

/**
  *
  * [Ref]https://gist.github.com/gakuzzzz/10081860
  *
  * StackPointer : can't override word!
  *
  */
sealed abstract class Register {
  var word: Int
  def ++(v: Int) = {
    this.word = this.word + v
  }

}

object Register {
  case class GeneralRegister(override var word: Int) extends Register
  case class IndexRegister(override var word: Int) extends Register
  case class StackPointer(override var word: Int) extends Register
  case class ProgramRegister(override var word: Int) extends Register
  case class FlagRegister(override var word: Int) extends Register

}
