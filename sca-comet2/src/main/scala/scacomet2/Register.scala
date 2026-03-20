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
  class GeneralRegister(var word: Int) extends Register
  class IndexRegister(var word: Int) extends Register
  class StackPointer(var word: Int) extends Register
  class ProgramRegister(var word: Int) extends Register
  class FlagRegister(var word: Int) extends Register

}
