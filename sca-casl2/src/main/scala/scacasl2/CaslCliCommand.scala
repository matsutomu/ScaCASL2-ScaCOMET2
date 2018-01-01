package scacasl2

sealed abstract class CaslCliCommand

object CaslCliCommand {
  case object Run extends CaslCliCommand
  case object Dump extends CaslCliCommand
  case object Help extends CaslCliCommand
  case object Version extends CaslCliCommand
  case object InputError extends CaslCliCommand
}
