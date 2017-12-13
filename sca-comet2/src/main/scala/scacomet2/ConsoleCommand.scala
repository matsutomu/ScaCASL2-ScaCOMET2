package scacomet2

sealed abstract class CliCommand
object CliCommand {
  case object Run extends CliCommand
  case object Debug extends CliCommand
  case object Help extends CliCommand
  case object Version extends CliCommand
  case object InputError extends CliCommand
}


sealed abstract class WaitForCommand
object WaitForCommand {
  case object Quit extends WaitForCommand
  case object AddBreakPoints extends WaitForCommand
  case object DumpToFile extends WaitForCommand
  case object Disassemble extends WaitForCommand
  case object DumpToConsole extends WaitForCommand
  case object DeleteBreakPoints extends WaitForCommand
  case object PrintHelp extends WaitForCommand
  case object PrintBreakPoints extends WaitForCommand
  case object JumpToAddress extends WaitForCommand
  case object WriteMemory extends WaitForCommand
  case object PrintStatus extends WaitForCommand
  case object Run extends WaitForCommand
  case object DumpStack extends WaitForCommand
  case object Step extends WaitForCommand
  case object Retry extends WaitForCommand
}
