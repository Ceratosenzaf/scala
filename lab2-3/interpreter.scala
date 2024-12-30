package brainFuck

import brainFuck.BrainFuckEnvironment

sealed trait Command
case class IncrementPointer() extends Command
case class DecrementPointer() extends Command
case class IncrementData() extends Command
case class DecrementData() extends Command
case class OutputData() extends Command
case class InputData() extends Command
case class Loop(expressions: List[Command]) extends Command

class BrainFuckInterpreter {
  private val env = new BrainFuckEnvironment

  def run(commands: List[Command]): Unit = {
    commands.foreach { 
      case IncrementPointer() => env.incrementPointer()
      case DecrementPointer() => env.decrementPointer()
      case IncrementData() => env.incrementData()
      case DecrementData() => env.decrementData()
      case OutputData() => print(env.get().toChar)
      case InputData() => env.put(Console.in.read())
      case Loop(innerCommands: List[Command]) => while (env.get() > 0) run(innerCommands)
    }
  }
}