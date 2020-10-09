package homeworks.basics

import scala.io.Source

object Task3 {

  /*
  * Homework: ControlStructures
  *
  * Create a command line application that reads various "commands" from the
  * stdin, evaluates them, and writes output to stdout.
  *
  * Commands are:
  *
  * divide 4 5
  * which should output "4 divided by 5 is 0.8"
  *
  *    sum 5 5 6 8.5
  * which should output "the sum of 5 5 6 8.5 is 24.5"
  *
  *    average 4 3 8.5 4
  * which should output "the average of 4 3 8.5 4 is 4.875"
  *
  *    min 4 -3 -17
  * which should output "the minimum of 4 -3 -17 is -17"
  *
  *    max 4 -3 -17
  * which should output "the maximum of 4 -3 -17 is 4"
  *
  *  In case of commands that cannot be parsed or calculations that cannot be performed,
  * output a single line starting with "Error: "
  * */

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result
  final case class CalculationResult(operation: Command, value:Double) extends Result

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    import homeworks.basics.Task3.Command._
    x.trim.replaceAll(" +", " ").split(" ").toList match {
      case Nil | _ :: Nil                           => Left(ErrorMessage("Error: Not enough data for calculation"))
      case _ :: xs
        if xs.map(_.toDoubleOption).contains(None)  => Left(ErrorMessage("Error: Incorrect number input: " + xs.mkString(" ")))
      case x :: xs                                  => x match {
          case "divide"                             => xs match {
              case xs if xs.length != 2             => Left(ErrorMessage("Error: Division requires two numbers"))
              case _                                => Right(Divide(xs(0).toDouble, xs(1).toDouble))
            }
          case "sum"                                => Right(Sum(xs.map(_.toDouble)))
          case "average"                            => Right(Average(xs.map(_.toDouble)))
          case "min"                                => Right(Min(xs.map(_.toDouble)))
          case "max"                                => Right(Max(xs.map(_.toDouble)))
          case _                                    => Left(ErrorMessage("Error: Invalid operation command"))
        }
      case _                                        => Left(ErrorMessage("Error: Invalid input data"))
    }
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = {
    import homeworks.basics.Task3.Command._
    x match {
      case Divide(dividend, divisor)      => divisor match {
        case 0                            => Left(ErrorMessage("Error: division by zero is prohibited"))
        case _                            => Right(CalculationResult(x, dividend / divisor))
      }
      case Sum(numbers)                   => Right(CalculationResult(x, numbers.sum))
      case Average(numbers)               => Right(CalculationResult(x, numbers.sum / numbers.length))
      case Min(numbers)                   => Right(CalculationResult(x, numbers.min))
      case Max(numbers)                   => Right(CalculationResult(x, numbers.max))
      case _                              => Left(ErrorMessage("Error: Invalid operation command"))
    }
  }

  def renderResult(x: Result): String = {
    import homeworks.basics.Task3.Command._
    x match {
      case CalculationResult(operation, result)   =>
        operation match {
          case Divide(dividend, divisor)      => f"${dividend} divided by ${divisor} is $result"
          case Sum(numbers)                   => f"the sum of ${numbers.mkString(" ")} is $result"
          case Average(numbers)               => f"the average of ${numbers.mkString(" ")} is $result"
          case Min(numbers)                   => f"the min of ${numbers.mkString(" ")} is $result"
          case Max(numbers)                   => f"the max of ${numbers.mkString(" ")} is $result"
          case _                              => "Error: Invalid operation command"
        }
    }
  }

  def process(x: String): String = {
    (for {
      command <- parseCommand(x)
      result <- calculate(command)
    } yield result).fold(left => s"$left", right => renderResult(right))
  }
}
