import solutions.*
import scala.io.AnsiColor.*
import os.*
import utils.Utils
import fastparse.internal.Util
import fansi.Color.{Green, Red, Blue, Yellow}
import fansi.{Underlined, Bold}
import java.util.Timer
import java.time.LocalTime

def emph(s: String) =
  Blue(s).overlay(Underlined.On).overlay(Bold.On)

inline def output(soln: Solution) =
  println(Green("part 1:"))
  println(Red(soln.run.toString))
  Utils.write("+".repeat(120))
  Utils.write("[part 2]:")
  println(Green(s"part 2:"))
  println(Red(soln.run2.toString))

inline def test(day: (Seq[String], Boolean) => Solution) =
  os.write.over(pwd / "POutput.txt", "")
  println(emph("[sample]"))
  output(day(os.read.lines(pwd / "sample.txt"), true))
  Utils.write("~".repeat(120))
  println(emph("[real]"))
  output(day(os.read.lines(pwd / "input.txt"), false))

@main def main: Unit =
  val t1 = System.nanoTime()
  test(Day06.apply(_, _))
  val t2 = System.nanoTime()
  println(Yellow(s"Time: ${(t2 - t1) / 1000000}ms"))
