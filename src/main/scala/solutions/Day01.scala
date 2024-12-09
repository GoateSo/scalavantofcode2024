package solutions
import utils.Utils.*
class Day01(input: Seq[String], isSample: Boolean) extends Solution(input, isSample):
  val (c1, c2) = input.map { case s"$a   $b" => (a.toInt, b.toInt) }.unzip
  override def run =
    c1.sorted.zip(c2.sorted).map((a, b) => math.abs(a - b)).sum

  override def run2 =
    c1.map(x => x * c2.count(_ == x)).sum
