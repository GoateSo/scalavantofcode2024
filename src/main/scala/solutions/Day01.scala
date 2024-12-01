package solutions
import utils.Utils.*
class Day01(input: Seq[String], isSample: Boolean)
    extends Solution(input, isSample):

  val pairs = input.map(_.ssplit(" ").map(_.toInt)).map(x => (x(0), x(1)))
  val (c1, c2) = pairs.unzip
  override def run =
    c1.sorted.zip(c2.sorted).map((a, b) => math.abs(a - b)).sum

  override def run2 =
    c1.map(x => x * c2.count(_ == x)).sum
