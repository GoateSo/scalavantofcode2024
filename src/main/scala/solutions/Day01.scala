package solutions
import utils.Utils.*
class Day01(input: Seq[String], isSample: Boolean)
    extends Solution(input, isSample):
  // split by empty lines and map to sum of calories
  val elves = input.splitBy(_.isEmpty).map(_.map(_.toInt).sum)

  override def run =
    1

  override def run2 =
    2
