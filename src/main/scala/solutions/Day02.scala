package solutions
import utils.Utils.*
class Day02(input: Seq[String], isSample: Boolean) extends Solution(input, isSample):
  val nums = input.map(_.ssplit(" ").map(_.toInt))

  def good(xs: Seq[Int]): Boolean =
    (xs.sorted == xs || xs.sorted == xs.reverse)
      && xs.sliding(2).forall { case Seq(a, b) => math.abs(a - b) >= 1 && math.abs(a - b) <= 3 }

  override def run =
    nums.count(good)

  override def run2 =
    nums
      .filter: lvl =>
        (1 to lvl.size + 1).map(n => lvl.take(n - 1) ++ lvl.drop(n)).exists(good)
      .size
