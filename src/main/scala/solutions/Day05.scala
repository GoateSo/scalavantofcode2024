package solutions
import utils.Utils.*

class Day05(input: Seq[String], isSample: Boolean) extends Solution(input, isSample):
  val (p1, p2) = input.span(_ != "")
  val order    = p1.map { case s"$a|$b" => (a.toInt, b.toInt) }

  def less(x: Int, y: Int) =
    order.exists(_ == x && _ == y)

  val updates = p2.tail.map(_.ssplit(",").map(_.toInt))

  def good(update: Seq[Int]) =
    update.combinations(2).forall(x => less(x(0), x(1)))

  override def run =
    updates.filter(good).map(x => x(x.size / 2)).sum

  override def run2 =
    updates.filterNot(good).map(_.sortWith(less)).map(x => x(x.size / 2)).sum
