package solutions
import utils.Utils.*

class Day05(input: Seq[String], isSample: Boolean) extends Solution(input, isSample):
  val (p1, p2) = input.span(_ != "")
  val order    = p1.map(_.ssplit("\\|").map(_.toInt)).map { case Seq(a, b) => (a, b) }

  def less(x: Int, y: Int): Bool =
    order.exists(_ == x && _ == y)

  val updates = p2.tail.map(_.ssplit(",").map(_.toInt))

  def hasViolation(update: Seq[Int]): Bool =
    val arr = for
      i <- 0 until update.length
      j <- i + 1 until update.length
    yield less(update(i), update(j))
    arr.exists(_ == false)

  override def run =
    updates.filterNot(hasViolation).map(x => x(x.size / 2)).sum

  override def run2 =
    updates.filter(hasViolation).map(_.sortWith(less)).map(x => x(x.size / 2)).sum
