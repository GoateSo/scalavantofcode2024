package solutions
import utils.Utils.*

class Day07(input: Seq[String], isSample: Boolean) extends Solution(input, isSample):
  def eval(nums: Seq[Long], ops: Seq[String]): Long =
    nums.tail.zip(ops).foldLeft(nums.head) { case (acc, (n, op)) =>
      op match
        case "+"  => acc + n
        case "*"  => acc * n
        case "||" => s"$acc$n".toLong
    }

  def allCombos(n: Int, base: Seq[String]): Seq[Seq[String]] = n match
    case 1 => base.map(Seq(_))
    case _ => allCombos(n - 1, base).flatMap(p => base.map(_ +: p))

  val parsed =
    for case s"$goal: $ops" <- input
    yield (goal.toLong, ops.ssplit(" ").map(_.toLong))

  override def run =
    parsed.sumBy((goal, nums) =>
      val opCombos = allCombos(nums.length - 1, Seq("+", "*")).map(eval(nums, _))
      if opCombos.exists(_ == goal) then goal else 0
    )

  override def run2 =
    parsed.sumBy((goal, nums) =>
      val opCombos = allCombos(nums.length - 1, Seq("+", "*", "||")).map(eval(nums, _))
      if opCombos.exists(_ == goal) then goal else 0
    )
