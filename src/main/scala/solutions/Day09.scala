package solutions
import scala.collection.mutable.HashMap
import utils.Utils.*
import scala.collection.mutable.ArrayDeque
import scala.collection.mutable.ListBuffer
class Day09(input: Seq[String], isSample: Boolean) extends Solution(input, isSample):
  val arr = input(0).map(_.toInt - '0').zipWithIndex.toArray
  val base = arr.map((a, i) =>
    i % 2 match
      case 1 => (1 to a).map(_ => -1).toArray
      case _ => (1 to a).map(_ => i / 2).toArray
  )

  // just lookat one step on each side
  override def run =
    val xs  = base.flatten.map(BigInt.apply).to(ArrayDeque)
    val res = ListBuffer[Option[BigInt]]()
    while xs.nonEmpty do
      val x = xs.removeHead(false)
      if x == -1 then
        xs.removeLastWhile(_ == -1)
        res += xs.removeLastOption(false)
      else res += Some(x)
    res.flatten.toList.zipWithIndex.map((a, b) => a * b).sum

  // look through all in bound steps that are possible
  override def run2 =
    val array = base.filter(_.nonEmpty)
    val iinds = array.map(_ => 0)
    for i <- array.size - 1 to 0 by -1 do
      if array(i).last == -1 then ()
      else
        val best = array.zipWithIndex
          .find((arr, j) => arr.last == -1 && arr.size - iinds(j) >= array(i).size && iinds(i) == 0)
        best match
          case Some((_, j)) if j < i =>
            for k <- 0 until array(i).size do array(j)(iinds(j) + k) = array(i).last
            iinds(j) += array(i).size
            array(i) = Array.fill(array(i).size)(-1)
          case _ => ()
    array
      .flatMap(_.toSeq)
      .toSeq
      .map(x => math.max(x, 0))
      .zipWithIndex
      .map { case (a, b) => a * BigInt(b) }
      .sum
