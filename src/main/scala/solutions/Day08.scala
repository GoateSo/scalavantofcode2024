package solutions
import scala.collection.mutable.HashMap

class Day08(input: Seq[String], isSample: Boolean) extends Solution(input, isSample):
  val pmap = HashMap[Char, IndexedSeq[(Int, Int)]]()

  val n = input.size
  val m = input(0).size

  for
    i <- 0 until n
    j <- 0 until m
    c = input(i)(j) if c != '.'
  do pmap(c) = pmap.getOrElse(c, IndexedSeq()) :+ (i, j)

  def getAntis(a: (Int, Int), b: (Int, Int), stepSet: Seq[Int]) =
    val (x1, y1) = a
    val (x2, y2) = b
    val d1       = x2 - x1
    val d2       = y2 - y1

    for steps <- stepSet
    yield (x1 + steps * d1, y1 + steps * d2)

  val nodes = pmap.toList.map(_._2)
  def antis(stepSet: Seq[Int]) =
    val combos = for
      ps <- nodes
      i  <- 0 until ps.size
      j  <- i + 1 until ps.size
    yield (ps(i), ps(j))
    combos
      .flatMap(getAntis(_, _, stepSet))
      .filter((x, y) => x >= 0 && y >= 0 && x < n && y < m)
      .distinct
      .toList

  // just lookat one step on each side
  override def run = antis(List(-1, 1)).size

  // look through all in bound steps that are possible
  override def run2 = antis(-(n + m) / 2 to (n + m) / 2).size
