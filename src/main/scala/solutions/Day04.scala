package solutions
import utils.Utils.*
import scala.util.boundary, boundary.break

class Day04(input: Seq[String], isSample: Boolean) extends Solution(input, isSample):
  val n = input.size
  val m = input(0).size

  def hasXmas(input: Seq[String], i: Int, j: Int, di: Int, dj: Int): Boolean =
    boundary:
      for v <- 0 to 3 do
        val ni = i + di * v
        val nj = j + dj * v
        if ni < 0 || ni >= n || nj < 0 || nj >= m then break(false)
        if input(ni)(nj) != "XMAS" (v)            then break(false)
      true

  override def run =
    val xmases = for
      i        <- 0 until n
      j        <- 0 until m
      (di, dj) <- surrounding(0, 0)
      if hasXmas(input, i, j, di, dj) 
    yield 1
    xmases.sum

  val set = Set('M', 'A', 'S')
  def diagMas(xs: Seq[Seq[Char]]): Boolean =
    val d1 = Set(xs(0)(0), xs(1)(1), xs(2)(2))
    val d2 = Set(xs(0)(2), xs(1)(1), xs(2)(0))
    d1 == set && d2 == set && xs(1)(1) == 'A'

  override def run2 =
    val xmases = for
      i <- 1 until input.size - 1
      j <- 1 until input(0).size - 1
      if input(i)(j) == 'A' // diagonal enforcement
    yield
      val xs = (-1 to 1).map(di => (-1 to 1).map(dj => input(i + di)(j + dj)))
      if diagMas(xs) then 1 else 0
    xmases.sum
