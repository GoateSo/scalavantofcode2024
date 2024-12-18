package solutions
import utils.Utils.*
import scala.collection.mutable.HashSet
import scala.util.boundary, boundary.break

class Day06(input: Seq[String], isSample: Boolean) extends Solution(input, isSample):
  val n = input.size
  val m = input(0).size

  val dmap = Map(
    "^" -> (-1, 0),
    "v" -> (1, 0),
    "<" -> (0, -1),
    ">" -> (0, 1)
  )

  def rotRight(dir: (Int, Int)) = dir match
    case (a, b) => (b, -a)

  val vs = dmap.keySet.map(_(0))
  val slist = for
    i <- 0 until n
    j <- 0 until m
    if vs(input(i)(j))
  yield (i, j)
  val start    = slist(0)
  val v        = input(start._1)(start._2)
  val startdir = dmap(v.toString)

  def isLoop(start: (Int, Int), startdir: (Int, Int), grid: Seq[String]): Boolean =
    val vis = HashSet[(Int, Int, (Int, Int))]()
    var ci  = start._1
    var cj  = start._2
    var cd  = startdir
    boundary:
      while true do
        if vis((ci, cj, cd)) then break(true) // exact same scenario
        vis.add((ci, cj, cd))
        val (ni, nj) = (ci + cd._1, cj + cd._2)
        if ni < 0 || ni >= n || nj < 0 || nj >= m then break(false) // escape
        if grid(ni)(nj) == '#'                    then cd = rotRight(cd)
        else
          ci = ni
          cj = nj
      break(false)

  val part1Vis = HashSet[(Int, Int)]()
  // find total # of encountered cells before escaping bounds
  override def run =
    boundary:
      var ci = start._1
      var cj = start._2
      var cd = startdir
      while true do
        part1Vis.add((ci, cj))
        val (ni, nj) = (ci + cd._1, cj + cd._2)
        if ni < 0 || ni >= n || nj < 0 || nj >= m then break() // escape
        if input(ni)(nj) == '#'                   then cd = rotRight(cd)
        else
          ci = ni
          cj = nj
    part1Vis.size // total encountered

  // found number of places where loop is formed by inserting a wall
  override def run2 =
    val cands = for
      i <- 0 until n
      j <- 0 until m
      if input(i)(j) != '#' && !vs(input(i)(j)) && part1Vis((i, j))
    yield
      val newGrid = input.updated(i, input(i).updated(j, '#'))
      if isLoop(start, startdir, newGrid) then Some(1) else None
    cands.flatten.size
