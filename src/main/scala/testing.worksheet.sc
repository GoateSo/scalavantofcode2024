import utils.Utils.*
import math.*
import os.*
import scala.collection.mutable.{Queue, HashSet}
os.write.over(pwd / "POutput.txt", "")

var fname = "sample.txt"
fname = "input.txt"

val lines = os.read.lines(os.pwd / fname)
val n     = lines.length
val m     = lines(0).length

def search(a: Int, b: Int, repeat: Bool): Int =
  val vis = HashSet[(Int, Int)]()
  val q   = Queue[(Int, Int)]()
  var res = 0
  q.enqueue((a, b))
  var rating = 1
  while q.nonEmpty do
    val (x, y) = q.dequeue()
    if lines(x)(y) == '9' then res += 1
    for
      (i, j) <- neighbors(x, y).bound(n, m)
      if lines(i)(j) - lines(x)(y) == 1
      if repeat || !vis((i, j))
    do
      vis.add((i, j))
      q.enqueue((i, j))
  res

var cntn = 0

val p1 = for
  i <- 0 until lines.length
  j <- 0 until lines(i).length
  if lines(i)(j) == '0'
yield search(i, j, false)
p1.sum

val p2 = for
  i <- 0 until lines.length
  j <- 0 until lines(i).length
  if lines(i)(j) == '0'
yield search(i, j, true)
p2.sum
