package utils
import scala.util._, boundary.break
import scala.math._
import scala.util.matching._
import scala.util.matching.Regex.Match
import scala.collection.mutable.{ArrayBuffer, HashMap}
import os._
import scala.compiletime.ops.string
import scala.reflect.ClassTag

/**
 * collection of useful utility methods & shorthands
 */
object Utils:
  type Bool = Boolean
  // alphanbetic regex shorthand
  val al = "[a-zA-Z]".r

  // interval/ range ops
  type Range = (Long, Long) | Unit
  import Range.*
  extension (r: Range)
    def isEmpty = r == ()
    def _1 = r match
      case ()     => throw Exception("get in empty range")
      case (a, b) => a
    def _2 = r match
      case ()     => throw Exception("get in empty range")
      case (a, b) => b

    def get: (Long, Long) = r match
      case (a, b) => (a, b)
      case ()     => throw Exception("empty range")

    def map(f: Long => Long) = r match
      case ()            => ()
      case (left, right) => (f(left), f(right))

    infix def &(that: Range) = (r, that) match
      case ((), _) | (_, ()) => ()
      case ((l1, r1), (l2, r2)) =>
        if l1 > r2 || l2 > r1 then ()
        else Some(max(l1, l2), min(r1, r2))
    // subtract range from range -- possibly break into left and right (as a list)
    infix def -(that: Range): List[Range] = (r, that) match
      case ((), _) => List(())
      case (_, ()) => List(r)
      case ((l1, r1), (l2, r2)) =>
        if l1 > r2 || l2 > r1 then List(r) // doesn't intersect
        else if l1 < l2 && r1 > r2 then List((l1, l2 - 1), (r2 + 1, r1))
        else if l1 < l2 then List((l1, l2 - 1))
        else if r1 > r2 then List((r2 + 1, r1))
        else List(())
  final val empty: Range             = ()
  def range(a: Long, b: Long): Range = if a > b then () else (a, b)
  def range(a: Int, b: Int): Range   = if a > b then () else (a, b)

  // surroundings of value in grid
  opaque type Surrounding <: Seq[(Int, Int)] = List[(Int, Int)]
  // format: off
  def surrounding(i : Int, j: Int): Surrounding =
      List((i-1,j-1), (i-1,j), (i-1,j+1),
           (i,j-1),              (i,j+1),
           (i+1,j-1), (i+1,j), (i+1,j+1))
  def neighbors(i: Int, j: Int): Surrounding = 
    List(           (i - 1, j), 
          (i, j-1),             (i, j + 1),
                    (i + 1, j)            )
  // format: on
  extension (sur: Surrounding)
    def bound(a: Int, b: Int): Surrounding =
      sur.filter((i, j) => i >= 0 && i < a && j >= 0 && j < b)

  // modulo using the sign of the divisor
  extension (x: Int) def +%(y: Int)   = Math.floorMod(x, y)
  extension (x: Long) def +%(y: Long) = Math.floorMod(x, y)

  extension [T, U, V, W](p: (T, U)) def bimap(f: T => V, g: U => W) = (f(p._1), g(p._2))

  // exponentiation shorthand
  extension (n: Double) def **(m: Double) = Math.pow(n, m)

  /**
   * performs extended euclidean algorithm on a and b to find GCD
   *
   * @param a integer input 1
   * @param b integer input 2
   * @return triplet (g,x,y) such that g = a*x + b*y
   */
  def egcd(a: Int, b: Int): (Int, Int, Int) =
    if a == 0 then (b, 0, 1)
    else
      val (g, x, y) = egcd(b % a, a)
      (g, y - (b / a) * x, x)

  /** computes multiplicative inverse of a mod m */
  def modInv(a: Int, m: Int): Int =
    val (g, x, y) = egcd(a, m)
    if g == 1 then (x + m) % m else 0

  /** basic primality check for a number ~ O(n^0.5) */
  def isPrime(n: BigInt): Boolean =
    if n < 2 then return false
    boundary:
      for i <- 2 to math.sqrt(n.toDouble).toInt do if n % i == 0 then break(false)
      true

  // random string stuff kinda like in lua's string library
  extension (str: String)
    /**
     * Splits a string into subsequences based on a given separator (as opposed to sub-arrays for display purposes)
     * @param s separator
     * @param filterEmp whether to filter out empty subseqs
     */
    def ssplit(s: String, filterEmp: Boolean = true) =
      if filterEmp then str.split(s).filter(_.nonEmpty).toList
      else str.split(s).toSeq // allow me to actually view it
    /**
     * gets a substring with behavior similar to lua's string.sub, allowing for negative indices to represent indices
     * from the end
     *
     * @param start starting index of substring
     * @param end ending index of substring
     */
    inline def sub(start: Int, end: Int): String =
      val st = if start < 0 then str.length + start else start
      val en = if end < 0   then str.length + end + 1 else end
      str.substring(st, en)

    /**
     * gets a substring starting from a particular index, allowing for negative indices to represent indices from the
     * end
     *
     * @param start starting index of substring
     */
    inline def sub(start: Int): String = sub(start, str.length)
    // convert str -> int or str -> Long using given radix
    def toLong             = java.lang.Long.parseLong(str)
    def toLong(radix: Int) = java.lang.Long.parseLong(str, radix)
    def toInt              = Integer.parseInt(str)
    def toInt(radix: Int)  = Integer.parseInt(str, radix)
    def toDouble           = java.lang.Double.parseDouble(str)

    /**
     * apply used in context of charat
     *
     * @param i index of char
     * @return character at given index
     */
    def apply(i: Int) = str.charAt(i)

    /**
     * apply used in context of "contains"
     *
     * @param c character to check element relationship
     * @return whether the given character is contained within the string
     */
    def apply(c: Char) = str.contains(c)

    /**
     * apply used in context of checking regex matching
     *
     * @param r regex to match on string
     * @return either subgroups if any are present or whole matched string if there are no capture groups
     */
    def apply(r: Regex) =
      r.findFirstMatchIn(str) map (m =>
        val sg = m.subgroups
        if sg.isEmpty then List(m.matched)
        else sg
      ) getOrElse Nil

    /**
     * method for subgroup matching, DO NOT USE WITHOUT CAPTURE GROUPS
     *
     * @param reg regex to match against
     * @return all matched subgroups
     */
    def smatch(reg: String): Seq[String] =
      reg.r
        .findFirstMatchIn(str)
        .map(_.subgroups)
        .getOrElse(Seq())

    /**
     * finds first match for regex expr, but ignores captures, if you want to capture, use smatch
     *
     * @param reg regex to match against
     * @return matched string
     */
    def fmatch(reg: String): String =
      reg.r
        .findFirstMatchIn(str)
        .map(_.matched)
        .getOrElse("")

    def findOrElse(reg: Regex, back: String): String =
      reg.findFirstIn(str).getOrElse(back)

    /**
     * global substitution using function mapping to substitute values
     *
     * @param reg regex to match
     * @param f function to map
     * @return resultant string
     */
    def gsub(reg: Regex, f: Seq[String] => String) =
      reg.replaceAllIn(
        str,
        _ match {
          case reg(xs*) => f(xs)
        }
      )
    def gsub(reg: Regex, f: Seq[String] => String, times: Int) =
      var occurs = 0
      reg.replaceSomeIn(
        str,
        _ match {
          case reg(xs*) if occurs < times =>
            occurs += 1
            Some(f(xs))
          case _ => None
        }
      )
    def gsub(reg: Regex, rep: String) =
      reg.replaceAllIn(str, rep)
    def gsub(reg: Regex, rep: String, times: Int) =
      var occurs = 0
      reg.replaceSomeIn(
        str,
        _ match {
          case reg(_*) if occurs < times =>
            occurs += 1
            Some(rep)
          case _ => None
        }
      )
    def gsub(reg: Regex, tbl: Map[String, String]) =
      reg.replaceAllIn(
        str,
        m => tbl.getOrElse(m.matched, m.matched)
      )

  // special map for counting elements in a sequence
  opaque type Counter[T] = Map[T, Int]
  extension [T](c: Counter[T]) def apply(t: T) = c.getOrElse(t, 0)

  extension [T](seq: Seq[T])
    // numerical reductions
    def sumBy[U: Numeric](f: T => U)  = seq.map(f).sum
    def prodBy[U: Numeric](f: T => U) = seq.map(f).product

    /** split by predicate */
    def splitBy(p: T => Boolean) = seq.foldLeft(Seq(Seq.empty[T])) {
      case (acc, s) if p(s) => acc :+ Seq.empty[T]
      case (acc, s)         => acc.init :+ (acc.last :+ s)
    }

    /** split by value */
    def splitBy(v: T, filterEmp: Boolean = true): Seq[Seq[T]] =
      if filterEmp then seq.splitBy(_ == v).filter(_.nonEmpty)
      else seq.splitBy(_ == v)

    /** distinct, but not an iterator (so i don't forget the name) */
    def unique = seq.distinct.toSeq

    /** mimics python's Counter object */
    def counter: Counter[T] = seq.groupMapReduce(identity)(_ => 1)(_ + _)

  // similar to seqs, except for arrays
  extension [T: ClassTag](arr: Array[T])
    // numerical reductions
    def sumBy[U: Numeric: ClassTag](f: T => U) =
      val num = summon[Numeric[U]]
      arr.foldLeft(num.zero)((acc, v) => num.plus(acc, f(v)))
    def prodBy[U: Numeric: ClassTag](f: T => U) =
      val num = summon[Numeric[U]]
      arr.foldLeft(num.zero)((acc, v) => num.times(acc, f(v)))
    // split by predicate or value
    def splitBy(p: T => Boolean) = arr.foldLeft(Seq(Seq.empty[T])) {
      case (acc, s) if p(s) => acc :+ Seq.empty[T]
      case (acc, s)         => acc.init :+ (acc.last :+ s)
    }
    def splitBy(v: T): Seq[Seq[T]] = splitBy(_ == v)
    // distinct, but not an iterator (so i don't forget the name)
    def unique = arr.distinct.toSeq
    def display = arr.mkString("[", ", ", "]")

  extension [T](grid: Seq[Seq[T]])
    def columns: Seq[Seq[T]] =
      for i <- 0 until grid(0).length yield grid.map(_(i))
  extension (lines: Seq[String])
    /**
     * get character columns of a string
     *
     * @return essentially transpose of the string
     */
    def chrCols = lines.map(_.toSeq).transpose.map(_.mkString)

  extension [T](arr: Array[Array[T]])
    /**
     * converts an array grid into viewable format
     *
     * @param sep separator between elements
     * @return string representation of grid
     */
    def str(sep: String = "") =
      arr.map(_.mkString("[", sep, "]")).mkString("\n")

  /** print to file; reduce clutter in stdout */
  def write(xs: Any*) =
    os.write.append(pwd / "POutput.txt", (xs mkString " ") + "\n")

  // GCD and LCM
  import math.Integral.Implicits.infixIntegralOps
  def euclid[T: Integral](x: T, y: T): T =
    if y == 0
    then x
    else euclid(y, x % y)
  def lcm[T: Integral](x: T, y: T): T = x * y / euclid(x, y)

  // generalized integer statistics
  extension [T: Integral](xs: List[T])
    def gcd    = xs.reduce(euclid)
    def lcm    = xs.reduce((a, b) => a * b / euclid(a, b))
    def median = xs.sorted.apply(xs.size / 2)
  extension (xs: List[Double]) def mean = xs.sum / xs.size

  // shorthands
  extension (i: Int)
    def toBin                  = i.toBinaryString
    def toHex                  = i.toHexString
    infix def +(that: Boolean) = i + (if that then 1 else 0)
    infix def -(that: Boolean) = i - (if that then 1 else 0)
  extension (i: Long)
    def toBin                  = i.toBinaryString
    def toHex                  = i.toHexString
    infix def +(that: Boolean) = i + (if that then 1 else 0)
    infix def -(that: Boolean) = i - (if that then 1 else 0)

  /** inclusive random integer */
  def randInt(b1In: Int, b2In: Int) =
    val (mi, ma) = if b1In > b2In then (b2In, b1In) else (b1In, b2In)
    Random.between(mi, ma + 1)

  // priority queue w/ deckey operation
  class MinPq[T](xs: T, priority: Double):
    var arr = ArrayBuffer(null, (xs, priority))
    val map = HashMap(xs -> 1)

    override def toString =
      arr.mkString("[", ",", "]") ++ "\n" ++ map.mkString("[", ",", "]")

    def isEmpty  = arr.size <= 1
    def nonEmpty = arr.size > 1
    def +=(n: T, priority: Double): Unit =
      arr += ((n, priority))
      map(n) = arr.length - 1
      swim(arr.length - 1)

    /**
     * @return element with lowest priority
     */
    def top = arr(1)

    def pop: (T, Double) =
      val ret = arr(1)
      swap(1, arr.length - 1)
      map -= ret._1
      arr = arr.take(arr.length - 1)
      sink(1)
      ret

    /**
     * decrease key (priority) of elem to new value
     *
     * @param n element
     * @param nDist new priority
     */
    def decKey(n: T, nDist: Double): Unit =
      val i = map(n)
      arr(i) = (n, nDist)
      swim(i)

    private def swap(i: Int, j: Int): Unit =
      map(arr(i)._1) = j
      map(arr(j)._1) = i
      val n = arr(i)
      arr(i) = arr(j)
      arr(j) = n

    private def swim(i: Int): Unit =
      var p = i
      while p > 1 && arr(p / 2)._2 > arr(p)._2 do
        swap(p / 2, p)
        p /= 2

    private def sink(i: Int): Unit =
      val l    = i * 2
      val r    = l + 1
      var smol = i
      if l < arr.length && arr(l)._2 < arr(smol)._2 then smol = l
      if r < arr.length && arr(r)._2 < arr(smol)._2 then smol = r
      if smol != i then
        swap(i, smol)
        sink(smol)
  end MinPq
  // digraph with weights
  class Digraph[T]:
    var adj = HashMap[T, Set[(T, Double)]]()
    def addEdge(from: T, to: T, weight: Double = 1) =
      adj(from) = adj.getOrElse(from, Set.empty[(T, Double)]) + ((to, weight))
    def addEdges(from: T, tos: Seq[(T, Double)]) =
      adj(from) = adj.getOrElse(from, Set.empty[(T, Double)]) ++ tos
    def bfs(start: T, f: T => Unit): Unit =
      import scala.collection.mutable.{Set, Queue}
      var visited = Set(start)
      var queue   = Queue(start)
      while queue.nonEmpty do
        val n = queue.dequeue()
        f(n)
        for (e, w) <- adj(n) do
          if !visited.contains(e) then
            visited += e
            queue.enqueue(e)

    def dfs(start: T, f: T => Unit): Unit =
      import scala.collection.mutable.{Set, Stack}
      var visited = Set(start)
      var stack   = Stack(start)
      while stack.nonEmpty do
        val n = stack.pop()
        f(n)
        for (e, w) <- adj(n) do
          if !visited.contains(e) then
            visited += e
            stack.push(e)
    def path(start: T, end: T): (Double, List[T]) =
      // perform dijkstra's algorithm on graph using minPQ from above
      val dist  = HashMap[T, Double]()
      val prevs = HashMap[T, T]()

      def getPath(x: T): List[T] =
        if x == start then List(start)
        else x :: getPath(prevs(x))

      val pq = MinPq[T](start, 0)
      dist(start) = 0
      var retval = (Double.MaxValue, List.empty[T])
      while pq.arr.length > 1 do
        val (n, nDist) = pq.pop
        if n == end then retval = (nDist, getPath(n))
        else
          for (e, w) <- adj(n) do
            val newDist = nDist + w
            if !dist.contains(e) || newDist < dist(e) then
              dist(e) = newDist
              prevs(e) = n
              pq += (e, newDist)
      retval
    override def toString(): String = adj.mkString("\n")

    // def pathNeg(start : T, end : T)
  end Digraph
  def toDigraph[T](arr: Array[Array[T]]): Digraph[T] =
    // constuct digraph between array elements and their 4 neighors in grid
    val g = Digraph[T]()
    for i <- 0 until arr.length do
      for j <- 0 until arr(0).length do
        val n = arr(i)(j)
        val neighs = Seq(
          if i > 0                 then Some((arr(i - 1)(j), 1.0)) else None,
          if i < arr.length - 1    then Some((arr(i + 1)(j), 1.0)) else None,
          if j > 0                 then Some((arr(i)(j - 1), 1.0)) else None,
          if j < arr(0).length - 1 then Some((arr(i)(j + 1), 1.0)) else None
        ).flatten
        g.addEdges(n, neighs)
    g
  def toWeightedDigraph(grid: Array[Array[Double]]): Digraph[(Int, Int)] =
    // constuct digraph between array elements and their 4 neighors in grid
    val g = Digraph[(Int, Int)]()
    for
      i <- 0 until grid.length
      j <- 0 until grid(0).length
      n = (i, j)
    do
      val neighs = Seq(
        if i > 0                  then Some(((i - 1, j), grid(i - 1)(j))) else None,
        if i < grid.length - 1    then Some(((i + 1, j), grid(i + 1)(j))) else None,
        if j > 0                  then Some(((i, j - 1), grid(i)(j - 1))) else None,
        if j < grid(0).length - 1 then Some(((i, j + 1), grid(i)(j + 1))) else None
      ).flatten
      g.addEdges(n, neighs)
    g

  // discrete ranges, so ((1, 5), (6, 7)) -> (1,7)
  // possibly turn into an interval tree down the line
  class Ranges(intervals: (Int, Int)*):
    private var internal_arr =
      intervals.to(ArrayBuffer)
    internal_arr.sortInPlaceBy(_._1)
    internal_sort()
    // subtract range from collection of intervals
    def -(range: (Int, Int)) =
      if internal_arr.isEmpty then this
      else
        val (rl, rr)    = range
        var (narr, rem) = internal_arr.span(_._2 < rl)
        boundary:
          while rem.nonEmpty do
            val (l, r) = rem.remove(0)
            if l < rl then narr.addOne((l, math.min(rl - 1, r)))
            if r > rr then
              narr.addOne((math.max(rr + 1, l), r))
              break()
            rem.dropWhileInPlace((l, r) => r <= rr && l >= rl)
        Ranges(narr.toSeq*)

    def |(that: Ranges) =
      val cpy = internal_arr.clone()
      cpy.appendAll(that.internal_arr)
      Ranges(cpy.toSeq*)

    def +(range: (Int, Int)) =
      val cpy = internal_arr.clone()
      cpy.prepend(range)
      Ranges(cpy.toSeq*)

    def count: Int =
      internal_arr.map((l, r) => r - l + 1).sum

    override def toString(): String =
      internal_arr.mkString("Ranges[", ", ", "]")

    private def internal_sort() =
      if internal_arr.nonEmpty then
        val narr = ArrayBuffer[(Int, Int)]()
        narr.addOne(internal_arr.head)
        for (i, j) <- internal_arr.tail if i <= j do
          val (pi, pj) = narr.remove(narr.size - 1)
          if pj < i - 1 then narr ++= List((pi, pj), (i, j))
          else narr ++= List((pi, math.max(pj, j)))
        internal_arr = narr
  end Ranges
end Utils
