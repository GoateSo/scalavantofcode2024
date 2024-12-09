package solutions
import scala.collection.mutable.ArrayDeque
class Day09(input: Seq[String], isSample: Boolean) extends Solution(input, isSample):
  // basic input parsing
  val arr = input(0).map(_.toInt - '0').zipWithIndex.toArray
  val base = arr.map: (a, i) =>
    i % 2 match
      case 1 => (1 to a).map(_ => -1L).toArray
      case _ => (1 to a).map(_ => i / 2L).toArray

  override def run =
    val xs  = base.flatten.to(ArrayDeque)
    val res = ArrayDeque[Option[Long]]()
    while xs.nonEmpty do
      // check if head is a hole
      val head = xs.removeHead(false)
      if head == -1 then
        // try to fill from tail if it is
        xs.removeLastWhile(_ == -1)
        res += xs.removeLastOption(false)
      else res += Some(head)
    // get checksum of prefix
    res.zipWithIndex.collect { case (Some(a), b) => a * b }.sum

  override def run2 =
    val array = base.filter(_.nonEmpty)
    val iinds = array.map(_ => 0)
    for i <- array.indices.reverse do
      if array(i).last == -1 then ()
      else
        val holeInd = // find first hole that's large enough to fit the current section
          array.iterator
            .take(i)
            .zipWithIndex
            .find: (arr, j) =>
              arr.last == -1 && arr.size - iinds(j) >= array(i).size && iinds(i) == 0
        holeInd match
          case Some(_, j) =>
            // if theres a valid hole, fill it in with this block
            for k <- 0 until array(i).size do array(j)(iinds(j) + k) = array(i).last
            array(i).mapInPlace(_ => -1)
            iinds(j) += array(i).size
          case _ => ()
    array.flatten.zipWithIndex.collect { case (a, b) if a > 0 => a * b }.sum
