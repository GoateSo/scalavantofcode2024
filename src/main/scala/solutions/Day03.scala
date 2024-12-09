package solutions

class Day03(input: Seq[String], isSample: Boolean) extends Solution(input, isSample):
  val regex = "mul\\(\\d+,\\d+\\)|do(?:n't)?".r

  def simulate(f: (Boolean, Int) => Int) =
    input.flatMap(regex.findAllMatchIn).foldLeft(0, true) { case ((n, good), m) =>
      m.matched match
        case "do"          => (n, true)
        case "don't"       => (n, false)
        case s"mul($a,$b)" => (n + f(good, a.toInt * b.toInt), good)
    }

  override def run =
    simulate((_, v) => v)._1

  override def run2 =
    simulate((good, v) => if good then v else 0)._1
