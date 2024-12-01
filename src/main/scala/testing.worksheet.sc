import utils.Utils.*
import math.*
import os.* 
var fname = "sample.txt"
// val fname = "input.txt"

val lines = os.read.lines(os.pwd / fname)

val Seq(c1, c2) = lines.map(_.smatch("(\\d+)\\s+(\\d+)").map(_.toInt)).transpose

val c2c = c2.counter 
c1.sorted.zip(c2.sorted).map((a, b) => abs(a - b)).sum
c1.map(x => x * c2c(x)).sum