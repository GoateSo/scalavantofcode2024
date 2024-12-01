import utils.Utils.*
import math.*
import os.*

var fname = "sample.txt"
// val fname = "input.txt"

// (1 to 1000).toList.filter(miller_rabin)

def check(n: BigInt, a: BigInt): Boolean = 
    var d = n-1
    var m = 0 
    while (d&1) == 0  do 
      d /= 2
      m += 1
    val r = a.modPow(d,n)
    r == 1 || r == n-1

def miller_rabin(n: Long): Boolean = 
  def check(n: BigInt, a: BigInt): Boolean = 
    var d = n-1
    var m = 0 
    while (d&1) == 0 do 
      d /= 2
      m += 1
    val r = a.modPow(d,n)
    r == 1 || r == n-1
  val checks = if n < 2152302898747L
    then List(2, 3, 5, 7, 11)
    else List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37)
  println(checks)
  checks.forall(a => check(n, a))


List(2, 3, 5, 7, 11).map(a => check(4, a))

List(2, 3, 5, 7, 11).map(a => check(3, a))

List(2, 3, 5, 7, 11).map(a => check(2, a))

val lines = os.read.lines(os.pwd / fname)


lines.map(x => "" ++ x.find(_.isDigit) ++ "" ++ x.findLast(_.isDigit) )
    .map(_.toInt).sum