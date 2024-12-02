import utils.Utils.*
import math.*
import os.* 
// var fname = "sample.txt"
val fname = "input.txt"

val lines = os.read.lines(os.pwd / fname)

val xss = lines.map(_.ssplit(" ").map(_.toInt))

// val s1 = xss.filter(xs =>
//     xs.sorted == xs || xs.sorted == xs.reverse
// ).filter(xs =>
//     xs.sliding(2).forall(ys => abs(ys(0) - ys(1)) > 0 && abs(ys(0) - ys(1)) < 4)
// ).length
// List(1,2,3).sliding(2).toList

val s1 = xss.map(
    xs => (xs +: (1 to xs.length).map(n => xs.take(n-1) ++ xs.drop(n))).filter(
        l => l.sorted == l || l.sorted == l.reverse
    ).filter(
        l => l.sliding(2).forall(ys => abs(ys(0) - ys(1)) > 0 && abs(ys(0) - ys(1)) < 4)
    ).size
    // .filter(
    //     l => l.sliding(2).forall(ys => 
    //         println(ys)
    //         abs(ys(0) - ys(1)) > 0 && abs(ys(0) - ys(1)) < 4)
    // )

).count(x => x > 0)
// s1.mkString("\n")