package advent_2020

import scala.io.Source

object day1 {
  val data1a: List[Int] = Source.fromResource("data1a.csv").getLines.toList.map(_.toInt)


  def solution_1a(): Int = {
    val sums: List[(Int, Int, Int)] =
      for (n <- data1a;
           m <- data1a)
        yield (n, m, n + m)

    val elems = sums.filter(_._3 == 2020).head

    elems._1 * elems._2
  }

  def solution_1b(): Int = {
    val sums: List[(Int, Int, Int, Int)] =
      for (n <- data1a;
           m <- data1a;
           k <- data1a)
        yield (n, m, k, n + m + k)

    val elems = sums.filter(_._4 == 2020).head

    elems._1 * elems._2 * elems._3
  }
}
