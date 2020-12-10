package advent_2020

import scala.annotation.tailrec
import scala.io.Source

object day10 {
  val data: List[Int] = Source.fromResource("data10.csv").getLines.toList.map(_.toInt)

  @tailrec
  def explorePaths(paths: List[List[Int]], N: Int): List[List[Int]] = {
    if (N == data.length) paths
    else {
      val nextPaths: List[(List[List[Int]], Boolean)] = for {path <- paths}
        yield {
          val curr = path.head
          val leftAdapters = data.filterNot(path.contains(_))
          val options = leftAdapters.filter(Set(curr + 1, curr + 2, curr + 3).contains(_))
          if (path.head == data.max) (List(path), true)
          else if (options.isEmpty) (List(path), false)
          else {
            if (leftAdapters.contains(curr + 1)) (List((curr + 1) :: path), true)
            else if (leftAdapters.contains(curr + 2)) (List((curr + 2) :: path), true)
            else (List((curr + 3) :: path), true)
          }
        }
      explorePaths(nextPaths.collect { case ps if ps._2 => ps._1 }.flatten, N + 1)
    }
  }

  def countN(path: List[Int], N: Int): Int = {
    val diffs = path.zip(path.tail)
    diffs.count{case (n, m) => (m - n) == N}
  }

  def solutionA(): Int = {
    val path = explorePaths(List(List(0)), 0).head.reverse
    countN(path, 1) * (countN(path, 3) + 1)
  }

  def solutionB(): BigInt = {
    val path = explorePaths(List(List(0)), 0).head.reverse

    val diffs = path.zip(path.tail).map{case (x,y) => y - x}
    println(diffs)
    // counter of 1s. counter of 2 1's, 3 1's, 4 1's
    val tuplesOfOnes = diffs.foldLeft((0, 0, 0, 0)){case (acc, n) => {
      if (n == 3) {
        if (acc._1 == 2) (0, acc._2 + 1, acc._3, acc._4)
        else if (acc._1 == 3) (0, acc._2, acc._3 + 1, acc._4)
        else if (acc._1 == 4) (0, acc._2, acc._3, acc._4 + 1)
        else (0, acc._2, acc._3, acc._4)
      }
      else (acc._1 + 1, acc._2, acc._3, acc._4)
    }
    }

    BigInt(2).pow(tuplesOfOnes._2 + 1)*BigInt(4).pow(tuplesOfOnes._3)*BigInt(7).pow(tuplesOfOnes._4)

  }
}
