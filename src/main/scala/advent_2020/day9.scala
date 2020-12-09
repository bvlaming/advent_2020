package advent_2020

import scala.annotation.tailrec
import scala.io.Source

object day9 {

  @tailrec
  def isValid(preamble: Array[BigInt], x: BigInt, currentBool: Boolean): Boolean = {
    // true if x is the sum of two values in the preamble
    if (preamble.isEmpty) false
    else if (currentBool) currentBool
    else {
      isValid(preamble.tail, x, preamble.tail.toSet.contains(x - preamble.head))
    }
  }

  val data: Array[BigInt] = Source.fromResource("data9.csv").getLines.toArray.map(BigInt(_))

  val preAmbleSize: Int = 25

  def solutionA(): BigInt = {

    @tailrec
    def checkItems(n: Int): BigInt = {
      if (isValid(data.slice(n - preAmbleSize, n), data(n), false)) checkItems(n + 1)
      else data(n)
    }

    checkItems(preAmbleSize)
  }

  val target: BigInt = BigInt("217430975")

  // start from n=0.
  def solutionB(): BigInt = {

    @tailrec
    def checkSums(n: Int, m: Int, acc: BigInt): BigInt = {
      if (acc == target) {
        val slice = data.slice(n, m)
        slice.max + slice.min
      }
      else if (acc > target) checkSums(n + 1, n + 2, data(n + 1) + data(n + 2))
      else checkSums(n, m + 1, acc + data(m + 1))
    }

    checkSums(0, 1, data(0) + data(1))
  }

}

