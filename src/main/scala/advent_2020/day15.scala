package advent_2020

import scala.annotation.tailrec

object day15 {
  val data = List(8,0,17,4,1,12)

  @tailrec
  def recurseNumbers(xs: Map[Int, Int], lastNr: Int, n: Int, nMax: Int): Int = {
    val prevNrs = xs.keySet
    if (n==nMax-1) lastNr
    else {
        val nextNr = if (prevNrs.contains(lastNr)) n - xs(lastNr) else 0
        val newXs = xs + (lastNr -> n)
        recurseNumbers(newXs, nextNr, n + 1, nMax)
    }
  }

  lazy val solutionA: Int = {
    val initMap = data.init.zipWithIndex.toMap

    recurseNumbers(initMap, data.last, data.length - 1, 30000000)

  }
}
