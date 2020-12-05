package advent_2020

import scala.io.Source

object day5 {
  val transformer: Map[Char, Int] = Map('F' -> 0, 'B' -> 1, 'L' -> 0, 'R' -> 1)

  def binaryStringToInt(s: String): Int = {
    s.map(transformer).reverse.zipWithIndex.foldLeft(0: Int) {
      case (acc, (x, n)) => acc + x * math.pow(2, n).toInt
    }
  }

  def parseBoardingPass(s: String): (Int, Int) = {
    val (row, seat) = s.splitAt(7)
    (binaryStringToInt(row), binaryStringToInt(seat))
  }

  def seatId(row: Int, seat: Int): Int = 8 * row + seat

  val data1a: List[String] = Source.fromResource("data5.csv").getLines.toList
  val rowsSeats: List[(Int, Int)] = data1a.map(parseBoardingPass)
  val seatIds: List[Int] = rowsSeats.map { case (row, seat) => seatId(row, seat) }
  val solutionA: Int = seatIds.max

  val solutionB: Int = {
    Range(0, solutionA).find(n => Set(n - 1, n + 1).subsetOf(seatIds.toSet) & (!seatIds.toSet.contains(n))).get
  }

}
