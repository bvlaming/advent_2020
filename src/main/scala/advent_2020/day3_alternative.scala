package advent_2020

import scala.io.Source

object day3_alternative {
  def parseLine(idxLine: (String, Int)): Array[Point] = {
    val (line, y) = idxLine
    val treesOnRow = line.zipWithIndex.filter(_._1 == '#')
    treesOnRow.map { case (_, x) => Point(x, y) }.toArray
  }

  val trees: List[Point] = Source.fromResource("data3.csv").getLines.toList.zipWithIndex.flatMap(parseLine)

  val period: Int = Source.fromResource("data3.csv").getLines.toList.head.length
  val nrLines = 323

  def solution(tX: Int, tY: Int): Int = {
    val nMax = nrLines / tY.floor.toInt
    val pointsOnTrajectory = Range(0, nMax).map(n => Point(n * tX % period, n * tY))
    pointsOnTrajectory.toSet.intersect(trees.toSet).size
  }

}
