package advent_2020

import scala.io.Source

object day3_alternative {
  def parseLine(idxLine: (String, Int)): Array[Point] = {
    val (line, y) = idxLine
    val treesOnRow = line.zipWithIndex.filter(_._1 == '#')
    treesOnRow.map { case (_, x) => Point(x, y) }.toArray
  }

  val inputData: List[String] = Source.fromResource("data3.csv").getLines.toList
  val trees: List[Point] = inputData.zipWithIndex.flatMap(parseLine)

  val period: Int = inputData.head.length
  val nrLines: Int = inputData.length

  def solution(tX: Int, tY: Int): Int = {
    val nMax = nrLines / tY.floor.toInt
    val pointsOnTrajectory = Range(0, nMax).map(n => Point(n * tX % period, n * tY))
    pointsOnTrajectory.toSet.intersect(trees.toSet).size
  }

}
