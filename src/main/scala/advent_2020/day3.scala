package advent_2020

import scala.io.Source

case class Point(x: Int, y: Int){
  def isOnTrajectory(tX: Int, tY: Int): Boolean = {
    tX * y == tY * x
  }
}

object day3 {
  def parseLine(idxLine: (String, Int)): Array[Point] = {
    val (line, y) = idxLine
    val treesOnRow = line.zipWithIndex.filter(_._1 == '#')
    treesOnRow.map { case (_, x) => Point(x, y) }.toArray
  }

  val trees: List[Point] = Source.fromResource("data3.csv").getLines.toList.zipWithIndex.flatMap(parseLine)

  val period: Int = Source.fromResource("data3.csv").getLines.toList.head.length

  def solution(): Int = {
    val maxReps = 100
    val forest = for {
      n <- Range(0, maxReps)
    } yield trees.map(p => Point(p.x + n * period, p.y))
    forest.flatten.count(_.isOnTrajectory(1, 2))
  }
}
