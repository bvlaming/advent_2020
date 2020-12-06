package advent_2020

import scala.io.Source

object day6_alternative {

  val groups: Array[String] = Source.fromResource("data6.csv").mkString.split("\n\n")
  val parsedData:Array[Array[Array[Char]]] = groups.map(_.split("\n").map(_.toCharArray))

  val solutionA: Int = parsedData.map(_.flatten.toSet.size).sum
  val solutionB: Int = parsedData.map(_.reduce((p, q) => p.intersect(q)).length).sum
}
