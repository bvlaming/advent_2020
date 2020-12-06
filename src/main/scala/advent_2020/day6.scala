package advent_2020

import scala.io.Source

object day6 {
  def parseGroup(group: String): Int = {
    // Number of questions to which the entire group answered 'yes'.
    val groupSize = group.split("\n").length
    val answersWithCount = group.toCharArray.filter(_.isLetter).groupBy(identity).map{case (c, arr) => (c, arr.length)}
    answersWithCount.count(_._2 == groupSize)
  }

  def distinctAnswerCountInGroup(group: String): Int = group.toCharArray.filter(_.isLetter).toSet.size

  val separator: String = "\n\n"
  val groups: Array[String] = Source.fromResource("data6.csv").mkString.split(separator)

  val solutionA: Int = groups.map(distinctAnswerCountInGroup).sum
  val solutionB: Int = groups.map(parseGroup).sum
}
