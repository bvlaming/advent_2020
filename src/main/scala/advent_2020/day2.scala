package advent_2020

import scala.io.Source

case class Code(minVal: Int, maxVal: Int, c: Char, password: String) {
  def isValid: Boolean = {
    val freq = password.toCharArray.count(_ == c)
    (freq >= minVal) & (freq <= maxVal)
  }

  def isValidB: Boolean = {
    if (maxVal > password.length) false
    else if (minVal == 0) false
    else {
      val condition1 = password.toCharArray()(minVal - 1) == c
      val condition2 = password.toCharArray()(maxVal - 1) == c
      condition1 ^ condition2
    }
  }
}

object day2 {
  def parseLine(line: String): Code = {
    val separators: Array[Char] = Array('-', ':', ' ')
    val elems = line.split(separators).filter(_.nonEmpty)
    Code(elems(0).toInt, elems(1).toInt, elems(2).head, elems(3))
  }

  val data2: List[Code] = Source.fromResource("data2.csv").getLines.toList.map(parseLine)
  println(data2.head)

  def solution2(): Int = data2.count(_.isValid)

  def solution2b(): Int = data2.count(_.isValidB)
}
