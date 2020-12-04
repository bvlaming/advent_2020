package advent_2020

import scala.io.Source
import scala.util.Try, scala.util.Success, scala.util.Failure

object day4 {
  def parseLine(line: String): Map[String, String] = {
    val entry = line.replace("\n", " ").split(' ')
    entry.map {
      x =>
        val y = x.split(':')
        (y(0), y(1))
    }.toMap
  }

  val requiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  def isValidHcl(c: Char) = (c.isDigit) | Set('a', 'b', 'c', 'd', 'e', 'f').contains(c)

  def isValid(passport: Map[String, String]): Boolean = {
    (requiredFields -- passport.keySet).isEmpty
  }

  val validEcl = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  def isValid2(passport: Map[String, String]): Boolean = {
    Try {
      val b1 = (passport("byr").toInt >= 1920) & (passport("byr").toInt <= 2002)
      val b2 = (passport("iyr").toInt >= 2010) & (passport("iyr").toInt <= 2020)
      val b3 = (passport("eyr").toInt >= 2020) & (passport("eyr").toInt <= 2030)
      val b4 = {
        val hgt = passport("hgt")
        val ext = hgt.takeRight(2)
        if (ext == "cm") (hgt.take(3).toInt >= 150) & (hgt.take(3).toInt <= 193) & (hgt.length == 5)
        else if (ext == "in") (hgt.take(2).toInt >= 59) & (hgt.take(2).toInt <= 76) & (hgt.length == 4)
        else false
      }
      val b5: Boolean = {
        val x = passport("hcl")
        (x.head == '#') & (x.length == 7) & (x.tail.forall(isValidHcl))
      }
      val b6: Boolean = validEcl.contains(passport("ecl"))
      val b7: Boolean = {
        val pid = passport("pid").toCharArray
        (pid.length == 9) & (pid.forall(_.isDigit))
      }
      val b = b1 & b2 & b3 & b4 & b5 & b6 & b7
      b
    }
    match {
      case Success(v) => v
      case Failure(_) => false
    }
  }

  val separator: String = "\n\n"
  val inputData: Array[String] = Source.fromResource("data4.csv").mkString.split(separator)
  
  val solution: Int = inputData.map(parseLine).count(isValid)
  val solution2: Int  = inputData.map(parseLine).filter(isValid).count(isValid2)

}
