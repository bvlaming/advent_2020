package advent_2020

import scala.annotation.tailrec
import scala.io.Source


object day11 {
  implicit def bool2int(b: Boolean): Int = if (b) 1 else 0

  def neighbours(p: Point): List[Point] = List(
    Point(p.x - 1, p.y - 1),
    Point(p.x - 1, p.y),
    Point(p.x - 1, p.y + 1),
    Point(p.x, p.y - 1),
    Point(p.x, p.y + 1),
    Point(p.x + 1, p.y - 1),
    Point(p.x + 1, p.y),
    Point(p.x + 1, p.y + 1))


  def parseLine(idxLine: (String, Int)): List[(Point, Char)] = {
    val (line, y) = idxLine
    line.zipWithIndex.map { case (s, x) => (Point(x, y), s) }.toList
  }


  val inputData: List[String] = Source.fromResource("data11.csv").getLines.toList
  val (xmax, ymax) = (inputData.head.length, inputData.length)
  val grid: Map[Point, Char] = inputData.zipWithIndex.flatMap(parseLine).toMap
  val boundary: Map[Point, Char] = {
    // add floor spaces around: 0, 0...ymax. xmax, 0, ... ymax
    val left = Range(-1, xmax + 1).map(x => (Point(x, -1), '.'))
    val right = Range(-1, xmax + 1).map(x => (Point(x, ymax), '.'))
    val up = Range(-1, ymax + 1).map(y => (Point(-1, y), '.'))
    val down = Range(-1, ymax + 1).map(y => (Point(xmax, y), '.'))
    List(up, down, left, right).flatten.toMap
  }

  def countNeighbours(p: Point, state: Map[Point, Char]): Int = {
    neighbours(p).map(state ++ boundary).count(_ == 'X')
  }

  def countNeighboursB(p: Point, state: Map[Point, Char]): Int = {
    val filled: List[Point] = state.collect { case (q, c) if c == 'X' => q }.toList
    val left = filled.exists(q => (q.y == p.y) & (q.x < p.x)).toInt
    val right = filled.exists(q => (q.y == p.y) & (q.x > p.x)).toInt
    val up = filled.exists(q => (q.y < p.y) & (q.x == p.x)).toInt
    val down = filled.exists(q => (q.y > p.y) & (q.x == p.x)).toInt
    val NW = filled.exists(q => (q.y < p.y) & (q.x < p.x) & (q.y - p.y == q.x - p.x)).toInt
    val SE = filled.exists(q => (q.y > p.y) & (q.x > p.x) & (q.y - p.y == q.x - p.x)).toInt
    val NE = filled.exists(q => (q.y < p.y) & (q.x > p.x) & (q.y - p.y == p.x - q.x)).toInt
    val SW = filled.exists(q => (q.y > p.y) & (q.x < p.x) & (q.y - p.y == p.x - q.x)).toInt
    left + right + up + down + NW + SE + NE + SW
  }

  @tailrec
  def evolveGrid(curr: Map[Point, Char]): Map[Point, Char] = {
    println(curr.count(_._2 == 'X'))
    val nextGrid: Map[Point, Char] = curr.map { case (p, c) =>
      if (c == 'L' & countNeighbours(p, curr) == 0) (p, 'X')
      else if (c == 'X' & countNeighbours(p, curr) >= 4) (p, 'L')
      else (p, c)
    }
    if (nextGrid == curr) curr
    else evolveGrid(nextGrid)
  }

  @tailrec
  def evolveGridB(curr: Map[Point, Char]): Map[Point, Char] = {
    println(curr.count(_._2 == 'X'))
    val nextGrid: Map[Point, Char] = curr.map { case (p, c) =>
      if (c == 'L' & countNeighboursB(p, curr) == 0) (p, 'X')
      else if (c == 'X' & countNeighboursB(p, curr) >= 5) (p, 'L')
      else (p, c)
    }
    if (nextGrid == curr) curr
    else evolveGridB(nextGrid)
  }

  val solutionA: Int = {
    evolveGrid(grid).count(_._2 == 'X')
  }

  val solutionB: Int = {
    evolveGridB(grid).count(_._2 == 'X')
  }

}

