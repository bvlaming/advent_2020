package advent_2020

import scala.annotation.tailrec
import scala.io.Source


object day11 {
  def neighbours(p: Point): Set[Point] = Set(
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

  def countNeighbours(p: Point, pointSet: Set[Point]): Int = {
    pointSet.intersect(neighbours(p)).size
  }

  @tailrec
  def traverseLine(q: Point,
                   dx: Int, dy: Int,
                   emptySeats: Set[Point],
                   filledSeats: Set[Point]): Int = {
    if (emptySeats.contains(q)) 0
    else if (filledSeats.contains(q)) 1
    else if (q.x < 0 || q.x > xmax || q.y < 0 || q.y > ymax) 0
    else traverseLine(Point(q.x + dx, q.y + dy), dx, dy, emptySeats, filledSeats)
  }

  def countNeighboursB(p: Point,
                       emptySeats: Set[Point],
                       filledSeats: Set[Point]): Int = {
    val dirs = List((1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1))

    val filled = for {(x, y) <- dirs}
      yield {
        traverseLine(Point(p.x + x, p.y + y), x, y, emptySeats, filledSeats)
      }
    filled.sum
  }

  @tailrec
  def evolveGrid(curr: Map[Point, Char]): Map[Point, Char] = {
    println(curr.count(_._2 == 'X'))
    val filled: Set[Point] = curr.collect { case (q, c) if c == 'X' => q }.toSet
    val nextGrid: Map[Point, Char] = curr.map { case (p, c) =>
      if (c == 'L' & countNeighbours(p, filled) == 0) (p, 'X')
      else if (c == 'X' & countNeighbours(p, filled) >= 4) (p, 'L')
      else (p, c)
    }
    if (nextGrid == curr) curr
    else evolveGrid(nextGrid)
  }

  @tailrec
  def evolveGridB(curr: Map[Point, Char]): Map[Point, Char] = {
    println(curr.count(_._2 == 'X'))
    val filled: Set[Point] = curr.collect { case (q, c) if c == 'X' => q }.toSet
    val emp: Set[Point] = curr.collect { case (q, c) if c == 'L' => q }.toSet

    val nextGrid: Map[Point, Char] = curr.map { case (p, c) =>
      if (c == 'L' & countNeighboursB(p, emp, filled) == 0) (p, 'X')
      else if (c == 'X' & countNeighboursB(p, emp, filled) >= 5) (p, 'L')
      else (p, c)
    }
    if (nextGrid == curr) curr
    else evolveGridB(nextGrid)
  }

  lazy val solutionA: Int = {
    evolveGrid(grid).count(_._2 == 'X')
  }

  lazy val solutionB: Int = {
    evolveGridB(grid).count(_._2 == 'X')
  }

}

