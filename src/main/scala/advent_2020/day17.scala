package advent_2020

import scala.annotation.tailrec
import scala.io.Source

case class P3D(x: Int, y: Int, z: Int, w: Int)

object day17 {
  // generalise to 3D. Of course, also make a 3D point case class
  def neighbours(p: P3D): Set[P3D] = {
    val dirs = for {
      dx <- List(-1,0,1)
      dy <- List(-1,0,1)
      dz <- List(-1,0,1)
      dw <- List(-1,0,1)
      if (dx*dx + dy*dy + dz*dz + dw*dw > 0)
    } yield {
      P3D(p.x + dx, p.y + dy, p.z + dz, p.w + dw)
    }
    dirs.toSet}


  def parseLine(idxLine: (String, Int)): Array[P3D] = {
    val (line, y) = idxLine
    val activeOnRow = line.zipWithIndex.filter(_._1 == '#')
    activeOnRow.map { case (_, x) => P3D(x, y, 0, 0) }.toArray
  }

  val inputData: List[String] = Source.fromResource("data17.csv").getLines.toList
  val trees: List[P3D] = inputData.zipWithIndex.flatMap(parseLine)

  println(neighbours(P3D(0,0,0,0)).size)

  def countNeighbours(p: P3D,
                      actives: Set[P3D]): Int = neighbours(p).intersect(actives).size

  val gridSize = 6
  @tailrec
  def evolveGrid(curr: Set[P3D], n: Int): Set[P3D] = {
    println(n, curr.size)
    if (n == 6) curr
    else {
      val newGrid = for {x <- Range(-gridSize, inputData.length + gridSize + 1)
                         y <- Range(-gridSize, inputData.length + gridSize + 1)
                         z <- Range(-gridSize, inputData.length + gridSize + 1)
                         w <- Range(-gridSize, inputData.length + gridSize + 1)
                         if {
                           val p = P3D(x, y, z, w)
                           (curr.contains(p) & Set(2, 3).contains(countNeighbours(p, curr)) || countNeighbours(p, curr) == 3)
                         }
                         }
        yield {
          P3D(x, y, z, w)
        }
      evolveGrid(newGrid.toSet, n + 1)
    }
  }

  lazy val solutionA: Int = {
    evolveGrid(trees.toSet, 0).size
  }
}
