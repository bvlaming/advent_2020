package advent_2020

import scala.annotation.tailrec
import scala.io.Source

case class ShipMove(dir: Char, n: Int)
case class Ship(face: Char, x: Int, y: Int)


object day12 {
  val dirToAngle: Map[Char, Int] = Map(('N', 0), ('E', 90), ('S', 180), ('W', 270))
  val angleToDir: Map[Int, Char] = dirToAngle.map(_.swap)
  def parseLine(s: String): ShipMove = ShipMove(s.head, s.tail.toInt)
  val inputData: List[ShipMove] = Source.fromResource("data12.csv").getLines.toList.map(parseLine)

  @tailrec
  def moveShip(ship: Ship, instruction: ShipMove): Ship = {
    instruction.dir match {
      case 'N' => Ship(ship.face, ship.x, ship.y + instruction.n)
      case 'S' => Ship(ship.face, ship.x, ship.y - instruction.n)
      case 'E' => Ship(ship.face, ship.x + instruction.n, ship.y)
      case 'W' => Ship(ship.face, ship.x - instruction.n, ship.y)
      case 'L' => {
        val newAngle = (dirToAngle(ship.face) - instruction.n + 360) % 360
        Ship(angleToDir(newAngle), ship.x, ship.y)
      }
      case 'R' => {
        val newAngle = (dirToAngle(ship.face) + instruction.n) % 360
        Ship(angleToDir(newAngle), ship.x, ship.y)
      }
      case 'F' => moveShip(ship, ShipMove(ship.face, instruction.n))
    }
  }

  def moveShipWaypoint(ship: Ship, waypoint: Ship, instruction: ShipMove): (Ship, Ship) = {
    println(ship)
    println(waypoint)
    instruction.dir match {
      case 'N' => (ship, Ship(waypoint.face, waypoint.x, waypoint.y + instruction.n))
      case 'S' => (ship, Ship(waypoint.face, waypoint.x, waypoint.y - instruction.n))
      case 'E' => (ship, Ship(waypoint.face, waypoint.x + instruction.n, waypoint.y))
      case 'W' => (ship, Ship(waypoint.face, waypoint.x - instruction.n, waypoint.y))
      case 'F' => {
        val (dx, dy) = (waypoint.x - ship.x, waypoint.y - ship.y)
        (Ship(ship.face, ship.x + instruction.n * dx, ship.y + instruction.n * dy),
          Ship(waypoint.face, waypoint.x + instruction.n * dx, waypoint.y + instruction.n * dy))
      }
      case c: Char => {
        val sgn = if (c == 'L') -1 else 1
        val newAngle: Int = ((sgn * instruction.n + 360) % 360)
        val dx = waypoint.x - ship.x
        val dy = waypoint.y - ship.y
         newAngle match {
          case 0 => (ship, waypoint)
          case 90 => (ship, Ship(waypoint.face, ship.x + dy, ship.y - dx))
          case 180 => (ship, Ship(waypoint.face, ship.x - dx, ship.y - dy))
          case 270 => (ship, Ship(waypoint.face, ship.x -dy, ship.y + dx))
        }
      }
    }
  }

  lazy val solutionA: Int = {
    val initShip: Ship = Ship('E', 0, 0)
    val endShip = inputData.foldLeft(initShip){case (ship, instr) => moveShip(ship, instr)}

    math.abs(endShip.x - initShip.x) + math.abs(endShip.y - initShip.y)
  }

  lazy val solutionB: Int = {
    val initShip: Ship = Ship('E', 0, 0)
    val initWayPoint: Ship = Ship('N', 10, 1)
    val (endShip, endWP) = inputData.foldLeft((initShip, initWayPoint)){case ((ship, wp), instr) => moveShipWaypoint(ship, wp, instr)}
    math.abs(endShip.x - initShip.x) + math.abs(endShip.y - initShip.y)
  }
}
