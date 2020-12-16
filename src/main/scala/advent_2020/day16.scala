package advent_2020

import scala.annotation.tailrec
import scala.io.Source

// condition, l1-l2 or u1-u2
object day16 {
  val myTicket = List(109,101,79,127,71,59,67,61,173,157,163,103,83,97,73,167,53,107,89,131)
  val nearbyTickets: List[Array[Int]] = Source.fromResource("data16b.csv").getLines().map{
    ln => ln.split(",").map(_.toInt)}.toList

  // (cond, l1, l2, u1, u2)
  val conditions: List[(String, Int, Int, Int, Int)] = Source.fromResource("data16a.csv").getLines().map{
    ln => {
      val parts = ln.split(": ")
      val cond = parts.head
      val intervals = parts.tail.head.split(" or ")
      val lower = intervals.head.split("-")
      val upper = intervals.tail.head.split("-")
      (cond, lower.head.toInt, lower.tail.head.toInt, upper.head.toInt, upper.tail.head.toInt)
    }
  }.toList

//  println(conditions)

  def valueIsValid(x: Int): Boolean = {
    conditions.exists{
      case (_, l1, l2, u1, u2) => (x >= l1 & x <= l2) || (x >= u1 & x <= u2)
    }
  }

  def valueMatchesCond(x: Int): List[String] = {
    conditions.collect{
      case (c, l1, l2, u1, u2) if (x >= l1 & x <= l2) || (x >= u1 & x <= u2) => c
    }
  }

  def valueMatchesSingleCond(x: Int, cond: (String, Int, Int, Int, Int)): Boolean = {
    val (c, l1, l2, u1, u2) = cond
    (x >= l1 & x <= l2) || (x >= u1 & x <= u2)
  }

  def ticketInvalidValues(ticket: Array[Int]): Array[Int] = {
    ticket.filterNot(valueIsValid)
  }

  @tailrec
  def filterMapping(mapsToDo: Map[Int, List[String]],
                    mapsDone: Map[Int, String]): Map[Int, String] = {

    if (mapsToDo.isEmpty) mapsDone
    else {
      val singleMapping = mapsToDo.collect {
        case (n, cs) if (cs.length == 1) => (n, cs.head)
      }

      val cleanedMapping: Map[Int, List[String]] = mapsToDo.map {
        case (n, cs) => (n, cs.filterNot(c => singleMapping.values.toSet.contains(c)))
      }.filterNot(_._2.isEmpty)
      filterMapping(cleanedMapping, mapsDone ++ singleMapping)
    }
  }


  def conditionOrder(tickets: List[Array[Int]]): Map[Int, String] = {
    // for each cond: find what is the position of its value in the tickets
    val ticketsIdx: List[Array[(Int, Int)]] = tickets.map(_.zipWithIndex)
    val idxValues = for {n <- conditions.indices} yield {
      (n, ticketsIdx.map(p => p.find(_._2 == n).get._1))
    }
    // (n, all n'th values)
    val idxMap: Map[Int, List[Int]] = idxValues.toMap
    // for each position: find cond s.t. all vals
    val z: Map[Int, List[String]] = idxMap.map{
      case (n, vals) => (n, conditions.filter{
        case c => vals.forall(x => valueMatchesSingleCond(x, c))
      }.map(_._1))
    }

    filterMapping(z, Map.empty)

  }

  lazy val solutionA: Int = {
    nearbyTickets.flatMap(ticketInvalidValues).sum
  }

  lazy val solutionB: BigInt = {
    val validTickets = nearbyTickets.filter(t => ticketInvalidValues(t).isEmpty)
    println(conditionOrder(validTickets))

    val departureIdx: Set[Int] = conditionOrder(validTickets).filter(_._2.startsWith("departure")).keySet
    myTicket.zipWithIndex.collect{case (v, idx) if departureIdx.contains(idx) => v}.map(x => BigInt(x)).product

  }
}
