package advent_2020

import scala.annotation.tailrec
import scala.io.Source

object day13 {
  val inputData: List[String] = Source.fromResource("data13.csv").getLines.toList
  val ts: Double = inputData.head.toDouble
  val buses: List[Int] = inputData.tail.head.split(",").filterNot(_ == "x").map(_.toInt).toList

  val buses2: List[(Int, Int)] = inputData.tail.head.split(",").zipWithIndex.collect{
    case (b, idx) if (b != "x") => (b.toInt, idx)
  }.toList


  // bus, ts of next bus, delta ts of next bus
  val nextBusTs: List[(Int, Int, Int)] = buses.map{
     b => {
       val nextTs = math.ceil(ts/b).toInt*b
       (b, nextTs, (nextTs - ts).toInt)
     }
  }

  @tailrec
  def findT(t: BigInt, x: BigInt, offset: Int, k:BigInt): BigInt = {
      if (k % 10000 == 0) println(k)
      val t0 = t - offset
      if (buses2.forall { case (b, n) => (t0 + n) % b == 0 }) t0
      else findT(t + x, x, offset, k + 1)
  }
  lazy val solutionA: Int = {
    val (nextBus, _, delta) = nextBusTs.minBy(_._2)
    delta * nextBus
  }

  lazy val solutionB: BigInt = {
//    val t0 = 1068781
//    println(buses2.map{ case (b, n) => (t0 + n) % b })
    val (fMax, delta) = buses2.maxBy(_._1)
    findT(BigInt(fMax* Int.MaxValue), BigInt(fMax), delta, 0)
  }
}
