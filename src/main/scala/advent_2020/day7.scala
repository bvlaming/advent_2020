package advent_2020

import scala.annotation.tailrec
import scala.io.Source


case class Bag(qualifier: String)


object day7 {
  def parseLeaf(bagStr: String): (Bag, Int) = {
    def elements = bagStr.split(" ")

    if (elements.head == "no") (Bag(elements.tail.init.mkString(" ")), 0)
    else (Bag(elements.tail.init.mkString(" ")), elements.head.toInt)
  }

  def parseLine(line: String): (Bag, Array[(Bag, Int)]) = {
    val ln = line.replace("bags", "bag").split(" contain ")
    val branch = parseLeaf("1 " + ln.head)._1
    val leaves: Array[(Bag, Int)] = ln.tail.head.split(", ").map(parseLeaf)
    (branch, leaves)
  }

  @tailrec
  def collectFathers(acc: Set[Bag],
                     newFathers: Set[Bag],
                     alreadyChecked: Set[Bag]): Set[Bag] = {
    val bagsToCheck = newFathers -- alreadyChecked
    if (bagsToCheck.isEmpty) acc
    else {
      val nextBags = bagFathers.filter { case (c, _) => bagsToCheck.contains(c) }.map(_._2).toSet
      collectFathers(acc ++ nextBags, nextBags, alreadyChecked ++ bagsToCheck)
    }
  }


  @tailrec
  def collectChildren(acc: List[(Int, List[(Bag, Int)])],
                      lastGen: List[(Bag, Int)],
                      n: Int): List[(Int, List[(Bag, Int)])] = {
    // for each generation n: check what/how many bags it contains
    if (lastGen.isEmpty) acc
    else {
      val bagsToAdd: List[(Bag, Int)] = lastGen.flatMap {
        case (bag, x) => bagChildren.collect {
          case (f, c, y) if (bag == f) => (c, x * y)
        }
      }.filter(_._2 > 0)
      collectChildren((n, bagsToAdd) :: acc,
        bagsToAdd,
        n + 1)
    }
  }

  val data: List[String] = Source.fromResource("data7.csv").getLines.toList
  val bagContents: List[(Bag, Array[(Bag, Int)])] = data.map(parseLine)

  // from bag, to the bags it can be in
  val bagFathers: List[(Bag, Bag)] = bagContents.flatMap { case (f, cs) => cs.map(c => (c._1, f)) }
  // from bag, to the bags it contains
  val bagChildren: List[(Bag, Bag, Int)] = bagContents.flatMap { case (f, cs) => cs.map(c => (f, c._1, c._2)) }

  val solutionA: Int = {
    val fs = collectFathers(Set.empty, Set(Bag("shiny gold")), Set.empty)
    fs.size
  }

  val solutionB: Int = {
    val startBag = List((Bag("shiny gold"), 1))
    val generations = collectChildren(List((0, startBag)), startBag, 1)

    val generationsWithCount = generations.map { case (c, ls) => (c, ls.map(_._2).sum) }
    generationsWithCount.map(_._2).sum - 1
  }
}
