package advent_2020

import scala.io.Source

case class Instr14(mask: List[(Char, Int)], mem: Map[BigInt, BigInt])

case class BitArray(arr: String)

object day14 {

  def fillArray(mask: List[(Char, Int)], x: Int): BitArray = {
    val binary = x.toBinaryString.reverse.padTo(36, '0').reverse

    BitArray(binary.zip(mask).map {
      case (b, (c, _)) => c match {
        case 'X' => b
        case _ => c
      }
    }.mkString)

  }

  def fillArrayB(mask: List[(Char, Int)], x: Int): BitArray = {
    val binary = x.toBinaryString.reverse.padTo(36, '0').reverse

    BitArray(binary.zip(mask).map {
      case (b, (c, _)) => c match {
        case 0 => b
        case 1 => c
        case 'X' => b
      }
    }.mkString)

  }

  val inputData: List[String] = Source.fromResource("data14test.csv").getLines().toList

  def runInstructions(currMask: List[(Char, Int)],
                      currStatus: Map[Int, BitArray],
                      instructionsToDo: List[String]): Map[Int, BitArray] = {
    if (instructionsToDo.isEmpty) currStatus
    else {
      val instrLine = instructionsToDo.head
      val s = instrLine.split(" = ")
      if (s.head == "mask") runInstructions(s.tail.head.zipWithIndex.toList, currStatus, instructionsToDo.tail)
      else {
        val arrIndex = s.head.filter(_.isDigit).mkString.toInt
        val updateVal = s.tail.head.toInt

        runInstructions(
          currMask,
          currStatus + (arrIndex -> fillArray(currMask, updateVal)),
            instructionsToDo.tail
          )
      }
    }
  }

  def runInstructionsB(currMask: List[(Char, Int)],
                      currStatus: Map[Int, BitArray],
                      instructionsToDo: List[String]): Map[Int, BitArray] = {
    if (instructionsToDo.isEmpty) currStatus
    else {
      val instrLine = instructionsToDo.head
      val s = instrLine.split(" = ")
      if (s.head == "mask") runInstructions(s.tail.head.zipWithIndex.toList, currStatus, instructionsToDo.tail)
      else {
        val arrIndex = s.head.filter(_.isDigit).mkString.toInt
        val updateVal = s.tail.head.toInt

        runInstructions(
          currMask,
          currStatus + (arrIndex -> fillArrayB(currMask, updateVal)),
          instructionsToDo.tail
        )
      }
    }
  }

  lazy val solutionA: BigInt = {
    val arrays = runInstructions(List.empty, Map.empty, inputData)
    arrays.map{case (_, arr) => BigInt.apply(arr.arr, 2)}.sum
  }

  lazy val solutioB: BigInt = {
    val arrays = runInstructionsB(List.empty, Map.empty, inputData)
    // for every X we encounter: take a entry with a 0, and one with a 1

    arrays.map{case (_, arr) => BigInt.apply(arr.arr, 2)}.sum
  }
}
