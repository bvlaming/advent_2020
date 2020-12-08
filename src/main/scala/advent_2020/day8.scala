package advent_2020

import scala.annotation.tailrec
import scala.io.Source

case class Instruction(op: String, arg: Int)

object day8 {

  def parseLine(s: String): Instruction = {
    val line = s.split(" ")
    Instruction(line.head, line.tail.head.toInt)
  }

  @tailrec
  def followInstructions(previousPointers: Set[Int],
                         pointer: Int,
                         acc: Int): Int = {
    if (pointer > instructions.length) acc
    else if (previousPointers.contains(pointer)) acc
    else {
      val pointerSet = previousPointers + pointer
      val nextInstruction = instructions(pointer)
      nextInstruction.op match {
        case "acc" => followInstructions(pointerSet, pointer + 1, acc + nextInstruction.arg)
        case "jmp" => followInstructions(pointerSet, pointer + nextInstruction.arg, acc)
        case "nop" => followInstructions(pointerSet, pointer + 1, acc)
      }
    }
  }


  def checkCorruptions(): Int = {
    @tailrec
    def followInstructionsforCorruption(wrongPointer: Int,
                                        previousPointers: Set[Int],
                                        pointer: Int,
                                        acc: Int): Int = {
      if (pointer > instructions.length) -666
      else if (previousPointers.contains(pointer)) -666
      else if (pointer == instructions.length) acc
      else {
        val pointerSet = previousPointers + pointer
        val nextInstruction = instructions(pointer)
        if (pointer != wrongPointer)
          nextInstruction.op match {
            case "acc" => followInstructionsforCorruption(wrongPointer, pointerSet, pointer + 1, acc + nextInstruction.arg)
            case "jmp" => followInstructionsforCorruption(wrongPointer, pointerSet, pointer + nextInstruction.arg, acc)
            case "nop" => followInstructionsforCorruption(wrongPointer, pointerSet, pointer + 1, acc)
          }
        else {
          nextInstruction.op match {
            case "acc" => followInstructionsforCorruption(wrongPointer, pointerSet, pointer + 1, acc + nextInstruction.arg)
            case "nop" => followInstructionsforCorruption(wrongPointer, pointerSet, pointer + nextInstruction.arg, acc)
            case "jmp" => followInstructionsforCorruption(wrongPointer, pointerSet, pointer + 1, acc)
          }
        }
      }
    }

    val possibleWrongPointers: Array[Int] = instructions.zipWithIndex.collect {
      case x if Set("nop", "jmp").contains(x._1.op) => x._2
    }

    possibleWrongPointers.map {
      n => (n, followInstructionsforCorruption(n, Set.empty, 0, 0))
    }.filterNot(_._2 == -666).head._2
  }


  val data: List[String] = Source.fromResource("data8.csv").getLines.toList
  val instructions: Array[Instruction] = data.map(parseLine).toArray

  val solutionA: Int = followInstructions(Set.empty, 0, 0)
  val solutionB: Int = checkCorruptions()
}
