package day07

import util.Input

@main def main(): Unit =
  val input: Seq[String] = Input.load("day07")
  println(part_1(input))
  println(part_2(input))

def part_1(input: Seq[String]): Long =
  val h: Int = input.length
  var total: Int = 0
  var indexes: Set[Int] = Set(input(0).indexOf("S"))
  for (r <- Range(2, h, 2)) {
    var splitIndexes: Set[Int] = indexes
    for (i <- indexes) {
      if (input(r)(i) == '^') {
        total += 1
        splitIndexes -= i
        splitIndexes = splitIndexes | Set(i - 1, i + 1)
      }
    }
    indexes = splitIndexes
  }
  return total

def part_2(input: Seq[String]): Long =
  val h: Int = input.length
  var total: Int = 1
  var indexes: Map[Int, Long] = Map(input(0).indexOf("S") -> 1.toLong)
  for (r <- Range(2, h, 2)) {
    // println("row " + r + " / " + h)
    var splitIndexes: Map[Int, Long] = indexes
    for ((i, c) <- indexes) {
      if (input(r)(i) == '^') {
        splitIndexes -= i
        splitIndexes = splitIndexes.updated(
          i - 1,
          splitIndexes.getOrElse(i - 1, 0.toLong) + c
        )
        splitIndexes = splitIndexes.updated(
          i + 1,
          splitIndexes.getOrElse(i + 1, 0.toLong) + c
        )
      }
    }
    indexes = splitIndexes
    // println(indexes)
  }
  return indexes.values.sum
