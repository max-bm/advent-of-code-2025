package day03

import util.Input

@main def main(): Unit =
    val joltages: Seq[String] = Input.load("day03")
    println(part_1(joltages))
    println(part_2(joltages))

def findMaxIndex(joltages: List[Int], offset: Int): Int =
    return joltages.indexOf(joltages.take(joltages.length - offset + 1).max)

def part_1(joltages: Seq[String]): Int =
    var outputJoltages: Seq[Int] = List()
    for (j <- joltages) {
        var output: String = ""
        var jList: List[Int] = j.toList.map(_.asDigit)
        for (offset <- Range(2, 0, -1)) {
            val index: Int = findMaxIndex(jList, offset)
            output += jList(index).toString()
            jList = jList.takeRight(jList.length - 1 - index)
        }
        outputJoltages = outputJoltages :+ output.toInt
    }
    return outputJoltages.sum

def part_2(joltages: Seq[String]): Long =
    var outputJoltages: Seq[Long] = List()
    for (j <- joltages) {
        var output: String = ""
        var jList: List[Int] = j.toList.map(_.asDigit)
        for (offset <- Range(12, 0, -1)) {
            val index: Int = findMaxIndex(jList, offset)
            output += jList(index).toString()
            jList = jList.takeRight(jList.length - 1 - index)
        }
        outputJoltages = outputJoltages :+ output.toLong
    }
    return outputJoltages.sum
