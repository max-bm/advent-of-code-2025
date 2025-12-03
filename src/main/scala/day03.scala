package day03

import util.Input

@main def main(): Unit =
    val joltages: Seq[String] = Input.load("day03")
    val parsedJoltages = joltages.map(x => x.toList.map(_.asDigit))
    println(part_1(parsedJoltages))
    println(part_2(parsedJoltages))

def findMaxIndex(joltages: List[Int], offset: Int): Int =
    return joltages.indexOf(joltages.take(joltages.length - offset + 1).max)

def findOutputJoltage(bank: List[Int], length: Int): String =
    var output: String = ""
    var b: List[Int] = bank
    for (offset <- Range(length, 0, -1)) {
        val index: Int = findMaxIndex(b, offset)
        output += b(index).toString()
        b = b.takeRight(b.length - 1 - index)
    }
    return output

def part_1(joltages: Seq[List[Int]]): Int =
    var outputJoltages: Seq[Int] =
        for (bank <- joltages) yield findOutputJoltage(bank, 2).toInt
    return outputJoltages.sum

def part_2(joltages: Seq[List[Int]]): Long =
    var outputJoltages: Seq[Long] =
        for (bank <- joltages) yield findOutputJoltage(bank, 12).toLong
    return outputJoltages.sum
