package day02

import scala.util.boundary
import scala.util.boundary.break
import scala.util.matching.Regex
import util.Input

@main def main(): Unit =
    val ranges: String = Input.loadString("day02")
    println(part_1(parseRanges(ranges)))
    println(part_2(parseRanges(ranges)))

def parseRange(range: String): scala.collection.immutable.NumericRange.Exclusive[Long] =
    var splitRange = range.split("-")
    return Range.Long(splitRange(0).toLong, splitRange(1).toLong + 1, 1)

def parseRanges(ranges: String): List[scala.collection.immutable.NumericRange.Exclusive[Long]] =
    val rangePattern: Regex = """(\d+)-(\d+)""".r
    return rangePattern.findAllIn(ranges).toList.map(parseRange)

def isInvalidId(id: Long, pattern: Regex): Boolean =
    return pattern.matches(id.toString())

def part_1(ranges: List[scala.collection.immutable.NumericRange.Exclusive[Long]]): Long =
    var invalidIds: Seq[Long] = List()
    for (r <- ranges) {
        for (n <- r) {
            if isInvalidId(n, """^(\d+)\1$""".r) then invalidIds :+= n
        }
    }
    return invalidIds.sum

def part_2(ranges: List[scala.collection.immutable.NumericRange.Exclusive[Long]]): Long =
    var invalidIds: Seq[Long] = List()
    for (r <- ranges) {
        for (n <- r) {
            if isInvalidId(n, """^(\d+)\1+$""".r) then invalidIds :+= n
        }
    }
    return invalidIds.sum
