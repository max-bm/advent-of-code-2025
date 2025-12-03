package day02

import scala.util.boundary
import scala.util.boundary.break
import util.Input

@main def main(): Unit =
    val ranges: String = Input.loadString("day02")
    println(part_1(parseRanges(ranges)))
    println(part_2(parseRanges(ranges)))

def parseRange(range: String): scala.collection.immutable.NumericRange.Exclusive[Long] =
    var splitRange = range.split("-")
    return Range.Long(splitRange(0).toLong, splitRange(1).toLong + 1, 1)

def parseRanges(ranges: String): Array[scala.collection.immutable.NumericRange.Exclusive[Long]] =
    return ranges.split(",").map(parseRange)

def isInvalidId(id: Long): Boolean =
    val idString: String = id.toString()
    val length: Int = idString.length()
    if (length % 2 != 0) {
        return false
    }
    val start: String = idString.substring(0, length / 2)
    val end: String = idString.substring(length / 2)
    return start == end

def isInvalidIdPart2(id: Long): Boolean =
    val idString: String = id.toString()
    val length: Int = idString.length()
    var isInvalid: Boolean = false
    boundary {
        for (l <- Range(1, length / 2 + 1)) {
            boundary {
                if (length % l != 0) {
                    boundary.break()
                }
            }
            var repeatedSubstring: String = idString.substring(0, l) * (length / l)
            if (repeatedSubstring == idString) {
                isInvalid = true
                boundary.break()
            }
        }
    }
    return isInvalid

def part_1(ranges: Array[scala.collection.immutable.NumericRange.Exclusive[Long]]): Long =
    var invalidIds: Seq[Long] = List()
    for (r <- ranges) {
        for (n <- r) {
            if isInvalidId(n) then invalidIds :+= n
        }
    }
    return invalidIds.sum

def part_2(ranges: Array[scala.collection.immutable.NumericRange.Exclusive[Long]]): Long =
    var invalidIds: Seq[Long] = List()
    for (r <- ranges) {
        for (n <- r) {
            if isInvalidIdPart2(n) then invalidIds :+= n
        }
    }
    return invalidIds.sum
