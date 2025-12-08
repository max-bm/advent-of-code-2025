package day05

import scala.util.boundary
import scala.util.boundary.break
import scala.util.matching.Regex
import util.Input

@main def main(): Unit =
  val input: String = Input.loadString("day05")
  println(part_1(input))
  println(part_2(input))

def part_1(input: String): Int =
  val Array(id_ranges, available_ids) = input.split("\n\n")
  var count: Int = 0
  for (id <- available_ids.split("\n")) {
    boundary {
      for (r <- id_ranges.split("\n")) {
        val Array(start, stop) = r.split("-")
        if (start.toLong <= id.toLong && id.toLong <= stop.toLong) {
          count += 1
          boundary.break()
        }
      }
    }
  }
  return count

case class Interval(start: Long, end: Long) {
  def overlaps(other: Interval): Boolean =
    this.start <= other.end && other.start <= this.end

  def length(): Long =
    this.end - this.start + 1
}

def part_2(input: String): Long =
  val Array(idRanges, _) = input.split("\n\n")
  val rangePattern: Regex = """(\d+)-(\d+)""".r
  val ranges = rangePattern
    .findAllIn(idRanges)
    .toList
    .map(x => Interval(x.split("-")(0).toLong, x.split("-")(1).toLong))
  var mergedRanges = ranges
    .sortBy(_.start)
    .foldLeft(List.empty[Interval]) { (acc, next) =>
      acc match
        case head :: tail if head.overlaps(next) =>
          Interval(
            math.min(head.start, next.start),
            math.max(head.end, next.end)
          ) :: tail
        case _ =>
          next :: acc
    }
  return mergedRanges.map(x => x.length()).sum
