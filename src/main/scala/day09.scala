package day09

import scala.math.abs
import scala.math.min
import scala.math.signum
import util.Input

@main def main(): Unit =
  val input: Seq[String] = Input.load("day09")
  println(part_1(input))
  println(part_2(input))

def calculateArea(a: (Long, Long), b: (Long, Long)): Long =
  return (abs(a(0) - b(0)) + 1) * (abs(a(1) - b(1)) + 1)

def part_1(input: Seq[String]): Long =
  val coords =
    input.map(x => x.split(",").toList.map(_.toLong)).map(x => (x(0), x(1)))
  val pairs =
    for (
      a <- coords.zipWithIndex;
      b <- coords.zipWithIndex;
      if a(1) < b(1)
    ) yield (a(0), b(0))
  return pairs.map((a, b) => calculateArea((a(0), a(1)), (b(0), b(1)))).max

def intersectsBoundary(
    a: (Long, Long),
    b: (Long, Long),
    boundary: List[Seq[(Long, Long)]]
): Boolean =
  val (x, y) = a
  val (u, v) = b
  val intersection =
    for (Seq((p, q), (r, s)) <- boundary)
      yield p.min(r) < x.max(u)
        && q.min(s) < y.max(v)
        && p.max(r) > x.min(u)
        && q.max(s) > y.min(v)
  return !intersection.contains(true)

def part_2(input: Seq[String]): Long =
  val coords =
    input.map(x => x.split(",").toList.map(_.toLong)).map(x => (x(0), x(1)))
  val pairs =
    for (
      a <- coords.zipWithIndex;
      b <- coords.zipWithIndex;
      if a(1) < b(1)
    ) yield (a(0), b(0))
  val boundary =
    (coords :+ coords(0))
      .sliding(2, 1)
      .toList
  val areas = pairs
    .map((a, b) => (calculateArea(a, b), intersectsBoundary(a, b, boundary)))
    .filter(x => x(1))
    .sortBy(x => x(0))
    .reverse
  return areas(0)(0)
