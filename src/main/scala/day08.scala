package day08

import scala.math.pow
import scala.math.sqrt
import util.Input

@main def main(): Unit =
  val input: Seq[String] = Input.load("day08")
  println(part_1(input))
  println(part_2(input))

case class Coord(x: Int, y: Int, z: Int) {
  def distance(other: Coord): Double =
    return sqrt(
      pow(other.x - this.x, 2) + pow(other.y - this.y, 2) + pow(
        other.z - this.z,
        2
      )
    )
}

def mergeSets[A](sets: Seq[Set[A]]): Vector[Set[A]] =
  sets.foldLeft(Vector.empty[Set[A]]) { (acc, s) =>
    // find sets in acc that intersect with s
    val (overlapping, disjoint) = acc.partition(_.intersect(s).nonEmpty)
    // merge s with all overlapping ones
    val merged = overlapping.foldLeft(s)(_ union _)
    // keep merged + all disjoint sets
    disjoint :+ merged
  }

def part_1(input: Seq[String]): Int =
  val coords: Seq[(Coord, Int)] =
    input
      .map(_.split(",").map(_.toInt))
      .map(c => Coord(c(0), c(1), c(2)))
      .zipWithIndex
  val pairs =
    for (
      x <- coords;
      y <- coords;
      if x(1) < y(1)
    ) yield (x, y)
  val distances =
    pairs
      .map(x => (Set(x(0)(1), x(1)(1)), x(0)(0).distance(x(1)(0))))
      .sortBy(_(1))
  val closestPairs = for (i <- Range(0, 10)) yield distances(i)(0)
  val circuits = mergeSets(closestPairs)
  return circuits.map(_.size).sorted.reverse.slice(0, 3).product

def part_2(input: Seq[String]): Long =
  val numJboxes: Int = input.length
  val coords: Seq[(Coord, Int)] =
    input
      .map(_.split(",").map(_.toInt))
      .map(c => Coord(c(0), c(1), c(2)))
      .zipWithIndex
  val pairs =
    for (
      x <- coords;
      y <- coords;
      if x(1) < y(1)
    ) yield (x, y)
  var distances =
    pairs
      .map(x => (Set(x(0)(1), x(1)(1)), x(0)(0).distance(x(1)(0))))
      .sortBy(_(1))
  var latest = distances(0)(0)
  var circuits: Seq[Set[Int]] = Array(latest)
  distances = distances.drop(1)
  while (!(circuits.length == 1 && circuits(0).size == numJboxes)) {
    latest = distances(0)(0)
    circuits = mergeSets(circuits :+ latest)
    distances = distances.drop(1)
  }
  return latest.toList.map(coords(_)(0).x.toLong).product
