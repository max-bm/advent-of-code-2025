package day10

import util.Input
import scala.collection.mutable

@main def main(): Unit =
  val input = Input.load("day10")
  println(part1(input))
  println(part2(input))

case class Machine(
    indicators: Vector[Int],
    buttons: Vector[Int], // bitmask representation
    joltages: Vector[Int]
)

def parseMachine(machine: String): Machine =
  machine match
    case s"[$indicatorsStr] $buttonsStr {$joltagesStr}" =>
      val indicators =
        indicatorsStr.map(_ == '#').map(b => if b then 1 else 0).toVector
      val indicatorCount = indicators.length
      val buttons = buttonsStr
        .split(" ")
        .map(_.tail.init.split(",").map(i => 1 << i.toInt).foldLeft(0)(_ | _))
        .toVector
      val joltages = joltagesStr.split(",").map(_.toInt).toVector
      return Machine(indicators, buttons, joltages)

def buildParityMap(
    machine: Machine
): Map[Vector[Int], Vector[(Int, Vector[Int])]] =
  val result = mutable.Map.empty[Vector[Int], Vector[(Int, Vector[Int])]]

  val n = machine.buttons.length
  val totalMasks = 1 << n

  for mask <- 0 until totalMasks do
    var pressCount = 0
    val counts = Array.fill(machine.indicators.length)(0)

    for i <- 0 until n do
      if ((mask & (1 << i)) != 0) then
        pressCount += 1
        val b = machine.buttons(i)
        for j <- 0 until machine.indicators.length do
          if ((b & (1 << j)) != 0) counts(j) += 1

    val parity = counts.map(_ % 2).toVector
    val incs = counts.toVector

    result.updateWith(parity) {
      case Some(v) => Some((pressCount, incs) +: v)
      case None    => Some(Vector((pressCount, incs)))
    }

  result.toMap

val INF = 1_000_000

def solveJoltages(
    joltages: Vector[Int],
    parityMap: Map[Vector[Int], Vector[(Int, Vector[Int])]],
    memo: mutable.Map[Vector[Int], Int]
): Int =
  if joltages.forall(_ == 0) then 0
  else
    memo.getOrElseUpdate(
      joltages, {
        val parity = joltages.map(_ % 2)

        parityMap.get(parity) match
          case None             => INF
          case Some(candidates) =>
            candidates.map { case (presses, incs) =>
              val rest = joltages.zip(incs).map(_ - _)
              if rest.exists(_ < 0) then INF
              else
                presses + 2 * solveJoltages(
                  rest.map(_ / 2),
                  parityMap,
                  memo
                )
            }.min
      }
    )

def part1(input: Seq[String]): Long =
  input
    .map(parseMachine)
    .map { m =>
      val parityMap = buildParityMap(m)
      parityMap(m.indicators).map(_._1).min
    }
    .sum

def part2(input: Seq[String]): Long =
  input
    .map(parseMachine)
    .map { m =>
      val parityMap = buildParityMap(m)
      solveJoltages(m.joltages, parityMap, mutable.Map.empty)
    }
    .sum
