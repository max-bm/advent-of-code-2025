package day12

import util.Input

@main def main(): Unit =
  val input = Input.loadString("day12")
  println(part1(input))

case class Present(shape: String, area: Int)

case class Region(area: Int, shapes: List[Int])

def parseInput(input: String): (Map[Int, Present], List[Region]) =
  val splitInput = input.split("\n\n")
  val presents = for (present <- splitInput.init) yield {
    val Array(number, shape) = present.split(":\n")
    (number.toInt, Present(shape, shape.count(_ == '#')))
  }
  val regions = splitInput.last
    .split("\n")
    .map(x => {
      val splitRegion = x.split(": ")
      Region(
        splitRegion(0).split("x").map(_.toInt).product,
        splitRegion(1).split(" ").toList.map(_.toInt)
      )
    })
    .toList
  return (presents.toMap, regions)

def part1(input: String): Long =
  val (shapes, regions) = parseInput(input)
  val validRegion = regions.map(r => {
    val totalArea = for n <- 0 until r.shapes.length yield {
      r.shapes(n) * shapes(n).area
    }
    totalArea.sum <= r.area
  })
  return validRegion.count(_ == true)
