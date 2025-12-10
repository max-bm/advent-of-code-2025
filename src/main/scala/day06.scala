package day06

import util.Input

@main def main(): Unit =
  val input: Seq[String] = Input.load("day06")
  println(part_1(input))
  println(part_2(input))

def part_1(input: Seq[String]): Long =
  val parsedInput: Seq[Seq[String]] =
    input.toList.map(x => x.trim().split("\\s+")).transpose
  val calculations: Seq[Long] =
    parsedInput.map(x => {
      val operator: String = x.last
      val digits: Seq[Long] = x.init.map(y => y.toLong)
      operator match
        case "+" => digits.sum
        case "*" => digits.product
    })
  return calculations.sum

def part_2(input: Seq[String]): Long =
  val parsedInput: Seq[Seq[String]] =
    input.toList.map(x => x.split(""))
  val operators: Seq[String] =
    parsedInput.last.filter(_.trim().nonEmpty).reverse
  val transposedInput: Seq[String] =
    parsedInput.init.transpose.map(_.mkString.trim)
  val foldedInput: Seq[Seq[Long]] =
    transposedInput.foldLeft(List(List.empty[Long])) { (acc, next) =>
      next match
        case "" =>
          List.empty[Long] :: acc
        case _ =>
          (next.toLong :: acc.head) :: acc.tail
    }
  val calculations: Seq[Long] =
    foldedInput.zipWithIndex.map((digits, index) => {
      val operator: String = operators(index)
      operator match
        case "+" => digits.sum
        case "*" => digits.product
    })
  return calculations.sum
