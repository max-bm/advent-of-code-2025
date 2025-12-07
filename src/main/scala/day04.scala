package day04

import scala.util.boundary
import scala.util.boundary.break
import util.Input

@main def main(): Unit =
    val input: Seq[String] = Input.load("day04")
    println(part_1(input))
    println(part_2(input))

def remove_rolls(input: Seq[String]): Seq[(Int, Int)] =
    val h = input.length
    val w = input(0).length()
    var to_remove: Seq[(Int, Int)] = List()
    for (j <- Range(0, h)) {
        for (i <- Range(0, w)) {
            boundary {
                var value = input(j)(i)
                if (value != '@') then boundary.break()
                var adjacent: Seq[Char] = List()
                for (y <- Range(List(j-1, 0).max, List(j+2, h).min)) {
                    for (x <- Range(List(i-1, 0).max, List(i+2, w).min)) {
                        adjacent = adjacent :+ input(y)(x)
                    }
                }
                if (adjacent.count(_=='@') -1 < 4) {
                    to_remove = to_remove :+ (j, i)
                }
            }
        }
    }
    return to_remove

def part_1(input: Seq[String]): Int =
    return remove_rolls(input).length

def part_2(input: Seq[String]): Int =
    var state: Seq[String] = input
    var count: Int = 0
    boundary {
        while (true) {
            var to_remove: Seq[(Int, Int)] = remove_rolls(state)
            if (to_remove.length == 0) then boundary.break()
            for (c <- to_remove) {
                state = state.updated(c._1, state(c._1).updated(c._2, 'x'))
            }
            count += to_remove.length
        }
    }
    return count
