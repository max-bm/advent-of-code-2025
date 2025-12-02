//> using scala 3.7.4
//> using toolkit default
import scala.io.StdIn.readLine
import os.readLink

@main
def main(): Unit =
    val filename: String = "input.txt"
    val path: os.Path = os.pwd / filename
    val instructions: Seq[String] = os.read.lines(path)
    println(part_1(parseInstructions(instructions)))
    println(part_2(parseInstructions(instructions)))

def parseInstruction(s: String): Int = s match {
    case s"L$i" => -i.toInt
    case s"R$i" => i.toInt
}

def parseInstructions(input: Seq[String]): Seq[Int] = input.map(parseInstruction).toSeq

def part_1(instructions: Seq[Int]): Int =
    var pos: Int = 50
    val positions: Seq[Int] = for (i <- instructions) yield {
        pos = (pos + i) % 100
        pos = if (pos >= 0) pos else 100 + pos
        pos
    }
    return positions.count(_ == 0)


def part_2(instructions: Seq[Int]): Int =
    var pos: Int = 50
    val flatInstructions: Seq[Int] = instructions.flatMap(i => Seq.fill(i.abs)(i.sign))
    val positions: Seq[Int] = for (i <- flatInstructions) yield {
        pos = (pos + i) % 100
        pos = if (pos >= 0) pos else 100 + pos
        pos
    }
    return positions.count(_ == 0)
