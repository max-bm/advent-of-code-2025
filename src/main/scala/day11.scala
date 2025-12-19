package day11

import scala.collection.mutable
import util.Input

@main def main(): Unit =
  val input_1 = Input.load("day11")
  println(part1(input_1))
  val input_2 = Input.load("day11")
  println(part2(input_2))

def buildRouteMap(routes: Seq[String]): mutable.Map[String, List[String]] =
  var routeMap = mutable.Map.empty[String, List[String]]
  for route <- routes do
    val Array(input, outputs) = route.split(": ")
    val outputArray = outputs.split(" ")
    routeMap.update(input, outputArray.toList)
  return routeMap

def countPaths1(
    node: String,
    routeMap: mutable.Map[String, List[String]],
    memo: mutable.Map[String, Long]
): Long =
  memo.getOrElseUpdate(
    node,
    if node == "out" then 1
    else routeMap(node).map { next => countPaths1(next, routeMap, memo) }.sum
  )

case class State(node: String, seenFft: Boolean, seenDac: Boolean)

def countPaths2(
    state: State,
    routeMap: mutable.Map[String, List[String]],
    memo: mutable.Map[State, Long]
): Long =
  memo.getOrElseUpdate(
    state,
    if state.node == "out" then if state.seenFft && state.seenDac then 1 else 0
    else
      routeMap(state.node).map { next =>
        countPaths2(
          State(
            next,
            state.seenFft || next == "fft",
            state.seenDac || next == "dac"
          ),
          routeMap,
          memo
        )
      }.sum
  )

def part1(input: Seq[String]): Long =
  val routeMap = buildRouteMap(input)
  countPaths1("you", routeMap, mutable.Map.empty)

def part2(input: Seq[String]): Long =
  val routeMap = buildRouteMap(input)
  countPaths2(State("svr", false, false), routeMap, mutable.Map.empty)
