//> using lib "org.typelevel::cats-core::2.10.0"

import cats.syntax.all.*
import cats.*
import scala.io.Source
import scala.annotation.tailrec

def getLines = Source.fromFile("input").getLines()

def readRep[A](in: Iterator[String], reader: Iterator[String] => A) =
  val b = List.newBuilder[A]
  while (in.hasNext) b.addOne(reader(in))
  b.result

def readInts(str: String)  = str.split(' ').filterNot(_.isEmpty).map(_.toInt)
def readLongs(str: String) = str.split(' ').filterNot(_.isEmpty).map(_.toLong)

def gcd(a: Int, b: Int): Int    = if b == 0 then a else gcd(b, a % b)
def lcm(a: Int, b: Int): Int    = (a * b) / gcd(a, b)
def gcd(a: Long, b: Long): Long = if b == 0 then a else gcd(b, a % b)
def lcm(a: Long, b: Long): Long = (a * b) / gcd(a, b)

enum Switch:
  case L, R

@main def part1 =
  val lines = getLines

  val routelist = lines
    .next()
    .map {
      case 'L' => Switch.L
      case 'R' => Switch.R
    }
    .toList
  lines.next()
  val routemap = lines.map { case s"$from = ($l, $r)" =>
    from -> (l, r)
  }.toMap

  val end = "ZZZ"

  def rec(route: List[Switch], location: String, taken: List[Switch]): List[Switch] =
    if (location == end) taken
    else
      route match
        case Nil              => rec(routelist, location, taken)
        case Switch.L :: tail => rec(tail, routemap(location)._1, Switch.L :: taken)
        case Switch.R :: tail => rec(tail, routemap(location)._2, Switch.R :: taken)

  println(rec(routelist, "AAA", Nil).length)

@main def part2 =
  val lines = getLines

  val routelist = lines
    .next()
    .map {
      case 'L' => Switch.L
      case 'R' => Switch.R
    }
    .toList
  lines.next()
  val routemap = lines.map { case s"$from = ($l, $r)" =>
    from -> (l, r)
  }.toMap

  // we should at some point enter a cycle. We know we're in a cycle if we hit the same ABC location at the same
  // point in the routelist.
  // that means a cycle has a length, and a set of which nodes are visited at which steps of the cycle

  // for each start location, we find the list of steps that takes us to the start point of the cycle, and then the cycle
  def findCycle(
      route: List[Switch],
      routeIndex: Int,
      loc: String,
      cyclemap: Map[Int, List[(Int, String)]],
      step: Int
  ): Int =
    route match
      case Nil => findCycle(routelist, 0, loc, cyclemap, step)
      case head :: tail =>
        val onThisIndex = cyclemap.getOrElse(routeIndex, Nil)
        (onThisIndex.collectFirst { case (firstAt, `loc`) => firstAt }) match
          case Some(seen) => step - seen

          case None =>
            val loc1      = if head == Switch.L then routemap(loc)._1 else routemap(loc)._2
            val cyclemap1 = cyclemap.updated(routeIndex, (step, loc) :: onThisIndex)
            findCycle(tail, routeIndex + 1, loc1, cyclemap1, step + 1)

  val cycles =
    for (key <- routemap.keys if key(2) == 'A') yield findCycle(routelist, 0, key, Map.empty, 0)

  println(cycles.map(_.toLong).reduce(lcm))
