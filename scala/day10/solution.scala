//> using lib "org.typelevel::cats-core::2.10.0"

import cats.syntax.all.*
import cats.*
import scala.io.Source
import scala.annotation.tailrec
import scala.util.chaining.*
import scala.collection.mutable
import javax.print.attribute.standard.Sides

// accumulated utils from previous days

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

type Coord = (Int, Int)

//day-specific
def debugPrint(debugVisited: Set[Coord], charmap: Map[Coord, Char]) =
  val minx = debugVisited.map(_._1).min
  val miny = debugVisited.map(_._2).min
  val maxx = debugVisited.map(_._1).max
  val maxy = debugVisited.map(_._2).max
  for
    y <- (miny to maxy)
    x <- (minx to maxx)
  do
    if x == minx then println()
    val char =
      if debugVisited.contains(x -> y) then charmap(x -> y)
      else '`'
    print(char)

  println()

// day-specific
@main def part1 =
  val lines = getLines

  val charmap = (for {
    (line, y) <- lines.zipWithIndex
    (char, x) <- line.zipWithIndex
  } yield ((x, y), char)).toMap

  val (starts, jumpmaplist) = charmap
    .collect {
      case ((x, y), '-') => (x, y) -> Some((x - 1, y), (x + 1, y))
      case ((x, y), '|') => (x, y) -> Some((x, y - 1), (x, y + 1))
      case ((x, y), 'J') => (x, y) -> Some((x - 1, y), (x, y - 1))
      case ((x, y), 'F') => (x, y) -> Some((x + 1, y), (x, y + 1))
      case ((x, y), '7') => (x, y) -> Some((x - 1, y), (x, y + 1))
      case ((x, y), 'L') => (x, y) -> Some((x, y - 1), (x + 1, y))
      case ((x, y), 'S') => (x, y) -> None
    }
    .partitionMap {
      case (coord, None)         => Left(coord)
      case (coord, Some(coords)) => Right(coord -> coords)
    }

  val start = starts.head
  val List(first, second) = jumpmaplist.collect {
    case (from, (`start`, _)) => from
    case (from, (_, `start`)) => from
  }.toList

  val jumpmap: Map[Coord, (Coord, Coord)] = jumpmaplist.toMap + (start -> (first, second))

  println(s"${charmap(start)} should be S")

  def rec(fromA: Coord, a: Coord, fromB: Coord, b: Coord, steps: Int, visited: Set[Coord]): Int =
    if (a == b && steps > 1) steps
    else
      val nextA = jumpmap(a).toList.find((c: Coord) => c != fromA).get
      val nextB = jumpmap(b).toList.find((c: Coord) => c != fromB).get
      rec(a, nextA, b, nextB, steps + 1, visited ++ List(a, b))

  println(rec(start, first, start, second, 1, Set(start)))

@main def part2 =
  val lines = getLines

  val charlines = (for {
    (line, y) <- lines.zipWithIndex
    (char, x) <- line.zipWithIndex
  } yield ((x, y), char))

  val charmap = charlines.toMap

  val (starts, jumpmaplist) = charmap
    .collect {
      case ((x, y), '-') => (x, y) -> Some((x - 1, y), (x + 1, y))
      case ((x, y), '|') => (x, y) -> Some((x, y - 1), (x, y + 1))
      case ((x, y), 'J') => (x, y) -> Some((x - 1, y), (x, y - 1))
      case ((x, y), 'F') => (x, y) -> Some((x + 1, y), (x, y + 1))
      case ((x, y), '7') => (x, y) -> Some((x - 1, y), (x, y + 1))
      case ((x, y), 'L') => (x, y) -> Some((x, y - 1), (x + 1, y))
      case ((x, y), 'S') => (x, y) -> None
    }
    .partitionMap {
      case (coord, None)         => Left(coord)
      case (coord, Some(coords)) => Right(coord -> coords)
    }

  val start = starts.head
  val List(first, second) = jumpmaplist.collect {
    case (from, (`start`, _)) => from
    case (from, (_, `start`)) => from
  }.toList

  val jumpmap: Map[Coord, (Coord, Coord)] = jumpmaplist.toMap + (start -> (first, second))

  def rec(fromA: Coord, a: Coord, fromB: Coord, b: Coord, steps: Int, visited: Set[Coord]): Set[Coord] =
    if (a == b && steps > 1) visited
    else
      val nextA = jumpmap(a).toList.find((c: Coord) => c != fromA).get
      val nextB = jumpmap(b).toList.find((c: Coord) => c != fromB).get
      rec(a, nextA, b, nextB, steps + 1, visited ++ List(a, b))

  val visited = rec(start, first, start, second, 1, Set(start))

  val maxx = jumpmap.keySet.map(_._1).max
  val maxy = jumpmap.keySet.map(_._2).max

  enum Side:
    case Out, In

  def other(side: Side) = side match
    case Side.Out => Side.In
    case Side.In  => Side.Out

  def countLine(y: Int): Int =
    val (count, _) = (0 to maxx).foldLeft((0, Side.Out)) {
      case ((c, side), x) if visited(x -> y) =>
        if ('|' == charmap(x -> y)) c -> other(side)
        else (c, side)
      case ((c, Side.In), _)  => (c + 1, Side.In)
      case ((c, Side.Out), _) => (c, Side.Out)
    }
    count

  def enclosedInLine(y: Int): Set[Coord] =
    (0 to maxx)
      .foldLeft((Set.empty[Coord], Side.Out)) {
        case ((c, side), x) if visited(x -> y) =>
          if ('|' == charmap(x -> y)) c -> other(side)
          else (c, side)
        case ((c, Side.In), x)  => (c + (x -> y), Side.In)
        case ((c, Side.Out), _) => (c, Side.Out)
      }
      ._1

  def enclosedInCol(x: Int): Set[Coord] =
    (0 to maxy)
      .foldLeft((Set.empty[Coord], Side.Out)) {
        case ((c, side), y) if visited(x -> y) =>
          if ('-' == charmap(x -> y)) c -> other(side)
          else (c, side)
        case ((c, Side.In), y)  => (c + (x -> y), Side.In)
        case ((c, Side.Out), _) => (c, Side.Out)
      }
      ._1

  val enclosedx = (0 to maxy).toSet.flatMap(enclosedInLine)
  val enclosedy = (0 to maxx).toSet.flatMap(enclosedInCol)

  val enclosed = enclosedx intersect enclosedy

  for
    y <- (0 to maxy)
    x <- (0 to maxx)
  do
    if x == 0 then println()
    val char =
      if visited.contains(x -> y) then charmap(x -> y)
      else if enclosed.contains(x -> y) then 'I'
      else '`'
    print(char)
  println()

  println((0 to maxy).map(countLine).sum)
