//> using lib "org.typelevel::cats-core::2.10.0"

import cats.syntax.all.*
import cats.*
import scala.io.Source
import scala.annotation.tailrec
import scala.util.chaining.*
import scala.collection.mutable

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

def charmap(lines: Iterator[String]) = (for {
  (line, y) <- lines.zipWithIndex
  (char, x) <- line.zipWithIndex
} yield ((x, y), char)).toMap

type Coord = (Int, Int)

// day-specific
@main def part1 =
  val lines    = getLines
  val rawmap   = charmap(lines)
  val galaxies = rawmap.iterator.collect { case (k, '#') => k }.toSet
  val maxx     = rawmap.keySet.map(_._1).max
  val maxy     = rawmap.keySet.map(_._2).max

  println(s"there are ${galaxies.size} galaxies")

  val emptyRows = (0 to maxy).filter { y =>
    (0 to maxx).forall(x => !galaxies.contains(x -> y))
  }
  val emptyCols = (0 to maxx).filter { x =>
    (0 to maxy).forall(y => !galaxies.contains(x -> y))
  }

  def distance(start: Coord, end: Coord): Int =
    val List(startx, endx) = List(start._1, end._1).sorted
    val List(starty, endy) = List(start._2, end._2).sorted
    val xDist              = endx - startx + emptyCols.dropWhile(_ < startx).takeWhile(_ < endx).size
    val yDist              = endy - starty + emptyRows.dropWhile(_ < starty).takeWhile(_ < endy).size
    xDist + yDist

  val pairs = galaxies.tails
    .filter(_.nonEmpty)
    .flatMap(xs => xs.tail.map((xs.head, _)))
    .toList

  val d = pairs.toList.foldMap(distance)
  println(d)

@main def part2 =
  val lines    = getLines
  val rawmap   = charmap(lines)
  val galaxies = rawmap.iterator.collect { case (k, '#') => k }.toSet
  val maxx     = rawmap.keySet.map(_._1).max
  val maxy     = rawmap.keySet.map(_._2).max

  println(s"there are ${galaxies.size} galaxies")

  val emptyRows = (0 to maxy).filter { y =>
    (0 to maxx).forall(x => !galaxies.contains(x -> y))
  }
  val emptyCols = (0 to maxx).filter { x =>
    (0 to maxy).forall(y => !galaxies.contains(x -> y))
  }

  def distance(start: Coord, end: Coord): Long =
    val List(startx, endx) = List(start._1, end._1).sorted
    val List(starty, endy) = List(start._2, end._2).sorted
    val xDist = (endx - startx).toLong + (emptyCols.dropWhile(_ <= startx).takeWhile(_ < endx).size.toLong * (1000000 - 1))
    val yDist = (endy - starty).toLong + (emptyRows.dropWhile(_ <= starty).takeWhile(_ < endy).size.toLong * (1000000 - 1))
    xDist + yDist

  val pairs = galaxies.tails
    .filter(_.nonEmpty)
    .flatMap(xs => xs.tail.map((xs.head, _)))
    .toList

  val d = pairs.toList.foldMap(distance)
  println(d)
