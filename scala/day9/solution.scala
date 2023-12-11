//> using lib "org.typelevel::cats-core::2.10.0"

import cats.syntax.all.*
import cats.*
import scala.io.Source
import scala.annotation.tailrec
import scala.util.chaining.*

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

// day-specific

@main def part1 =
  val lines = getLines

  def diffs(ints: LazyList[Long]) = ints.zip(ints.tail).map { case (head, next) => next - head }

  def next(puzzle: LazyList[Long]): Long =
    if (puzzle.forall(_ == 0)) 0
    else
      val nextDiff = next(diffs(puzzle))
      puzzle.last + nextDiff // reversing will help here

  lines
    .map(readLongs)
    .map(arr => arr.iterator.to(LazyList)) // reverseIterator?
    .map(x => x :+ next(x))
    .map(_.toList)
    .map(_.last)
    .sum
    .pipe(println)

@main def part2 =
  val lines = getLines

  def diffs(ints: LazyList[Long]) = ints.zip(ints.tail).map { case (head, next) => next - head }

  def prev(puzzle: LazyList[Long]): Long =
    if (puzzle.forall(_ == 0)) 0
    else
      val nextDiff = prev(diffs(puzzle))
      puzzle.head - nextDiff // reversing will help here


  lines
    .map(readLongs)
    .map(arr => arr.iterator.to(LazyList)) // reverseIterator?
    .map(x => prev(x) +: x)
    .map(_.head)
    .sum
    .pipe(println)

