//> using lib "org.typelevel::cats-core::2.10.0"

import cats.syntax.all.*
import scala.io.Source

@main def part1 =
  val solution = Source
    .fromFile("input")
    .getLines
    .toList
    .foldMap(line =>
      line.collectFirst { case ch if ch.isDigit => ch.asDigit * 10 } |+|
        line.reverseIterator.collectFirst { case ch if ch.isDigit => ch.asDigit }
    )
  println(solution)

def digit(str: String, i: Int): Option[Int] =
  val spelt = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  if (str(i).isDigit) str(i).asDigit.some
  else
    spelt.zipWithIndex.collectFirst { case (number, n) if str.startsWith(number, i) => n + 1 }

@main def part2 =
  val solution = Source
    .fromFile("input")
    .getLines
    .toList
    .foldMap(line =>
      val nums = (0 until line.length).collect((i => digit(line, i)).unlift)
      (nums.head * 10) + nums.last
    )
  println(solution)
