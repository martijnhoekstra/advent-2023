//> using lib "org.typelevel::cats-core::2.10.0"

import cats.syntax.all.*
import cats.*
import scala.io.Source

case class PlacedNumber(value: Long, start: Int, end: Int)
case class Symbol(sym: Char, loc: Int)

def parse(line: String) =
  def parse0(i: Int, agg: List[Either[Symbol, PlacedNumber]]): List[Either[Symbol, PlacedNumber]] =
    if i >= line.length then agg.reverse
    else
      line(i) match
        case '.' => parse0(i + 1, agg)
        case ch if ch.isDigit =>
          val num = line.substring(i).takeWhile(_.isDigit)
          val end = i + num.length
          parse0(end, Right(PlacedNumber(num.toLong, i, end - 1)) :: agg)
        case sym => parse0(i + 1, Left(Symbol(sym, i)) :: agg)
  parse0(0, Nil)

@main def part1 =
  val solution = (Iterator("") ++ Source
    .fromFile("input")
    .getLines() ++ Iterator(""))
    .map(parse)
    .toVector
    .sliding3
    .flatMap { case (prev, cur, next) =>
      val symbols = (prev ++ cur ++ next).collect { case Left(Symbol(_, i)) => i }.toSet
      cur.collect {
        case Right(PlacedNumber(value, start, end)) if symbols.exists(sym => sym >= (start - 1) && sym <= (end + 1)) =>
          value
      }
    }
    .combineAll
  println(solution)

@main def part2 =
  val solution = (Iterator("") ++ Source
    .fromFile("input")
    .getLines() ++ Iterator(""))
    .map(parse)
    .toVector
    .sliding3
    .flatMap { case (prev, cur, next) =>
      val wheels = cur.collect { case Left(Symbol('*', i)) => i }

      val numbers = (prev ++ cur ++ next).collect { case Right(number) => number }
      val gearOptions = wheels.map(wheel => wheel -> numbers.filter(number => wheel >= number.start - 1 && wheel <= number.end + 1))
      gearOptions.collect {
        case (wheel, List(PlacedNumber(v1, _, _), PlacedNumber(v2, _, _))) => v1 * v2
      }
    }
    .combineAll
  println(solution)
