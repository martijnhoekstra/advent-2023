//> using lib "org.typelevel::cats-core::2.10.0"

import cats.syntax.all.*
import cats.*
import scala.io.Source

case class Card(winning: List[Int], have: List[Int])

def parse(line: String) =
  line match
    case s"Card $n: $winning | $our" =>
      val w = winning.split(' ').map(_.trim.toIntOption).collect { case Some(i) => i }
      val h = our.split(' ').map(_.trim.toIntOption).collect { case Some(i) => i }
      Card(w.toList, h.toList)

@main def part1 =
  val solution = Source
    .fromFile("input")
    .getLines()
    .map(parse)
    .toList
    .foldMap { case Card(winning, have) =>
      have.count(winning.contains) match
        case 0 => 0
        case n => 1 << (n - 1)
    }
  println(solution)

@main def part2 =
  val it = Source
    .fromFile("input")
    .getLines()
    .scanLeft((0, LazyList.continually(1))) { case ((sum, multiples), line) =>
      val Card(winning, have) = parse(line)
      val score               = have.count(winning.contains)
      sum + multiples.head -> LazyList
        .fill(score)(multiples.head)
        .padZipWith(multiples.tail)((o1, o2) => (o1 |+| o2).getOrElse(0))
    }
  println(it.map(_._1).toList.last)
