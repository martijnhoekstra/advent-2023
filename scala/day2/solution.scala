//> using lib "org.typelevel::cats-core::2.10.0"

import cats.syntax.all.*
import cats.*
import scala.io.Source

case class RGB(red: Int, green: Int, blue: Int)
given Monoid[RGB] with
  def combine(x: RGB, y: RGB): RGB = RGB(x.red + y.red, x.green + y.green, x.blue + y.blue)
  def empty: RGB                   = RGB(0, 0, 0)

@main def part1 =
  val solution = Source
    .fromFile("input")
    .getLines
    .toList
    .foldMap { case s"Game $n: $game" =>
      val draws = game.split("; ")
      val parsed = draws.map(_.split(", ").toList.foldMap {
        case s"$r red"   => RGB(r.toInt, 0, 0)
        case s"$g green" => RGB(0, g.toInt, 0)
        case s"$b blue"  => RGB(0, 0, b.toInt)
      })
      Option.unless(parsed.exists { case RGB(r, g, b) => r > 12 || g > 13 || b > 14 })(n.toLong)
    }
  println(solution)

@main def part2 =
  val max = Monoid.instance(
    RGB(0, 0, 0),
    (x, y) => RGB(math.max(x.red, y.red), math.max(x.green, y.green), math.max(x.blue, y.blue))
  )

  val solution = Source
    .fromFile("input")
    .getLines
    .toList
    .foldMap { case s"Game $n: $game" =>
      val draws = game.split("; ")
      val parsed = draws.map(_.split(", ").toList.foldMap {
        case s"$r red"   => RGB(r.toInt, 0, 0)
        case s"$g green" => RGB(0, g.toInt, 0)
        case s"$b blue"  => RGB(0, 0, b.toInt)
      })
      val maxed = parsed.toList.combineAll(max)
      maxed.red.toLong * maxed.green * maxed.blue
    }
  println(solution)
