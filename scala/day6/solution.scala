//> using lib "org.typelevel::cats-core::2.10.0"

import cats.syntax.all.*
import cats.*
import scala.io.Source

def getLines = Source.fromFile("input").getLines()

def readRep[A](in: Iterator[String], reader: Iterator[String] => A) =
  val b = List.newBuilder[A]
  while (in.hasNext) b.addOne(reader(in))
  b.result

def readInts(str: String)  = str.split(' ').filterNot(_.isEmpty).map(_.toInt)
def readLongs(str: String) = str.split(' ').filterNot(_.isEmpty).map(_.toLong)

def winningChargeTimes(d: Long, limit: Long) =
  def t(ct: Long)    = ct + (d / ct)
  def satt(ct: Long) = t(ct) < limit
  def optChargeTime  = Math.sqrt(d).toLong
  def rec(minMis: Long, minHit: Long, maxHit: Long, maxMis: Long): (Long, Long) =
    inline def recMin: (Long, Long) =
      val nextMin = (minHit + minMis) / 2
      println(s"recMin $nextMin")
      if (satt(nextMin))
        rec(minMis, nextMin, maxHit, maxMis)
      else
        rec(nextMin, minHit, maxHit, maxMis)
    inline def recMax: (Long, Long) =
      val nextMax = (maxHit + maxMis) / 2
      if (satt(nextMax))
        rec(minMis, minHit, nextMax, maxMis)
      else
        rec(minMis, minHit, maxHit, nextMax)

    if (minMis < minHit - 1) recMin
    else if (maxMis > maxHit + 1) recMax
    else (minHit, maxHit)
  rec(0, optChargeTime, optChargeTime, d)

@main def part1 =
  val lines            = getLines
  val s"Time: $ts"     = lines.next()
  val s"Distance: $ds" = lines.next()

  val r = readLongs(ts)
    .zip(readLongs(ds))
    .map { case (t, d) =>
      val (min, max) = winningChargeTimes(d, t)
      assert(max >= min)
      (max - min + 1).toLong
    }
    .reduce(_ * _)
  println(r)

@main def part2 =
  val t          = 61709066L
  val d          = 643118413621041L
  val (min, max) = winningChargeTimes(d, t)
  println(max - min + 1)
