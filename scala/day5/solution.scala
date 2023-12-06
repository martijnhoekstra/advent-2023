//> using lib "org.typelevel::cats-core::2.10.0"

import cats.syntax.all.*
import cats.*
import scala.io.Source

def getLines = Source.fromFile("input").getLines()

def readRep[A](in: Iterator[String], reader: Iterator[String] => A) =
  val b = List.newBuilder[A]
  while (in.hasNext) b.addOne(reader(in))
  b.result

final val target = "location"
final val source = "seed"

def splitRun(start: Long, len: Long, secondStart: Long) =
  val newlen = secondStart - start
  val secondlen = len - newlen
  (start -> newlen, secondStart -> secondlen)

case class Mapping(source: Long, dest: Long, length: Long):
  def last = source + length - 1

case class AMap(source: String, dest: String, mappings: List[Mapping]):
  def lookup(fromIndex: Long) = mappings
    .collectFirst {
      case m @ Mapping(src, dst, _) if fromIndex >= src && fromIndex <= m.last => dst + (fromIndex - src)
    }
    .getOrElse(fromIndex)
  def lookupRange(from: Long, len: Long): List[(Long, Long)] =
    def rec(remaining: List[(Long, Long)], targets: List[(Long, Long)], mappings: List[Mapping]): List[(Long, Long)] =
      remaining match
        case Nil => targets
        case (origFrom, len) :: tail =>
          val end = origFrom + len - 1
          mappings match
            case Nil                                          => (origFrom, len) :: tail ::: targets
            case (m @ Mapping(mapsrc, destStart, maplen)) :: mtail =>
              // the whole source is before the next mapping
              if (end < mapsrc)
                rec(tail, origFrom -> len :: targets, mappings)
              // the whole source is after the next mapping, skip the next mapping
              else if (origFrom > m.last) rec(remaining, targets, mtail)
              // the start of the source is before the next mapping, split it off the identity part
              else if (origFrom < mapsrc)
                val (prepart, postpart) = splitRun(origFrom, len, mapsrc)
                rec(postpart :: tail, prepart :: targets, mappings)
              // the end of the source is after the next mapping, split up the part
              else if (end > m.last)
                val (prepart, postpart) = splitRun(origFrom, len, m.last + 1)
                rec(prepart :: postpart :: tail, targets, mappings)
              // the whole source is mapped
              else
                val sdest = destStart + (origFrom - mapsrc)
                rec(tail, sdest -> len :: targets, mappings)
    rec(List(from -> len), Nil, mappings)

def readMap(in: Iterator[String]): AMap =
  val (from, to) = in.next() match
    case s"$from-to-$to map:" => from -> to
  val mappings = readMappings(in)
  AMap(from, to, mappings)

def readMappings(in: Iterator[String]) =
  def rec(acc: List[Mapping]): List[Mapping] =
    if (in.hasNext) then
      in.next() match
        case s"$to $from $len" => rec(Mapping(from.toLong, to.toLong, len.toLong) :: acc)
        case ""                => acc
    else acc
  rec(Nil).sortBy(_.source)

@main def part1 =
  val lines = getLines
  val seeds = lines.next() match
    case s"seeds: $s" => s.split(' ').map(_.toLong)
  lines.next()

  val mappings = readRep(lines, readMap)

  def rec(currentMap: AMap, currentIndices: List[Long]): Long =
    val targetIndices = currentIndices.map(i => currentMap.lookup(i))
    if currentMap.dest == target then targetIndices.min
    else
      rec(mappings.find(m => m.source == currentMap.dest).get, targetIndices)

  val minloc = rec(mappings.find(_.source == source).get, seeds.toList)
  println(minloc)

@main def part2 =
  val lines = getLines
  val seeds = lines.next() match
    case s"seeds: $s" =>
      s.split(' ').map(_.toLong).toList.grouped(2).collect { case List(fr, len) => (fr, len) }.toList.sortBy(_._1)
  lines.next()
  println(seeds)

  val mappings = readRep(lines, readMap)

  def rec(currentMap: AMap, currentMappings: List[(Long, Long)]): Long =
    val targetIndices = currentMappings.flatMap { case (from, len) => currentMap.lookupRange(from, len) }
    if currentMap.dest == target then targetIndices.map(_._1).min
    else rec(mappings.find(m => m.source == currentMap.dest).get, targetIndices)

  val minloc = rec(mappings.find(_.source == source).get, seeds.toList)
  println(minloc)
