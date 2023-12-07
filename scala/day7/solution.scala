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

given [A](using Order[A]): Order[List[A]] with
  def compare(x: List[A], y: List[A]): Int = (x, y) match
    case (Nil, Nil) => 0
    case (_, Nil)   => 1
    case (Nil, _)   => -1
    case (h1 :: t1, h2 :: t2) =>
      Order[A].compare(h1, h2) match 
        case 0 => compare(t1, t2)
        case x => x

case class Hand(hist: List[Int], singles: List[Int], bid: Int)
given Order[Hand] = Order.by(h => (h.hist, h.singles))

def readHand(input: String) = input.split(' ') match
  case Array(cards, bid) =>
    val scoredCards = cards.map {
      case ch if ch.isDigit => ch.asDigit
      case 'T'              => 10
      case 'J'              => 11
      case 'Q'              => 12
      case 'K'              => 13
      case 'A'              => 14
    }.toList
    Hand(scoredCards.foldMap(k => Map(k -> 1)).values.toList.sorted.reverse, scoredCards, bid.toInt)

def readHand2(input: String) = input.split(' ') match
  case Array(cards, bid) =>
    val scoredCards = cards.map {
      case ch if ch.isDigit => ch.asDigit
      case 'T'              => 10
      case 'J'              => 0
      case 'Q'              => 12
      case 'K'              => 13
      case 'A'              => 14
    }.toList
    val m = scoredCards.foldMap(k => Map(k -> 1))
    val jokers = m.getOrElse(0, 0)
    val jokered = m.toList.filterNot(_._1 == 0).sortBy(-_._2) match {
      case Nil => List(0 -> jokers)
      case (x -> count) :: tail => (x -> (jokers + count)) :: tail
    }
    Hand(jokered.map(_._2), scoredCards, bid.toInt)


@main def part1 =
  val hands = getLines.map(readHand).toList
  val inOrder = hands.sortWith(Order[Hand].lt)
  val score = inOrder.zipWithIndex.foldMap{
    case (hand, index) => hand.bid.toLong * (index + 1)
  }
  println(score)
@main def part2 = 
  val hands = getLines.map(readHand2).toList
  val inOrder = hands.sortWith(Order[Hand].lt)
  val score = inOrder.zipWithIndex.foldMap{
    case (hand, index) => hand.bid.toLong * (index + 1)
  }
  println(score)
