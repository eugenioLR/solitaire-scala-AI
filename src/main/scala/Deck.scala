import scala.util.matching.Regex
import scala.io.StdIn.readLine
import scala.util.Random

class Deck(val cards: List[Card]) {
  def revealTop: Deck = Deck(cards.head.reveal :: cards.tail)

  def shuffle: Deck =
    val rand = new Random
    Deck(rand.shuffle(cards))

  def pop: (Card, Deck) = (cards.head, Deck(cards.tail))

  def take(n: Int): (Deck, Deck) = (Deck(cards.take(n)), Deck(cards.drop(n)))

  def rotate: Deck = Deck(cards.tail :+ cards.head)

  override def toString: String =
    val bottomDeck = cards.tail.map(_ => "[").mkString("")
    bottomDeck + cards.head.toString
}

object Deck {
  def completeDeck: Deck =
    var cards: List[Card] = 
      (1 to 12).flatMap(
        s => 
          (1 to 4).map(
            n => 
              Card(n, s, true)
            )
          ).toList 
    new Deck(cards)
}
