import scala.util.matching.Regex
import scala.io.StdIn.readLine
import scala.util.Random

class Deck(val cards: List[Card] = Nil) {
  def revealTop: Deck = {
    if cards.isEmpty then
      Deck(Nil)
    else
      Deck(cards.head.reveal :: cards.tail)
  }
  
  def hideTop: Deck = {
    if cards.isEmpty then
      Deck(Nil)
    else
      Deck(cards.head.hide :: cards.tail)
  }

  def shuffle: Deck = {
    val rand = new Random
    Deck(rand.shuffle(cards))
  }

  def isEmpty: Boolean = cards.isEmpty

  def size: Int = cards.size

  def head: Card = cards.head

  def tail: Deck = Deck(cards.tail)

  def read(n: Int) = cards(n)

  def pop: (Card, Deck) = (cards.head, Deck(cards.tail).revealTop)

  def take(n: Int): (Deck, Deck) = {
    val remainingDeck = Deck(cards.drop(n))
    val remainingDeckRevealed = if remainingDeck.isEmpty then remainingDeck else remainingDeck.revealTop
    (Deck(cards.take(n)), remainingDeckRevealed)
  }

  def rotateLeft: Deck = Deck(cards.drop(1) ++ cards.take(1))
  
  def rotateRight: Deck = Deck(cards.takeRight(1) ++ cards.dropRight(1))

  def insert(c: Card, hidePrev: Boolean = false): Deck = {
    val prevCards = 
      if cards.isEmpty then{
        Nil
      } else {
        (if hidePrev then cards.head.hide else cards.head) :: cards.tail
      }
    Deck(c :: prevCards)
  }

  def merge(d: Deck): Deck = Deck(d.cards ++ cards)

  override def toString: String = {
    if cards.isEmpty then
      "[]"
    else {
      val bottomDeck = cards.tail.reverse.map(c => if c.isHidden then "[" else c.toString.dropRight(1)).mkString("")
      bottomDeck + cards.head.toString
    }
  }
}

object Deck {
  def completeDeck: Deck = {
    var cards: List[Card] = (1 to 4).flatMap(n => (1 to 13).map(Card(n, _, true))).toList 
    new Deck(cards)
  }
}
