@main def hello: Unit =
  val card1 = Card(1, 1, false)
  val card2 = Card(1, 12, false)
  println(card1)
  println(card2)
  println(Card.randomCard)
  println(Deck.completeDeck.revealTop)
  println(Deck.completeDeck.shuffle.revealTop)
