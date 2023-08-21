class GameState(
  val drawPile: Deck = new Deck, 
  val finalPiles: List[Deck] = (1 to 4).map(_ => new Deck).toList,
  val tablePiles: List[Deck] = (1 to 7).map(_ => new Deck).toList) {

  def finished: Boolean = drawPile.isEmpty && tablePiles.forall(_.isEmpty)

  def fitness: Int = finalPiles.map(_.size).reduce(_+_)

  def rotate: GameState = GameState(drawPile.hideTop.rotateRight.revealTop, finalPiles, tablePiles)

  def canTableToFinal(pileN: Int, finalPileN: Int): Boolean = {
    if tablePiles(pileN).isEmpty then
      false
    else if finalPiles(finalPileN).isEmpty then
      tablePiles(pileN).head.number == 1
    else {
      tablePiles(pileN).head.number == finalPiles(finalPileN).head.number + 1 
      && tablePiles(pileN).head.suit == finalPiles(finalPileN).head.suit
    }
  }

  def tableToFinal(pileN: Int, finalPileN: Int): GameState = {
    val cardToMove = tablePiles(pileN).head
    val newTablePiles = tablePiles.updated(pileN, tablePiles(pileN).tail.revealTop)
    val newFinalPiles = finalPiles.updated(finalPileN, finalPiles(finalPileN).insert(cardToMove))
    GameState(drawPile, newFinalPiles, newTablePiles)
  }

  def canDrawToTable(pileN: Int): Boolean = {
    if drawPile.isEmpty then
      false
    else if tablePiles(pileN).isEmpty then
      drawPile.head.number == 13
    else {
      drawPile.head.number == tablePiles(pileN).head.number - 1 
      && drawPile.head.suit % 2 != tablePiles(pileN).head.suit % 2
    }
  }
  
  def drawToTable(pileN: Int): GameState = {
    val (cardToMove, new_drawPile) = drawPile.pop
    val newTablePiles = tablePiles.updated(pileN, tablePiles(pileN).insert(cardToMove))
    GameState(new_drawPile, finalPiles, newTablePiles)
  }

  def canTableToTable(pileFrom : Int, pileTo : Int, amount : Int) : Boolean = {
    val idx = amount - 1
    if tablePiles(pileFrom).size < amount then
      false
    else if tablePiles(pileTo).isEmpty then
      tablePiles(pileFrom).read(idx).number == 13 
    else {
      tablePiles(pileFrom).read(idx).number == tablePiles(pileTo).head.number - 1
      && tablePiles(pileFrom).read(idx).suit % 2 != tablePiles(pileTo).head.suit % 2
    }
  }
  
  def tableToTable(pileFrom: Int, pileTo: Int, amount: Int): GameState = {
    val (deckToMove, tablePileTaken) = tablePiles(pileFrom).take(amount)
    val tablePilesTaken = tablePiles.updated(pileFrom, tablePileTaken)
    val newTablePiles = tablePilesTaken.updated(pileTo, 
                                                    tablePilesTaken(pileTo).merge(deckToMove))
    GameState(drawPile, finalPiles, newTablePiles)
  }


  def canDrawToFinal(pileN : Int) : Boolean = {
    if drawPile.isEmpty then
      false
    else if finalPiles(pileN).isEmpty then
      drawPile.head.number == 1
    else 
      drawPile.head.number == finalPiles(pileN).head.number + 1 
      && drawPile.head.suit == finalPiles(pileN).head.suit
  }

  def drawToFinal(finalPileN: Int): GameState = {
    val (cardToMove, new_drawPile) = drawPile.pop
    val newFinalPiles = finalPiles.updated(finalPileN, finalPiles(finalPileN).insert(cardToMove, true))
    GameState(new_drawPile, newFinalPiles, tablePiles)
  }

  override def toString: String = {
      "Game State\n"
      + "\nDraw pile:\n"
      + drawPile.toString + "\n"
      + "\nFinal Piles:\n"
      + finalPiles.mkString("\n") + "\n" 
      + "\nTable Piles:\n"
      + tablePiles.mkString("\n")
  }
}

object GameState {
  def takePiles(deck: Deck, n: Int = 7): (List[Deck], Deck) = {
    if n < 1 then
      (Nil, deck)
    else {
      val (newDeck, remaining) = deck.take(n)
      val (decks, lastRemaining) = takePiles(remaining, n - 1)
      (newDeck :: decks, lastRemaining)
    }
  }

  def init: GameState = {
    val deck_full = Deck.completeDeck.shuffle
    val (piles, remaining) = takePiles(deck_full)
    val finalPiles = (1 to 4).map(_ => (new Deck).revealTop).toList
    new GameState(remaining.revealTop, finalPiles, piles.map(_.revealTop).toList)
  }
}
