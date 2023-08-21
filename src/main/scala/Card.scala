import scala.util.Random

class Card(val suit: Int = 0, val number: Int = 1, val hidden : Boolean = true){
  def copy(suit : Int = suit, number : Int = number, hidden : Boolean = hidden) : Card = {
    new Card(suit, number, hidden)
  }

  def isHidden: Boolean = hidden

  def hide: Card = new Card(suit, number, true)

  def reveal: Card = new Card(suit, number, false)

  override def toString: String = {
    if(hidden) then
      "[???]"
    else{
      val suitSymbol = suit match {
        case 1 => "♠"
        case 2 => "♡"
        case 3 => "♣"
        case 4 => "♢"
        case _ => "?"
      }
      
      val numberStr = number match {
        case 1  => "A"
        case 11 => "J"
        case 12 => "Q"
        case 13 => "K"
        case _  => s"$number"
      }

      val pad = if(number != 10) " " else ""
      s"[$pad$numberStr$suitSymbol]"
    }
  }
}

object Card {
  def randomCard: Card = {
    val rand = new Random
    new Card(Math.floorMod(rand.nextInt, 4)+1, Math.floorMod(rand.nextInt, 13)+1, false)
  }
}
