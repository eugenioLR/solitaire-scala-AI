import scala.util.Random

class Card(val suit: Int = 0, val number: Int = 1, hidden : Boolean = true){
  def copy(suit : Int = this.suit, number : Int = this.number, hidden : Boolean = this.hidden) : Card = {
    new Card(suit, number, hidden)
  } 

  def hide: Card = new Card(this.suit, this.number, true)
  
  def reveal: Card = new Card(this.suit, this.number, false)

  override def toString: String ={
    if(this.hidden){
      "[???]"
    }else{
      val suit_symbol = suit match {
        case 1 => "♠"
        case 2 => "♡"
        case 3 => "♣"
        case 4 => "♢"
        case _ => "?"
      }
      val number_str = number match {
        case 1  => "A"
        case 11 => "J"
        case 12 => "Q"
        case 13 => "K"
        case _  => s"$number"
      }

      val pad = if(number != 10) " " else ""
      s"[$pad$number_str$suit_symbol]"
    }
  }
}

object Card {
  def randomCard: Card = 
    val rand = new Random
    new Card(Math.floorMod(rand.nextInt, 4)+1, Math.floorMod(rand.nextInt, 13)+1, false)
}
