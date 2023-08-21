import scala.util.matching.Regex
import scala.io.StdIn.readLine

object Game {
  def showInfo() : Unit = {
    println("r                : rotate")
    println("dt [n]           : draw pile to table pile n")
    println("df [n]           : draw pile to final pile n")
    println("tt [n], [m], [a] : take a cards from table pile n to table pile m")
    println("tf [n], [m]      : table pile n to final pile m")
    println("e                : end the game")
    print("input: ")
  }

  def inputPileN(inputStr : String) : List[Int] = {
    val numberPattern = new Regex("[0-9]+")
    numberPattern.findAllIn(inputStr).toList.map(_.toInt)
  }

  def validInput(state: GameState, inputStr : String) : Boolean = {
    val orderPattern = new Regex("^(r|e|dt|df|tt|tf)")

    if (orderPattern findFirstIn inputStr) != None then {
      val str = orderPattern findFirstIn inputStr
      val args = inputPileN(inputStr)
      str.get match {
        case "r" => true
        case "e" => true
        case "dt" => args.size == 1 && args(0) >= 0 && args(0) <= 7
                     && state.canDrawToTable(args(0))
        case "df" => args.size == 1 && args(0) >= 0 && args(0) <= 4
                     && state.canDrawToFinal(args(0))
        case "tt" => args.size == 3 && args(0) >= 0 && args(0) <= 7 
                     && args(1) >= 0 && args(1) <= 7 
                     && args(2) >= 0
                     && state.canTableToTable(args(0), args(1), args(2))
        case "tf" => args.size == 2 && args(0) >= 0 && args(0) <= 7
                     && args(1) >= 0 && args(1) <= 4
                     && state.canTableToFinal(args(0), args(1))
        case _ => false
      }
    } else {
      false
    }
  }

  def doActionStr(state: GameState, inputStr: String): GameState = {
    val orderPattern = new Regex("^(r|exit|dt|df|tt|tf)")

    if (orderPattern findFirstIn inputStr) != None then {
      val str = orderPattern findFirstIn inputStr
      val args = inputPileN(inputStr)
      str.get match {
        case "r" => state.rotate
        case "dt" => state.drawToTable(args(0))
        case "df" => state.drawToFinal(args(0))
        case "tt" => state.tableToTable(args(0), args(1), args(2))
        case "tf" => state.tableToFinal(args(0), args(1))
        case _ => state
      }
    } else {
      state
    }
  }

  def play(state: GameState = GameState.init, info: String = ""): Unit = {
    println(state)

    println("------------------------------------------------------------------")
    println(info)
    showInfo()

    val line = readLine().trim
    
    println()
    println(line)
    println(validInput(state, line))

    if validInput(state, line) then {
      if line != "e" then
        play(doActionStr(state, line), "Successful move.")
      end if
    } else {
      play(state, "Cannot perform move.")
    }
  }
}
