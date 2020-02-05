import java.util.{Date, GregorianCalendar}
import java.text.SimpleDateFormat

import javax.swing.JOptionPane

case class Player(maxHealth: Int = 100, money: Int = 2000, bankMoney: Int = 0, debt: Int = 10000) {
    var health = maxHealth
    val drugs = Map(Speed -> 0, Acid -> 0, Ludes -> 0, Cocaine -> 0, Heroin -> 0)
}

case class GameState(player: Player = Player()) {
    var turnsTaken = 0
    var region: Region = Manhattan
    def date: String = {
        val cal = new GregorianCalendar(1983, 11, 3 + turnsTaken)
        val fmt = new SimpleDateFormat("dd-MMM-yyyy")
        fmt.setCalendar(cal)
        fmt.format(cal.getTime)
    }
    var activeContext: GameContext = StartContext(this)

    def changeRegion(newRegion: Region): Unit = {
        region = newRegion
        turnsTaken += 1
    }
}

case class Game(state: GameState = GameState()) {
    val contextMap = Map()
    val regionMap = Map()

    def step(action: Action): Unit = {
        action.execute(state) match {
            case Left(context) => state.activeContext = context
            case Right("") =>
            case Right(message) => JOptionPane.showMessageDialog(null, message)
        }
    }
}