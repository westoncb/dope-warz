import java.util.{GregorianCalendar}
import java.text.SimpleDateFormat
import scala.collection.mutable.LinkedHashMap

import javax.swing.JOptionPane

case class Player(maxHealth: Int = 100) {
    var health = maxHealth
    var money: Int = 2000
    var bankMoney: Int = 0
    var debt: Int = 10000
    var drugs: LinkedHashMap[Drug, Int] = LinkedHashMap(Speed -> 0, Acid -> 0, Ludes -> 0, Cocaine -> 0, Heroin -> 0)
}

case class GameState(player: Player = Player(), turnsAllowed: Int = 14) {
    var turnsTaken = 0
    def turnsRemaining = turnsAllowed - turnsTaken
    var region: Region = Manhattan
    var drugPrices: Map[Drug, Float] = Map()
    var lockCursor = false // a hack to not reset the cursor position on specific screens (e.g. buy/sell screens)

    def date: String = {
        val cal = new GregorianCalendar(1983, 11, 3 + turnsTaken)
        val fmt = new SimpleDateFormat("dd-MMM-yyyy")
        fmt.setCalendar(cal)
        fmt.format(cal.getTime)
    }

    var activeContext: GameContext = InitialContext(this)
}

case class Game(state: GameState = GameState()) {

    def step(action: Action): Unit = {
        action match {
            case RegionChangeAction(n, e, region) => changeRegion(region)
            case _ =>
        }

        action.execute(state) match {
            case Left(context) => {
                state.activeContext = context
            }
            case Right("") =>
            case Right(message) => JOptionPane.showMessageDialog(null, message)
        }
    }

    def changeRegion(newRegion: Region): Unit = {
        state.region = newRegion
        state.turnsTaken += 1

        if (state.turnsRemaining == 0 && state.player.debt > 0) {
            JOptionPane.showMessageDialog(null, "You remembered too late you were supposed to pay before today.\nYou'll make an attempt to leave New York in a hurry—but it's no use.")
            System.exit(0)
        }

        state.drugPrices = generateDrugPrices(state.region)
    }

    def generateDrugPrices(region: Region): Map[Drug, Float] = {
        Map(
            Speed -> generatePrice(10, 100, region.drugPriceBiases(Speed)),
            Acid -> generatePrice(50, 300, region.drugPriceBiases(Acid)),
            Ludes -> generatePrice(2, 50, region.drugPriceBiases(Ludes)),
            Cocaine -> generatePrice(100, 600, region.drugPriceBiases(Cocaine)),
            Heroin -> generatePrice(20, 200, region.drugPriceBiases(Heroin)),
        )
    }

    def generatePrice(min: Int, max: Int, bias: Float): Int = (Math.random() * bias * (max - min) + min).toInt
}