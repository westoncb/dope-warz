import java.util.GregorianCalendar

case class Player(maxHealth: Int = 100, money: Int = 2000, bankMoney: Int = 0, debt: Int = 10000) {
    var health = maxHealth
    val drugs = Map(Speed -> 0, Acid -> 0, Ludes -> 0, Cocaine -> 0, Heroin -> 0)
}

case class GameState(player: Player = Player(), turnsTaken: Int = 0, region: Region = Manhattan) {
    def date = new GregorianCalendar(1983, 11, 3 + turnsTaken).getTime
    var activeContext: GameContext = MarketContext(this)
}

case class Game(state: GameState = GameState()) {
    val contextMap = Map()
    val regionMap = Map()
}