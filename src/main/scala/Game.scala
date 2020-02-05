import java.util.GregorianCalendar

case class GameState(player: Player = Player(), turnsTaken: Int = 0, region: Region) {
    def date = new GregorianCalendar(1983, 11, 3 + turnsTaken).getTime
}

case class Game(state: GameState = GameState()) {
    val contextMap = Map()
    val regionMap = Map()
}