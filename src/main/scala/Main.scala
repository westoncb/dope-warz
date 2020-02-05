
object Main extends App {
    val game = Game(GameState())
    val gui = GUI(game, 1024, 768)

    gui.show
}