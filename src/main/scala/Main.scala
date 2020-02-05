
case class Player(maxHealth: Int = 100, money: Int = 2000, bankMoney: Int = 0, debt: Int = 10000) {
    var health = maxHealth
    val drugs = Map(Speed -> 0, Acid -> 0, Ludes -> 0, Cocaine -> 0, Heroin -> 0)
}

object Main extends App {
    println("this is a test: " + Game())

}