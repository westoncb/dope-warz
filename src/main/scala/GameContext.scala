case class Action(name: String, execute: GameState => Either[GameContext, String])

case object Action {
    def create(name: String, gameContext: GameContext): Action = {
        Action(name, gameState => Left(gameContext))
    }

    def create(name: String, message: String): Action = {
        Action(name, gameState => Right(message))
    }
}

sealed trait GameContext {
    def message: String
    def actionPrompt: String
    def actions: Seq[Action]
}

final case class RegionContext(gameState: GameState) extends GameContext {
    var regionName = gameState.region.name

    override def message: String = s"Welcome to ${regionName + ": " + gameState.player}"

    override def actionPrompt: String = "Where would you like to go, dude?"

    override def actions: Seq[Action] = {
        List(Action.create("Bank", BankContext(gameState)),
            Action.create("Store", StoreContext(gameState)),
            Action.create("Travel", TravelContext(gameState)),
            Action.create("Loan Shark", LoanSharkContext(gameState)))
    }
}

final case class BankContext(gameState: GameState) extends GameContext {
    override def message: String = "Welcome to the bank, dude."

    override def actionPrompt: String = "What would you like to do?"

    val depositAction: Action = Action("Deposit", gameState => {

        gameState.player.money -= 5
        gameState.player.bankMoney += 5
        Right("Deposited successfully")
    })

    val withdrawAction: Action = Action("Deposit", gameState => {

        gameState.player.bankMoney -= 5
        gameState.player.money += 5
        Right("Deposited successfully")
    })

    val exitAction = Action.create("Exit", RegionContext(gameState))

    override def actions: Seq[Action] = List(depositAction, withdrawAction, exitAction)
}

final case class StoreContext(gameState: GameState) extends GameContext {
    override def message: String = "Welcome to the store, bro."

    override def actionPrompt: String = "What would you like to buy?"

    val exitAction = Action.create("Exit", RegionContext(gameState))

    override def actions: Seq[Action] = List(exitAction)
}

final case class TravelContext(gameState: GameState) extends GameContext {
    override def message: String = "It's time to get outta here."

    override def actionPrompt: String = "Where to next?"

    val exitAction = Action.create("Exit", RegionContext(gameState))

    override def actions: Seq[Action] = List(exitAction)
}

final case class LoanSharkContext(gameState: GameState) extends GameContext {
    override def message: String = "You'd better have my money, bro."

    override def actionPrompt: String = "You gonna pay?"

    val payAction = Action("Pay", gameState => {
        gameState.player.money -= 5
        gameState.player.debt -= 5

        Right("Thanks buddy!")
    })
    val exitAction = Action.create("Exit", RegionContext(gameState))

    override def actions: Seq[Action] = List(payAction, exitAction)
}

final case class FightContext(gameState: GameState) extends GameContext {

    override def message: String = ???

    override def actionPrompt: String = ???

    val exitAction = Action.create("Exit", RegionContext(gameState))

    override def actions: Seq[Action] = List(exitAction)
}
