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

    override def message: String = s"Welcome to ${regionName}"

    override def actionPrompt: String = "Where would you like to go, dude?"

    override def actions: Seq[Action] = {

        List(
            Action.create("Market", MarketContext(gameState)),
            Action.create("Bank", BankContext(gameState)),
            Action.create("Gun Store", StoreContext(gameState)),
            Action.create("Travel", TravelContext(gameState)),
            Action.create("Loan Shark", LoanSharkContext(gameState))
        )
    }
}

final case class BankContext(gameState: GameState) extends GameContext {
    override def message: String = "Welcome to the bank, dude."

    override def actionPrompt: String = "What would you like to do?"

    val depositAction: Action = Action("Deposit", gameState => {

//        gameState.player.money -= 5
//        gameState.player.bankMoney += 5
        Right("Deposited successfully")
    })

    val withdrawAction: Action = Action("Withdraw", gameState => {

//        gameState.player.bankMoney -= 5
//        gameState.player.money += 5
        Right("Deposited successfully")
    })

    val exitAction = Action.create("Leave", RegionContext(gameState))

    override def actions: Seq[Action] = List(depositAction, withdrawAction, exitAction)
}

final case class StoreContext(gameState: GameState) extends GameContext {
    override def message: String = "Welcome to the store, bro."

    override def actionPrompt: String = "What would you like to buy?"

    val exitAction = Action.create("Leave", RegionContext(gameState))

    override def actions: Seq[Action] = List(exitAction)
}

final case class TravelContext(gameState: GameState) extends GameContext {
    override def message: String = "It's time to get outta here."

    override def actionPrompt: String = "Where to next?"

    val actionList = List(
        Action("The Bronx", gameState => {
            gameState changeRegion TheBronx
            Left(RegionContext(gameState))
        }),
        Action("The Ghetto", gameState => {
            gameState changeRegion TheGhetto
            Left(RegionContext(gameState))
        }),
        Action("Central Park", gameState => {
            gameState changeRegion CentralPark
            Left(RegionContext(gameState))
        }),
        Action("Manhattan", gameState => {
            gameState changeRegion Manhattan
            Left(RegionContext(gameState))
        }),
        Action("Coney Island", gameState => {
            gameState changeRegion ConeyIsland
            Left(RegionContext(gameState))
        }),
        Action("Brooklyn", gameState => {
            gameState changeRegion Brooklyn
            Left(RegionContext(gameState))
        }),
        Action.create("Go Back", RegionContext(gameState))
    )

    override def actions: Seq[Action] = actionList.filter(action => !action.name.eq(gameState.region.name))
}

final case class LoanSharkContext(gameState: GameState) extends GameContext {
    override def message: String = "You'd better have my money, bro."

    override def actionPrompt: String = "You gonna pay?"

    val payAction = Action("Pay", gameState => {
//        gameState.player.money -= 5
//        gameState.player.debt -= 5

        Right("Thanks buddy!")
    })
    val exitAction = Action.create("Leave", RegionContext(gameState))

    override def actions: Seq[Action] = List(payAction, exitAction)
}

final case class FightContext(gameState: GameState) extends GameContext {

    override def message: String = ???

    override def actionPrompt: String = ???

    val exitAction = Action.create("Run", RegionContext(gameState))

    override def actions: Seq[Action] = List(exitAction)
}

final case class StartContext(gameState: GameState) extends GameContext {

    override def message: String = s"You owe a loan shark ${"$" + gameState.player.debt}.\nAnd you've got 20 days to repay it."

    override def actionPrompt: String = "Ready to start?"

    val beginAction = Action.create("Yes", RegionContext(gameState))
    val exitAction = Action("No...", gameState => {
        System.exit(0)
        Right("")
    })

    override def actions: Seq[Action] = List(beginAction, exitAction)
}

final case class MarketContext(gameState: GameState) extends GameContext {

    override def message: String = s"This looks like a good spot to do business."

    sealed trait State
    case object Initial extends State
    case object Buying extends State
    case object Selling extends State

    var currentState: State = Initial

    val buyAction = Action("Buy", gameState => {
        currentState = Buying
        Right("")
    })
    val sellAction = Action("Sell", gameState => {
        currentState = Selling
        Right("")
    })
    val goBackAction = Action("Go Back", gameState => {
        currentState = Initial
        Right("")
    })
    val exitAction = Action.create("Leave", RegionContext(gameState))

    override def actionPrompt: String = {
        currentState match {
            case Initial => "Whatchya wanna do?"
            case Buying => "Whatchya wanna buy?"
            case Selling => "Whatchya wanna sell?"
        }
    }

    override def actions: Seq[Action] = {
        currentState match {
            case Initial => List(buyAction, sellAction, exitAction)
            case Buying => List(goBackAction)
            case Selling => List(goBackAction)
        }
    }
}
