sealed trait Action {
    def name: String
    def execute: GameState => Either[GameContext, String]
}

case class DefaultAction(name: String, execute: GameState => Either[GameContext, String]) extends Action

case object DefaultAction {
    def create(name: String, gameContext: GameContext): DefaultAction = {
        DefaultAction(name, gameState => Left(gameContext))
    }

    def create(name: String, message: String): DefaultAction = {
        DefaultAction(name, gameState => Right(message))
    }
}

case class RegionChangeAction(name: String, execute: GameState => Either[GameContext, String], nextRegion: Region) extends Action

sealed trait GameContext {
    def message: String
    def actionPrompt: String
    def actions: Seq[Action]
}

final case class RegionContext(gameState: GameState) extends GameContext {
    var region = gameState.region

    override def message: String = s"Welcome to ${region.name}"

    override def actionPrompt: String = "Where would you like to go, dude?"

    override def actions: Seq[DefaultAction] = {

        List(
            DefaultAction.create("Market", MarketContext(gameState)),
            DefaultAction.create("Travel", TravelContext(gameState)),
            DefaultAction.create("Bank", BankContext(gameState)),
            DefaultAction.create("Gun Store", StoreContext(gameState)),
            DefaultAction.create("Loan Shark", LoanSharkContext(gameState))
        )
    }
}

final case class BankContext(gameState: GameState) extends GameContext {
    override def message: String = "Welcome to the bank, dude."

    override def actionPrompt: String = "What would you like to do?"

    val depositAction: DefaultAction = DefaultAction("Deposit", gameState => {

        Right("Deposited successfully")
    })

    val withdrawAction: DefaultAction = DefaultAction("Withdraw", gameState => {

        Right("Withdrew successfully")
    })

    val exitAction = DefaultAction.create("Leave", RegionContext(gameState))

    override def actions: Seq[DefaultAction] = List(depositAction, withdrawAction, exitAction)
}

final case class StoreContext(gameState: GameState) extends GameContext {
    override def message: String = "Welcome to the store, bro."

    override def actionPrompt: String = "What would you like to buy?"

    val exitAction = DefaultAction.create("Leave", RegionContext(gameState))

    override def actions: Seq[DefaultAction] = List(exitAction)
}

final case class TravelContext(gameState: GameState) extends GameContext {
    override def message: String = "It's time to get outta here."

    override def actionPrompt: String = "Where to next?"

    val actionList = List(
        RegionChangeAction("The Bronx", gameState => {
            Left(RegionContext(gameState))
        }, TheBronx),
        RegionChangeAction("The Ghetto", gameState => {
            Left(RegionContext(gameState))
        }, TheGhetto),
        RegionChangeAction("Central Park", gameState => {
            Left(RegionContext(gameState))
        }, CentralPark),
        RegionChangeAction("Manhattan", gameState => {
            Left(RegionContext(gameState))
        }, Manhattan),
        RegionChangeAction("Coney Island", gameState => {
            Left(RegionContext(gameState))
        }, ConeyIsland),
        RegionChangeAction("Brooklyn", gameState => {
            Left(RegionContext(gameState))
        }, Brooklyn),
        DefaultAction.create("Go Back", RegionContext(gameState))
    )

    override def actions: Seq[Action] = actionList.filter(action => !action.name.eq(gameState.region.name))
}

final case class LoanSharkContext(gameState: GameState) extends GameContext {
    override def message: String = s"You'd better have my money, bro.\nYou still owe me ${"$" + gameState.player.debt}."

    override def actionPrompt: String = "You gonna pay?"

    val payAction = DefaultAction("Pay", gameState => {

        if (gameState.player.money >= gameState.player.debt) {
            gameState.player.money -= gameState.player.debt
            gameState.player.debt = 0
            Right("Thanks buddy!")
        } else {
            Right("You tryin' to screw me!?")
        }

    })
    val exitAction = DefaultAction.create("Leave", RegionContext(gameState))

    override def actions: Seq[Action] = List(payAction, exitAction)
}

final case class FightContext(gameState: GameState) extends GameContext {

    override def message: String = ???

    override def actionPrompt: String = ???

    val exitAction = DefaultAction.create("Run", RegionContext(gameState))

    override def actions: Seq[Action] = List(exitAction)
}

final case class InitialContext(gameState: GameState) extends GameContext {

    override def message: String = s"You owe a loan shark ${"$" + gameState.player.debt}.\nAnd you've got 20 days to repay it."

    override def actionPrompt: String = "Ready to start?"

    val beginAction = RegionChangeAction("Yes", gameState => Left(RegionContext(gameState)), Manhattan)
    val exitAction = DefaultAction("No...", gameState => {
        System.exit(0)
        Right("")
    })

    override def actions: Seq[Action] = List(beginAction, exitAction)
}

final case class MarketContext(gameState: GameState) extends GameContext {

    override def message: String = s"It's time to make some money."

    sealed trait State
    case object Initial extends State
    case object Buying extends State
    case object Selling extends State

    var currentState: State = Initial

    val buyAction = DefaultAction("Buy", gameState => {
        gameState.lockCursor = true
        currentState = Buying
        Right("")
    })
    val sellAction = DefaultAction("Sell", gameState => {
        gameState.lockCursor = true
        currentState = Selling
        Right("")
    })
    val goBackAction = DefaultAction("Go Back", gameState => {
        gameState.lockCursor = false
        currentState = Initial
        Right("")
    })
    val exitAction = DefaultAction.create("Leave", RegionContext(gameState))

    override def actionPrompt: String = {
        currentState match {
            case Initial => "Whatchya wanna do?"
            case Buying => "Whatchya wanna buy?"
            case Selling => "Whatchya wanna sell?"
        }
    }

    val prices = gameState.drugPrices

    val buy = (drug: Drug) => {
        var success = true

        if (gameState.player.money >= prices(drug)) {
            gameState.player.money -= prices(drug).toInt
            gameState.player.drugs(drug) = gameState.player.drugs(drug) + 1
        } else
            success = false

        success
    }

    val sell = (drug: Drug) => {
        var success = true

        if (gameState.player.drugs(drug) > 0) {
            gameState.player.drugs(drug) = gameState.player.drugs(drug) - 1
            gameState.player.money += gameState.drugPrices(drug).toInt
        } else
            success = false

        success
    }

    val buyActions = gameState.drugPrices.keys.map(drug => DefaultAction(drug.toString, gameState => {
        if (buy(drug)) Right("") else Right("You're broke punk—get outta here!")
    }))

    val sellActions = gameState.drugPrices.keys.map(drug => DefaultAction(drug.toString, gameState => {
        if (sell(drug)) Right("") else Right(s"You've got no more ${drug} to sell!")
    }))

    override def actions: Seq[Action] = {
        currentState match {
            case Initial => List(buyAction, sellAction, exitAction)
            case Buying => List() ++ buyActions ++ List(goBackAction)
            case Selling => List() ++ sellActions ++ List(goBackAction)
        }
    }
}
