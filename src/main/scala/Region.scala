sealed trait Region {
    def name: String
    def drugPriceBiases: Map[Drug, Float]
}

final case object Manhattan extends Region {
    override def name: String = "Manhattan"
    override def drugPriceBiases: Map[Drug, Float] = {
        Map(Speed -> 1f,
            Acid -> 1f,
            Ludes -> 1f,
            Cocaine -> 1f,
            Heroin -> 1f)
    }
}

final case object TheBronx extends Region {
    override def name: String = "The Bronx"
    override def drugPriceBiases: Map[Drug, Float] = {
        Map(Speed -> 1f,
            Acid -> 1f,
            Ludes -> 1f,
            Cocaine -> 1f,
            Heroin -> 1f)
    }
}

final case object TheGhetto extends Region {
    override def name: String = "The Ghetto"
    override def drugPriceBiases: Map[Drug, Float] = {
        Map(Speed -> 1f,
            Acid -> 1f,
            Ludes -> 1f,
            Cocaine -> 1f,
            Heroin -> 1f)
    }
}

final case object ConeyIsland extends Region {
    override def name: String = "Coney Island"
    override def drugPriceBiases: Map[Drug, Float] = {
        Map(Speed -> 1f,
            Acid -> 1f,
            Ludes -> 1f,
            Cocaine -> 1f,
            Heroin -> 1f)
    }
}

final case object CentralPark extends Region {
    override def name: String = "Central Park"
    override def drugPriceBiases: Map[Drug, Float] = {
        Map(Speed -> 1f,
            Acid -> 1f,
            Ludes -> 1f,
            Cocaine -> 1f,
            Heroin -> 1f)
    }
}

final case object Brooklyn extends Region {
    override def name: String = "Brooklyn"
    override def drugPriceBiases: Map[Drug, Float] = {
        Map(Speed -> 1f,
            Acid -> 1f,
            Ludes -> 1f,
            Cocaine -> 1f,
            Heroin -> 1f)
    }
}