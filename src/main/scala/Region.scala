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
