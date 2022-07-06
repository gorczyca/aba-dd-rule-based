package experiments.runner.actualFinalExperiments

object ExperimentsMode extends Enumeration {

  type ExperimentsModeType = Value
  val
    Normal,
    AbaStrategy,
    Approximation,
    Preferred,
    Grounded = Value
}
