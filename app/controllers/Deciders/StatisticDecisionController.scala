package controllers.Deciders

import models.GameInterface
import services.StatisticLoggingService

case class StatisticDecisionController() extends DecisionMaker {
  override def decide(game: GameInterface): String = {
    logInfectedCities(game)

    endRound()
  }

  def logInfectedCities(game: GameInterface): Unit = {
    StatisticLoggingService().logInfectedCities(game)
  }
}
