package controllers.Deciders

import models.GameInterface
import services.{CityCalculator, StatisticLoggingService}

case class StatisticDecisionController(game: GameInterface) extends DecisionMaker {
  override def decide(): String = {
    //logInfectedCities()
    testPathogenLifetime("Admiral Trips")
    //endRound()
  }

  def testPathogenLifetime(pat: String): String = {
    val pathogen = game.pathogens.filter(p => p.name == pat) match {
      case p if p.length == 1 =>
        p.apply(0)
      case _ => null
    }
    if (pathogen != null && game.points >= 40 && game.round == 1) {
      putUnderQuarantine(CityCalculator().mostInfectedCity(game, pathogen).get.name, 2)
    } else {
      endRound()
    }
  }

  def logInfectedCities(): Unit = {
    StatisticLoggingService().logInfectedCities(game)
  }

  override def toString: String = {
    "StatisticDecider"
  }
}
