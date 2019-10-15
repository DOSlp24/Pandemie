package services

import models.{GameInterface, PathogenInterface}
import play.api.libs.json.{JsValue, Json, Writes}

case class JsonGenerator() {
  implicit val writePathogen: Writes[PathogenInterface] = new Writes[PathogenInterface] {
    override def writes(pathogen: PathogenInterface): JsValue = {
      Json.obj(
        "name" -> pathogen.name,
        "infectivity" -> pathogen.infectivity,
        "mobility" -> pathogen.mobility,
        "duration" -> pathogen.duration,
        "lethality" -> pathogen.lethality
      )
    }
  }

  implicit val writeGameViz: Writes[GameInterface] = new Writes[GameInterface] {
    override def writes(game: GameInterface): JsValue = {
      Json.obj(
        "round" -> game.round,
        "population" -> CityCalculator().getTotalPopulation(game),
        "pathogens" -> game.pathogens.map(p =>
          Json.obj(
            "pathogen" -> Json.toJson(p),
            "totalInfected" -> PathogenCalculator().getTotalInfectedDudes(game, p),
            "infectedCities" -> CityCalculator().infectedCities(game, p).size
          ))
      )
    }
  }

  def generateGameVizJson(game: GameInterface): JsValue = {
    Json.toJson(game)
  }
}
