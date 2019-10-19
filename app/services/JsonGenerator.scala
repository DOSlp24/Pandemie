package services

import models.{CityInterface, GameInterface, PathogenInterface}
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

  implicit val writeCity: Writes[CityInterface] = new Writes[CityInterface] {
    override def writes(city: CityInterface): JsValue = {
      val pathogens = city.pathogens match {
        case null => Vector()
        case c => c.keys
      }
      Json.obj(
        "name" -> city.name,
        "latitude" -> city.lat,
        "longitude" -> city.long,
        "connections" -> city.connections,
        "hygiene" -> city.hygiene,
        "awareness" -> city.awareness,
        "economy" -> city.economy,
        "government" -> city.government,
        "population" -> city.population,
        "pathogens" -> pathogens
      )
    }
  }

  implicit val writeGameViz: Writes[GameInterface] = new Writes[GameInterface] {
    override def writes(game: GameInterface): JsValue = {
      Json.obj(
        "round" -> game.round,
        "points" -> game.points,
        "population" -> CityCalculator().getTotalPopulation(game),
        "notInfected" -> (CityCalculator().getTotalPopulation(game) - game.pathogens.map(p => PathogenCalculator().getTotalInfectedDudes(game, p)).sum),
        "pathogens" -> game.pathogens.map(p =>
          Json.obj(
            "pathogen" -> Json.toJson(p),
            "totalInfected" -> PathogenCalculator().getTotalInfectedDudes(game, p),
            "numberInfectedCities" -> CityCalculator().infectedCities(game, p).size,
            "infectedCities" -> CityCalculator().infectedCities(game, p)
          ))
        ,
        "cities" -> game.cities.values
      )
    }
  }

  def generateGameVizJson(game: GameInterface): JsValue = {
    Json.toJson(game)
  }
}
