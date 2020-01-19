package services

import java.io._

import models.GameInterface
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}

case class StatisticLoggingService() {
  def log(game: GameInterface, decider: String): Unit = {
    logWinrate(game, decider)
    //logInfectedCities(game)
    //logEvents(game)
    //logPathogens(game)
  }

  def logInfectedCities(game: GameInterface): Unit = {
    if (game.round == 1) {
      val file = new File("./public/statistics/InfectedCities.txt")
      val br = new BufferedReader(new FileReader(file))
      val index = br.lines().filter(s => s.contains("#=>")).count()
      br.close()
      val fw = new FileWriter(file, true)
      fw.write(index + " #=>\n")
      game.pathogens.foreach(p => fw.write("\t\tPathogen *" + p.name + "* \t\tInfected Cities: " + CityCalculator().numberOfInfectedCities(game, p) + "\n"))
      fw.close()
    }
  }

  def logWinrate(game: GameInterface, decider: String): Unit = {
    if (game.outcome != "pending") {
      val file = new File("./public/statistics/Winrate.json")
      val stream = new FileInputStream(file)
      val js = try {
        Json.parse(stream)
      } finally {
        stream.close()
      }
      js match {
        case jsObj: JsObject =>
          if (jsObj.keys.contains(decider)) {
            val jsonData = jsObj \ decider
            val winrate = (jsonData \ "winrate").as[Double]
            val wins = (jsonData \ "wins").as[Int]
            val wRounds = (jsonData \ "wRounds").as[Int]
            val wRoundsTotal = (jsonData \ "wRoundsTotal").as[Int]
            val looserate = (jsonData \ "looserate").as[Double]
            val looses = (jsonData \ "looses").as[Int]
            val lRounds = (jsonData \ "lRounds").as[Int]
            val lRoundsTotal = (jsonData \ "lRoundsTotal").as[Int]
            val averageRounds = (jsonData \ "averageRounds").as[Int]
            val roundsTotal = (jsonData \ "roundsTotal").as[Int]
            val runsTotal = (jsonData \ "runsTotal").as[Int]
            val updatedJson = game.outcome match {
              case "win" => Json.obj(
                "winrate" -> ((wins + 1).asInstanceOf[Double] / (runsTotal + 1).asInstanceOf[Double]),
                "wins" -> (wins + 1),
                "wRounds" -> ((wRoundsTotal + game.round) / (wins + 1)),
                "wRoundsTotal" -> (wRoundsTotal + game.round),
                "looserate" -> looserate,
                "looses" -> looses,
                "lRounds" -> lRounds,
                "lRoundsTotal" -> lRoundsTotal,
                "roundsTotal" -> (roundsTotal + game.round),
                "averageRounds" -> ((roundsTotal + game.round) / (runsTotal + 1)),
                "runsTotal" -> (runsTotal + 1)
              )
              case "loss" => Json.obj(
                "winrate" -> winrate,
                "wins" -> wins,
                "wRounds" -> wRounds,
                "wRoundsTotal" -> wRoundsTotal,
                "looserate" -> ((looses + 1).asInstanceOf[Double] / (runsTotal + 1).asInstanceOf[Double]),
                "looses" -> (looses + 1),
                "lRounds" -> ((lRoundsTotal + game.round) / (looses + 1)),
                "lRoundsTotal" -> (lRoundsTotal + game.round),
                "roundsTotal" -> (roundsTotal + game.round),
                "averageRounds" -> ((roundsTotal + game.round) / (runsTotal + 1)),
                "runsTotal" -> (runsTotal + 1)
              )
            }
            val fullJson = jsObj ++ Json.obj(decider -> updatedJson)
            val fw = new FileWriter(file)
            fw.write(Json.stringify(fullJson))
            fw.close()
          } else {
            val updatedJson = game.outcome match {
              case "win" => Json.obj(
                "winrate" -> 1,
                "wins" -> 1,
                "wRounds" -> game.round,
                "wRoundsTotal" -> game.round,
                "looserate" -> 0,
                "looses" -> 0,
                "lRounds" -> 0,
                "lRoundsTotal" -> 0,
                "roundsTotal" -> game.round,
                "averageRounds" -> game.round,
                "runsTotal" -> 1
              )
              case "loss" => Json.obj(
                "winrate" -> 0,
                "wins" -> 0,
                "wRounds" -> 0,
                "wRoundsTotal" -> 0,
                "looserate" -> 1,
                "looses" -> 1,
                "lRounds" -> game.round,
                "lRoundsTotal" -> game.round,
                "roundsTotal" -> game.round,
                "averageRounds" -> game.round,
                "runsTotal" -> 1
              )
            }
            val fullJson = jsObj ++ Json.obj(decider -> updatedJson)
            val fw = new FileWriter(file)
            fw.write(Json.stringify(fullJson))
            fw.close()
          }
      }
    }
  }

  def logEvents(game: GameInterface): Unit = {
    val file = new File("./public/statistics/EventCollection.json")
    val stream = new FileInputStream(file)
    val js = try {
      Json.parse(stream).as[Map[String, JsValue]]
    } finally {
      stream.close()
    }
    val events: Map[String, JsValue] = game.events match {
      case a: JsArray => a.value.map(js => Map[String, JsValue]("Game Event: " + (js \ "type").as[String] -> js)).reduce((a, b) => a ++ b)
    }

    val cityEvents: Map[String, JsValue] = game.cities.filter(c => c._2.events != null).map(c => c._2.events) match {
      case a: JsArray => a.value.map(js => Map[String, JsValue]("City Event: " + (js \ "type").as[String] -> js)).reduce((a, b) => a ++ b)
      case b: List[Object] =>
        b.toVector.map(e => Map("City Event: " + e.apply(0).apply("type").as[String] -> e)).distinct.reduce((a, b) => a ++ b)
    }

    val allEvents = events ++ cityEvents

    val updatedJs = new JsObject(allEvents ++ js)

    val fw = new FileWriter(file)
    fw.write(Json.stringify(updatedJs))
    fw.close()
  }

  def logPathogens(game: GameInterface): Unit = {
    val file = new File("./public/statistics/PathogenList.json")
    val stream = new FileInputStream(file)
    val js = try {
      Json.parse(stream).as[Map[String, JsValue]]
    } finally {
      stream.close()
    }
    val pats: Map[String, JsValue] = game.pathogens.map(pat => Map[String, JsValue](pat.name -> Json.obj(
      "infectivity" -> pat.infectivity,
      "mobility" -> pat.mobility,
      "duration" -> pat.duration,
      "lethality" -> pat.lethality
    ))).reduce((a, b) => a ++ b)

    val updatedJs = new JsObject(pats ++ js)

    val fw = new FileWriter(file)
    fw.write(Json.stringify(updatedJs))
    fw.close()
  }

  def allKeys(json: JsValue): Vector[String] = json match {
    case o: JsObject => o.keys.toVector
    case _ => Vector()
  }
}
