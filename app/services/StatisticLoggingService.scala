package services

import java.io._

import models.GameInterface
import play.api.libs.json.Json

case class StatisticLoggingService() {
  def log(game: GameInterface): Unit = {
    logWinrate(game)
  }

  def logInfectedCities(game: GameInterface): Unit = {
    if (game.round == 1) {
      val file = new File("./statistics/InfectedCities.txt")
      val br = new BufferedReader(new FileReader(file))
      val index = br.lines().filter(s => s.contains("#=>")).count()
      br.close()
      val fw = new FileWriter(file, true)
      fw.write(index + " #=>\n")
      game.pathogens.foreach(p => fw.write("\t\tPathogen *" + p.name + "* \t\tInfected Cities: " + CityCalculator().numberOfInfectedCities(game, p) + "\n"))
      fw.close()
    }
  }

  def logWinrate(game: GameInterface): Unit = {
    if (game.outcome != "pending") {
      val file = new File("./statistics/Winrate.json")
      val stream = new FileInputStream(file)
      val js = try {
        Json.parse(stream)
      } finally {
        stream.close()
      }
      val wins = (js \ "win" \ "wins").as[Int]
      val looses = (js \ "loss" \ "looses").as[Int]
      val wRounds = (js \ "win" \ "rounds").as[Int]
      val lRounds = (js \ "loss" \ "rounds").as[Int]
      val total = (js \ "total").as[Int]
      val updatedJs = game.outcome match {
        case "win" => "{\"win\": {\"wins\": " + (wins + 1) + ", \"rounds\": " +
          (wRounds + game.round) + "}, \"loss\": {\"looses\": " + looses + ", \"rounds\": " +
          lRounds + "}, \"total\": " + (total + 1) + "}"
        case "loss" => "{\"win\": {\"wins\": " + wins + ", \"rounds\": " +
          wRounds + "}, \"loss\": {\"looses\": " + (looses + 1) + ", \"rounds\": " +
          (lRounds + game.round) + "}, \"total\": " + (total + 1) + "}"
      }
      val fw = new FileWriter(file)
      fw.write(updatedJs)
      fw.close()
    }
  }
}
