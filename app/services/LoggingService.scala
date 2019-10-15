package services

import java.io.{BufferedReader, File, FileReader, FileWriter}

import models.GameInterface

case class LoggingService() {
  val file = new File("./logging/Logfile.txt")

  def logGameState(game: GameInterface): Unit = {
    if (game.round == 1 && game.points == 40) {
      // Delete File Content
      val fw = new FileWriter(file)
      fw.write(game.prettyPrint())
      fw.write("\n\n")
      fw.close()
    } else {
      val fw = new FileWriter(file, true)
      fw.write(game.prettyPrint())
      fw.write("\n\n")
      fw.close()
    }
  }

  def logString(str: String): Unit = {
    val fw = new FileWriter(file, true)
    fw.write(str)
    fw.close()
  }
}
