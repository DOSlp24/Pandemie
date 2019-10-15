package services

import java.io.{BufferedReader, File, FileReader, FileWriter}

import models.GameInterface

case class LoggingService() {
  def logGameState(game: GameInterface): Unit = {
    val file = new File("./logging/Logfile.txt")
    val br = new BufferedReader(new FileReader(file))
    val fw = new FileWriter(file, true)
    fw.write(game.prettyPrint())
    fw.write("\n\n")
    fw.close()
  }

  def logString(str: String): Unit = {
    val file = new File("./logging/Logfile.txt")
    val br = new BufferedReader(new FileReader(file))
    val fw = new FileWriter(file, true)
    fw.write(str)
    fw.close()
  }
}
