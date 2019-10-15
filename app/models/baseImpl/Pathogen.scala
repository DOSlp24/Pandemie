package models.baseImpl

import models.PathogenInterface

case class Pathogen(name: String, infectivity: String, mobility: String, duration: String, lethality: String) extends PathogenInterface {
  override def prettyPrint(): String = {
    "|\t" + infectivity + "\t|\t" + mobility + "\t|\t" + duration + "\t|\t" + lethality + "\t|\t" + name + "\t|"
  }
}
