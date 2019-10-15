package models.baseImpl

import models.VaccineInterface

case class Vaccine(pathogen: Pathogen, status: String = "developing", progress: String = "0/6") extends VaccineInterface {
  override def completeResearch(): Vaccine = {
    copy(status = "available")
  }

  override def setProgress(prog: Int): Vaccine = {
    copy(progress = prog + "/6")
  }

  override def prettyPrint(): String = {
    "Vac #" + pathogen.name + "# => " + status + "\n"
  }
}
