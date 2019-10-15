package models.baseImpl

import models.MedicineInterface

case class Medicine(pathogen: Pathogen, status: String = "developing", progress: String = "0/3") extends MedicineInterface {
  override def completeResearch(): Medicine = {
    copy(status = "available")
  }

  override def setProgress(prog: Int): Medicine = {
    copy(progress = prog + "/3")
  }

  override def prettyPrint(): String = {
    "Med #" + pathogen.name + "# => " + status + "\n"
  }
}
