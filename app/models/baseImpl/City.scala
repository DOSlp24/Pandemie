package models.baseImpl

import models.{CityInterface, PathogenInterface, VaccineInterface}
import play.api.libs.json.JsValue

case class City(name: String, lat: Float, long: Float, population: Int, connections: Vector[String], economy: String, government: String, hygiene: String, awareness: String, pathogens: Map[PathogenInterface, Float], vaccines: Vector[VaccineInterface], events: JsValue) extends CityInterface {

  override def deployVaccine(v: Vector[VaccineInterface]): City = {
    copy(vaccines = vaccines ++ v)
  }

  override def prettyPrint(): String = {
    "\t**" + name + "**\n" +
      "\tEvents\n" +
      "\t\t" + events + "\n" +
      "\tpopulation: *" + population + "*"
  }
}
