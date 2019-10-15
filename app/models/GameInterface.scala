package models

import play.api.libs.json.JsValue

trait GameInterface extends PandemieModelInterface {
  val round: Int
  val points: Int
  val outcome: String
  val jsCityObject: JsValue
  val cityNames: Vector[String]
  val cities: Map[String, CityInterface]
  val events: JsValue
  val otherEvents: Vector[JsValue]
  val pathogens: Vector[PathogenInterface]
  val medicines: Vector[MedicineInterface]
  val vaccines: Vector[VaccineInterface]
}
