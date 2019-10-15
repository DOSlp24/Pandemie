package models

import play.api.libs.json.JsValue

trait CityInterface extends PandemieModelInterface {
  val name: String
  val lat: Float
  val long: Float
  val population: Int
  val connections: Vector[String]
  val economy: String
  val government: String
  val hygiene: String
  val awareness: String
  val pathogens: Map[PathogenInterface, Float]
  val vaccines: Vector[VaccineInterface]
  val events: JsValue

  def deployVaccine(v: Vector[VaccineInterface]): CityInterface
}
