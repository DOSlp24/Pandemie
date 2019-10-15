package models

trait PathogenInterface extends PandemieModelInterface {
  val name: String
  val infectivity: String
  val mobility: String
  val duration: String
  val lethality: String
}
