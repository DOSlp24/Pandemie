package models

trait MedicationInterface extends PandemieModelInterface {
  val pathogen: PathogenInterface
  val status: String
  val progress: String

  def completeResearch(): MedicationInterface

  def setProgress(prog: Int): MedicationInterface
}
