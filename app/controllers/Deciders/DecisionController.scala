package controllers.Deciders

import models.{GameInterface, PathogenInterface}
import services.{CityCalculator, PathogenCalculator}

case class DecisionController(game: GameInterface) extends DecisionMaker {
  override def decide(): String = {
    println("DecisionController:")

    if (game.points == 0) {
      println("#### I choose to end the Round because you have no points left ####\n")
      return endRound()
    }

    PathogenCalculator().getPrioritisedAlphaPathogen(game) match {
      case Some(p) => developForAlphaPathogen(game, p)
      case None => deployMedication(game)
    }
  }

  def developForAlphaPathogen(game: GameInterface, pathogen: PathogenInterface): String = {
    // Alpha pathogen is a pathogen we neither have a Vaccine nor a Medicine for
    if (pathogen.infectivity == "++" || pathogen.infectivity == "+") {
      println("#=> " + pathogen.name + " is infective af!!\n")
      if (game.points >= 30) {
        println("#### I choose to develop a Medicine because you have enough Points. ####\n")
        return devMed(pathogen.name)
      } else {
        println("#=> You don't have enough Points to develop Medicine.\n")
        deployMedication(game)
      }
    } else {
      if (game.points >= 40) {
        println("#### I choose to develop a Vaccine because you have enough Points. ####\n")
        return devVaccine(pathogen.name)
      } else {
        println("#=> You don't have enough Points to develop Vaccine.\n")
        deployMedication(game)
      }
    }
  }

  def deployMedication(game: GameInterface): String = {
    PathogenCalculator().getPrioritisedNemesisPathogens(game) match {
      case Some(nemesisPathogens) =>
        nemesisPathogens.find(p =>
          PathogenCalculator().hasMedicineAvailable(game, p) ||
            PathogenCalculator().hasVaccineAvailable(game, p)) match {
          case Some(p) =>
            print("#=> I have found a Nemesis Pathogen with ")
            if (PathogenCalculator().hasMedicineAvailable(game, p)) {
              println("a Medicine available!\n")
              deployMedicine(game, p)
            } else {
              println("a Vaccine available!\n")
              deployVaccine(game, p)
            }
          case None =>
            println("#=> I could not find a Nemesis Pathogen with a Vaccine or Medicine available..\n")
            sideObjective(game)
        }
      case None =>
        println("#=> Calculating Nemesis Pathogens failed.")
        sideObjective(game)
    }
  }

  def deployMedicine(game: GameInterface, pathogen: PathogenInterface): String = {
    if (game.points < 10) {
      println("#### I wanted to deploy Medicine but you didn't have enough Points! ####\n")
      sideObjective(game)
    } else {
      CityCalculator().mostInfectedCity(game, pathogen) match {
        case Some(c) =>
          println("#### I choose to deploy Medicine against " + pathogen.name + " in " + c.name + " ####\n")
          depMed(pathogen.name, c.name)
        case None =>
          println("#=> Most Infected City failed..\n")
          sideObjective(game)
      }
    }
  }

  def deployVaccine(game: GameInterface, pathogen: PathogenInterface): String = {
    if (game.points < 5) {
      println("#### I wanted to deploy Vaccine but you didn't have enough Points! ####\n")
      sideObjective(game)
    } else {
      CityCalculator().leastInfectedCity(game, pathogen) match {
        case Some(c) =>
          println("#### I choose to deploy Vaccine against " + pathogen.name + " in " + c.name + " ####\n")
          depVaccine(pathogen.name, c.name)
        case None =>
          println("#=> Least Infected City failed..\n")
          sideObjective(game)
      }
    }
  }

  def sideObjective(game: GameInterface): String = {
    println("#=> I am in Side Objective Mode!")
    endRound()
  }

  def drownInDespair(): String = {
    println("#### I choose to end the Round because I don't know what else to do ####\n")
    endRound()
  }

  override def toString: String = {
    "PrimitiveDecider"
  }
}
