package controllers.Deciders

import models.{GameInterface, PathogenInterface}
import services.{CityCalculator, LoggingService, PathogenCalculator}

case class FirstTryDecider() extends DecisionMaker {
  override def decide(game: GameInterface): String = {
    LoggingService().logString("#########################################################################################################\n")
    LoggingService().logString("########################################### First Try Decider ###########################################\n")
    LoggingService().logString("#########################################################################################################\n\n")

    if (game.round == 1) {
      PathogenCalculator().getPrioritisedAlphaPathogen(game) match {
        case Some(pat) =>
          LoggingService().logString("#=> First Round = Different Rules. Sort by Pathogen Value\n")
          isInfectiveOrSpread(game, pat)
        case None =>
          LoggingService().logString("#=> No Alpha Pathogen found")
          endRound()
      }
    } else {
      PathogenCalculator().getPrioritisedAlphaNemesisPathogens(game) match {
        case Some(alphaNemesisPats) =>
          val selectedPathogen: PathogenInterface = alphaNemesisPats.length match {
            case 1 => alphaNemesisPats.apply(0)
            case x if x > 1 => alphaNemesisPats.map(p => (PathogenCalculator().getTotalInfectedDudes(game, p), p)).reduce((a, b) => {
              if (a._1 >= b._1) {
                a
              } else {
                b
              }
            })._2
          }
          LoggingService().logString("#=> I have found and selected the Alpha Nemesis Pathogen " + selectedPathogen.name + "\n")
          isInfectiveOrSpread(game, selectedPathogen)
        case None =>
          LoggingService().logString("#=> No Nemesis Alpha Pathogen found.\n")
          nemesisPathogen(game)
      }
    }
  }

  def nemesisPathogen(game: GameInterface): String = {
    PathogenCalculator().getPrioritisedNemesisPathogens(game) match {
      case Some(nemesisPathogens) =>
        val maxInfectPathogen = nemesisPathogens.length match {
          case 1 => nemesisPathogens.apply(0)
          case x if x > 1 => nemesisPathogens.map(p => (PathogenCalculator().getTotalInfectedDudes(game, p), p)).reduce((a, b) => {
            if (a._1 >= b._1) {
              a
            } else {
              b
            }
          })._2
          case _ => null
        }
        LoggingService().logString("#=> Nemesis Pathogen " + maxInfectPathogen.name + " found\n")
        drugsAvailable(game, maxInfectPathogen)
      case None =>
        LoggingService().logString("!! No Nemesis Pathogen was found though.. Get ready to drown in despair !!")
        drownInDespair()
    }
  }

  def isAlpha(game: GameInterface, nemesisPathogens: Vector[PathogenInterface]): String = {
    val maxInfectPathogen = nemesisPathogens.length match {
      case 1 => nemesisPathogens.apply(0)
      case x if x > 1 => nemesisPathogens.map(p => (PathogenCalculator().getTotalInfectedDudes(game, p), p)).reduce((a, b) => {
        if (a._1 >= b._1) {
          a
        } else {
          b
        }
      })._2
      case _ => null
    }
    maxInfectPathogen match {
      case alpha if !PathogenCalculator().hasMedicine(game, alpha) && !PathogenCalculator().hasVaccine(game, alpha) =>
        LoggingService().logString("#=> Nemesis Pathogen is Alpha Pathogen. Selected Pathogen: " + alpha.name + "\n")
        isInfectiveOrSpread(game, alpha)
      case pathogen =>
        LoggingService().logString("#=> Nemesis Pathogen is not an Alpha Pathogen. Selected Pathogen: " + pathogen.name + "\n")
        drugsAvailable(game, pathogen)
    }
  }

  def isInfectiveOrSpread(game: GameInterface, pathogen: PathogenInterface): String = {
    if (pathogen.infectivity == "++" || pathogen.infectivity == "+" || pathogen.infectivity == "o" ||
      (PathogenCalculator().getTotalInfectedDudes(game, pathogen).toFloat / CityCalculator().getTotalPopulation(game).toFloat) >= 0.1) {
      LoggingService().logString("#=> " + pathogen.name + " is infective or spread af! We need a Medicine.\n")
      developMedicine(game, pathogen)
    } else {
      LoggingService().logString("#=> " + pathogen.name + " seems to be nothong special. I'll slap some Vaccine on it.\n")
      developVaccine(game, pathogen)
    }
  }

  def drugsAvailable(game: GameInterface, pathogen: PathogenInterface): String = {
    pathogen match {
      case medPat if PathogenCalculator().hasMedicineAvailable(game, medPat) =>
        LoggingService().logString("#=> Selected Pathogen has Medicine available.\n")
        medDeployable(game, medPat)
      case vacPat if PathogenCalculator().hasVaccineAvailable(game, vacPat) =>
        LoggingService().logString("#=> Selected Pathogen has Vaccine available.\n")
        vacDeployable(game, vacPat)
      case druglessPat =>
        LoggingService().logString("#=> Selected Pathogen has no Drugs available. I decide to move to Drugs in Development.\n")
        drugsInDev(game, druglessPat)
    }
  }

  def drugsInDev(game: GameInterface, pathogen: PathogenInterface): String = {
    pathogen match {
      case omega if PathogenCalculator().hasMedicine(game, omega) && PathogenCalculator().hasVaccine(game, omega) =>
        LoggingService().logString("#=> Selected Pathogen is Omega Pathogen\n")
        checkCityStandards(game, omega)
      case medPat if PathogenCalculator().hasMedicine(game, medPat) =>
        CityCalculator().mostInfectedCity(game, medPat) match {
          case Some(c) =>
            if (c.pathogens.apply(pathogen) <= 0.1) {
              LoggingService().logString("#=> Most Infected City is below 10%. Vaccine is needed.\n")
              developVaccine(game, medPat)
            } else {
              LoggingService().logString("#=> Moving to Side Objectives\n")
              checkCityStandards(game, medPat)
            }
          case None =>
            LoggingService().logString("!! No Most infected City was found though !!\n")
            drownInDespair()
        }
      case vacPat if PathogenCalculator().hasVaccine(game, vacPat) =>
        CityCalculator().infectedCities(game, pathogen).filter(c => !c.vaccines.exists(v => v.pathogen == pathogen)).find(c => c.pathogens.apply(pathogen) >= 0.6) match {
          case Some(c) =>
            LoggingService().logString("#=> Least Infected City is above 60%. Medicine is needed.\n")
            developMedicine(game, pathogen)
          case None =>
            LoggingService().logString("#=> I decide to move to Side Objective Mode\n")
            checkCityStandards(game, vacPat)
        }
    }
  }

  def checkCityStandards(game: GameInterface, pathogen: PathogenInterface): String = {
    LoggingService().logString("#=> I decided to check on the infected cities standards\n")
    if (CityCalculator().infectedCities(game, pathogen)
      .count(p => p.awareness == "--" || p.awareness == "-" || p.hygiene == "--" || p.hygiene == "-" || p.economy == "--" || p.government == "--") < 20) {
      LoggingService().logString("#=> I decide to move to Side Objectives.\n")
      sideObjective(game, pathogen)
    } else {
      LoggingService().logString("# Too much Cities.. Ending Round #\n")
      endRound()
    }
  }

  def developMedicine(game: GameInterface, pathogen: PathogenInterface): String = {
    if (game.points >= 20 && !PathogenCalculator().hasMedicine(game, pathogen)) {
      LoggingService().logString("# I decide to develop a Medicine for " + pathogen.name + " #\n")
      devMed(pathogen.name)
    } else {
      LoggingService().logString("# You don't have enough points to develop a Medicine or already has Med  ->  Eco #\n")
      endRound()
    }
  }

  def developVaccine(game: GameInterface, pathogen: PathogenInterface): String = {
    if (game.points >= 40 && !PathogenCalculator().hasVaccine(game, pathogen)) {
      LoggingService().logString("# I decide to develop a Vaccine for " + pathogen.name + " #\n")
      devVaccine(pathogen.name)
    } else {
      LoggingService().logString("# You don't have enough points to develop a Vaccine or already has Vac  ->  Eco #\n")
      endRound()
    }
  }

  def medDeployable(game: GameInterface, pathogen: PathogenInterface): String = {
    CityCalculator().mostInfectedCity(game, pathogen) match {
      case Some(c) =>
        if (c.pathogens.apply(pathogen) <= 0.1) {
          LoggingService().logString("#=> Most Infected City is below 10%. Vaccine is needed.\n")
          developVaccine(game, pathogen)
        } else {
          if (game.points >= 10) {
            LoggingService().logString("# I decide to deploy a " + pathogen.name + " Medicine in " + c.name + " #\n")
            depMed(pathogen.name, c.name)
          } else {
            LoggingService().logString("# You don't have enough Points to deploy Medicine  ->  Eco #\n")
            endRound()
          }
        }
      case None =>
        LoggingService().logString("!! No Most infected City was found though !!\n")
        drownInDespair()
    }
  }

  def vacDeployable(game: GameInterface, pathogen: PathogenInterface): String = {
    CityCalculator().infectedCities(game, pathogen).filter(c => !c.vaccines.exists(v => v.pathogen == pathogen)).find(c => c.pathogens.apply(pathogen) >= 0.6) match {
      case Some(c) =>
        LoggingService().logString("#=> Least Infected City is above 60%. Medicine is needed.\n")
        developMedicine(game, pathogen)
      case None =>
        if (game.points >= 5) {
          CityCalculator().leastInfectedCity(game, pathogen) match {
            case Some(lc) =>
              LoggingService().logString("# I decide to deploy a " + pathogen.name + " Vaccine in " + lc.name + " #\n")
              depVaccine(pathogen.name, lc.name)
            case None =>
              LoggingService().logString("# No Least Infected City found -> Eco #")
              endRound()
          }
        } else {
          LoggingService().logString("# You don't have enough Points to deploy Vaccine  ->  Eco #\n")
          endRound()
        }
    }
  }

  def sideObjective(game: GameInterface, pathogen: PathogenInterface): String = {
    if (game.points >= 3) {
      LoggingService().logString("#=> I am in Side Objective Mode\n")
      checkPolitics(game, pathogen)
    } else {
      LoggingService().logString("# You don't have enough Points to perform any Side Objective :/ #\n")
      endRound()
    }
  }

  def checkPolitics(game: GameInterface, pathogen: PathogenInterface): String = {
    CityCalculator().infectedCities(game, pathogen).find(c => c.government == "--") match {
      case Some(c) =>
        LoggingService().logString("# " + c.name + " has -- Government. I decide to call elections. #\n")
        callElections(c.name)
      case None =>
        LoggingService().logString("#=> All Cities have OKish Governments\n")
        checkEconomy(game, pathogen)
    }
  }

  def checkEconomy(game: GameInterface, pathogen: PathogenInterface): String = {
    CityCalculator().infectedCities(game, pathogen).find(c => c.economy == "--") match {
      case Some(c) =>
        LoggingService().logString("# " + c.name + " has -- Economy. I decide to exert Influence. #\n")
        exertInfluence(c.name)
      case None =>
        LoggingService().logString("#=> All Cities have a stable Economy.\n")
        checkHygiene(game, pathogen)
    }
  }

  def checkHygiene(game: GameInterface, pathogen: PathogenInterface): String = {
    CityCalculator().infectedCities(game, pathogen).find(c => c.hygiene == "--" || c.hygiene == "-") match {
      case Some(c) =>
        LoggingService().logString("# " + c.name + " has bad Hygiene. I decide to apply hygiene measures. #\n")
        applyHygienicMeasures(c.name)
      case None =>
        LoggingService().logString("#=> All Cities have have good hygiene standards\n")
        checkAwareness(game, pathogen)
    }
  }

  def checkAwareness(game: GameInterface, pathogen: PathogenInterface): String = {
    CityCalculator().infectedCities(game, pathogen).find(c => c.awareness == "--" || c.awareness == "-") match {
      case Some(c) =>
        LoggingService().logString("# " + c.name + " has almost no awareness. I decide to start a campaign. #\n")
        launchCampaign(c.name)
      case None =>
        LoggingService().logString("# Nothing to do though. Awareness is good too. #\n")
        endRound()
    }
  }

  def drownInDespair(): String = {
    LoggingService().logString("#### I choose to end the Round because I don't know what else to do =( ####\n")
    endRound()
  }
}
