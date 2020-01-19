package controllers.Deciders

import models.{GameInterface, PathogenInterface}
import play.api.libs.json.{JsArray, JsValue}
import services.{CityCalculator, LoggingService, PathogenCalculator}

case class FirstTryDecider(game: GameInterface) extends DecisionMaker {
  override def decide(): String = {
    LoggingService().logString("#########################################################################################################\n")
    LoggingService().logString("########################################### First Try Decider ###########################################\n")
    LoggingService().logString("#########################################################################################################\n\n")

    PathogenCalculator().getPrioritisedNemesisPathogens(game) match {
      case Some(patogens) =>
        val nemesisPat = patogens.count(p => CityCalculator().infectedCities(game, p).size == 1) match {
          case 1 => patogens.filter(p => CityCalculator().infectedCities(game, p).size == 1).apply(0)
          case x if x > 1 => patogens.map(p => (PathogenCalculator().pathogenValue(p), p)).reduce((a, b) => {
            if (a._1 >= b._1) {
              a
            } else {
              b
            }
          })._2
          case _ => patogens.apply(0)
        }


        val infectedByNemesis = CityCalculator().infectedCities(game, nemesisPat)
        val estimatedLifetime = PathogenCalculator().getEstimatedLifetime(nemesisPat)
        val estimatedCost = 20 + 10 * estimatedLifetime
        if (infectedByNemesis.length == 1) {
          LoggingService().logString("#=> Found a Nemesis Pathogen *" + nemesisPat.name  + "* with only one City infected.\n")
          val infectedCity = infectedByNemesis.apply(0)
          if (game.points >= estimatedCost) {
            LoggingService().logString("#=> We have enough Points to put in under Quarantine.\n")
            val isApplyable = infectedCity.events match {
              case a: JsArray => !a.value.exists(js => (js \ "type").as[String] == "quarantine")
            }
            if (isApplyable) {
              LoggingService().logString("# Putting *" + infectedCity.name + "* under Quarantine for " + estimatedLifetime + " Rounds. #\n")
              return putUnderQuarantine(infectedCity.name, estimatedLifetime)
            }
          }
        }
        quarantineLess()
      case _ =>
        quarantineLess()
    }


  }

  def quarantineLess(): String = {
    if (game.round == 1) {
      PathogenCalculator().getPrioritisedAlphaPathogen(game) match {
        case Some(pat) =>
          LoggingService().logString("#=> First Round = Different Rules. Sort by Pathogen Value\n")
          isInfectiveOrSpread(pat)
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
          isInfectiveOrSpread(selectedPathogen)
        case None =>
          LoggingService().logString("#=> No Nemesis Alpha Pathogen found.\n")
          nemesisPathogen()
      }
    }
  }

  def nemesisPathogen(): String = {
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
        if (game.points >= 80) {
          LoggingService().logString("Over 80 Points -> Do some Side Objective.\n")
          sideObjective(maxInfectPathogen)
        } else {
          drugsAvailable(maxInfectPathogen)
        }
      case None =>
        LoggingService().logString("!! No Nemesis Pathogen was found though.. Get ready to drown in despair !!")
        drownInDespair()
    }
  }

  def isAlpha(nemesisPathogens: Vector[PathogenInterface]): String = {
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
        isInfectiveOrSpread(alpha)
      case pathogen =>
        LoggingService().logString("#=> Nemesis Pathogen is not an Alpha Pathogen. Selected Pathogen: " + pathogen.name + "\n")
        drugsAvailable(pathogen)
    }
  }

  def isInfectiveOrSpread(pathogen: PathogenInterface): String = {
    if (pathogen.infectivity == "++" || pathogen.infectivity == "+" || pathogen.infectivity == "o" ||
      (PathogenCalculator().getTotalInfectedDudes(game, pathogen).toFloat / CityCalculator().getTotalPopulation(game).toFloat) >= 0.1) {
      LoggingService().logString("#=> " + pathogen.name + " is infective or spread af! We need a Medicine.\n")
      developMedicine(pathogen)
    } else {
      LoggingService().logString("#=> " + pathogen.name + " seems to be nothing special. I'll slap some Vaccine on it.\n")
      developVaccine(pathogen)
    }
  }

  def drugsAvailable(pathogen: PathogenInterface): String = {
    pathogen match {
      case medPat if PathogenCalculator().hasMedicineAvailable(game, medPat) =>
        LoggingService().logString("#=> Selected Pathogen has Medicine available.\n")
        medDeployable(medPat)
      case vacPat if PathogenCalculator().hasVaccineAvailable(game, vacPat) =>
        LoggingService().logString("#=> Selected Pathogen has Vaccine available.\n")
        vacDeployable(vacPat)
      case druglessPat =>
        LoggingService().logString("#=> Selected Pathogen has no Drugs available. I decide to move to Drugs in Development.\n")
        drugsInDev(druglessPat)
    }
  }

  def drugsInDev(pathogen: PathogenInterface): String = {
    pathogen match {
      case omega if PathogenCalculator().hasMedicine(game, omega) && PathogenCalculator().hasVaccine(game, omega) =>
        LoggingService().logString("#=> Selected Pathogen is Omega Pathogen\n")
        checkCityStandards(omega)
      case medPat if PathogenCalculator().hasMedicine(game, medPat) =>
        CityCalculator().mostInfectedCity(game, medPat) match {
          case Some(c) =>
            if (c.pathogens.apply(pathogen) <= 0.1) {
              LoggingService().logString("#=> Most Infected City is below 10%. Vaccine is needed.\n")
              if (PathogenCalculator().hasVaccineAvailable(game, medPat)) {
                LoggingService().logString("#=> Vac Available - Deploy it.\n")
                vacDeployable(medPat)
              } else if (PathogenCalculator().hasVaccine(game, medPat)) {
                LoggingService().logString("#=> No Vac Available - Develop one.\n")
                developVaccine(medPat)
              } else {
                LoggingService().logString("#=> Vaccine in Dev - Eco.\n")
                endRound()
              }
            } else {
              //LoggingService().logString("#=> Moving to Side Objectives\n")
              //checkCityStandards(medPat)
              if (PathogenCalculator().hasMedicineAvailable(game, medPat)) {
                LoggingService().logString("#=> Deploy Medicine.\n")
                medDeployable(medPat)
              } else {
                LoggingService().logString("#=> Med in Development - Eco\n")
                endRound()
              }
            }
          case None =>
            LoggingService().logString("!! No Most infected City was found though !!\n")
            drownInDespair()
        }
      case vacPat if PathogenCalculator().hasVaccine(game, vacPat) =>
        CityCalculator().infectedCities(game, pathogen).filter(c => !c.vaccines.exists(v => v.pathogen == pathogen)).find(c => c.pathogens.apply(pathogen) >= 0.6) match {
          case Some(c) =>
            LoggingService().logString("#=> Least Infected City is above 60%. Medicine is needed.\n")

            if (PathogenCalculator().hasMedicineAvailable(game, vacPat)) {
              LoggingService().logString("#=> Medicine allready available - Deploy.\n")
              medDeployable(vacPat)
            } else if (!PathogenCalculator().hasMedicine(game, vacPat)) {
              LoggingService().logString("#=> No Medicine available - Develop.\n")
              developMedicine(vacPat)
            } else {
              LoggingService().logString("#=> Medicine in Development - Eco.\n")
              endRound()
            }
          case None =>
            LoggingService().logString("#=> I decide to move to Side Objective Mode\n")
            //checkCityStandards(vacPat)
            if (PathogenCalculator().hasVaccineAvailable(game, vacPat)) {
              LoggingService().logString("#=> Deploy Vac\n")
              vacDeployable(vacPat)
            } else {
              LoggingService().logString("#=> Vac in Development - Eco\n")
              endRound()
            }
        }
    }
  }

  def checkCityStandards(pathogen: PathogenInterface): String = {
    LoggingService().logString("#=> I decided to check on the infected cities standards\n")
    if (CityCalculator().infectedCities(game, pathogen)
      .count(p => p.awareness == "--" || p.awareness == "-" || p.hygiene == "--" || p.hygiene == "-" || p.economy == "--" || p.government == "--") < 20) {
      LoggingService().logString("#=> I decide to move to Side Objectives.\n")
      sideObjective(pathogen)
    } else {
      LoggingService().logString("# Too much Cities.. Ending Round #\n")
      endRound()
    }
  }

  def developMedicine(pathogen: PathogenInterface): String = {
    if (game.points >= 20 && !PathogenCalculator().hasMedicine(game, pathogen)) {
      LoggingService().logString("# I decide to develop a Medicine for " + pathogen.name + " #\n")
      devMed(pathogen.name)
    } else {
      LoggingService().logString("# You don't have enough points to develop a Medicine or already has Med  ->  Eco #\n")
      endRound()
    }
  }

  def developVaccine(pathogen: PathogenInterface): String = {
    if (game.points >= 40 && !PathogenCalculator().hasVaccine(game, pathogen)) {
      LoggingService().logString("# I decide to develop a Vaccine for " + pathogen.name + " #\n")
      devVaccine(pathogen.name)
    } else {
      LoggingService().logString("# You don't have enough points to develop a Vaccine or already has Vac  ->  Eco #\n")
      endRound()
    }
  }

  def medDeployable(pathogen: PathogenInterface): String = {
    CityCalculator().mostInfectedCity(game, pathogen) match {
      case Some(c) =>
        if (c.pathogens.apply(pathogen) <= 0.1) {
          LoggingService().logString("#=> Most Infected City is below 10%. Vaccine is needed.\n")
          developVaccine(pathogen)
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

  def vacDeployable(pathogen: PathogenInterface): String = {
    CityCalculator().infectedCities(game, pathogen).filter(c => !c.vaccines.exists(v => v.pathogen == pathogen)).find(c => c.pathogens.apply(pathogen) >= 0.6) match {
      case Some(c) =>
        LoggingService().logString("#=> Least Infected City is above 60%. Medicine is needed.\n")
        developMedicine(pathogen)
      case None =>
        if (game.points >= 5) {
          CityCalculator().leastInfectedCity(game, pathogen) match {
            case Some(lc) =>
              LoggingService().logString("# I decide to deploy a " + pathogen.name + " Vaccine in " + lc.name + " #\n")
              depVaccine(pathogen.name, lc.name)
            case None =>
              LoggingService().logString("# No Least Infected City found -> Eco #\n")
              endRound()
          }
        } else {
          LoggingService().logString("# You don't have enough Points to deploy Vaccine  ->  Eco #\n")
          endRound()
        }
    }
  }

  def sideObjective(pathogen: PathogenInterface): String = {
    if (game.points >= 3) {
      LoggingService().logString("#=> I am in Side Objective Mode\n")
      checkPolitics(pathogen)
    } else {
      LoggingService().logString("# You don't have enough Points to perform any Side Objective :/ #\n")
      endRound()
    }
  }

  def checkPolitics(pathogen: PathogenInterface): String = {
    CityCalculator().infectedCities(game, pathogen).find(c => c.government == "--") match {
      case Some(c) =>
        LoggingService().logString("# " + c.name + " has -- Government. I decide to call elections. #\n")
        callElections(c.name)
      case None =>
        LoggingService().logString("#=> All Cities have OKish Governments\n")
        checkEconomy(pathogen)
    }
  }

  def checkEconomy(pathogen: PathogenInterface): String = {
    CityCalculator().infectedCities(game, pathogen).find(c => c.economy == "--") match {
      case Some(c) =>
        LoggingService().logString("# " + c.name + " has -- Economy. I decide to exert Influence. #\n")
        exertInfluence(c.name)
      case None =>
        LoggingService().logString("#=> All Cities have a stable Economy.\n")
        checkHygiene(pathogen)
    }
  }

  def checkHygiene(pathogen: PathogenInterface): String = {
    CityCalculator().infectedCities(game, pathogen).find(c => c.hygiene == "--" || c.hygiene == "-") match {
      case Some(c) =>
        LoggingService().logString("# " + c.name + " has bad Hygiene. I decide to apply hygiene measures. #\n")
        applyHygienicMeasures(c.name)
      case None =>
        LoggingService().logString("#=> All Cities have have good hygiene standards\n")
        checkAwareness(pathogen)
    }
  }

  def checkAwareness(pathogen: PathogenInterface): String = {
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

  override def toString: String = {
    "FirstTryDecider"
  }
}
